%%-------------------------------------------------------------------
%%% @author David Gao <david@Davids-MacBook-Pro.local>
%%% @copyright (C) 2019, David Gao
%%% @doc
%%%
%%% @end
%%% Created :  4 Jan 2019 by David Gao <david@Davids-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(proxy_utp_server_worker).

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3, format_status/2]).

-export([open_tunnel/4,tunnel_data/2,close_tunnel/1]).

-define(SERVER, ?MODULE).

-define(IPV4, 16#01).
-define(IPV6, 16#04).
-define(DOMAIN, 16#03).

-define(OPTIONS,[ binary,
	{reuseaddr, true},
	{active, once}
]).

-define(TIMEOUT,30000).


-record(state, {
                parent = undefined,
                monitor = undefined,
                tid = undefined,
                socket = undefined,
                remote = undefined,
                chiper = undefined
               }).

%%%===================================================================
%%% API
%%%===================================================================
open_tunnel(Worker,Socket,Tid,Data)->
	gen_server:cast(Worker,{open_tunnel,Socket,Tid,Data}).
tunnel_data(Worker,Data)->
	gen_server:cast(Worker,{tunnel_data,Data}).
close_tunnel(Worker)->
	gen_server:cast(Worker,close_tunnel).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link(pid(),Chiper :: tuple()) -> {ok, Pid :: pid()} |
	{error, Error :: {already_started, pid()}} |
	{error, Error :: term()} |
	ignore.
start_link(Parent,Chiper) ->
	gen_server:start_link(?MODULE, [Parent,Chiper], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
	{ok, State :: term(), Timeout :: timeout()} |
	{ok, State :: term(), hibernate} |
	{stop, Reason :: term()} |
	ignore.
init([Parent,Chiper]) ->
  Monitor = erlang:monitor(process,Parent),
  {ok, #state{chiper = Chiper,parent = Parent,
              monitor = Monitor}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
	{reply, Reply :: term(), NewState :: term()} |
	{reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
	{reply, Reply :: term(), NewState :: term(), hibernate} |
	{noreply, NewState :: term()} |
	{noreply, NewState :: term(), Timeout :: timeout()} |
	{noreply, NewState :: term(), hibernate} |
	{stop, Reason :: term(), Reply :: term(), NewState :: term()} |
	{stop, Reason :: term(), NewState :: term()}.
handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
	{noreply, NewState :: term()} |
	{noreply, NewState :: term(), Timeout :: timeout()} |
	{noreply, NewState :: term(), hibernate} |
	{stop, Reason :: term(), NewState :: term()}.
handle_cast({open_tunnel,Socket,Tid,Data}, State)->
	do_open_tunnel(Socket,Tid,Data,State);
handle_cast(close_tunnel,#state{remote = Remote} = State)->
	if
		Remote == undefined -> ok ;
		true -> catch ranch_tcp:close(Remote)
	end,
	{stop,normal,State};
handle_cast({tunnel_data,Data},#state{remote = Remote} = State)->
	ok = ranch_tcp:send(Remote,Data),
	{noreply,State};
handle_cast(_Request, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
	{noreply, NewState :: term()} |
	{noreply, NewState :: term(), Timeout :: timeout()} |
	{noreply, NewState :: term(), hibernate} |
	{stop, Reason :: normal | term(), NewState :: term()}.
handle_info({tcp, Remote, Bin},
            #state{tid = Tid,
                   socket = Socket,
                   remote = Remote,
                   chiper = Chiper
                  } = State)->
  {ok,Data} = proxy_utp_protocol:encode(Tid, 3, Bin, Chiper),
  ok = ai_utp:send(Socket,Data),
  ranch_tcp:setopts(Remote, [{active, once}]),
  {noreply, State};
%% 远程主动关闭
handle_info({tcp_closed, Remote}, #state{remote = Remote} = State) ->
    {stop,normal,State#state{remote = undefined}};

handle_info({'DOWN',MRef,process,Parent,_Reason},
            #state{parent = Parent,monitor = MRef} = State) ->
  {stop,normal,State#state{parent = undefined,monitor = undefined}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
	State :: term()) -> any().
terminate(_Reason,#state{remote = Remote,monitor = Monitor })->
  if Remote /= undefined ->
      ranch_tcp:close(Remote);
     true -> ok
  end,
  if Monitor /= undefined ->
      erlang:demonitor(Monitor);
     true -> ok
  end,
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
	State :: term(),
	Extra :: term()) -> {ok, NewState :: term()} |
	{error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
	Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
	Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
target(Data)->
    <<AType:8,AddrLen:8/big,Rest/bits>> = Data,
    <<Addr:AddrLen/binary,Port:16/big>> = Rest,
	Address = case AType of
		?IPV4 ->
			erlang:list_to_tuple(erlang:binary_to_list(Addr));
		?DOMAIN ->
			erlang:binary_to_list(Addr)
		end,
	{Address,Port}.

do_open_tunnel(Socket,Tid,Data,
               #state{chiper = Chiper} = State)->
	{Address,Port} = target(Data),

	try
		Result = ranch_tcp:connect(Address, Port, ?OPTIONS, ?TIMEOUT),
		case Result of
			{ok, RemoteSocket} ->
        NewState =
          State#state{
            tid = Tid,
            socket = Socket,
            remote = RemoteSocket
           },
        {ok,{BindAddr,BindPort}} = inet:sockname(RemoteSocket),
        BindAddr0 = erlang:list_to_binary(erlang:tuple_to_list(BindAddr)),
        Bind = <<BindAddr0/binary,BindPort:16/big>>,
        {ok,Data0} = proxy_utp_protocol:encode(Tid, 2, Bind, Chiper),
        ok = ai_utp:send(Socket,Data0),
				ranch_tcp:setopts(RemoteSocket, [{active, once}]),
				{noreply,NewState};
			Error -> {stop,Error,State}
		end
	catch
		Reason:Error1 -> {stop,{Reason,Error1},State}
	end.
