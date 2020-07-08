%%%-------------------------------------------------------------------
%%% @author David Gao <david.alpha.fox@gmail.com>
%%% @copyright (C) 2020, David Gao
%%% @doc
%%%
%%% @end
%%% Created : 20 Jun 2020 by David Gao <david.alpha.fox@gmail.com>
%%%-------------------------------------------------------------------
-module(proxy_utp_host).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).
-export([add/1,alloc/1,release/1,set_context/2]).

-define(SERVER, ?MODULE).
-define(TAB,?MODULE).
-record(state, {master,port,hosts,size,index}).

%%%===================================================================
%%% API
%%%===================================================================
add(Host)-> gen_server:call(?SERVER,{add,Host}).
alloc(Target)->
  Pool = pool(Target),
  case ets:match(?TAB,{Pool,'$1'}) of
    [[Conn]] -> {ok,Conn};
    [] -> gen_server:call(?SERVER,{alloc,Pool})
  end.
release(Pool)->
  gen_server:cast(?SERVER,{release,Pool}).

set_context(Master,Port)->
  gen_server:call(?SERVER,{set_context,Master,Port}).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
        {error, Error :: {already_started, pid()}} |
        {error, Error :: term()} |
        ignore.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
init([]) ->
  ?TAB = ets:new(?TAB,[set,protected,named_table]),
  {ok, #state{hosts = maps:new(),size = 0,index = 0}}.

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
handle_call({add,Host},_From,#state{hosts = Hosts,size = Size} = State)->
  Hosts0 = maps:put(Size,Host,Hosts),
  {reply,ok,State#state{hosts = Hosts0,size = Size + 1}};
handle_call({alloc,Pool},_From,
            #state{master = Master,port = Port} = State)->
  case ets:match(?TAB,{Pool,'$1'}) of
    [[Conn]] -> {reply,{ok,Conn},State};
    [] ->
      {Host0,State0} = fetch(State),
      case supervisor:start_child(proxy_utp_conn_sup,
                                  [Master,Host0,Port,Pool]) of
        {ok,Conn}->
          ets:insert(?TAB, {Pool,Conn}),
          {reply,{ok,Conn},State0};
        Error ->
          {reply,Error,State0}
      end
  end;
handle_call({set_context,Master,Port},_From,State)->
  {reply,ok,State#state{master = Master,port = Port}}.

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
handle_cast({release,Pool},State)->
  ets:delete(?TAB, Pool),
  {noreply,State}.

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
handle_info(_Info, State) ->
  {noreply, State}.

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
terminate(_Reason, _State) ->
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
fetch(#state{size = Size,hosts = Hosts,index = Index} = State)->
  NewIndex = (Index + 1) rem Size,
  {maps:get(Index, Hosts),State#state{index = NewIndex}}.

pool(Host)->
  Pool = proxy_conf:get_value(<<"utp_client">>, <<"pool">>,64),
  erlang:phash2(Host, Pool).
