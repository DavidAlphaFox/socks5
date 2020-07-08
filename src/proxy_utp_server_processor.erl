
%%%-------------------------------------------------------------------
%%% @author David Gao <david.alpha.fox@gmail.com>
%%% @copyright (C) 2020, David Gao
%%% @doc
%%%
%%% @end
%%% Created :  7 Jun 2020 by David Gao <david.alpha.fox@gmail.com>
%%%-------------------------------------------------------------------
-module(proxy_utp_server_processor).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-export([switch/1]).
-define(SERVER, ?MODULE).

-record(state, {socket,
                tunnels,
                monitors,
                chiper,
                coder}).

%%%===================================================================
%%% API
%%%===================================================================
switch(Socket)->
  Key = proxy_conf:get_value(<<"secret">>, <<"key">>),
  State = #state{socket = Socket,
                 tunnels = maps:new(),
                 monitors = maps:new(),
                 chiper = proxy_chiper:init(Key),
                 coder = proxy_utp_protocol:new()
                },
  gen_server:enter_loop(?MODULE, [], State).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link(pid()) -> {ok, Pid :: pid()} |
        {error, Error :: {already_started, pid()}} |
        {error, Error :: term()} |
        ignore.
start_link(Socket) ->
  gen_server:start_link(?MODULE, [Socket], []).

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
init([Socket]) ->
  Key = proxy_conf:get_value(<<"secret">>, <<"key">>),
  {ok, #state{socket = Socket,
              tunnels = maps:new(),
              monitors = maps:new(),
              chiper = proxy_chiper:init(Key),
              coder = proxy_utp_protocol:new()
             }}.

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
handle_cast(_Request, State) -> {noreply, State}.

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

handle_info({utp,Socket,Frame},
            #state{chiper = Chiper,coder = Coder,socket = Socket} = State)->
  {Commands,Coder0} = proxy_utp_protocol:decode_frame(Frame, Coder),
  State0 = lists:foldl(
             fun({Tid,Data},Acc)->
                 {Command,Payload} = proxy_utp_protocol:decode(Data, Chiper),
                 handle_frame(Command,Tid,Payload,Acc)
             end,State#state{coder = Coder0},Commands),
  ai_utp:active(Socket,true),
  {noreply,State0};
handle_info({utp_close,Socket,_},#state{socket = Socket} = State)->
  {stop,normal,State#state{socket = undefined}};
%% 工作进程退出，主动或者异常
handle_info({'DOWN', _MRef, process, Worker, _Reason},
            #state{tunnels = T,monitors = M,
                   socket = Socket,chiper = Chiper} = State)->
  M0 = demonitor_tunnel(Worker, M),
  Tid =
    maps:fold(
      fun(Key,Value,Acc)->
          if (Acc == undefined) and (Value == Worker) -> Key;
             true -> Acc
          end
      end,undefined,T),
  if
    Tid == undefined -> ok;
    true ->
      {ok,Data} = proxy_utp_protocol:encode(Tid, 5, <<>>, Chiper),
      ok = ai_utp:send(Socket,Data)
  end,
  {noreply,State#state{tunnels = maps:remove(Tid, T),
                       monitors = M0}}.

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

terminate(_Reason,#state{tunnels = T,monitors = M,socket = Socket})->
  if Socket /= undefined -> catch ai_utp:close(Socket);
     true -> ok
  end,
  lists:foreach(
    fun({_Tid,Worker})->
        demonitor_tunnel(Worker, M),
        catch proxy_utp_server_worker:close_tunnel(Worker)
    end,maps:to_list(T)),
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
handle_frame(1,Tid,Data,
             #state{tunnels = T,
                    socket = Socket,
                    chiper = Chiper} = State)->
  case maps:get(Tid,T,undefined) of
    undefined -> try_open_tunnel(Tid,Data,State);
    _ ->
      {ok,Binary} = proxy_utp_protocol:encode(Tid, 5, <<>>, Chiper),
      ok = ai_utp:send(Socket,Binary),
      State
  end;
handle_frame(3,Tid,Data
            ,#state{tunnels = T,
                    socket = Socket,
                    chiper = Chiper} = State)->
  case maps:get(Tid,T,undefined) of
    undefined ->
      {ok,Binary} = proxy_utp_protocol:encode(Tid, 5, <<>>, Chiper),
      ok = ai_utp:send(Socket,Binary);
    Worker -> catch proxy_utp_server_worker:tunnel_data(Worker,Data)
  end,
  State;
handle_frame(4,Tid,_Data,
             #state{tunnels = T,monitors = M} = State)->
  case maps:get(Tid,T,undefined) of
    undefined -> State;
    Worker ->
      M0 = demonitor_tunnel(Worker, M),
      catch proxy_utp_server_worker:close_tunnel(Worker),
      State#state{
        tunnels = maps:remove(Tid,T),
        monitors = M0}
  end.
try_open_tunnel(Tid,Data,
                #state{tunnels = T,
                       monitors = M,
                       socket = Socket,
                       chiper = Chiper} = State)->
    try
      %% 新建进程，异步打开
      {ok,Worker} = supervisor:start_child(proxy_utp_server_sup , [self(),Chiper]),
      MRef = erlang:monitor(process, Worker),
      proxy_utp_server_worker:open_tunnel(Worker,Socket,Tid,Data),
      State#state{
        tunnels = maps:put(Tid,Worker,T),
        monitors = maps:put(Worker,{MRef,Tid},M)
       }
    catch
      _Error:_Reason ->
        {ok,Binary} = proxy_utp_protocol:encode(Tid, 5, <<>>, Chiper),
        ok = ai_utp:send(Socket,Binary),
        State
    end.
demonitor_tunnel(Worker,Monitors)->
  case maps:get(Worker,Monitors,undefined) of
    undefined -> Monitors;
    {MRef,_} ->
      erlang:demonitor(MRef,[flush]),
      maps:remove(Worker, Monitors)
  end.

