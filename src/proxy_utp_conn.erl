%%%-------------------------------------------------------------------
%%% @author David Gao <david.alpha.fox@gmail.com>
%%% @copyright (C) 2020, David Gao
%%% @doc
%%%
%%% @end
%%% Created : 20 Jun 2020 by David Gao <david.alpha.fox@gmail.com>
%%%-------------------------------------------------------------------
-module(proxy_utp_conn).

-behaviour(gen_server).

%% API
-export([start_link/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).
-define(MAX_IDLE,7200000).

-record(state, {socket,
                tunnels,
                monitors,
                master,
                host,
                port,
                target,
                chiper,
                coder,
                timer
               }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link(pid(),string(),integer(),string()) -> {ok, Pid :: pid()} |
        {error, Error :: {already_started, pid()}} |
        {error, Error :: term()} |
        ignore.
start_link(Master,Host,Port,Target) ->
  gen_server:start_link(?MODULE, [Master,Host,Port,Target], []).

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
init([Master,Host,Port,Target]) ->
  self() ! connect,
  Key = proxy_conf:get_value(<<"secret">>, <<"key">>),
  {ok, #state{tunnels = maps:new(),
              monitors = maps:new(),
              host = Host,
              target = Target,
              master = Master,
              port = Port,
              chiper = proxy_chiper:init(Key),
              coder = proxy_utp_protocol:new()}}.

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
handle_call({sock_exit,Tid},_From,State)->
  {reply,ok, demonitor_tunnel(Tid,State)};
handle_call({close_tunnel,Tid,Data},_From,
            #state{socket = Socket} = State)->
  State0 =  demonitor_tunnel(Tid, State),
  to_server(Socket, Data),
  {reply,ok,State0};
handle_call(socket,_From,#state{socket = Socket} = State)->
  {reply,{ok,Socket},State};
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
handle_cast({open_tunnel,Sock,Tid,Data},
            #state{socket = Socket } = State)->
  State0 = monitor_tunnel(Tid,Sock,State),
  try
    to_server(Socket,Data),
    {noreply,State0}
  catch
    _:_ ->
      {stop,normal,State0}
  end;

handle_cast({tunnel_data,Data},#state{socket = Socket} = State)->
    to_server(Socket,Data),
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
handle_info({timeout,TRef,{timer,_}},
            #state{socket = Socket,timer = {set,TRef},
                   tunnels = T} = State)->
  Size = maps:size(T),
  if Size > 0 ->
      {noreply,State#state{timer = undefined}};
     true ->
      catch ai_utp:close(Socket),
      {stop,normal,State#state{socket = undefined}}
  end;
handle_info(connect,#state{master = Master,
                           port = Port,
                           host = Host} = State)->
  case ai_utp:connect(Master,Host,Port) of
    {ok,Socket} ->
      ai_utp:active(Socket,true),
      {noreply,State#state{socket = Socket}};
    _ -> {stop,normal,State}
  end;
handle_info({utp,Socket,Data},
            #state{socket = Socket,coder = Coder} = State)->
  {Commands,Coder0} = proxy_utp_protocol:decode_frame(Data,Coder),
  lists:foreach(
    fun(Command)-> handle_frame(Command,State) end,
    Commands),
  ai_utp:active(Socket,true),
  {noreply,State#state{coder = Coder0}};
handle_info({utp_close,Socket,_},
            #state{socket = Socket} = State)->
  {stop,normal,State#state{
                 socket = undefined,
                 chiper = undefined}};
handle_info({'DOWN', MRef, process, Sock, _Reason}, State) ->
  NewState = terminate_tunnel(Sock,MRef,State),
  {noreply,NewState};
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
terminate(_Reason,#state{target=Address} = State) ->
  proxy_utp_host:release(Address),
  terminate_tunnels(State),
  ok;
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
to_server(undefined,_) -> ok;
to_server(Socket,Data)-> ok = ai_utp:send(Socket,Data).

terminate_tunnels(#state{tunnels = T,monitors = M})->
    lists:foreach(
      fun({_Sock,{MRef,_Tid}})-> erlang:demonitor(MRef,[flush]) end,
      maps:to_list(M)),
    lists:foreach(
      fun({_Tid,Sock})-> Sock ! terminate_tunnel end,
      maps:to_list(T)).

handle_frame({Tid,Data},#state{tunnels = T})->
  case maps:get(Tid,T,undefined) of
    undefined -> ok;
    Sock -> Sock ! {tunnel_data,Data}
  end.

terminate_tunnel(Sock,MRef,#state{socket = Socket,
                                  tunnels = T,monitors = M,
                                  timer = Timer,chiper = Chiper} = State)->
  erlang:demonitor(MRef),
  case maps:get(Sock,M,undefined) of
    undefined -> State;
    {_MRef,Tid} ->
      {ok,Data} = proxy_utp_protocol:encode(Tid,4,<<>>,Chiper),
      to_server(Socket, Data),
      Size = maps:size(T),
      if Size > 1 ->
          State#state{
            tunnels = maps:remove(Tid,T),
            monitors = maps:remove(Sock,M)};
         true ->
          NewTimer = start_tick_timer(?MAX_IDLE, Timer),
          State#state{
            timer = NewTimer,
            tunnels = maps:remove(Tid,T),
            monitors = maps:remove(Sock,M)}
      end
  end.
demonitor_tunnel(Tid,#state{tunnels = T, monitors = M,
                            timer = Timer} = State)->
  case maps:get(Tid,T,undefined) of
    undefined -> State;
    Sock ->
      {MRef,_} = maps:get(Sock,M),
      erlang:demonitor(MRef,[flush]),
      Size = maps:size(T),
      if Size > 1 ->
          State#state{
            tunnels = maps:remove(Tid,T),
            monitors = maps:remove(Sock,M)};
         true ->
          NewTimer = start_tick_timer(?MAX_IDLE, Timer),
          State#state{
            timer = NewTimer,
            tunnels = maps:remove(Tid,T),
            monitors = maps:remove(Sock,M)}
      end
  end.
monitor_tunnel(Tid,Sock,#state{tunnels = T,monitors = M,
                               timer = Timer,
                               host = Host} = State)->
  cancle_tick_timer(Timer),
  MRef = erlang:monitor(process,Sock),
  State#state{
    timer = undefined,
    tunnels = maps:put(Tid,Sock,T),
    monitors = maps:put(Sock,{MRef,Tid},M)}.


start_tick_timer(N,undefined) ->
  Ref = erlang:start_timer(N, self(),{timer,N}),
  {set, Ref};
start_tick_timer(N, {set, Ref}) ->
  erlang:cancel_timer(Ref),
  NewRef = erlang:start_timer(N, self(),{timer,N}),
  {set, NewRef}.

cancle_tick_timer(undefined) -> undefined;
cancle_tick_timer({set,Ref}) ->
  erlang:cancel_timer(Ref),
  undefined.
