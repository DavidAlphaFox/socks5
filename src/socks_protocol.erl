-module(socks_protocol).

-behavior(ranch_protocol).

-export([start_link/4, init/3]).

-export([pretty_address/1]).
-export([loop/1]).

-include("socks.hrl").

get_client(proxy_utp_client_worker)->
  {ok,'utp_client',undefined}.


start_link(Ref, _Socket,Transport, Opts) ->
  Pid = spawn_link(?MODULE, init, [Ref, Transport, Opts]),
  {ok, Pid}.

init(Ref, Transport, Opts ) ->
  {ok, Socket} = ranch:handshake(Ref),

  Module = maps:get(module,Opts),
  Protocol = maps:get(protocol,Opts),
  {ok,Client,MonitorRef} = get_client(Module),

  erlang:put(Client,MonitorRef),

  {ok,ID} = ai_id:next_id(),
  State = #state{
             auth_methods = [?AUTH_NOAUTH],
             transport = Transport,
             incoming_socket = Socket,
             id = ID,
             client = Client,
             module = Module,
             protocol = Protocol},
    R = Transport:recv(Socket, 1, ?TIMEOUT),
    case R of
      {ok,<<Version>>}->
        case Version of
          ?VERSION5 ->
            {ok,Chiper} = Module:sock_chiper(Client),
            loop(socks5:process(State#state{chiper = Chiper}));
          _ ->
            if MonitorRef /= undefined ->
                erlang:demonitor(MonitorRef);
               true -> ok
            end,
            Transport:close(Socket)
        end;
      {error,_Reason}->
        erlang:demonitor(MonitorRef)
    end.
loop(ok)-> ok;
loop(#state{transport = Transport, incoming_socket = ISocket,
            id = ID,client = Client,chiper = Chiper,
            outgoing_socket = OSocket,
            module = Module,protocol = Protocol} = State) ->
    inet:setopts(ISocket, [{active, once}]),
    {OK, Closed, Error} = Transport:messages(),
    receive
      {OK, ISocket, Data} ->
        {ok,Data0} = Protocol:encode(ID, 3, Data, Chiper),
        do_send(Module, OSocket, Client, Data0),
        ?MODULE:loop(State);
      {Closed, ISocket} ->
        MonitorRef = erlang:get(Client),
        erlang:demonitor(MonitorRef),
        {ok,Data0} = Protocol:encode(ID, 4, <<>>, Chiper),
        Module:close_tunnel(Client, ID, Data0);
      {Error, ISocket, _Reason} ->
        MonitorRef = erlang:get(Client),
        erlang:demonitor(MonitorRef),
        {ok,Data0} = Protocol:encode(ID, 4, <<>>, Chiper),
        Module:close_tunnel(Client, ID, Data0);
      {tunnel_data,Data} ->
        NewState = handle_frame(Data, State),
        ?MODULE:loop(NewState);
      %% 通道断了
      terminate_tunnel ->
        MonitorRef = erlang:get(Client),
        erlang:demonitor(MonitorRef),
        Transport:close(ISocket);
      {'DOWN', _MonitorRef, process, Client, _Info}->
        io:format("Socket closed: ~p~n",[Client]),
        Transport:close(ISocket)
    end.

pretty_address(Addr) when is_tuple(Addr) ->
    inet_parse:ntoa(Addr);
pretty_address(Addr) ->
    Addr.

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_send(Module,Socket,Client,Data)->
  if Socket /= undefined -> Module:write_tunnel(Socket,Data);
     true -> Module:write_tunnel(Client,Data)
  end.
handle_frame(Frame,#state{chiper = Chiper,
                          protocol = Protocol} = State)->
  {Command,Payload} = Protocol:decode(Frame, Chiper),
  handle_frame(Command,Payload,State).
%% 收到了远程的数据
handle_frame(3,Payload,
             #state {
                transport = Transport,
                incoming_socket = ISocket
               } = State) ->
  Transport:send(ISocket, Payload),
  State;
%% 对面主动关闭
handle_frame(5,_,#state{
                    transport = Transport,
                    incoming_socket = ISocket,
                    id = ID,
                    client = Client,
                    module = Module}) ->
  MonitorRef = erlang:get(Client),
  erlang:demonitor(MonitorRef),
  catch Transport:close(ISocket),
  Module:sock_exit(Client, ID),
  ok.
