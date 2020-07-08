-module(socks5).

-export([process/1]).

-include("socks.hrl").

-define(VERSION, 16#05).
-define(RSV, 16#00).
-define(IPV4, 16#01).
-define(IPV6, 16#04).
-define(DOMAIN, 16#03).

-define(CMD_CONNECT, 16#01).
-define(CMD_BIND, 16#02).
-define(CMD_UDP_ASSOCIATE,  16#03).


-define(REP_SUCCESS, 16#00).
-define(REP_SERVER_ERROR, 16#01).
-define(REP_FORBBIDEN, 16#02).
-define(REP_NET_NOTAVAILABLE, 16#03).
-define(REP_HOST_NOTAVAILABLE, 16#04).
-define(REP_FAILURE, 16#05).
-define(REP_TTL_EXPIRES, 16#06).
-define(REP_CMD_NOTSUPPORTED, 16#07).
-define(REP_ATYP_NOSUPPORTED, 16#08).
-define(REP_UNDEF, 16#FF).


process(State) -> auth(State).

auth(#state{transport = Transport, incoming_socket = ISocket} = State) ->
    {ok, <<NMethods>>} = Transport:recv(ISocket, 1, ?TIMEOUT),
    {ok, Data} = Transport:recv(ISocket, NMethods, ?TIMEOUT),
    doAuth(Data, State).

doAuth(Data, #state{auth_methods = AuthMethods, transport = Transport, incoming_socket = ISocket} = State) ->
    OfferAuthMethods = binary_to_list(Data),
    [Method | _] = lists:filter(fun(E) -> lists:member(E, OfferAuthMethods) end, AuthMethods),
    case Method of
        ?AUTH_NOAUTH -> 
            Transport:send(ISocket, <<?VERSION, Method>>),
            cmd(State);
        _ ->
            Transport:send(ISocket, <<?VERSION, ?AUTH_UNDEF>>),
            throw(auth_not_supported)
    end.

cmd(#state{transport = Transport, incoming_socket = ISocket} = State) ->
  case Transport:recv(ISocket, 4, ?TIMEOUT) of
    {ok, <<?VERSION, CMD, ?RSV, ATYP>>}  -> doCmd(CMD, ATYP, State);
    {error,_} -> ok
  end.

doCmd(?CMD_CONNECT, ATYP, #state{transport = Transport,
                                 incoming_socket = ISocket,
                                 id = ID,
                                 client = Client,
                                 chiper = Chiper,
                                 module = Module,
                                 protocol = Protocol} = State)->
  {ok, Data} = get_address_port(ATYP, Transport, ISocket),
  {{AType,Address} , Port} = parse_addr_port(ATYP, Data),
  AddrLen = erlang:byte_size(Address),
  OpenData = <<AType:8,AddrLen:8/big,Address/binary,Port:16/big>>,
  {ok,Data0} = Protocol:encode(ID, 1, OpenData, Chiper),
  Result =
    case Module:open_tunnel(Client, ID, Address,Data0) of
      {switch,NewClient}->
        NewMonitor = erlang:monitor(process, NewClient),
        erlang:put(NewClient,NewMonitor),
        wait(State#state{client = NewClient});
      stop ->
        Transport:close(ISocket),
        ok;
      _ ->
        wait(State)
    end,
  if Result == ok -> Result;
     true ->
      OSocket = Module:sock_socket(Result#state.client),
      Result#state{outgoing_socket = OSocket}
  end;



doCmd(_Cmd, _, #state{
                  transport = Transport,
                  incoming_socket = ISocket,
                  client = Client
                 }) ->
  ok = Transport:send(ISocket,<<?VERSION,?REP_FORBBIDEN,?RSV>>),
  Transport:close(ISocket),
  MonitorRef = erlang:get(Client),
  erlang:demonitor(MonitorRef),
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_address_port(ATYP, Transport, Socket) ->
  case ATYP of
    ?IPV4 -> Transport:recv(Socket, 6, ?TIMEOUT);
    ?IPV6 -> throw(address_not_supported);
    %% Transport:recv(Socket, 18, ?TIMEOUT);
    ?DOMAIN ->
      {ok, <<DLen>>} = Transport:recv(Socket, 1, ?TIMEOUT),
      {ok, AddrPort} = Transport:recv(Socket, DLen+2, ?TIMEOUT),
      {ok, <<DLen, AddrPort/binary>>};
    true -> throw(unknown_atyp)
  end.

parse_addr_port(?IPV4, <<Addr:4/binary, Port:16>>) ->
  {{?IPV4,Addr}, Port};
parse_addr_port(?IPV6, <<Addr:16/binary, Port:16>>) ->
  {{?IPV6,Addr}, Port};
parse_addr_port(?DOMAIN, <<Len, Addr:Len/binary, Port:16>>) ->
  {{?DOMAIN,Addr}, Port}.

wait(#state{transport = Transport,
            incoming_socket = ISocket,
            id = ID,
            client = Client,
            chiper = Chiper,
            module = Module,
            protocol = Protocol
           } = State) ->
  inet:setopts(ISocket, [{active, once}]),
  {_OK, Closed, Error} = Transport:messages(),
  receive
    {Error,ISocket,_Reason}->
      MonitorRef = erlang:get(Client),
      erlang:demonitor(MonitorRef),
      {ok,Data0} = Protocol:encode(ID, 4, <<>>, Chiper),
      Module:close_tunnel(Client, ID, Data0),
      ok;
    {Closed,ISocket} ->
      MonitorRef = erlang:get(Client),
      erlang:demonitor(MonitorRef),
      {ok,Data0} = Protocol:encode(ID, 4, <<>>, Chiper),
      Module:close_tunnel(Client, ID, Data0),
      ok;
    {tunnel_data,Data}-> handle_frame(Data,State);
    terminate_tunnel ->
      MonitorRef = erlang:get(Client),
      erlang:demonitor(MonitorRef),
      Transport:close(ISocket),
      ok;
    {'DOWN', _MonitorRef, process, Client, _Info}->
      Transport:close(ISocket),
      ok
  end.

handle_frame(Frame,#state{chiper = Chiper,
                          protocol = Protocol} = State)->
  {Command,Payload} = Protocol:decode(Frame, Chiper),
  handle_frame(Command,Payload,State).
handle_frame(2,Payload,#state{
                          transport = Transport,
                          incoming_socket = ISocket
                         } = State)->
  ok = Transport:send(ISocket,
                      <<?VERSION, ?REP_SUCCESS, ?RSV, ?IPV4, Payload/binary>>),
  Transport:setopts(ISocket,[{nodelay,true},
                               {high_msgq_watermark,65535},
                               {high_watermark,65536}]),
  State;
  
handle_frame(5,_,#state{
                    transport = Transport,
                    incoming_socket = ISocket,
                    id = ID,
                    client = Client,
                    module = Module
                   }) ->
  MonitorRef = erlang:get(Client),
  erlang:demonitor(MonitorRef),
  catch Transport:close(ISocket),
  Module:sock_exit(Client, ID),
  ok.
