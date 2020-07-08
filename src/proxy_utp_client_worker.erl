%%%-------------------------------------------------------------------
%%% @author David Gao <david.alpha.fox@gmail.com>
%%% @copyright (C) 2020, David Gao
%%% @doc
%%%
%%% @end
%%% Created :  7 Jun 2020 by David Gao <david.alpha.fox@gmail.com>
%%%-------------------------------------------------------------------
-module(proxy_utp_client_worker).


-export([open_tunnel/4,
         write_tunnel/2,
         close_tunnel/3,
         sock_exit/2,
         sock_chiper/1,
         sock_socket/1]).

%%%===================================================================
%%% API
%%%===================================================================
%% 客户端开启
open_tunnel(_,Tid,Address,Data)->
  Sock = self(),
  Key = <<Address/binary,Tid:64/big-integer>>,
  case proxy_utp_host:alloc(Key) of
    {ok,Server} ->
      gen_server:cast(Server,{open_tunnel,Sock,Tid,Data}),
      {switch,Server};
    _ -> stop
  end.
%% 客户端主动关闭
close_tunnel(Server,Tid,Data)->
  gen_server:call(Server,{close_tunnel,Tid,Data},infinity).

%% 发送数据
write_tunnel(Socket,Data)->
  %%gen_server:cast(Server,{tunnel_data,Data}).
  ok = ai_utp:send(Socket,Data).
%% 远程关闭后，sock通知退出
sock_exit(Server,Tid)->
  gen_server:call(Server,{sock_exit,Tid},infinity).

%% 获取加密参数
sock_chiper(_)->
  Key = proxy_conf:get_value(<<"secret">>, <<"key">>),
  {ok,proxy_chiper:init(Key)}.

sock_socket(Server)->
  {ok,Socket} = gen_server:call(Server,socket,infinity),
  Socket.
