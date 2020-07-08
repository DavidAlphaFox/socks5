-module(proxy_protocol).
-export([encode/4,decode/2]).

%% 1 open tunnel  c -> s
%% 2 open tunnel success s -> c
%% 3 tunnel data c -> s or s -> c
%% 4 close tunnel c -> s
%% 5 close tunnel s -> c

encode(ID,Command,Data,Chiper)->
  Payload = <<Command:16/big-unsigned-integer,Data/binary>>,
  {ok,Enc} = proxy_chiper:encrypt(Payload, Chiper),
  {ok,<<ID:64/big-unsigned-integer,Enc/binary>>}.

decode(Payload,Chiper)->
  {ok,Dec} = proxy_chiper:decrypt(Payload, Chiper),
  <<Command:16/big-unsigned-integer,Data/binary>> = Dec,
  {Command,Data}.
