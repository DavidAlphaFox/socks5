-module(proxy_utp_protocol).

-export([new/0,encode/4,decode_frame/2,decode/2]).

-record(state,{buffer = <<>>}).
               

new()-> #state{}.
decode(Payload,Chiper)->
  {ok,Dec} = proxy_chiper:decrypt(Payload, Chiper),
  <<Command:16/big-unsigned-integer,Data/binary>> = Dec,
  {Command,Data}.
encode(ID,Command,Data,Chiper)->
  Payload = <<Command:16/big-unsigned-integer,Data/binary>>,
  {ok,Enc} = proxy_chiper:encrypt(Payload, Chiper),
  Size = erlang:byte_size(Enc),
  {ok,<<Size:32/big-unsigned-integer,
        ID:64/big-unsigned-integer,
        Enc/binary>>}.

decode_frame(Payload,#state{buffer = Buffer} = Ctx)->
  {Rest,Command} = loop(<<Buffer/binary,Payload/binary>>,[]),
  {Command,Ctx#state{buffer = Rest}}.
loop(Payload,Acc)
  when erlang:byte_size(Payload) < 12 ->
  {Payload,lists:reverse(Acc)};
loop(<<Size:32/big-unsigned-integer,
       ID:64/big-unsigned-integer,
       Body/binary>>,Acc)
  when erlang:byte_size(Body) >= Size ->
  <<Payload:Size/binary,Rest/binary>> = Body,
  loop(Rest,[{ID,Payload}|Acc]);
loop(Payload,Acc) -> {Payload,lists:reverse(Acc)}.
