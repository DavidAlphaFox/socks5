-module(proxy_chiper).
-export([init/1]).
-export([encrypt/2,decrypt/2]).

-export([create_token/0,verify_token/1]).

-record(proxy_chiper,{
                      passowrd,
                      iv
                     }).
init(Token)->
  SHA = crypto:hash(sha256,Token),
  Bin = ai_string:sha256_string(SHA,lower),
  <<Pwd:8/binary,_M1:11/binary,IV:8/binary,_R/bits>> = Bin,
  #proxy_chiper{passowrd = Pwd,iv = IV}.

encrypt(Data,#proxy_chiper{passowrd = Pwd,iv = IV })->
  Bin = crypto:block_encrypt(blowfish_ofb64,Pwd,IV,Data),
  {ok,Bin}.

decrypt(Data,#proxy_chiper{passowrd = Pwd,iv = IV })->
  Bin = crypto:block_decrypt(blowfish_ofb64,Pwd,IV,Data),
  {ok,Bin}.

create_token()->
  Key = proxy_conf:get_value(<<"secret">>, <<"key">>),
  Token = crypto:strong_rand_bytes(8),
  Token0 = <<Token/binary,Key/binary>>,
  Hash = crypto:hash(sha512, Token0),
  {ok,<<Token/binary,Hash/binary>>}.
verify_token(Payload)->
  Key = proxy_conf:get_value(<<"secret">>, <<"key">>),
  <<Token:8/binary,Hash/binary>> = Payload,
  Token0 = <<Token/binary,Key/binary>>,
  Hash0 = crypto:hash(sha512,Token0),
  Hash0 == Hash.
