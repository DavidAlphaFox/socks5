-module(proxy).
-export([start/0]).
start() ->
    ok = application:start(crypto),
    ok = application:start(asn1),
    ok = application:start(public_key),
    ok = application:start(ssl),
    ok = application:start(inets),
    ok = application:start(ranch),
    ok = application:start(compiler),
    ok = application:start(syntax_tools),
    ok = application:start(xmerl),
    ok = application:start(ailib),
    ok = application:start(jiffy),
    ok = application:start(aiconf),
    ok = application:start(proxy).
