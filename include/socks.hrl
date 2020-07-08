-record(state, {
                incoming_socket:: gen_tcp:socket(),
                auth_methods,
                transport :: module(),
                id :: integer(),
                client,
                chiper,
                module,
                protocol,
                outgoing_socket
               }).

-define(TIMEOUT, timer:seconds(15)).
-define(AUTH_NOAUTH, 16#00).
-define(AUTH_UNDEF, 16#FF).

-define(VERSION4, 16#04).
-define(VERSION5, 16#05).
