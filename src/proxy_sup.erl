-module(proxy_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
  Role = proxy_conf:get_value(<<"mode">>,<<"role">>),
  Partition = proxy_conf:get_value(<<"id">>,<<"partition">>,0),
  {ok,Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, [Role,Partition]),
  if
    Role == <<"server">> -> start_server();
    Role == <<"client">> -> start_client();
    true -> throw({error,unknown_role})
  end,
  {ok,Pid}.

init([Role,Partition]) ->
  SupFlags = #{strategy => one_for_one,
               intensity => 1,
               period => 5},
  IDWorker = #{id => ai_id,
               start => {ai_id, start_link, [[{partition,Partition}]]},
               restart => transient,
               shutdown => 5000,
               type => worker,
               modules => [ai_id]},
  UTPServerSup = #{id => proxy_utp_server_sup,
                 start => {proxy_utp_server_sup, start_link, []},
                 restart => transient,
                 shutdown => 5000,
                 type => supervisor,
                 modules => [proxy_utp_server_sup]},

  UTPConnSup = #{id => proxy_utp_conn_sup,
                   start => {proxy_utp_conn_sup, start_link, []},
                   restart => transient,
                   shutdown => 5000,
                   type => supervisor,
                   modules => [proxy_utp_conn_sup]},
  UTPHost = #{id => proxy_utp_host,
              start => {proxy_utp_host, start_link, []},
              restart => transient,
              shutdown => 5000,
              type => worker,
              modules => [proxy_utp_host]},
  Children =
    if
      Role == <<"client">> ->
        [IDWorker,UTPConnSup,UTPHost];
      true -> [UTPServerSup]
    end,
  {ok, {SupFlags, Children}}.


start_server() ->
  ServerConf = proxy_conf:get_section(<<"server">>),
  Port = proplists:get_value(<<"utp_port">> ,ServerConf),
  Options = options(),
  ChildSpec = #{id => proxy_utp_sevrer,
                start => {proxy_utp_server,start_link,[Port,Options]},
                restart => transient,
                shutdown => 5000,
                type => supervisor,
                modules => [proxy_utp_server]},
  supervisor:start_child(?MODULE, ChildSpec).
   

start_client()->
  ClientConf = proxy_conf:get_section(<<"utp_client">>),
  Port = proplists:get_value(<<"port">>,ClientConf),
  ClientPort = proplists:get_value(<<"utp_port">>,ClientConf,0),

  Hosts = proplists:get_value(<<"server_host">>,ClientConf),
  lists:foreach(fun(Host) ->
                      Host0 = erlang:binary_to_list(Host),
                      proxy_utp_host:add(Host0)
                  end,Hosts),
  ServerPort = proplists:get_value(<<"server_port">>,ClientConf),
  Options = options(),
  {ok,Master} = ai_utp:open(ClientPort,Options),

  proxy_utp_host:set_context(Master,ServerPort),
  ranch:start_listener(socks_protocol,ranch_tcp,
                       [{port, Port}],
                       socks_protocol,
                       #{module => proxy_utp_client_worker,
                         protocol => proxy_utp_protocol}).
options()->
  UTPOptions = proxy_conf:get_section(<<"utp_options">>,[]),
  options(UTPOptions,[]).
options([],Acc)-> Acc;
options([{<<"ignore_lost">>,Value}|T],Acc)->
  options(T,[{utp_ignore_lost,Value}|Acc]);
options([{<<"brust">>,Value}|T],Acc)->
  options(T,[{utp_brust,Value}|Acc]).
