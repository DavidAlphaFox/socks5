-module(proxy_conf).

-export([init/0,get_value/2,get_value/3]).
-export([get_section/2,get_section/1,sections/0]).

-define(CONF,"PROXY_CONF_FILE").
-define(DEFAULT_CONF,"/etc/proxy/default.json").
-define(LOCAL_CONF,"etc/config/default.json").
%% 默认是配置到sys.config中
get_value(Section,Key)-> ai_conf:value(proxy,Section,Key).

get_value(Section,Key,Default)-> ai_conf:value(proxy,Section,Key,Default).

get_section(Section)->ai_conf:section(proxy,Section).

get_section(Section,Default)-> ai_conf:section(proxy,Section,Default).

sections()-> ai_conf:sections(proxy).

init() ->
    Conf = get_config_file(),
    ai_conf:load_conf(proxy,[Conf]).

get_config_file()->
    {ok, CurrentDirectory} = file:get_cwd(),
    Conf = filename:join([CurrentDirectory,?LOCAL_CONF]),
    Exists = filelib:is_regular(Conf),
    case os:getenv(?CONF) of
        false ->
            if
                Exists == true  -> Conf;
                true -> ?DEFAULT_CONF
            end;
        File ->
            {ok, CurrentDirectory} = file:get_cwd(),
            filename:join([CurrentDirectory,File])
    end.
