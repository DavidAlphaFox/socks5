%%%-------------------------------------------------------------------
%%% @author David Gao <david.alpha.fox@gmail.com>
%%% @copyright (C) 2020, David Gao
%%% @doc
%%%
%%% @end
%%% Created :  7 Jun 2020 by David Gao <david.alpha.fox@gmail.com>
%%%-------------------------------------------------------------------
-module(proxy_utp_server).
-export([start_link/2,init/2,accept/2]).

            

start_link(Port,Options) ->
  Pid = erlang:spawn_link(?MODULE,init,[Port,Options]),
  {ok,Pid}.

init(Port,Options)->
  {ok,Master} = ai_utp:open(Port,Options),
  ok = ai_utp:listen(Master),
  erlang:process_flag(trap_exit,true),
  loop(Master).
loop(Master)->
  Self = self(),
  {ok,Pid} = proc_lib:start_link(?MODULE,accept,[Self,Master]),
  receive
    {accepted,Pid} -> loop(Master);
    {'EXIT',Pid,_} -> loop(Master);
    {'EXIT',Self,_} -> ok;
    _ -> loop(Master)
  end.

accept(Parent,Master)->
  proc_lib:init_ack(Parent, {ok, self()}),
  {ok,Client} = ai_utp:accept(Master),
  Parent ! {accepted,self()},
  erlang:unlink(Parent),
  ai_utp:active(Client,true),
  proxy_utp_server_processor:switch(Client).
