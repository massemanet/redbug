%% -*- mode: erlang; erlang-indent-level: 4 -*-
-module(redbug_dist_eunit).

-include_lib("eunit/include/eunit.hrl").

x_test_() ->
    [?_assertEqual([],setup())].

setup() ->
    {ok, Host} = inet:gethostname(),
    {ok, Slave} = slave:start_link(Host, bla),
    Pid = spawn(
            fun() -> Res = redbug:start("erlang:nodes/0",[{target,Slave},blocking,{time,300}]),
                     receive P -> P ! Res end
            end),
    timer:sleep(100),
    [_] = rpc:call(Slave,erlang,nodes,[]),
    timer:sleep(100),
    ok = slave:stop(Slave),
    Pid ! self(),
    receive X -> X after 1000 -> timeout end.
