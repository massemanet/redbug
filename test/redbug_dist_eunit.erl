%% -*- mode: erlang; erlang-indent-level: 4 -*-
-module(redbug_dist_eunit).

-include_lib("eunit/include/eunit.hrl").

x_test_() ->
    [?_assertMatch(
        {noconnection,
         [{call,{{erlang,nodes,[]},<<>>},_,_}|_]},
        runner(mk_tracer("erlang:nodes/0", 3000),
               mk_action(100, 100, erlang, nodes, []))),
     ?_assertMatch(
        {timeout,
         [{call,{{erlang,nodes,[]},<<>>},_,_}|_]},
        runner(mk_tracer("erlang:nodes/0", 300),
               mk_action(100, 100, erlang, nodes, [])))].

mk_tracer(RTP, Timeout) ->
    fun(Slave) ->
        Opts = [{target, Slave}, blocking, {time, Timeout}],
        Res = redbug:start(RTP, Opts),
        receive {pid, P} -> P ! {res, Res} end
    end.

mk_action(PreTO, PostTO, M, F, A) ->
    fun(Slave) ->
        timer:sleep(PreTO),
        rpc:call(Slave, M, F, A),
        timer:sleep(PostTO)
    end.

runner(Tracer, Action) ->
    Opts = [{kill_if_fail, true}, {monitor_master, true}, {boot_timeout, 5}],
    {ok, Slave} = ct_slave:start(eunit_inferior, Opts),
    {Pid, _} = spawn_monitor(fun() -> Tracer(Slave) end),
    Action(Slave),
    {ok, Slave} = ct_slave:stop(Slave),
    Pid ! {pid, self()},
    receive {res, X} -> X after 1000 -> timeout end.

%% mk_interpreted_fun(Str) ->
%%     {ok, Ts, _} = erl_scan:string(Str),
%%     {ok, [AST]} = erl_parse:parse_exprs(Ts),
%%     {value, Fun, []} = erl_eval:expr(AST, []),
%%     Fun.
