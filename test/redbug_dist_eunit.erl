%% -*- mode: erlang; erlang-indent-level: 4 -*-
-module(redbug_dist_eunit).

-include_lib("eunit/include/eunit.hrl").

x_test_() ->
    [?_assertMatch(
        {noconnection,
         [{call, {{erlang, nodes, []}, <<>>}, _, _}|_]},
        runner(
          mk_tracer("erlang:nodes/0", [{time, 3000}]),
          mk_action(100, 100, "erlang:nodes()"))),

     ?_assertMatch(
        {timeout,
         [{call, {{erlang, nodes, []}, <<>>}, _, _}|_]},
        runner(
          mk_tracer("erlang:nodes/0", [{time, 300}]),
          mk_action(100, 100, "erlang:nodes()"))),

     ?_assertMatch(
        "",
        runner(
          mk_tracer(
            ["erlang:setelement(_, {_, file#file_info{type=directory}}, _)",
             "file:read_file_info->return"],
            [{time, 300}, {records, file}]),
          mk_action(100, 100, "setelement(1, file:read_file_info(\"/\"), bla)"))),

     ?_assertMatch(
        "",
        runner(
          mk_tracer(
            ["erlang:setelement(_, {_, file#file_info{type=regular}}, _)",
             "file:read_file_info->return"],
            [{time, 300}, {records, file}]),
          mk_action(100, 100, "setelement(1, file:read_file_info(\"/\"), bla)"))),

     ?_assertMatch(
        "",
        runner(
          mk_tracer(
            ["erlang:setelement(_, {_, file#file_info{type=directory}}, _)",
             "file:read_file_info->return"],
            [{time, 300}]),
          mk_action(100, 100, "setelement(1, file:read_file_info(\"/\"), bla)")))].

mk_tracer(RTP, Opts) ->
    fun(Slave) ->
        Res = redbug:start(RTP, [{target, Slave}, blocking]++Opts),
        receive {pid, P} -> P ! {res, Res} end
    end.

mk_action(PreTO, PostTO, Str) ->
    fun(Slave) ->
        timer:sleep(PreTO),
        rpc:call(Slave, erl_eval, eval_str, [Str++". "]),
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
