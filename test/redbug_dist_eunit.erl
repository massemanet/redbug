%% -*- mode: erlang; erlang-indent-level: 4 -*-
-module(redbug_dist_eunit).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

x_test_() ->
    [?_assertMatch(
        {noconnection,
         [{call, {{erlang, nodes, []}, <<>>}, _, _}|_]},
        runner(
          mk_tracer("erlang:nodes/0", [{time, 3000}]),
          mk_action(100, 100, "erlang:nodes(). "))),

     ?_assertMatch(
        {timeout,
         [{call, {{erlang, nodes, []}, <<>>}, _, _}|_]},
        runner(
          mk_tracer("erlang:nodes/0", [{time, 300}]),
          mk_action(100, 100, "erlang:nodes(). "))),

     ?_assertMatch(
        {timeout,
         [{call,{{file,read_file_info,["/"]},<<>>},_,_},
          {retn,{{file,read_file_info,1},{ok,#{'_RECORD':=file_info}}},_,_},
          {call,{{erlang,setelement,[1,{ok,#{'_RECORD':=file_info}},bla]},<<>>},_,_}]},
        runner(
          mk_tracer(
            ["erlang:setelement(_, {_, file#file_info{type=directory}}, _)",
             "file:read_file_info->return"],
            [{time, 300}, {records, [file]}]),
          mk_action(100, 100, "setelement(1, file:read_file_info(\"/\"), bla). "))),

     ?_assertMatch(
        {timeout,
         [{call,{{file,read_file_info,["/"]},<<>>},_,_},
          {retn,{{file,read_file_info,1},{ok,#{'_RECORD':=file_info}}},_,_}]},
        runner(
          mk_tracer(
            ["erlang:setelement(_, {_, file#file_info{type=regular}}, _)",
             "file:read_file_info->return"],
            [{time, 300}, {records, file}]),
          mk_action(100, 100, "setelement(1, file:read_file_info(\"/\"), bla). "))),

     ?_assertMatch(
        {timeout,
         [{call,{{file,read_file_info,["/"]},<<>>},_,_},
          {retn,{{file,read_file_info,1},{ok,#file_info{}}},_,_},
          {call,{{erlang,setelement,[1,{ok,#file_info{}},bla]},<<>>},_,_}]},
        runner(
          mk_tracer(
            ["erlang:setelement(_, {_, file#file_info{type=directory}}, _)",
             "file:read_file_info->return"],
            [{time, 300}]),
          mk_action(100, 100, "setelement(1, file:read_file_info(\"/\"), bla). ")))].

mk_tracer(RTP, Opts) ->
    fun(PeerNode) ->
        Res = redbug:start(RTP, [{target, PeerNode}, blocking]++Opts),
        receive {pid, P} -> P ! {res, Res} end
    end.

mk_action(PreTO, PostTO, Str) ->
    {done, {ok, Ts, 0}, []} = erl_scan:tokens([], Str, 0),
    {ok, Es} = erl_parse:parse_exprs(Ts),
    Bs = erl_eval:new_bindings(),
    fun(PeerNode) ->
        timer:sleep(PreTO),
        rpc:call(PeerNode, erl_eval, exprs, [Es, Bs]),
        timer:sleep(PostTO)
    end.

runner(Tracer, Action) ->
    os:cmd("epmd -daemon"),
    [net_kernel:start([eunit_master, shortnames]) || node() =:= nonode@nohost],
    PeerName = eunit_inferior,
    {ControllingPid, PeerNode} = start_peer(PeerName),
    {Pid, _} = spawn_monitor(fun() -> Tracer(PeerNode) end),
    Action(PeerNode),
    stop_peer(ControllingPid, PeerNode, PeerName),
    Pid ! {pid, self()},
    receive {res, X} -> X after 1000 -> timeout end.

-ifdef(OTP_24_OR_EARLIER).
start_peer(PeerName) ->
  Opts = [{kill_if_fail, true}, {monitor_master, true}, {boot_timeout, 5}],
  {ok, PeerNode} = ct_slave:start(PeerName, Opts),
  {undefined, PeerNode}.
-else.
-include_lib("common_test/include/ct.hrl").
start_peer(PeerName) ->
  Opts = #{name => PeerName, wait_boot => 5000},
  {ok, Pid, PeerNode} = ?CT_PEER(Opts),
  {Pid, PeerNode}.
-endif.

-ifdef(OTP_20_OR_EARLIER).
stop_peer(_, PeerNode, PeerName) -> {ok, PeerNode} = ct_slave:stop(PeerName).
-else.
-ifdef(OTP_24_OR_EARLIER).
stop_peer(_, PeerNode, _) -> {ok, PeerNode} = ct_slave:stop(PeerNode).
-else.
stop_peer(ControllingPid, _, _) -> ok = peer:stop(ControllingPid).
-endif.
-endif.

%% mk_interpreted_fun(Str) ->
%%     {ok, Ts, _} = erl_scan:string(Str),
%%     {ok, [AST]} = erl_parse:parse_exprs(Ts),
%%     {value, Fun, []} = erl_eval:expr(AST, []),
%%     Fun.
