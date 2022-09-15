%% -*- mode: erlang; erlang-indent-level: 4 -*-
-module(redbug_dist_eunit).

-export(
   [start_peer/0,
    stop_peer/2]).

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
          mk_action(100, 400, "erlang:nodes(). "))),

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
          mk_action(100, 400, "setelement(1, file:read_file_info(\"/\"), bla). "))),

     ?_assertMatch(
        {timeout,
         [{call,{{file,read_file_info,["/"]},<<>>},_,_},
          {retn,{{file,read_file_info,1},{ok,#{'_RECORD':=file_info}}},_,_}]},
        runner(
          mk_tracer(
            ["erlang:setelement(_, {_, file#file_info{type=regular}}, _)",
             "file:read_file_info->return"],
            [{time, 300}, {records, file}]),
          mk_action(100, 400, "setelement(1, file:read_file_info(\"/\"), bla). "))),

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
          mk_action(100, 400, "setelement(1, file:read_file_info(\"/\"), bla). ")))].

mk_tracer(RTP, Opts) ->
    fun(Peer) ->
        Res = redbug:start(RTP, [{target, Peer}, blocking]++Opts),
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
    {Peer, PeerNodeName} = start_peer(),
    {Pid, _} = spawn_monitor(fun() -> Tracer(PeerNodeName) end),
    Action(PeerNodeName),
    ok = stop_peer(Peer, PeerNodeName),
    Pid ! {pid, self()},
    receive {res, X} -> X after 1000 -> timeout end.

start_peer() ->
    [start_dist(eunit_master) || node() =:= nonode@nohost],
    start_peer(eunit_inferior).

start_dist(Name) ->
    os:cmd("epmd -daemon"),
    net_kernel:start([Name, shortnames]).

-ifdef(USE_PEER).
start_peer(NodeName) ->
    {ok, Peer, Node} = peer:start(#{name => NodeName, peer_down => crash, wait_boot => 5000}),
    {Peer, Node}.
-else.
start_peer(NodeName) ->
    Opts = [{kill_if_fail, true}, {monitor_master, true}, {boot_timeout, 5}],
    {ok, Peer} = ct_slave:start(NodeName, Opts),
    {Peer, NodeName}.
-endif.

-ifdef(USE_PEER).
stop_peer(Peer, _) ->
    ok = peer:stop(Peer).
-elif(OTP_RELEASE).
stop_peer(Peer, _) ->
    {ok, Peer} = ct_slave:stop(Peer),
    ok.
-else.
stop_peer(Peer, NodeName) ->
    {ok, Peer} = ct_slave:stop(NodeName),
    ok.
-endif.

%% mk_interpreted_fun(Str) ->
%%     {ok, Ts, _} = erl_scan:string(Str),
%%     {ok, [AST]} = erl_parse:parse_exprs(Ts),
%%     {value, Fun, []} = erl_eval:expr(AST, []),
%%     Fun.
