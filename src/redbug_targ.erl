%%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% File    : redbug_targ.erl
%%% Author  : Mats Cronqvist <masse@cronqvi.st>
%%% Description :
%%%
%%% Created : 18 Jan 2007 by Mats Cronqvist <locmacr@mwlx084>
%%%-------------------------------------------------------------------

%%% @hidden

-module(redbug_targ).

-export([start/2]).
-export([init/1]).
-export([get_rec_fields/2]).

-record(ld, {buffering,
             count,
             dest,
             file,
             flags,
             host_pid,
             loop_fun,
             maxqueue,
             maxsize,
             port_no,
             procs,
             queue_size,
             asts,
             trace_patterns,
             style,
             time,
             timer,
             tracer,
             records,
             where,
             wrap_count,
             wrap_size}).

%%% runs on host %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% unless the target has up to date versions of redbug code, we send it over
%% and load it. Then we start the rarget process.

start(Node, Cnf) ->
  assert_loaded(Node, [?MODULE, redbug_compiler]),
  do_start(Node, mk_ld(Cnf)).

mk_ld(Props) ->
  #ld{host_pid = self(),
      time     = proplists:get_value(time, Props),
      flags    = proplists:get_value(flags, Props),
      asts     = proplists:get_value(asts, Props),
      procs    = proplists:get_value(procs, Props),
      records  = proplists:get_value(records, Props),
      where    = proplists:get_value(where, Props)}.

assert_loaded(Node, Modules) ->
  lists:foreach(fun(M) -> assert_load(Node, M) end, Modules).

assert_load(nonode@nohost, Mod) -> {module, Mod} = c:l(Mod);
assert_load(Node, Mod) ->
  case rpc:call(Node, Mod, module_info, [compile]) of
    {badrpc, {'EXIT', {undef, _}}} ->              %no code
      netload(Node, Mod),
      assert_load(Node, Mod);
    {badrpc, _} ->
      ok;
    CompInfo when is_list(CompInfo) ->
      case {ftime(CompInfo), ftime(Mod:module_info(compile))} of
        {interpreted, _} ->
          ok;
        {TargT, HostT} when TargT < HostT -> %old code on target
          netload(Node, Mod),
          assert_load(Node, Mod);
        _ ->
          ok
      end
  end.

netload(Node, Mod) ->
  {Mod, Bin, Fname} = code:get_object_code(Mod),
  {module, Mod} = rpc:call(Node, code, load_binary, [Mod, Fname, Bin]).

ftime([]) -> interpreted;
ftime([{time, T}|_]) -> T;
ftime([_|T]) -> ftime(T).

do_start(Node, LD) ->
  case Node =:= nonode@nohost orelse net_adm:ping(Node) =:= pong of
    true ->
      Pid = spawn_link(Node, fun init/0),
      Pid ! LD,
      Pid;
    false ->
      exit(node_down)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% trace control process
%%% #ld.where = {buffer, Pid, Count, MaxQueue, MaxSize} |
%%%             {stream, Pid, Count, MaxQueue, MaxSize} |
%%%             {discard, Pid, Count, MaxQueue, MaxSize} |
%%%             {file, File, Size, Count} |
%%%             {ip, Port, Queue}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init() -> no_return().
init() ->
  receive
    LD -> init(LD)
  end.

-define(pipe(A, B, C, D, E, F), F(E(D(C(B(A)))))).

init(LD0) ->
  unset_tps(),
  LD = ?pipe(LD0,
             codegen,
             maybe_load_mods,
             expand_underscores,
             fix_procs,
             consumer),
  [send_meta({recs, get_rec_fields(Mod)}) || Mod <- LD#ld.records],
  NoProcs = start_trace(LD),
  untrace(family(redbug)++family(?MODULE), LD#ld.flags),
  NoFuncs = set_tps(LD#ld.trace_patterns),
  assert_trace_targets(NoProcs, NoFuncs, LD#ld.flags, LD#ld.procs),
  LD#ld.host_pid ! {{starting, self(), NoProcs, NoFuncs}},
  exit((LD#ld.loop_fun)()).

codegen(LD) ->
  LD#ld{trace_patterns = lists:map(fun redbug_compiler:generate/1, LD#ld.asts)}.

-define(fold_field(Rec, Field, Fun, Acc0),
        Rec#ld{Field = lists:foldl(Fun, Acc0, Rec#ld.Field)}).

maybe_load_mods(LD) ->
  ?fold_field(LD, trace_patterns, fun maybe_load_mod/2, []).

maybe_load_mod({{M, _, _}, _, _} = Rtp, O) ->
  try
    case code:which(M) of
      preloaded         -> ok;
      non_existing      -> throw(non_existing_module);
      L when is_list(L) -> [c:l(M) || false =:= code:is_loaded(M)]
    end,
    [Rtp|O]
  catch
    _:_ -> O
  end.

expand_underscores(LD) ->
  ?fold_field(LD, trace_patterns, fun expand_underscore/2, []).

expand_underscore({{'_', '_', '_'}, MatchSpec, Flags}, O) ->
  lists:foldl(mk_expand_module(MatchSpec, Flags), O, modules());
expand_underscore({{M, '_', '_'}, MatchSpec, Flags}, O) ->
  lists:foldl(mk_expand_function(M, MatchSpec, Flags), O, functions(M));
expand_underscore({{M, F, '_'}, MatchSpec, Flags}, O) ->
  lists:foldl(mk_expand_arity(M, F, MatchSpec, Flags), O, arities(M, F));
expand_underscore(ExpandedRtp, O) ->
  [ExpandedRtp|O].

mk_expand_module(MatchSpec, Flags) ->
  fun(M, A) -> expand_underscore({{M, '_', '_'}, MatchSpec, Flags}, A) end.

mk_expand_function(M, MatchSpec, Flags) ->
  fun({F, Ari}, A) -> expand_underscore({{M, F, Ari}, MatchSpec, Flags}, A) end.

mk_expand_arity(M, F, MatchSpec, Flags) ->
  fun(Ari, A) -> expand_underscore({{M, F, Ari}, MatchSpec, Flags}, A) end.

modules() ->
  [M || {M, F} <- code:all_loaded(), is_list(F), filelib:is_regular(F)].

functions(M) ->
  locals(M)++globals(M).

arities(M, F) ->
  [Ari || {Fun, Ari} <- functions(M), Fun =:= F].

locals(M) ->
  case code:get_object_code(M) of
    error -> [];
    {_,Bin,_} ->
      case beam_lib:chunks(Bin,[locals]) of
        {ok,{M,[{locals,Locals}]}} ->
          Locals;
        {error, beam_lib, _} ->
          []
      end
  end.

globals(M) ->
  M:module_info(exports).

fix_procs(LD) ->
  ?fold_field(LD, procs, fun mk_prc/2, []).

mk_prc(Ps, A) when Ps =:= running; Ps =:= all; Ps =:= new ->
  [Ps|A];
mk_prc(Reg, A) when is_atom(Reg) ->
  case whereis(Reg) of
    Pid when is_pid(Pid) -> mk_prc(Pid, A);
    undefined -> A
  end;
mk_prc({pid, P1, P2}, A) when is_integer(P1), is_integer(P2) ->
  mk_prc(c:pid(0, P1, P2), A);
mk_prc(Pid, A) when is_pid(Pid) ->
  case is_process_alive(Pid) of
    true -> [Pid|A];
    false-> A
  end.

family(Daddy) ->
  try D = whereis(Daddy),
        [D|element(2, process_info(D, links))]
  catch _:_-> []
  end.

untrace(Pids, Flags) ->
  [try erlang:trace(P, false, Flags)
   catch _:R -> exit({untrace_error, {R, erlang:trace_info(P, flags)}})
   end || P <- Pids,
          is_pid(P),
          node(P) =:= node(),
          {flags, []} =/= erlang:trace_info(P, flags)].

set_tps(TPs) ->
  lists:foldl(fun set_tp/2, 0, TPs).

set_tp({MFA, MatchSpec, Flags}, A) ->
  Count = erlang:trace_pattern(MFA, MatchSpec, Flags),
  A+Count.

assert_trace_targets(NoProcs, NoFuncs, Flags, Ps) ->
  case 0 < NoProcs orelse is_new_pidspec(Ps) of
    true -> ok;
    false-> exit(no_matching_processes)
  end,
  case 0 < NoFuncs orelse is_message_trace(Flags) of
    true -> ok;
    false-> exit(no_matching_functions)
  end.

is_new_pidspec(Ps) ->
  lists:member(new, Ps).

is_message_trace(Flags) ->
  (lists:member(send, Flags) orelse lists:member('receive', Flags)).

start_trace(LD) ->
  Flags = [{tracer, LD#ld.tracer}|LD#ld.flags],
  lists:sum([erlang:trace(P, true, Flags) || P <- LD#ld.procs]).

stop_trace(LD) ->
  erlang:trace(all, false, LD#ld.flags).

unset_tps() ->
  erlang:trace_pattern({'_', '_', '_'}, false, [local, call_count, call_time]),
  erlang:trace_pattern({'_', '_', '_'}, false, [global]).

send_meta(Msg) ->
  self() ! {meta, Msg}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
consumer(LD = #ld{where = {Buffering, Pid, Cnt, MaxQueue, MaxSize}}) ->
  init_local_pid(LD#ld{count = Cnt,
                       maxsize = MaxSize,
                       maxqueue = MaxQueue,
                       dest = Pid,
                       buffering = Buffering});
consumer(LD = #ld{where = {file, File, Size, WrapCount}}) ->
  init_local_port(LD#ld{style = file,
                        file = File,
                        wrap_size = Size,
                        wrap_count = WrapCount});
consumer(LD = #ld{where = {ip, Port, QueueSize}}) ->
  init_local_port(LD#ld{style = ip,
                        port_no = Port,
                        queue_size = QueueSize}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  local consumer process for port-style tracing.
%%%  writes trace messages directly to an erlang port.
%%%  flushes and quits when;
%%%    it gets a stop from the controller
%%%    timeout
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init_local_port(LD0) ->
  LD = LD0#ld{tracer = mk_port(LD0),
              timer = erlang:start_timer(LD0#ld.time, self(), nul)},
  LD#ld{loop_fun = fun() -> loop_local_port(LD) end}.

loop_local_port(LD = #ld{timer = Timer}) ->
  receive
    stop ->
      stop_trace_port(LD),
      stop;
    {timeout, Timer, _} ->
      stop_trace_port(LD),
      timeout
  end.

stop_trace_port(LD) ->
  stop_trace(LD),
  unset_tps(),
  dbg:deliver_and_flush(LD#ld.tracer).

mk_port(LD) ->
  (dbg:trace_port(LD#ld.style, trace_port_data(LD)))().

trace_port_data(#ld{style = ip, port_no = PortNo, queue_size = QueueSize}) ->
  {PortNo, QueueSize};
trace_port_data(#ld{style = file, file = F, wrap_count = C, wrap_size = S}) ->
  Suffix = ".trc",
  {F, wrap, Suffix, S*1024*1024, C}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  local consumer process for pid-style tracing.
%%%  buffers trace messages, and flushes them when;
%%%    it gets a stop from the controller
%%%    reaches count = 0
%%%    timeout
%%%    message queue too long
%%%    a trace message is too big
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_local_pid(LD0) ->
  LD = LD0#ld{timer = erlang:start_timer(LD0#ld.time, self(), nul),
              tracer = self()},
  LD#ld{loop_fun = fun() -> loop_lp(LD, buffering(LD), LD#ld.count) end}.

buffering(#ld{buffering = Buff}) ->
  case Buff of
    buffer  -> [];
    stream  -> no;
    discard -> discard
  end.

loop_lp(LD, Buff, Count) ->
  maybe_exit_queue(LD),
  maybe_exit_count(LD, Buff, Count),
  receive
    {trace_ts, Pid, Tag, A, TS} ->
      Buf = msg(Buff, LD, {Tag, Pid, TS, A}),
      loop_lp(LD, Buf, Count-1);
    {trace_ts, Pid, Tag, A, B, TS} ->
      Buf = msg(Buff, LD, {Tag, Pid, TS, {A, B}}),
      loop_lp(LD, Buf, Count-1);
    {meta, Meta} ->
      loop_lp(LD, buff(Buff, LD, {meta, Meta}), Count);
    {timeout, _, _} ->
      stop_trace_lp(LD, Buff),
      timeout;
    stop ->
      stop_trace_lp(LD, Buff),
      stop
  end.

stop_trace_lp(LD, Buff) ->
  stop_trace(LD),
  flush(LD, Buff),
  unset_tps().

msg(Buff, LD, Item) ->
  maybe_exit_size(LD, Item),
  buff(Buff, LD, Item).

buff(discard, _, _)   -> discard;
buff(no, LD, Item)    -> send_one(LD, Item), no;
buff(Buff, _LD, Item) -> [Item|Buff].

maybe_exit_count(LD, Buff, Count) ->
  case 0 < Count of
    true -> ok;
    false->
      flush(LD, Buff),
      exit(msg_count)
  end.

%% check queue length
maybe_exit_queue(#ld{maxqueue = MaxQlen}) ->
  case process_info(self(), message_queue_len) of
    {_, Qlen} when Qlen > MaxQlen -> exit({msg_queue, Qlen});
    _ -> ok
  end.

%% check size
%% recurse through the term
%% exit if there is a long list or a large binary
maybe_exit_size(#ld{maxsize = MS}, {_, _, _, Payload}) ->
  maybe_exit_size_loop(MS, Payload).

maybe_exit_size_loop(MS, T) when is_tuple(T) ->
  maybe_exit_size_loop(MS, tuple_to_list(T));
maybe_exit_size_loop(MS, L) when length(L) < MS ->
  lists:foreach(fun(E) -> maybe_exit_size_loop(MS, E)end, L);
maybe_exit_size_loop(MS, L) when MS =< length(L) ->
  exit({arg_length, length(L)});
maybe_exit_size_loop(MS, B) when MS < byte_size(B) ->
  exit({arg_size, byte_size(B)});
maybe_exit_size_loop(_, _) ->
  ok.

send_one(LD, Msg) -> LD#ld.dest ! [msg(Msg)].

flush(LD, Buffer) ->
  case is_list(Buffer) of
    true  -> LD#ld.dest ! lists:map(fun msg/1, lists:reverse(Buffer));
    false -> ok
  end,
  lists:foreach(mk_flush_time_count(LD#ld.dest), LD#ld.trace_patterns).

mk_flush_time_count(Where) ->
  fun({MFA, _MatchSpec, Flags}) ->
      case [time_count(MFA, Flag) || Flag <- Flags, is_counter(Flag)] of
        [] -> ok;
        Msgs -> Where ! Msgs
      end
  end.

is_counter(Flag) ->
  (Flag =:= call_count) orelse (Flag =:= call_time).

time_count(MFA, Flag) ->
  {Flag, {MFA, element(2, erlang:trace_info(MFA, Flag))}, [], ts(ts())}.

msg({'meta', Meta}) ->
  {'meta', Meta, undefined, ts(ts())};
msg({'send', Pid, TS, {Msg, To}}) ->
  {'send', {Msg, pi(To)}, pi(Pid), ts(TS)};
msg({'receive', Pid, TS, Msg}) ->
  {'recv', Msg, pi(Pid), ts(TS)};
msg({'return_from', Pid, TS, {MFA, V}}) ->
  {'retn', {MFA, V}, pi(Pid), ts(TS)};
msg({'exception_from', Pid, TS, {MFA, V}}) ->
  {'retn', {MFA, V}, pi(Pid), ts(TS)};
msg({'call', Pid, TS, {MFA, B}}) ->
  {'call', {MFA, B}, pi(Pid), ts(TS)};
msg({'call', Pid, TS, MFA}) ->
  {'call', {MFA, <<>>}, pi(Pid), ts(TS)}.

pi(P) when is_pid(P) ->
  try process_info(P, registered_name) of
      []        -> {P, mk_unreg_name(P)};
      {_, Name} -> {P, Name};
      undefined -> {P, dead}
  catch error:badarg -> {P, node(P)}
  end;
pi(P) when is_port(P) ->
  try
    {name, N} = erlang:port_info(P, name),
    [Hd|_] = string:tokens(N, " "),
    {P, lists:reverse(hd(string:tokens(lists:reverse(Hd), "/")))}
  catch _:_ -> {P, dead}
  end;
pi(R) when is_atom(R) ->
  R;
pi({R, Node}) when is_atom(R), Node =:= node() ->
  R;
pi({R, Node}) when is_atom(R), is_atom(Node) ->
  {R, Node}.

mk_unreg_name(P) ->
  case process_info(P, initial_call) of
    {_, {proc_lib, init_p, 5}} -> proc_lib:translate_initial_call(P);
    {_, MFA}                   -> MFA;
    undefined                  -> dead
  end.

ts(Nw) ->
  {_, {H, M, S}} = calendar:now_to_local_time(Nw),
  {H, M, S, element(3, Nw)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% records. records are identified by the {module name, record name} tuple.
%% we get the record info (i.e. the list of field names) from the beam file.

get_rec_fields(Mod, Rec) ->
  case lists:keyfind(Rec, 1, get_rec_fields(Mod)) of
    false -> die("no such record", Rec);
    {Rec, Fs} -> Fs
  end.

get_rec_fields(Mod) ->
  get_recs_fields(get_ast(Mod)).

get_ast(Mod) ->
  try
    {ok, _, Chunks} = beam_lib:all_chunks(code:which(Mod)),
    find_ast(Chunks)
  catch
    _:{_, {_, _, {file_error, _, enoent}}} -> die("no such module", Mod);
    _:_ -> die("no debug info", Mod)
  end.

%% beam file format switched from Abst to Dbgi ~20.
find_ast([{"Dbgi", Dbgi}|_]) ->
  {debug_info_v1, _, {AST, _}} = binary_to_term(Dbgi),
  AST;
find_ast([{"Abst", Abst}|_]) ->
  {raw_abstract_v1, AST} = binary_to_term(Abst),
  AST;
find_ast([_|T]) ->
  find_ast(T).

get_recs_fields(AST) ->
  [{R, lists:map(fun get_field/1, F)} || {attribute, _, record, {R, F}} <- AST].

%% there are 4 kinds of record field info; typed/untyped and initialized/not

get_field({typed_record_field, RecordField, _}) -> get_field(RecordField);
get_field({record_field, _, {atom, _, F}}) -> F;
get_field({record_field, _, {atom, _, F}, _}) -> F.

die(Str, Term) ->
  exit({gen_error, lists:flatten(io_lib:format(Str++": ~p", [Term]))}).

-ifdef(USE_NOW).
ts() -> erlang:now().
-else.
ts() -> erlang:timestamp().
-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-include_lib("eunit/include/eunit.hrl").

start_dist() ->
  os:cmd("epmd -daemon"),
  net_kernel:start([eunit_master, shortnames]).

netload_test_() ->
  [fun netload0/0, fun netload1/0, fun netload2/0].

netload0() ->
  ?assertMatch(ok, assert_load(foo, bla)),
  ?assertMatch({module, redbug}, assert_load(nonode@nohost, redbug)).

netload1() ->
  [start_dist() || node() =:= nonode@nohost],
  PeerName = eunit_inferior,
  {ControllingPid, PeerNode} = start_peer(PeerName),
  ?assertMatch(ok, assert_load(PeerNode, redbug_dist_eunit)),
  stop_peer(ControllingPid, PeerNode, PeerName).

netload2() ->
  [start_dist() || node() =:= nonode@nohost],
  PeerName = eunit_inferior,
  {ControllingPid, PeerNode} = start_peer(PeerName),
  Ebin = filename:dirname(code:which(redbug)),
  Test = re:replace(re:replace(Ebin, "default", "test"), "ebin", "test"),
  code:add_patha(unicode:characters_to_list(Test)),
  ?assertMatch(ok, assert_load(PeerNode, redbug_dist_eunit)),
  stop_peer(ControllingPid, PeerNode, PeerName).

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
