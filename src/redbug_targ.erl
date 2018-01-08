%%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% File    : redbug_targ.erl
%%% Author  : Mats Cronqvist <locmacr@mwlx084>
%%% Description :
%%%
%%% Created : 18 Jan 2007 by Mats Cronqvist <locmacr@mwlx084>
%%%-------------------------------------------------------------------
-module(redbug_targ).

-export([start/2]).

%%% runs on host %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start(Node,Cnf) ->
  assert_loaded(Node),
  do_start(Node,dict:store(host_pid,self(),Cnf)).

assert_loaded(Node) ->
  lists:foreach(fun(M) -> ass_loaded(Node,M) end,[?MODULE]).

ass_loaded(nonode@nohost,Mod) -> {module,Mod}=c:l(Mod);
ass_loaded(Node,Mod) ->
  case rpc:call(Node,Mod,module_info,[compile]) of
    {badrpc,{'EXIT',{undef,_}}} ->              %no code
      netload(Node,Mod),
      ass_loaded(Node,Mod);
    {badrpc,_} ->
      ok;
    CompInfo when is_list(CompInfo) ->
      case {ftime(CompInfo),ftime(Mod:module_info(compile))} of
        {interpreted,_} ->
          ok;
        {TargT,HostT} when TargT < HostT -> %old code on target
          netload(Node,Mod),
          ass_loaded(Node,Mod);
        _ ->
          ok
      end
  end.

netload(Node,Mod) ->
  {Mod,Bin,Fname} = code:get_object_code(Mod),
  {module,Mod} = rpc:call(Node,code,load_binary,[Mod,Fname,Bin]).

ftime([]) -> interpreted;
ftime([{time,T}|_]) -> T;
ftime([_|T]) -> ftime(T).

do_start(nonode@nohost,Cnf) ->
  spawn_link(fun() -> init(Cnf) end);
do_start(Node,Cnf) ->
  case net_adm:ping(Node) of
    pang->
      exit(node_down);
    pong ->
      spawn_link(Node,fun() -> init(Cnf) end)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% trace control process
%%% LD = idle | {host_pid,timer,consumer,cnf}
%%% Cnf = {time,flags,rtps,procs,where}
%%% Where = {term_buffer,{Pid,Count,MaxQueue,MaxSize}} |
%%%         {term_stream,{Pid,Count,MaxQueue,MaxSize}} |
%%%         {term_discard,{Pid,Count,MaxQueue,MaxSize}} |
%%%         {file,File,Size,Count} |
%%%         {ip,Port,Queue}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(Cnf) ->
  process_flag(trap_exit,true),
  active(start_trace(Cnf)).

active(Cnf) ->
  Cons = dict:fetch(consumer,Cnf),
  HostPid = dict:fetch(host_pid,Cnf),
  receive
    stop                -> remote_stop(Cons,Cnf);
    {'EXIT',HostPid,_}  -> remote_stop(Cons,Cnf);
    {local_stop,R}      -> local_stop(HostPid,Cnf,R);
    {'EXIT',Cons,R}     -> local_stop(HostPid,Cnf,R);
    _                   -> active(Cnf)
  end.

local_stop(HostPid,Cnf,R) ->
  stop_trace(Cnf),
  HostPid ! {?MODULE,{stopping,R}}.

remote_stop(Consumer,Cnf) ->
  Consumer ! stop,
  stop_trace(Cnf).

stop_trace(Cnf) ->
  erlang:trace(all,false,dict:fetch(flags,Cnf)),
  unset_tps().

start_trace(Cnf) ->
  Rtps = expand_underscores(maybe_load_rtps(dict:fetch(rtps,Cnf))),
  do_start_trace(dict:store(rtps,Rtps,Cnf)).

expand_underscores(Rtps) ->
  lists:foldl(fun expand_underscore/2,[],Rtps).

expand_underscore({{'_','_','_'},MatchSpec,Flags},O) ->
  ExpandModule =
    fun(M,A) -> expand_underscore({{M,'_','_'},MatchSpec,Flags},A) end,
  lists:foldl(ExpandModule,O,modules());
expand_underscore({{M,'_','_'},MatchSpec,Flags},O) ->
  ExpandFunction =
    fun({F,Ari},A) -> expand_underscore({{M,F,Ari},MatchSpec,Flags},A) end,
  lists:foldl(ExpandFunction,O,functions(M));
expand_underscore({{M,F,'_'},MatchSpec,Flags},O) ->
  ExpandArity =
    fun(Ari,A) -> expand_underscore({{M,F,Ari},MatchSpec,Flags},A) end,
  lists:foldl(ExpandArity,O,arities(M,F));
expand_underscore(ExpandedRtp,O) ->
  [ExpandedRtp|O].

modules() ->
  [M || {M,F} <- code:all_loaded(),is_list(F),filelib:is_regular(F)].

functions(M) ->
  locals(M)++globals(M).

arities(M,F) ->
  [Ari || {Fun,Ari} <- functions(M),Fun =:= F].

locals(M) ->
  case code:which(M) of
    preloaded -> [];
    F ->
      case beam_lib:chunks(F,[locals]) of
        {ok,{M,[{locals,Locals}]}} ->
          Locals;
        {error,beam_lib,{missing_chunk,_,_}} ->
          []
      end
  end.

globals(M) ->
  M:module_info(exports).

maybe_load_rtps(Rtps) ->
  lists:foldl(fun maybe_load_rtp/2,[],Rtps).

maybe_load_rtp({{M,_,_},_MatchSpec,_Flags} = Rtp,O) ->
  try
    case code:which(M) of
      preloaded         -> ok;
      non_existing      -> throw(non_existing_module);
      L when is_list(L) -> [c:l(M) || false == code:is_loaded(M)]
    end,
    [Rtp|O]
  catch
    _:_ -> O
  end.

do_start_trace(Cnf) ->
  Consumer = consumer(dict:fetch(where,Cnf),Cnf),
  Ps = lists:foldl(fun mk_prc/2,[],dict:fetch(procs,Cnf)),
  Rtps = dict:fetch(rtps,Cnf),
  Flags = [{tracer,real_consumer(Consumer)}|dict:fetch(flags,Cnf)],
  unset_tps(),
  NoProcs = lists:sum([erlang:trace(P,true,Flags) || P <- Ps]),
  untrace(family(redbug)++family(?MODULE),Flags),
  NoFuncs = set_tps(Rtps),
  assert_trace_targets(NoProcs,NoFuncs,Flags,Ps),
  dict:fetch(host_pid,Cnf) ! {?MODULE,{starting,NoProcs,NoFuncs,Consumer}},
  dict:store(consumer,Consumer,Cnf).

family(Daddy) ->
  try D = whereis(Daddy),
      [D|element(2,process_info(D,links))]
  catch _:_->[]
  end.

untrace(Pids,Flags) ->
  [try erlang:trace(P,false,Flags)
   catch _:R-> erlang:display({R,process_info(P),erlang:trace_info(P,flags)})
   end || P <- Pids,
          is_pid(P),
          node(P)==node(),
          {flags,[]}=/=erlang:trace_info(P,flags)].

assert_trace_targets(NoProcs,NoFuncs,Flags,Ps) ->
  case 0 < NoProcs orelse is_new_pidspec(Ps) of
    true -> ok;
    false-> exit({?MODULE,no_matching_processes})
  end,
  case 0 < NoFuncs orelse is_message_trace(Flags) of
    true -> ok;
    false-> exit({?MODULE,no_matching_functions})
  end.

is_new_pidspec(Ps) ->
  lists:member(new,Ps).

is_message_trace(Flags) ->
  (lists:member(send,Flags) orelse lists:member('receive',Flags)).

unset_tps() ->
  erlang:trace_pattern({'_','_','_'},false,[local,call_count,call_time]),
  erlang:trace_pattern({'_','_','_'},false,[global]).

set_tps(TPs) ->
  lists:foldl(fun set_tps_f/2,0,TPs).

set_tps_f({MFA,MatchSpec,Flags},A) ->
  A+erlang:trace_pattern(MFA,MatchSpec,Flags).

mk_prc(Ps,A) when Ps == running; Ps == all; Ps == new ->
  [Ps|A];
mk_prc(Reg,A) when is_atom(Reg) ->
  case whereis(Reg) of
    Pid when is_pid(Pid) -> mk_prc(Pid,A);
    undefined -> A
  end;
mk_prc({pid,P1,P2},A) when is_integer(P1),is_integer(P2) ->
  mk_prc(c:pid(0,P1,P2),A);
mk_prc(Pid,A) when is_pid(Pid) ->
  case is_process_alive(Pid) of
    true -> [Pid|A];
    false-> A
  end.

real_consumer(C) ->
  Mon = erlang:monitor(process,C),
  C ! {show_port,self()},
  receive
    {'DOWN',Mon,_,C,R} -> exit({no_local_consumer,R});
    Port               -> erlang:demonitor(Mon,[flush]),
                          Port
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
consumer({term_discard,Term},Cnf)    -> consumer_pid(Term,discard,Cnf);
consumer({term_buffer,Term},Cnf)     -> consumer_pid(Term,yes,Cnf);
consumer({term_stream,Term},Cnf)     -> consumer_pid(Term,no,Cnf);
consumer({file,File,Size,Count},Cnf) -> consumer_file(File,Size,Count,Cnf);
consumer({ip,Port,Queue},Cnf)        -> consumer_ip(Port,Queue,Cnf).

consumer_pid({Pid,Cnt,MaxQueue,MaxSize},Buf,Cnf) ->
  C =
    dict:from_list(
      [{daddy,self()},
       {count,Cnt},
       {time,dict:fetch(time,Cnf)},
       {maxsize,MaxSize},
       {maxqueue,MaxQueue},
       {rtps,dict:fetch(rtps,Cnf)},
       {where,Pid},
       {buffering,Buf}]),
  spawn_link(fun() -> init_local_pid(C) end).

consumer_file(File,Size,WrapCount,Cnf) ->
  C =
    dict:from_list(
      [{style,file},
       {file,File},
       {size,Size},
       {wrap_count,WrapCount},
       {time,dict:fetch(time,Cnf)},
       {daddy,self()}]),
  spawn_link(fun() -> init_local_port(C) end).

consumer_ip(Port,QueueSize,Cnf) ->
  C =
    dict:from_list(
      [{style,ip},
       {port_no,Port},
       {queue_size,QueueSize},
       {time,dict:fetch(time,Cnf)},
       {daddy,self()}]),
  spawn_link(fun() -> init_local_port(C) end).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  local consumer process for port-style tracing.
%%%  writes trace messages directly to an erlang port.
%%%  flushes and quits when;
%%%    it gets a stop from the controller
%%%    timeout
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init_local_port(Cnf) ->
  erlang:start_timer(dict:fetch(time,Cnf),self(),dict:fetch(daddy,Cnf)),
  Port = mk_port(Cnf),
  loop_local_port(dict:store(port,Port,Cnf)).

loop_local_port(Cnf) ->
  Daddy = dict:fetch(daddy,Cnf),
  receive
    {show_port,Pid}   -> Pid ! dict:fetch(port,Cnf),
                         loop_local_port(Cnf);
    stop              -> dbg:flush_trace_port(),
                         exit(local_done);
    {timeout,_,Daddy} -> Daddy ! {local_stop,timeout},
                         dbg:flush_trace_port(),
                         exit(timeout)
  end.

mk_port(Cnf) ->
  case dict:fetch(style,Cnf) of
    ip ->
      Port = dict:fetch(port_no,Cnf),
      QueueSize = dict:fetch(queue_size,Cnf),
      (dbg:trace_port(ip,{Port,QueueSize}))();
    file ->
      File = dict:fetch(file,Cnf),
      WrapCount = dict:fetch(wrap_count,Cnf),
      WrapSize = dict:fetch(size,Cnf)*1024*1024,% file size (per file) in MB.
      Suffix = ".trc",
      (dbg:trace_port(file,{File,wrap,Suffix,WrapSize,WrapCount}))()
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  local consumer process for pid-style tracing.
%%%  buffers trace messages, and flushes them when;
%%%    it gets a stop from the controller
%%%    reaches count=0
%%%    timeout
%%%    message queue too long
%%%    a trace message is too big
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
-record(ld,{daddy,where,count,rtps,maxqueue,maxsize}).

init_local_pid(Cnf) ->
  erlang:start_timer(dict:fetch(time,Cnf),self(),dict:fetch(daddy,Cnf)),
  loop_lp({#ld{daddy    =dict:fetch(daddy,Cnf),
               where    =dict:fetch(where,Cnf),
               rtps     =dict:fetch(rtps,Cnf),
               maxsize  =dict:fetch(maxsize,Cnf),
               maxqueue =dict:fetch(maxqueue,Cnf)},
           buffering(dict:fetch(buffering,Cnf)),
           dict:fetch(count,Cnf)}).

buffering(yes) -> [];
buffering(Buf) -> Buf.

loop_lp({LD,Buff,Count}=State) ->
  maybe_exit(msg_queue,LD),
  maybe_exit(msg_count,{LD,Buff,Count}),
  receive
    {timeout,_,Daddy}         -> Daddy ! {local_stop,timeout},
                                 flush(LD,Buff),exit(timeout);
    stop                      -> flush(LD,Buff),exit(local_done);
    {show_port,Pid}           -> Pid ! self(),
                                 loop_lp(State);
    {trace_ts,Pid,Tag,A,TS}   -> loop_lp(msg(LD,Buff,Count,{Tag,Pid,TS,A}));
    {trace_ts,Pid,Tag,A,B,TS} -> loop_lp(msg(LD,Buff,Count,{Tag,Pid,TS,{A,B}}))
  end.

msg(LD,Buff,Count,Item) ->
  maybe_exit(msg_size,{LD,Item}),
  {LD,buff(Buff,LD,Item),Count-1}.

buff(discard,_,_)   -> discard;
buff(no,LD,Item)    -> send_one(LD,Item),no;
buff(Buff,_LD,Item) -> [Item|Buff].

maybe_exit(msg_count,{LD,Buff,0}) ->
  flush(LD,Buff),
  exit(msg_count);
maybe_exit(msg_queue,#ld{maxqueue=MQ}) ->
  maybe_exit_queue(MQ);
maybe_exit(msg_size,{#ld{maxsize=MS},{call,_,_,{MFA,B}}}) when is_binary(B) ->
  maybe_exit_call(MS,MFA,B);
maybe_exit(msg_size,{#ld{maxsize=MS},{call,_,_,MFA}}) ->
  maybe_exit_call(MS,MFA,<<>>);
maybe_exit(_,_) -> ok.

%% check the message queue length
maybe_exit_queue(MQ) ->
  case process_info(self(),message_queue_len) of
    {_,Q} when Q > MQ -> exit({msg_queue,Q});
    _ -> ok
  end.

%% can we handle the call trace msg, or is it too big?
maybe_exit_call(MS,{_M,_F,A},B) ->
  maybe_exit_stack(MS,B),
  maybe_exit_args(MS,A).

%% check the stack binary
maybe_exit_stack(MS,B) ->
  case MS < (Sz=size(B)) of
    true -> exit({stack_size,Sz});
    false-> ok
  end.

%% recurse through the args
%% exit if there is a long list or a large binary
maybe_exit_args(MS,T) when is_tuple(T) ->
  maybe_exit_args(MS,tuple_to_list(T));
maybe_exit_args(MS,L) when length(L) < MS ->
  lists:foreach(fun(E) -> maybe_exit_args(MS,E)end,L);
maybe_exit_args(MS,L) when MS =< length(L) ->
  exit({arg_length,length(L)});
maybe_exit_args(MS,B) when MS < byte_size(B) ->
  exit({arg_size,byte_size(B)});
maybe_exit_args(_,_) ->
  ok.

send_one(LD,Msg) -> LD#ld.where ! [msg(Msg)].

flush(LD,Buffer) ->
  case is_list(Buffer) of
    true  -> LD#ld.where ! lists:map(fun msg/1,lists:reverse(Buffer));
    false -> ok
  end,
  lists:foreach(fun(RTP) -> flush_time_count(RTP,LD#ld.where) end,LD#ld.rtps).

flush_time_count({MFA,_MatchSpec,Flags},Where) ->
  Where ! lists:foldl(fun(Flag,A) -> time_count(MFA,Flag,A) end,[],Flags).

time_count(MFA,Flag,A) when Flag == call_count; Flag == call_time ->
  [{Flag,{MFA,element(2,erlang:trace_info(MFA,Flag))},[],ts(ts())}|A];
time_count(_,_,A) ->
  A.

msg({'send',Pid,TS,{Msg,To}})          -> {'send',{Msg,pi(To)},pi(Pid),ts(TS)};
msg({'receive',Pid,TS,Msg})            -> {'recv',Msg,         pi(Pid),ts(TS)};
msg({'return_from',Pid,TS,{MFA,V}})    -> {'retn',{MFA,V},     pi(Pid),ts(TS)};
msg({'exception_from',Pid,TS,{MFA,V}}) -> {'retn',{MFA,V},     pi(Pid),ts(TS)};
msg({'call',Pid,TS,{MFA,B}})           -> {'call',{MFA,B},     pi(Pid),ts(TS)};
msg({'call',Pid,TS,MFA})               -> {'call',{MFA,<<>>},  pi(Pid),ts(TS)}.

pi(P) when is_pid(P) ->
  try process_info(P,registered_name) of
      [] ->
        case process_info(P,initial_call) of
          {_,{proc_lib,init_p,5}} -> {P,proc_lib:translate_initial_call(P)};
          {_,MFA}                 -> {P,MFA};
          undefined               -> {P,dead}
        end;
      {_,Nam}   -> {P,Nam};
      undefined -> {P,dead}
  catch
    error:badarg -> {P,node(P)}
  end;
pi(P) when is_port(P) ->
  {name,N} = erlang:port_info(P,name),
  [Hd|_] = string:tokens(N," "),
  {P,lists:reverse(hd(string:tokens(lists:reverse(Hd),"/")))};
pi(R) when is_atom(R) -> R;
pi({R,Node}) when is_atom(R),Node == node() -> R;
pi({R,Node}) when is_atom(R),is_atom(Node) -> {R,Node}.

ts(Nw) ->
  {_,{H,M,S}} = calendar:now_to_local_time(Nw),
  {H,M,S,element(3,Nw)}.

-ifdef(USE_NOW).
ts() -> erlang:now().
-else.
ts() -> erlang:timestamp().
-endif.
