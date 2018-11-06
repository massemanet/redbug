%%%-------------------------------------------------------------------
%%% File    : redbug_dtop.erl
%%% Created :  5 Sep 2005
%%% Created : 16 Dec 2003
%%% Created :  5 Dec 2003 by Mats Cronqvist <qthmacr@duna283>
%%% Created :  8 Dec 2003 by Mats Cronqvist <qthmacr@duna283>
%%% Created : 18 Oct 2005 by Mats Cronqvist <locmacr@mwlx084>
%%% Description : top-like client for beam
%%%-------------------------------------------------------------------
-module(redbug_dtop).
-author('Mats Cronqvist').

-export([start/0,
         stop/0,
         sort/1,
         max_procs/1]).

start() -> spawn(fun() -> loop(init()) end).

stop() -> exit(whereis(redbug_dtop), kill).

sort(Col) -> redbug_dtop ! {sort, Col}.

max_procs(MaxProcs) ->  redbug_dtop ! {max_procs, MaxProcs}.

%%%---------------------------
-record(ld, {fd=standard_io,
             sort=cpu,
             tick=2000,
             lines=19,
             strategy=strategy(),
             total_ram=0,
             constants=[],
             cores=1,
             cache=[],
             now=erlang:timestamp(),
             procs=6,
             max_procs=3000}).

init() ->
    register(redbug_dtop, self()),
    constants(#ld{}).

loop(LD) ->
    timer:sleep(LD#ld.tick),
    Data = [get_sys_data(LD), get_prc_data(LD), get_net_data(LD), get_mnesia_data(LD)],
    TS = erlang:timestamp(),
    print(LD, differ(timer:now_diff(TS, LD#ld.now)/1000000, LD#ld.cache, Data)),
    loop(LD#ld{now=TS, cache=Data}).

print(#ld{fd=FD, sort=Sort, lines=Lines}, {SysData, PrcData, _, _}) ->
    print_del(FD),
    print_sys(FD, SysData),
    lwrite(FD, "~n", []),
    print_tags(FD),
    print_procs(FD, Lines, SysData, which_sort(Sort, PrcData)).

print_sys(FD, Sys) ->
    lwrite(FD, "~s~n", [sys_str(Sys)]),
    lwrite(FD, memf(), memi(Sys)).

memf() ->
    "memory:      proc~8s, atom~8s, bin~8s, code~8s, ets~8s~n".

memi(Sys) ->
    try [human(lks(T, Sys)) || T <- [processes, atom, binary, code, ets]]
    catch _:_ -> ["", "", "", "", ""]
    end.

sys_str(Sys) ->
    {_, Time} = calendar:now_to_local_time(lks(now, Sys)),
    H         = pad(element(1, Time), 2, $0, left),
    M         = pad(element(2, Time), 2, $0, left),
    S         = pad(element(3, Time), 2, $0, left),
    Node      = to_list(lks(node, Sys)),
    MEMbeam   = human(lks(beam_vsz, Sys, 0)),
    MEM       = human(lks(total, Sys)),
    CPUbeam   = to_list(100*(lks(beam_user, Sys, 0)+lks(beam_kernel, Sys, 0))),
    CPU       = to_list(100*(lks(user, Sys, 0)+lks(kernel, Sys, 0))),
    Procs     = human(lks(procs, Sys)),
    RunQ      = human(lks(run_queue, Sys)),

    SYS = lists:sublist(lists:append(["size: "    , MEM,
                                      "("         , MEMbeam,
                                      "), cpu%: " , CPUbeam,
                                      "("         , CPU,
                                      "), procs: ", Procs,
                                      ", runq: "  , RunQ,
                                      ", ", H, ":", M, ":", S]), 79),
    pad(Node, 79-length(SYS), $ , right)++SYS.

pad(Item, Len, Pad, LeftRight) ->
    I = to_list(Item),
    case length(I) of
        L when L=:=Len -> I;
        L when L<Len -> case LeftRight of
                            left -> lists:duplicate(Len-L, Pad)++I;
                            right-> I++lists:duplicate(Len-L, Pad)
                        end;
        _ -> lists:sublist(I, Len)
    end.

print_del(FD) ->
    lwrite(FD, "~s~n", [lists:duplicate(79, $-)]).

format() -> "~-14s ~-28s ~-17s~7s~7s~4s~n".

tags() -> ["pid", "name", "current", "msgq", "mem", "cpu"].

print_tags(FD) ->
    lwrite(FD, format(), tags()).

%%make ints human readable
human(X) when not is_number(X) -> X;
human(I) when I < 0 -> "-"++human(-I);
human(I) when 0 < I ->
  case math:log10(I) of
    M when 15=<M -> human(M-15,"P");
    M when 12=<M -> human(M-12,"T");
    M when  9=<M -> human(M-9,"G");
    M when  6=<M -> human(M-6,"M");
    M when  3=<M -> human(M-3,"k");
    _            -> flat("~w",[I])
  end;
human(_) -> "0".

human(E,M) ->
  flat("~.1f~s",[math:pow(10,E),M]).

which_sort( cpu, PrfPrc) -> expand(lks(dreds, PrfPrc), lks(info, PrfPrc));
which_sort(msgq, PrfPrc) -> expand(lks( msgq, PrfPrc), lks(info, PrfPrc));
which_sort(dmem, PrfPrc) -> expand(lks( dmem, PrfPrc), lks(info, PrfPrc));
which_sort( mem, PrfPrc) -> expand(lks(  mem, PrfPrc), lks(info, PrfPrc)).

expand(Pids, Infos) ->
    lists:reverse([[{pid, Pid}|lks(Pid, Infos)] || Pid <- Pids]).

print_procs(FD, Items, PrfSys, Prcs) ->
    CpuPerRed = cpu_per_red(PrfSys),
    [procsI(FD, P, CpuPerRed) || P <- resize(Items, Prcs)].

resize(no_pad, Prcs) ->
    Prcs;
resize(Items, Prcs) ->
    case Items < length(Prcs) of
        true -> lists:sublist(Prcs, Items);
        false-> Prcs++lists:duplicate(Items-length(Prcs), [])
    end.

cpu_per_red(Sys) ->
    CPU =
        case lks(beam_user, Sys, 1)+lks(beam_kernel, Sys, 0) of
            0.0 -> 1;
            C -> C
        end,
    case lks(reductions, Sys) of
        0    -> 0;
        Reds -> 100*CPU/Reds
    end.

procsI(FD, PP, CpuPerRed) ->
    try
        lwrite(FD,
               format(),
               [pidf(to_list(lks(pid, PP))),
                funf(reg(PP)),
                funf(lks(current_function, PP)),
                human(lks(message_queue_len, PP)),
                human(lks(memory, PP)),
                to_list(lks(dreductions, PP)*CpuPerRed)])
    catch
        _:_ -> lwrite(FD, "~n", [])
    end.

reg(PP) ->
    case lks(registered_name, PP) of
        [] -> lks(initial_call, PP);
        Val -> Val
    end.

pidf(Pid) ->
    [_, A, B] = string:tokens(Pid, "."),
    lists:append(["<0.", A, ".", B]).

funf({M, F, A}) -> to_list(M)++":"++to_list(F)++"/"++to_list(A);
funf(Term) -> io_lib:fwrite("~p", [Term]).

to_list(A) when is_list(A) -> A;
to_list(A) when is_pid(A) -> pid_to_list(A);
to_list(A) when is_atom(A) -> atom_to_list(A);
to_list(A) when is_float(A) -> to_list(round(A));
to_list(A) when is_tuple(A) -> tuple_to_list(A);
to_list(A) when is_integer(A) -> integer_to_list(A).

lwrite({Tab, LineNo}, Format, As) when is_atom(Tab) ->
    L = ets:update_counter(Tab, LineNo, 1),
    ets:insert(Tab, {L, flat(Format, As)});
lwrite(FD, Format, As) ->
    io:fwrite(FD, Format, As).

flat(F, A) ->
    lists:flatten(io_lib:fwrite(F, A)).

%%----------------------------------------------------
%% calculate rates

differ(_, _, []) ->
    [];
differ(DT, [], [New|News]) ->
    [[{K, V, 0} || {K, V} <- New]|differ(DT, [], News)];
differ(DT, [Old|Olds], [New|News]) ->
    [diff(DT, Old, New)|differ(DT, Olds, News)].

diff(DT, [{Tag, OldVal}|Olds], [{Tag, NewVal}|News])->
    [{Tag, NewVal, df(DT, OldVal, NewVal)}|diff(DT, Olds, News)].

df(0, _, _) -> 0;
df(DT, X, Y) -> (Y-X)/DT.

lks(Tag, TVs, Def) ->
    try lks(Tag, TVs)
    catch {not_found, _} -> Def
    end.

lks(Tag, [])             -> throw({not_found, Tag});
lks(Tag, [{Tag, Val}|_]) -> Val;
lks(Tag, [_|List])       -> lks(Tag, List).
%%--------------------------------------------------------------------------
%% calculate process toplist
%%% Dreds, Dmems, Mems and Msgqs are sorted lists of pids
%%% PidInfo is a sorted list of {Pid, Info}
%%% Info is a list of tagged tuples {atom(), number()}

-define(INFO_ITEMS, [current_function, initial_call, registered_name, last_calls,
                     stack_size, heap_size, total_heap_size]).

select(LD = #ld{cache={Then, Olds}}, {Now, Curs}, Items) ->
    {DredL, DmemL, MemL, MsgqL} = topl(Olds, Curs, outf(Then, Now, Items), empties()),
    PidInfo = lists:usort([I || {_, I} <-lists:append([DredL, DmemL, MemL, MsgqL])]),
    [{node, node()},
     {dreds, e1e2(DredL)},
     {dmem, e1e2(DmemL)},
     {mem, e1e2(MemL)},
     {msgq, e1e2(MsgqL)},
     {info, complete(PidInfo)}].

e1e2(List) -> [E || {_, {E, _}} <- List].

complete(List) ->
    [{Pid, Info++pid_info(Pid, ?INFO_ITEMS)} || {Pid, Info} <- List].

topl([], _, _, Out) -> Out;
topl(_, [], _, Out) -> Out;
topl(Os=[{Po, _}|_], [{Pc, _}|Cs], Outf, Out) when Pc<Po -> topl(Os, Cs, Outf, Out);
topl([{Po, _}|Os], Cs=[{Pc, _}|_], Outf, Out) when Po<Pc -> topl(Os, Cs, Outf, Out);
topl([{P, Io}|Os], [{P, Ic}|Cs], Outf, Out) -> topl(Os, Cs, Outf, Outf(P, Io, Ic, Out)).

empties() -> {[], [], [], []}.

outf(Then, Now, Items) ->
    NowDiff = timer:now_diff(Now, Then)/1000000,
    fun(P, Io, Ic, Out) -> out(P, NowDiff, Io, Ic, Out, Items) end.

out(P, NowDiff, Io, Ic, O={Odred, Omem, Odmem, Omsgq}, Items) ->
    try
        Dred = dred(NowDiff, Io, Ic),
        Dmem = dmem(NowDiff, Io, Ic),
        Info = {P, [{dreductions, Dred}, {dmemory, Dmem}|Ic]},
        {new_topl(Odred, {Dred, Info}, Items),
         new_topl(Odmem, {Dmem, Info}, Items),
         new_topl(Omem, {mem(Ic), Info}, Items),
         new_topl(Omsgq, {msgq(Ic), Info}, Items)}
    catch
        _:_ -> O
    end.

new_topl(Top, {Item, _}, _Items) when 0 =:= Item; 0.0 =:= Item ->
    Top;
new_topl(Top, El, Items) when length(Top) < Items ->
    lists:sort([El|Top]);
new_topl(Top, El, _Items) ->
    case El < hd(Top) of
        true -> Top;
        false-> tl(lists:sort([El|Top]))
    end.


%%%-------------------------------------------------------------------
%% collects info about the OS and the Erlang system.
%% * emulator info
%%   call the BIFs memory/0, statistics/1 and system_info/1
%% * OS info
%%   if OS is Linux 2.6 or greater;
%%     - read from /proc/stat, /proc/meminfo (and /proc/net/dev ?)
%%     - read from /proc/self/stat (and /proc/self/statm ?)
%%   else if OS is darwin
%%     - run the ps command in a port
%%   else
%%     - return an empty list
%%
%% returns a list of tagged tuples
%%
%% tag                  [unit]    source
%% node                 [atom()]  erlang:node()
%% now                  [now()]   prfTime:ts()
%% procs                [count]   erlang:system_info(process_count)
%% context_switches     [count/s] erlang:statistics(context_switches)
%% gcs                  [count/s] erlang:statistics(garbage_collection)
%% gc_reclaimed         [byte/s]  erlang:statistics(garbage_collection)
%% io_in                [byte/s]  erlang:statistics(io)
%% io_out               [byte/s]  erlang:statistics(io)
%% reductions           [count/s] erlang:statistics(reductions)
%% run_queue            [count]   erlang:statistics(run_queue)
%% total                [byte]    erlang:memory()
%% processes            [byte]    erlang:memory()
%% processes_used       [byte]    erlang:memory()
%% system               [byte]    erlang:memory()
%% atom                 [byte]    erlang:memory()
%% atom_used            [byte]    erlang:memory()
%% binary               [byte]    erlang:memory()
%% code                 [byte]    erlang:memory()
%% ets                  [byte]    erlang:memory()
%% user                 [frac]    /proc/stat
%% nice                 [frac]    /proc/stat
%% kernel               [frac]    /proc/stat
%% idle                 [frac]    /proc/stat
%% iowait               [frac]    /proc/stat
%% ctxt                 [frac]    /proc/stat
%% beam_user,           [frac]    /proc/self/stat
%% beam_kernel,         [frac]    /proc/self/stat
%% beam_vsz             [byte]    /proc/self/stat
%% beam_rss             [pages]   /proc/self/stat
%% beam_minflt          [count/s] /proc/self/stat
%% beam_majflt          [count/s] /proc/self/stat
%% total_ram            [byte]    /proc/meminfo


get_sys_data(LD) -> LD#ld.constants++stats()++os_info(LD#ld.strategy).

stats() ->
    Procs                           = erlang:system_info(process_count),
    {Ctxt, 0}                       = erlang:statistics(context_switches),
    {GCs, GCwords, 0}               = erlang:statistics(garbage_collection),
    {{input, IoIn}, {output, IoOut}} = erlang:statistics(io),
    {Reds, _}                       = erlang:statistics(reductions),
    RunQ                            = erlang:statistics(run_queue),

    [{now, prfTime:ts()},
     {procs, Procs},
     {context_switches, Ctxt},
     {gcs, GCs},
     {gc_reclaimed, GCwords*4},
     {io_in, IoIn},
     {io_out, IoOut},
     {reductions, Reds},
     {run_queue, RunQ} |
     erlang:memory()].

constants(LD = #ld{strategy={linux, ProcStat, _}}) ->
    LD#ld{constants=[{total_ram, total_ram()}, {cores, cores(ProcStat)}]};
constants(LD) ->
    LD.

strategy() ->
    case os:type() of
        {unix, linux}  -> init_linux();
        {unix, darwin} -> init_ps();
        _              -> {}
    end.

%% OS info
os_info({linux, ProcStat, ProcSelfStat}) ->
    proc_stat(ProcStat)++proc_self_stat(ProcSelfStat);
os_info({ps, Port, Cmd}) ->
    do_ps(Port, Cmd);
os_info(_) ->
    [].

proc_stat(FD) ->
    %%user nice kernel idle iowait irq softirq steal
    {ok, Str} = file:pread(FD, 0, 200),
    [User, Nice, Kernel, Idle, Iowait] =
        case string:tokens(Str, " \n") of
            ["cpu", I1, I2, I3, I4, I5|_] -> [I1, I2, I3, I4, I5];
            _                        -> [0, 0, 0, 0, 0]
        end,
    lists:zip([user, nice, kernel, idle, iowait],
              [jiffy_to_sec(J) || J <- [User, Nice, Kernel, Idle, Iowait]]).

proc_self_stat(FD) ->
%%% pid, comm, state, ppid, pgrp, session, tty_nr, tpgid, flags,
%%% minflt, cminflt, majflt, cmajflt, utime, stime, cutime, cstime,
%%% priority, nice, num_threads, itrealvalue, starttime, vsize, rss
    {ok, Str} = file:pread(FD, 0, 200),
    {Minflt, Majflt, Utime, Stime, Vsz, Rss} =
        case string:tokens(Str, " ") of
            [_, _, _, _, _, _, _, _, _, I10, _, I12, _, I14, I15, _, _, _, _, _, _, _, I23, I24|_] ->
                {I10, I12, I14, I15, I23, I24};
            _ ->
                {0, 0, 0, 0, 0, 0}
        end,
    lists:zip([beam_user, beam_kernel, beam_vsz, beam_rss, beam_minflt, beam_majflt],
              [jiffy_to_sec(Utime), jiffy_to_sec(Stime),
               to_int(Vsz),   %% in bytes
               to_int(Rss),   %% in pages...
               to_int(Minflt), to_int(Majflt)]).

jiffy_to_sec(J) ->
    to_int(J)/100. %should use a better transform jiffies->secs

to_int("-") -> 0;
to_int(J) -> list_to_integer(J).

init_linux() ->
    {ok, ProcStat} = file:open("/proc/stat", [read, raw]),
    {ok, ProcSelfStat} = file:open("/proc/self/stat", [read, raw]),
    {linux, ProcStat, ProcSelfStat}.

cores({linux, ProcStat, _}) ->
    {ok, Str} = file:pread(ProcStat, 0, 1000),
    Toks = string:tokens(Str, "\n"),
    case length(lists:takewhile(fun(S)->lists:prefix("cpu", S) end, Toks)) of
        1 -> 1;
        M -> M-1
    end;
cores(_) ->
    1.

total_ram() ->
    case file:open("/proc/meminfo", [read, raw]) of
        {ok, FD} ->
            try {ok, Str} = file:pread(FD, 0, 30),
                 ["MemTotal:", T, "kB"|_] = string:tokens(Str, " \n"),
                 list_to_integer(T)*1024
            catch _:_ -> 0
            after file:close(FD)
            end;
        _ -> 0
    end.

init_ps() ->
    {ps,
     open_port({spawn, "/bin/sh"}, [stream]),
     "ps -o pid, utime, time, vsz, rss, majflt, minflt -p "++os:getpid()++"\n"}.

do_ps(Port, Cmd) ->
    case port_command(Port, Cmd, []) of
        true ->
            receive
                {Port, {data, Data}} ->
                    case[string:tokens(L, " ")||L<-string:tokens(Data, "\n")] of
                        [["PID", "UTIME", "TIME", "VSZ", "RSS", "MAJFLT", "MINFLT"],
                         %%           ["1", "0:00.20", "0:00.30", "2488932", "12600", "-", "-"]]
                         [_, Utime, Time, Vsz, Rss, MajFault, MinFault]] ->
                            UtimeSec = timestr_to_sec(Utime),
                            TimeSec =  timestr_to_sec(Time),              % system+user time
                            [{beam_user, UtimeSec},
                             {beam_kernel, TimeSec-UtimeSec},
                             {beam_vsz, to_int(Vsz)*1024},                 % to bytes
                             {beam_rss, to_int(Rss)},                      % in kB pages
                             {beam_minflt, to_int(MinFault)},
                             {beam_majflt, to_int(MajFault)}];
                        _ ->
                            []
                    end
            end;
        false ->
            []
    end.

%% "8:11.15"
timestr_to_sec(Str) ->
    case string:tokens(Str, ":.") of
        [Min, Sec, CentiSec] -> 60*to_int(Min)+to_int(Sec)+to_int(CentiSec)/100;
        _ -> 0
    end.

%%%-------------------------------------------------------------------
%% collect info about an erlang process.
%% uses erlang:process_info/2
%%
%% registered_name      atom | []
%% initial_call         {M, F, A}
%% current_function     {M, F, A}
%% last_calls           [{M, F, A}]
%% reductions           integer()
%% message_queue_len    integer()
%% memory               bytes
%% stack_size           bytes
%% heap_size            bytes
%% total_heap_size      bytes
%%%-------------------------------------------------------------------

%%% reductions, message_queue_len, memory
%%% current_function, initial_call, registered_name
%%% N.B. 'reductions' is reductions/sec

-define(TAGS, [reductions, memory, message_queue_len]).

get_prc_data(LD) ->
    case LD#ld.max_procs < erlang:system_info(process_count) of
        true ->
            [];
        false->
            [{P, pid_info(P, ?TAGS)} || P <- lists:sort(processes())]
    end.

dred(NowDiff, Io, Ic)-> (red(Ic)-red(Io))/NowDiff.
dmem(NowDiff, Io, Ic)-> abs((mem(Ic)-mem(Io))/NowDiff).

red([]) -> 0;
red([{reductions, Reds}|_]) -> Reds.

mem([]) -> 0;
mem([_, {memory, Mem}|_]) -> Mem.

msgq([]) -> 0;
msgq([_, _, {message_queue_len, Msgq}]) -> Msgq.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% pid_info/1

pid_info(Pid, Tags) when is_pid(Pid) ->
    case process_info(Pid, Tags) of
        undefined -> [{T, get_default(T, Pid)} || T <- Tags];
        TagVals   -> [{T, mod_val(Pid, T, V)} || {T, V} <- TagVals]
    end.

get_default(Tag, Pid) ->
    {_, Default} = pidinfo(Tag, Pid),
    Default.

mod_val(Pid, Tag, Val) ->
    {Modder, _} = pidinfo(Tag, Pid),
    Modder(Val).

pidinfo(stack_size, _) ->
    {fun(Val) -> 8*Val end,
     0};
pidinfo(heap_size, _) ->
    {fun(Val) -> 8*Val end,
     0};
pidinfo(total_heap_size, _) ->
    {fun(Val) -> 8*Val end,
     0};
pidinfo(last_calls, Pid) ->
    {fun(Val) ->
             case Val of
                 false -> try process_flag(Pid, save_calls, 16)
                          catch _:_ -> ok end,
                          [];
                 Calls -> lists:usort(Calls)
             end
     end,
     []};
pidinfo(registered_name, _) ->
    {fun(Val) -> Val end,
     []};
pidinfo(initial_call, Pid) ->
    {fun(Val) ->
             case Val of
                 {proc_lib, init_p, 5} ->
                     try proc_lib:translate_initial_call(Pid) of
                         {dets, init, 2}     -> pinf_dets(Pid);
                         {disk_log, init, 2} -> pinf_disk_log(Pid);
                         IC                -> IC
                     catch _:_ ->
                             Val
                     end;
                 _ -> Val
             end
     end,
     []};
pidinfo(_, _) ->
    {fun(Val) -> Val end,
     []}.

pinf_dets(Pid) ->
    case dets:pid2name(Pid) of
        {ok, Dets} -> {dets, Dets};
        undefined -> undefined_dets_table
    end.

pinf_disk_log(Pid) ->
    case disk_log:pid2name(Pid) of
        {ok, Log} -> {disk_log, Log};
        undefined -> undefined_disk_log
    end.

%%----------------------------------------------------------------------------------------
%% network activity

-include_lib("kernel/include/inet.hrl").

get_net_data(_LD) ->
    lists:sort([port_info(P) || P <- erlang:ports()]).

%%returns {Name::atom(), Stats::list()}
port_info(P) when is_port(P) ->
    try stats(P, name(P))
    catch _:_ -> {P, []}
    end.

stats(P, {driver, Name}) ->
    {{driver, Name}, erlang:port_info(P)};
stats(P, Name) ->
    case erlang:port_info(P) of
        undefined -> throw(port_gone);
        Info ->
            try {ok, Stats} = inet:getstat(P), {Name, Info++Stats}
            catch _:_ -> {Name, Info}
            end
    end.

name(P) -> name(erlang:port_info(P, name), P).

name({name, "udp_inet"}, P) ->
    {ok, Port} = inet:port(P),
    {udp, Port};
name({name, "tcp_inet"}, P) ->
    {ok, {IP, Port}} = inet:peername(P),
    memoize({prfNet, tcp_name, IP, Port}, fun tcp_name/1);
name({name, Name}, _P) ->
    {driver, Name}.

tcp_name({prfNet, tcp_name, IP, Port}) ->
    case inet:gethostbyaddr(IP) of
        {ok, #hostent{h_name=HostName}} ->
            try
                {ok, Names} = net_adm:names(HostName),
                {value, {NodeName, Port}} = lists:keysearch(Port, 2, Names),
                {node, NodeName++"@"++HostName}
            catch
                _:_ -> {tcp, {HostName, Port}}
            end;
        _ ->
            X = tuple_to_list(IP),
            {tcp, {tl(lists:flatten([[$., integer_to_list(I)]||I<-X])), Port}}
    end.

memoize(Key, F) ->
    case get(Key) of
        undefined -> put(Key, F(Key)), get(Key);
        Name -> Name
    end.

%%----------------------------------------------------------------------------------------
%% mnesia activity

%% We settle for only measuring changes over seconds
-define(TIME_DIVISOR, 1000000).

-type bytes()       :: non_neg_integer().
-type cache()       :: [metric()].
-type config()      :: [{config_name(), value()}].
-type config_name() :: atom().
-type count()       :: non_neg_integer().
-type data()        :: {metric_name(), [metric()]}.
-type metric()      :: {metric_name(), value()}.
-type metric_gauge():: {metric_name(), metric_value()}.
-type metric_name() :: atom().
-type metric_value():: float() | undefined.
-type collector()   :: {metric_name(), fun()}.
-type state()       :: #state{}.
-type table_name()  :: dets:tab_name() | ets:tab().
-type table_type()  :: dets | ets | remote_only.
-type time_diff()   :: number().
-type value()       :: term().

%%%_* API =====================================================================

get_mnesia_data(LD) ->
    [].

-spec collect_data(state()) -> [metric()].
collect_data(State) ->
  CurrentTime = prfTime:ts(),
  Data = collect_values(State) ++ collect_changes(State, CurrentTime),
  [{timestamp, CurrentTime}|Data].

-spec collect_values(state()) -> [metric()].
collect_values(State) ->
  ToCollect = get_value_collectors(State#state.collectors),
  [{Collector, get_metric(Collector)} || Collector <- ToCollect].

-spec collect_changes(state(), erlang:timestamp()) -> [metric()].
collect_changes(State, CurrentTime) ->
  ToCollect = get_change_collectors(State#state.collectors),
  Cache     = State#state.cache,
  Then      = State#state.timestamp,
  TimeDiff  = time_diff(Then, CurrentTime),
  [{Collector,
    get_metric(Collector, Cache, TimeDiff)} || Collector <- ToCollect].

-spec get_metric(metric_name()) -> value().
get_metric(MetricName) ->
  {MetricName, CollectorFun} = lists:keyfind(MetricName, 1,
                                             counters() ++ lists()),
  CollectorFun().

-spec get_metric(metric_name(), cache(), time_diff()) -> value().
get_metric(MetricName, Cache, TimeDiff) ->
  {MetricName, CollectorFun} = lists:keyfind(MetricName, 1, changes()),
  CollectorFun(Cache, TimeDiff).


%%%_* Collector specs =========================================================

-spec default_collectors() -> [metric_name()].
default_collectors() ->
  [held_locks, current_transactions, failed_transactions,
   committed_transactions].

-spec get_value_collectors() -> [metric_name()].
get_value_collectors() ->
  get_collector_names(counters() ++ lists()).

-spec get_value_collectors([metric_name()]) -> [metric_name()].
get_value_collectors(Collectors) ->
  filter_collectors(Collectors, get_value_collectors()).

-spec get_change_collectors() -> [metric_name()].
get_change_collectors() ->
  get_collector_names(changes()).

-spec get_collector_names([collector()]) -> [metric_name()].
get_collector_names(Collectors) ->
  [Name || {Name, _CollectorFun} <- Collectors].

-spec get_change_collectors([metric_name()]) -> [metric_name()].
get_change_collectors(Collectors) ->
  filter_collectors(Collectors, get_change_collectors()).

-spec filter_collectors([metric_name()], [metric_name()]) -> [metric_name()].
filter_collectors(Collectors, AllowedCollectors) ->
  [Collector || Collector <- Collectors,
                lists:member(Collector, AllowedCollectors)].

%% There are three types of statistics: counters, lists, and change
%% since last time. The first two has a zero-arity collector function,
%% the last a two-arity collector function where the first argument is
%% the cache and the second argument is the time since last call.
-spec collectors() -> [collector()].
collectors() ->
  counters() ++ lists() ++ changes().

-spec counters() -> [{metric_name(), fun()}].
counters() ->
  [ {held_locks,                    fun held_locks/0},
    {lock_queue,                    fun lock_queue/0},
    {subscribers,                   fun subscribers/0},
    {table_sizes,                   fun table_sizes/0},
    {object_counts,                 fun object_counts/0},
    {current_transactions,          fun current_transactions/0},
    {failed_transactions,           fun failed_transactions/0},
    {committed_transactions,        fun committed_transactions/0},
    {restarted_transactions,        fun restarted_transactions/0},
    {logged_transactions,           fun logged_transactions/0}
  ].

-spec lists() -> [{metric_name(), fun()}].
lists() ->
  [ {tables,                        fun tables/0}
  ].

-spec changes() -> [{metric_name(), fun()}].
changes() ->
  [ {held_locks_change,             fun held_locks_change/2},
    {lock_queue_change,             fun lock_queue_change/2},
    {table_size_changes,            fun table_size_changes/2},
    {object_count_changes,          fun object_count_changes/2},
    {current_transactions_change,   fun current_transactions_change/2},
    {failed_transactions_change,    fun failed_transactions_change/2},
    {committed_transactions_change, fun committed_transactions_change/2},
    {restarted_transactions_change, fun restarted_transactions_change/2},
    {logged_transactions_change,    fun logged_transactions_change/2}
  ].


%%%_* Helpers =================================================================

-spec new_state(state(), [{metric_name(), value()}]) -> state().
new_state(State, Data) ->
  Cache = [D || {Tag, _} = D <- Data,
                lists:member(Tag, available_collectors())],
  {timestamp, Timestamp} = lists:keyfind(timestamp, 1, Data),
  State#state{ cache = Cache, timestamp = Timestamp }.

-spec time_diff(erlang:timestamp(), erlang:timestamp()) -> float().
time_diff(Then, Now) ->
  timer:now_diff(Now, Then) / ?TIME_DIVISOR.

-spec calculate_changes([metric()], value() | [value()], time_diff()) ->
                           [metric_gauge()].
calculate_changes(CurrentValues, OldValues, TimeDiff) when
    is_list(CurrentValues) ->
  F = fun(CurrentValue) ->
          calculate_change(CurrentValue, OldValues, TimeDiff)
      end,
  lists:map(F, CurrentValues).

-spec calculate_change(metric() | value(), value(), time_diff()) ->
                          metric_gauge().
calculate_change({Key, _CurrentValue}, undefined, _TimeDiff) ->
  {Key, undefined};
calculate_change(_CurrentValue, undefined, _TimeDiff) ->
  undefined;
calculate_change({Key, CurrentValue}, OldValues, TimeDiff) when
    is_list(OldValues) ->
  case lists:keyfind(Key, 1, OldValues) of
    {Key, OldValue} -> {Key, (CurrentValue - OldValue) / TimeDiff};
    false           -> {Key, CurrentValue}
  end;
calculate_change(CurrentValue, OldValue, TimeDiff) when
    is_number(OldValue) ->
  (CurrentValue - OldValue) / TimeDiff.

-spec get_value_from_cache(metric_name(), cache()) -> value() | undefined.
get_value_from_cache(Key, Cache) ->
  case lists:keyfind(Key, 1, Cache) of
    {Key, Value} -> Value;
    false        -> undefined
  end.


%%%_* mnesia stats ============================================================

-spec held_locks() -> count().
held_locks() ->
  ets:info(mnesia_held_locks, size).

-spec held_locks_change(cache(), time_diff()) -> metric_gauge().
held_locks_change(Cache, TimeDiff) ->
  OldCount = get_value_from_cache(held_locks, Cache),
  calculate_change(held_locks(), OldCount, TimeDiff).

-spec lock_queue() -> count().
lock_queue() ->
  ets:info(mnesia_lock_queue, size).

-spec lock_queue_change(cache(), time_diff()) -> metric_gauge().
lock_queue_change(Cache, TimeDiff) ->
  OldCount = get_value_from_cache(lock_queue, Cache),
  calculate_change(lock_queue(), OldCount, TimeDiff).

-spec subscribers() -> [pid()].
subscribers() ->
  mnesia:system_info(subscribers).

-spec tables() -> [table_name()].
tables() ->
  mnesia:system_info(tables).

%% NB: Table size is in bytes
-spec table_sizes() -> [{table_name(), bytes()}].
table_sizes() ->
  F = fun(Table, Acc) ->
          case table_size(Table) of
            undefined -> Acc;
            Size      -> [{Table, Size}|Acc]
          end
      end,
  lists:foldl(F, [], tables()).

-spec table_size_changes(cache(), time_diff()) -> [metric_gauge()].
table_size_changes(Cache, TimeDiff) ->
  OldSizes = get_value_from_cache(table_sizes, Cache),
  calculate_changes(table_sizes(), OldSizes, TimeDiff).

-spec object_counts() -> [{table_name(), count()}].
object_counts() ->
  F = fun(Table, Acc) ->
          case object_count(Table) of
            undefined -> Acc;
            Count     -> [{Table, Count}|Acc]
          end
      end,
  lists:foldl(F, [], tables()).

-spec object_count_changes(cache(), time_diff()) -> [metric_gauge()].
object_count_changes(Cache, TimeDiff) ->
  OldCounts = get_value_from_cache(object_counts, Cache),
  calculate_changes(object_counts(), OldCounts, TimeDiff).

-spec current_transactions() -> count().
current_transactions() ->
  length(mnesia:system_info(transactions)).

-spec current_transactions_change(cache(), time_diff()) -> metric_gauge().
current_transactions_change(Cache, TimeDiff) ->
  OldCount = get_value_from_cache(current_transactions, Cache),
  calculate_change(current_transactions(), OldCount, TimeDiff).

-spec failed_transactions() -> count().
failed_transactions() ->
  mnesia:system_info(transaction_failures).

-spec failed_transactions_change(cache(), time_diff()) -> metric_gauge().
failed_transactions_change(Cache, TimeDiff) ->
  OldCount = get_value_from_cache(failed_transactions, Cache),
  calculate_change(failed_transactions(), OldCount, TimeDiff).

-spec committed_transactions() -> count().
committed_transactions() ->
  mnesia:system_info(transaction_commits).

-spec committed_transactions_change(cache(), time_diff()) -> metric_gauge().
committed_transactions_change(Cache, TimeDiff) ->
  OldCount = get_value_from_cache(committed_transactions, Cache),
  calculate_change(committed_transactions(), OldCount, TimeDiff).

-spec restarted_transactions() -> count().
restarted_transactions() ->
  mnesia:system_info(transaction_restarts).

-spec restarted_transactions_change(cache(), time_diff()) -> metric_gauge().
restarted_transactions_change(Cache, TimeDiff) ->
  OldCount = get_value_from_cache(restarted_transactions, Cache),
  calculate_change(restarted_transactions(), OldCount, TimeDiff).

-spec logged_transactions() -> count().
logged_transactions() ->
  mnesia:system_info(transaction_log_writes).

-spec logged_transactions_change(cache(), time_diff()) -> metric_gauge().
logged_transactions_change(Cache, TimeDiff) ->
  OldCount = get_value_from_cache(logged_transactions, Cache),
  calculate_change(logged_transactions(), OldCount, TimeDiff).

-spec object_count(table_name()) -> count() | undefined.
object_count(Table) ->
  object_count(Table, get_term_storage_type(Table)).

-spec object_count(table_name(), table_type()) -> count() | undefined.
object_count(_Table, remote_only) -> undefined;
object_count(Table,  dets)        -> dets_object_count(Table);
object_count(Table,  ets)         -> ets_object_count(Table).

-spec table_size(table_name()) -> bytes() | undefined.
table_size(Table) ->
  table_size(Table, get_term_storage_type(Table)).

-spec table_size(table_name(), table_type()) -> bytes() | undefined.
table_size(_Table, remote_only) -> undefined;
table_size(Table,  dets)        -> dets_size(Table);
table_size(Table,  ets)         -> ets_size(Table).

-spec get_term_storage_type(table_name()) -> table_type().
get_term_storage_type(Table) ->
  case mnesia:table_info(Table, storage_type) of
    disc_only_copies -> dets;
    undefined        -> remote_only;
    _                -> ets
  end.


%%%_* dets stats ==============================================================

-spec dets_size(dets:tab_name()) -> bytes().
dets_size(Table) ->
  dets:info(Table, file_size).

-spec dets_object_count(dets:tab_name()) -> count().
dets_object_count(Table) ->
  dets:info(Table, size).


%%%_* ets stats ===============================================================

-spec ets_size(ets:tab()) -> bytes().
ets_size(Table) ->
  ets:info(Table, memory) * erlang:system_info(wordsize).

-spec ets_object_count(ets:tab()) -> count().
ets_object_count(Table) ->
  ets:info(Table, size).
