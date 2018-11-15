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

%%%---------------------------
%%% API

%% workaround for get_stacktrace confusion in OTP 21
-ifdef(OTP_RELEASE). %% this implies 21 or higher
-define(EXCEPTION(Class, Reason, StackToken), Class:Reason:StackToken).
-define(GET_STACK(StackToken), StackToken).
-else.
-define(EXCEPTION(Class, Reason, _), Class:Reason).
-define(GET_STACK(_), erlang:get_stacktrace()).
-endif.

start() -> spawn(fun blocking_start/0).

blocking_start() ->
    try loop(init())
    catch ?EXCEPTION(C, R, S) -> erlang:display({C, R, ?GET_STACK(S)})
    end.

stop() -> exit(whereis(redbug_dtop), kill).

sort(Col) -> redbug_dtop ! {sort, Col}.

max_procs(MaxProcs) ->  redbug_dtop ! {max_procs, MaxProcs}.

%%%---------------------------
-record(ld,
        {fd=standard_io,
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

-record(pidinfo,
        {pid,
         reductions,
         memory,
         message_queue_len,
         cpu,
         current_function,
         initial_call,
         registered_name,
         last_calls,
         stack_size,
         heap_size,
         total_heap_size}).

%%%---------------------------
init() ->
    register(redbug_dtop, self()),
    LD0 = #ld{},
    LD1 = LD0#ld{constants = constants(LD0)},
    LD1#ld{cache = get_data(LD1)}.

loop(LD) ->
    timer:sleep(LD#ld.tick),
    Data = get_data(LD),
    TS = erlang:timestamp(),
    DT = delta_time(TS, LD#ld.now),
    print(LD, differ(DT, LD#ld.cache, Data)),
    loop(LD#ld{now=TS, cache=Data}).

get_data(LD) ->
    [get_sys_data(LD), get_prc_data(LD), get_net_data(), get_mnesia_data()].

print(LD, [SysData, PrcData|_]) ->
    print_del(LD#ld.fd),
    print_sys(LD#ld.fd, SysData),
    lwrite(LD#ld.fd, "~n", []),
    print_tags(LD#ld.fd),
    print_procs(LD, PrcData, SysData).

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
    Node      = node(),
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
    Str = to_list(Item),
    case length(Str) of
        L when L=:=Len -> Str;
        L when L<Len -> pad(Str, L, Len, Pad, LeftRight);
        _ -> lists:sublist(Str, Len)
    end.

pad(Str, L, Len, Pad, LeftRight) ->
    case LeftRight of
        left -> lists:duplicate(Len-L, Pad)++Str;
        right-> Str++lists:duplicate(Len-L, Pad)
    end.

print_del(FD) ->
    lwrite(FD, "~s~n", [lists:duplicate(79, $-)]).

format() -> "~-14s ~-28s ~-17s~7s~7s~4s~n".

tags() -> ["pid", "name", "current", "msgq", "mem", "cpu"].

print_tags(FD) ->
    lwrite(FD, format(), tags()).

print_procs(LD, PrcData, SysData) ->
    Prcs = complete_info(toplist(LD, PrcData), SysData),
    lists:foreach(fun(P) -> print_proc(LD, P) end, pad_lines(LD, Prcs)).

pad_lines(#ld{lines = Lines}, Prcs) ->
    case Lines < length(Prcs) of
        true -> lists:sublist(Prcs, Lines);
        false-> Prcs++lists:duplicate(Lines-length(Prcs), [])
    end.

print_proc(#ld{fd = FD}, PP) ->
    try
        lwrite(FD,
               format(),
               [pidf(to_list(PP#pidinfo.pid)),
                funf(reg(PP)),
                funf(PP#pidinfo.current_function),
                human(PP#pidinfo.message_queue_len),
                human(PP#pidinfo.memory),
                to_list(PP#pidinfo.cpu)])
    catch
        _:_ -> lwrite(FD, "~n", [])
    end.

reg(PP) ->
    case PP#pidinfo.registered_name of
        [] -> PP#pidinfo.initial_call;
        Val -> Val
    end.

pidf(Pid) ->
    [_, A, B] = string:tokens(Pid, "."),
    lists:append(["<0.", A, ".", B]).

funf({M, F, A}) -> to_list(M)++":"++to_list(F)++"/"++to_list(A);
funf(Term) -> io_lib:fwrite("~p", [Term]).

complete_info(PrcData, SysData) ->
    CpuPerRed = cpu_per_red(SysData),
    lists:map(fun(P) -> complete(P, CpuPerRed) end, PrcData).

complete(PidInfo, CpuPerRed) ->
    Pid = PidInfo#pidinfo.pid,
    PidInfo#pidinfo{
      cpu              = CpuPerRed*PidInfo#pidinfo.reductions,
      current_function = pid_info(Pid, current_function),
      initial_call     = pid_info(Pid, initial_call),
      registered_name  = pid_info(Pid, registered_name),
      last_calls       = pid_info(Pid, last_calls),
      stack_size       = pid_info(Pid, stack_size),
      heap_size        = pid_info(Pid, heap_size),
      total_heap_size  = pid_info(Pid, total_heap_size)}.

cpu_per_red(SysData) ->
    case lks(reductions, SysData) of
        0    -> 0;
        Reds -> 100*cpu(SysData)/Reds
    end.

cpu(SysData) ->
    case lks(beam_user, SysData, 1)+lks(beam_kernel, SysData, 0) of
        0.0 -> 1;
        C -> C
    end.

%%----------------------------------------------------
%% calculate rates

differ(_, [], []) ->
    [];
differ(DT, [Old|Olds], [New|News]) ->
    [diff(DT, Old, New)|differ(DT, Olds, News)].

diff(DT, Old, New) when is_tuple(Old) andalso is_tuple(New) ->
    tdiff(DT, Old, New);
diff(DT, [Old = #pidinfo{pid = Pid}|Olds], [New = #pidinfo{pid = Pid}|News]) ->
    [tdiff(DT, Old, New)|diff(DT, Olds, News)];
diff(DT, [#pidinfo{pid = PidO}|_] = Olds, [#pidinfo{pid = PidN}|_] = News) ->
    case PidO < PidN of
        true  -> diff(DT, tl(Olds), News);
        false -> diff(DT, Olds, tl(News))
    end;
diff(DT, [{Key, Val0}|Olds], [{Key, Val1}|News]) ->
    [tdiff(DT, {Key, Val0}, {Key, Val1})|diff(DT, Olds, News)];
diff(_, _, []) ->
    [];
diff(_, [], News) ->
    News.

tdiff(DT, Old, New) ->
    list_to_tuple(ldiff(DT, tuple_to_list(Old), tuple_to_list(New))).

ldiff(_, [], []) ->
    [];
ldiff(DT, [Old|Olds], [New|News]) when is_number(Old) andalso is_number(New)->
    [{New, df(DT, Old, New)}|ldiff(DT, Olds, News)];
ldiff(DT, [_|Olds], [New|News]) ->
    [New|ldiff(DT, Olds, News)].

df(0, _, _) -> 0;
df(DT, X, Y) -> (Y-X)/DT.

%%--------------------------------------------------------------------------
%% calculate process toplist
%%% Dreds, Dmems, Mems and Msgqs are sorted lists of pids
%%% PidInfo is a sorted list of {Pid, Info}
%%% Info is a list of tagged tuples {atom(), number()}

%% return [#pidinfo{}], with length =< integer(Lines), sorted on atom(Sort)
toplist(#ld{lines=Lines, sort=Sort}, PrcData) ->
    lists:sublist(lists:keysort(pidinfo_index(Sort), PrcData), Lines).

pidinfo_index(cpu) -> #pidinfo.cpu;
pidinfo_index(memory) -> #pidinfo.memory;
pidinfo_index(reductions) -> #pidinfo.reductions;
pidinfo_index(message_queue_len) -> #pidinfo.message_queue_len.

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
%% now                  [now()]   erlang:timestamp()
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


get_sys_data(LD) ->
    LD#ld.constants++stats()++os_info(LD#ld.strategy).

stats() ->
    Procs                            = erlang:system_info(process_count),
    {Ctxt, 0}                        = erlang:statistics(context_switches),
    {GCs, GCwords, 0}                = erlang:statistics(garbage_collection),
    {{input, IoIn}, {output, IoOut}} = erlang:statistics(io),
    {Reds, _}                        = erlang:statistics(reductions),
    RunQ                             = erlang:statistics(run_queue),

    [{now, erlang:timestamp()},
     {procs, Procs},
     {context_switches, Ctxt},
     {gcs, GCs},
     {gc_reclaimed, GCwords*4},
     {io_in, IoIn},
     {io_out, IoOut},
     {reductions, Reds},
     {run_queue, RunQ} |
     erlang:memory()].

constants(#ld{strategy={linux, ProcStat, _}}) ->
    [{total_ram, total_ram()}, {cores, cores(ProcStat)}];
constants(_) ->
    [].

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
                 to_int(T)*1024
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

get_prc_data(LD) ->
    case LD#ld.max_procs < erlang:system_info(process_count) of
        true  -> [];
        false -> lists:map(fun pid_info/1, lists:sort(processes()))
    end.

pid_info(Pid) ->
    #pidinfo{
       pid = Pid,
       memory = pid_info(Pid, memory),
       reductions = pid_info(Pid, reductions),
       message_queue_len = pid_info(Pid, message_queue_len)}.

pid_info(Pid, Tag) ->
    case erlang:process_info(Pid, Tag) of
        undefined -> get_default(Tag, Pid);
        Val       -> mod_val(Pid, Tag, Val)
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

get_net_data() ->
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
    {tcp, {IP, Port, tcp_name(IP, Port)}};
name({name, Name}, _P) ->
    {driver, Name}.

tcp_name(IP, Port) ->
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
            X = to_list(IP),
            {tcp, {tl(lists:flatten([[$., to_list(I)]||I<-X])), Port}}
    end.

%%----------------------------------------------------------------------------------------
%% mnesia activity

-record(mnesia_info,
        {logged_transactions,
         restarted_transactions,
         committed_transactions,
         failed_transactions,
         current_transactions,
         object_counts,
         table_sizes,
         subscribers,
         lock_queue,
         held_locks}).

get_mnesia_data() ->
    case mnesia:system_info(is_running) of
        no  -> {};
        yes -> mnesia_info()
    end.

mnesia_info() ->
    #mnesia_info{
       held_locks             = held_locks(),
       lock_queue             = lock_queue(),
       subscribers            = subscribers(),
       table_sizes            = table_sizes(),
       object_counts          = object_counts(),
       current_transactions   = current_transactions(),
       failed_transactions    = failed_transactions(),
       committed_transactions = committed_transactions(),
       restarted_transactions = restarted_transactions(),
       logged_transactions    = logged_transactions()}.

%%%_* mnesia stats ============================================================

        -type table_name()  :: dets:tab_name() | ets:tab().
-type table_type()  :: dets | ets | remote_only.
-type bytes()       :: non_neg_integer().
-type count()       :: non_neg_integer().

-spec held_locks() -> count().
held_locks() ->
    ets:info(mnesia_held_locks, size).

-spec lock_queue() -> count().
lock_queue() ->
    ets:info(mnesia_lock_queue, size).

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

-spec object_counts() -> [{table_name(), count()}].
object_counts() ->
    F = fun(Table, Acc) ->
                case object_count(Table) of
                    undefined -> Acc;
                    Count     -> [{Table, Count}|Acc]
                end
        end,
    lists:foldl(F, [], tables()).

-spec current_transactions() -> count().
current_transactions() ->
    length(mnesia:system_info(transactions)).

-spec failed_transactions() -> count().
failed_transactions() ->
    mnesia:system_info(transaction_failures).

-spec committed_transactions() -> count().
committed_transactions() ->
    mnesia:system_info(transaction_commits).

-spec restarted_transactions() -> count().
restarted_transactions() ->
    mnesia:system_info(transaction_restarts).

-spec logged_transactions() -> count().
logged_transactions() ->
    mnesia:system_info(transaction_log_writes).

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
-spec dets_size(dets:tab_name()) -> bytes().
dets_size(Table) ->
    dets:info(Table, file_size).

-spec dets_object_count(dets:tab_name()) -> count().
dets_object_count(Table) ->
    dets:info(Table, size).

-spec ets_size(ets:tab()) -> bytes().
ets_size(Table) ->
    ets:info(Table, memory) * erlang:system_info(wordsize).

-spec ets_object_count(ets:tab()) -> count().
ets_object_count(Table) ->
    ets:info(Table, size).

%% ---------------------------------------------------------------
%% utils

lks(Tag, TVs, Def) ->
    try lks(Tag, TVs)
    catch {not_found, _} -> Def
    end.

lks(Tag, [])             -> throw({not_found, Tag});
lks(Tag, [{Tag, Val}|_]) -> Val;
lks(Tag, [_|List])       -> lks(Tag, List).

flat(F, A) ->
    lists:flatten(io_lib:fwrite(F, A)).

to_list(A) when is_list(A) -> A;
to_list(A) when is_pid(A) -> pid_to_list(A);
to_list(A) when is_atom(A) -> atom_to_list(A);
to_list(A) when is_float(A) -> to_list(round(A));
to_list(A) when is_tuple(A) -> tuple_to_list(A);
to_list(A) when is_integer(A) -> integer_to_list(A).

to_int("-") -> 0;
to_int(J) -> list_to_integer(J).

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

human(E, M) ->
    flat("~.1f~s", [math:pow(10,E), M]).

delta_time(TS0, TS1) -> timer:now_diff(TS0, TS1)/1000000.

lwrite(FD, Format, As) ->
    io:fwrite(FD, Format, As).
