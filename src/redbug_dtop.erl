%%%-------------------------------------------------------------------
%%% File    : redbug_dtop.erl
%%% Created :  5 Sep 2005
%%% Created : 16 Dec 2003
%%% Created :  5 Dec 2003 by Mats Cronqvist <qthmacr@duna283>
%%% Created :  8 Dec 2003 by Mats Cronqvist <qthmacr@duna283>
%%% Created : 18 Oct 2005 by Mats Cronqvist <locmacr@mwlx084>
%%% Description : top-like client for beam
%%%-------------------------------------------------------------------

%% @private

-module(redbug_dtop).
-author('Mats Cronqvist').

-export([start/0,
         stop/0,
         cfg/1]).

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

start() ->
    P = spawn(fun init/0),
    try
        register(redbug_dtop, P)
    catch
        error:badarg ->
            P ! quit,
            already_started
    end.

stop() ->
    redbug_dtop ! quit.

cfg(Cfg) ->
    maps:map(fun cfg/2, Cfg).

cfg(fd, S) ->  %%% standard_io
    case is_pid(S) of
        true -> redbug_dtop ! {config, fd, S};
        false -> {error, not_fd}
    end;
cfg(tick, S) -> %%% 2000
    case is_integer(S) andalso 1 =< S andalso S =< 10 of
        true -> redbug_dtop ! {config, tick, 1000*S};
        false -> {error, must_be_between_1_and_10}
    end;
cfg(lines, S) -> %%% 19
    case is_integer(S) andalso 1 =< S andalso S =< 100 of
        true -> redbug_dtop ! {config, lines, S};
        false -> {error, must_be_between_1_and_100}
    end;
cfg(groupby, S) ->
    case lists:member(S, [pid, name]) of
        true -> redbug_dtop ! {config, groupby, S};
        false -> {error, unknown_grouping}
    end;
cfg(sort, S) ->
    case lists:member(S, [cpu, mem, msgq]) of
        true -> redbug_dtop ! {config, sort, S};
        false -> {error, unknown_criteria}
    end;
cfg(max_procs, Max) ->
    case is_integer(Max) of
        true -> redbug_dtop ! {config, max_procs, Max};
        false -> {error, not_integer}
    end.

%%%---------------------------
%% records!

%% statistics from the VM
-record(sys,
        {now = erlang:timestamp(),
         cpu_per_red = 1,      %% current cpu_beam(from the kernel) / delta_reds(from the VM)
         cpu_beam = 0,         %% Current CPU% of beam(from the kernel)
         cpu_total = 0,        %% Current CPU% of all jobs(from the kernel)
         prcs = 0,             %% [count]   erlang:system_info(process_count)
         context_switches = 0, %% [count/s] erlang:statistics(context_switches)
         gcs = 0,              %% [count/s] erlang:statistics(garbage_collection)
         gc_reclaimed = 0,     %% [byte/s]  erlang:statistics(garbage_collection)
         io_in = 0,            %% [byte/s]  erlang:statistics(io)
         io_out = 0,           %% [byte/s]  erlang:statistics(io)
         reductions = 0,       %% [count/s] erlang:statistics(reductions)
         run_queue = 0,        %% [count]   erlang:statistics(run_queue)
         total_mem = 0,        %% [byte]    erlang:memory()
         processes_mem = 0,    %% [byte]    erlang:memory()
         processes_used = 0,   %% [byte]    erlang:memory()
         system = 0,           %% [byte]    erlang:memory()
         atom = 0,             %% [byte]    erlang:memory()
         atom_used = 0,        %% [byte]    erlang:memory()
         binary = 0,           %% [byte]    erlang:memory()
         code = 0,             %% [byte]    erlang:memory()
         ets = 0,              %% [byte]    erlang:memory()
         user = 0,             %% [frac]    /proc/stat
         nice = 0,             %% [frac]    /proc/stat
         kernel = 0,           %% [frac]    /proc/stat
         idle = 0,             %% [frac]    /proc/stat
         iowait = 0,           %% [frac]    /proc/stat
         ctxt = 0,             %% [frac]    /proc/stat
         beam_user = 0,        %% [frac]    /proc/self/stat
         beam_kernel = 0,      %% [frac]    /proc/self/stat
         beam_vsz = 0,         %% [byte]    /proc/self/stat
         beam_rss = 0,         %% [pages]   /proc/self/stat
         beam_minflt = 0,      %% [count/s] /proc/self/stat
         beam_majflt = 0,      %% [count/s] /proc/self/stat
         cores = 0,            %% [count]   /proc/stat
         total_ram = 0}).      %% [byte]    /proc/meminfo

-define(SYS_CPU(N, R, BK, BU, K, U),
    #sys{now = N, reductions = R, beam_kernel = BK, beam_user = BU, kernel = K, user = U}).

%% how to talk to the OS
-record(sysapi,
        {type,            %% procfs | ps
         proc_stat,       %% fd()
         proc_self_stat,  %% fd()
         port,            %% port()
         ps_p,            %% ps -p command string
         ps_A}).          %% ps -A command string

%% statistics from the OS
-record(sys_os,
        {user        = 0,
         nice        = 0,
         kernel      = 0,
         idle        = 0,
         iowait      = 0,
         beam_user   = 0,
         beam_kernel = 0,
         vsz         = 0,
         rss         = 0,
         minflt      = 0,
         majflt      = 0
        }).

%% info about a process
-record(prc,
        {pid,
         cpu = 0,                 %% reductions * cpu_per_reduction
         reductions = 0,          %% integer()
         memory = 0,              %% bytes
         message_queue_len = 0,   %% integer()
         current_function = "",   %% {M, F, A}
         name = "",               %% (registered_name, label, or initial_call)
         last_calls = [],         %% [{M, F, A}]
         stack_size = 0,          %% bytes
         heap_size = 0,           %% bytes
         total_heap_size = 0}).   %% bytes

%% an abstract pattern/constructor for #prc{}
-define(PRC(Pid, Cpu, Mem, MQL, CFN, N),
    #prc{pid = Pid,
        cpu = Cpu,
        memory = Mem,
        message_queue_len = MQL,
        current_function = CFN,
        name = N}).

-include_lib("kernel/include/inet.hrl").

%% statistics from an interface
-record(net,
        {name,
         port,
         input,
         output,
         recv_avg,
         recv_cnt,
         recv_dvi,
         recv_max,
         recv_oct,
         send_avg,
         send_cnt,
         send_max,
         send_oct,
         send_pend}).

%% mnesia info
-record(mnesia,
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

%% a data wrapper
-record(data,
        {sys = #sys{},
         prc = #{},
         net = [],
         mnesia = #mnesia{}}).

-define(DATA(D, K, V), D#data{K = V}).

%% system HW constants
-record(sys_const,
        {total_ram,
         cores = erlang:system_info(logical_processors)}).

%% the loop data
-record(ld,
        {fd = standard_io,
         sort = cpu,
         tick = 2000,
         lines = 19,
         groupby = pid,
         max_procs = 3000,
         os = os(),
         sysapi = sysapi(),
         constants = constants(),
         data = #data{}}).

-define(UPDATE_LD_DATA(LD, D, K, V), LD#ld{data = ?DATA(D, K, V)}).

%%%---------------------------

init() ->
    LD = #ld{},
    erlang:send_after(LD#ld.tick, self(), timeout),
    loop(get_data(LD)).

loop(LD) ->
    receive
        timeout -> loop(printer(timer(LD)));
        {config, fd, Val}        -> loop(LD#ld{fd = Val});
        {config, sort, Val}      -> loop(LD#ld{sort = Val});
        {config, tick, Val}      -> loop(LD#ld{tick = Val});
        {config, lines, Val}     -> loop(LD#ld{lines = Val});
        {config, groupby, Val}   -> loop(LD#ld{groupby = Val});
        {config, max_procs, Val} -> loop(LD#ld{max_procs = Val});
        quit -> ok
    end.

timer(LD) ->
    erlang:send_after(LD#ld.tick, self(), timeout),
    LD.

printer(LD) ->
    print(get_data(LD)).

get_data(LD) ->
    pipe(LD,
         [fun fill_sys_data/1,
          fun fill_prc_data/1,
          fun fill_net_data/1,
          fun fill_mnesia_data/1]).

print(LD) ->
    print_del(LD#ld.fd),
    print_sys(LD#ld.fd, LD#ld.data),
    io:fwrite(LD#ld.fd, "~n", []),
    print_tags(LD#ld.fd),
    print_prcs(LD),
    LD.

print_sys(FD, #data{sys = Sys}) ->
    io:fwrite(FD, "~s~n", [sys_str(Sys)]),
    io:fwrite(FD, memf(), memi(Sys)).

memf() ->
    "memory:      proc~8s, atom~8s, bin~8s, code~8s, ets~8s~n".

memi(Sys) ->
    Ms = [Sys#sys.processes_mem, Sys#sys.atom, Sys#sys.binary, Sys#sys.code, Sys#sys.ets],
    [try human(M) catch _:_ -> "" end|| M <- Ms].

sys_str(Sys) ->
    {_, Time} = calendar:now_to_local_time(Sys#sys.now),
    H         = pad(element(1, Time), 2, $0, left),
    M         = pad(element(2, Time), 2, $0, left),
    S         = pad(element(3, Time), 2, $0, left),
    Node      = node(),
    MEMbeam   = human(Sys#sys.beam_vsz),
    MEM       = human(Sys#sys.total_mem),
    CPUbeam   = integer_to_list(round(100*Sys#sys.cpu_beam)),
    CPU       = integer_to_list(round(100*Sys#sys.cpu_total)),
    Prcs      = human(Sys#sys.prcs),
    RunQ      = human(Sys#sys.run_queue),

    SYS = lists:sublist(lists:append(["size: "    , MEM,
                                      "("         , MEMbeam,
                                      "), cpu%: " , CPUbeam,
                                      "("         , CPU,
                                      "), procs: ", Prcs,
                                      ", runq: "  , RunQ,
                                      ", ", H, ":", M, ":", S]), 79),
    pad(Node, 79-length(SYS), $ , right)++SYS.

pad(Item, Len, Pad, LeftRight) ->
    Str = to_list(Item),
    case length(Str) of
        L when L =:= Len -> Str;
        L when L<Len -> pad(Str, L, Len, Pad, LeftRight);
        _ -> lists:sublist(Str, Len)
    end.

pad(Str, L, Len, Pad, LeftRight) ->
    case LeftRight of
        left -> lists:duplicate(Len-L, Pad)++Str;
        right-> Str++lists:duplicate(Len-L, Pad)
    end.

print_del(FD) ->
    io:fwrite(FD, "~s~n", [lists:duplicate(79, $-)]).

format() -> "~-14s ~-28s ~-17s~7s~7s~4s~n".

tags() -> ["pid", "name", "current", "msgq", "mem", "cpu"].

print_tags(FD) ->
    io:fwrite(FD, format(), tags()).

%%------------------------------------------------------
%% print process data

print_prcs(LD) ->
    Prcs = toplist(LD, groupby(LD)),
    lists:foreach(mk_print_prc(LD#ld.fd), pad_lines(LD, Prcs)).

groupby(#ld{groupby = Groupby, data = #data{prc = PrcData}}) ->
    case Groupby of
        pid -> maps:values(PrcData);
        name -> groupbyname(PrcData)
    end.

groupbyname(Prcs) ->
    maps:values(maps:fold(fun groupbyname/3, #{}, Prcs)).

groupbyname(_, ?PRC(_, Cpu, Mem, MLQ, CFN, Name), O) ->
    P = ?PRC(1, Cpu, Mem, MLQ, CFN, Name),
    maps:update_with(Name, mk_groupbyname(P), P, O).

mk_groupbyname(P1) ->
    fun(P2) -> merge_prc_by_name(P1, P2) end.

merge_prc_by_name(Prc1, Prc2) ->
    ?PRC(P1, C1, M1, MQL1, CFN, Name) = Prc1,
    ?PRC(P2, C2, M2, MQL2, _, _) = Prc2,
    ?PRC(P1+P2, C1+C2, M1+M2, MQL1+MQL2, CFN, Name).

pad_lines(#ld{lines = Lines}, Prcs) ->
    case Lines < length(Prcs) of
        true -> lists:sublist(Prcs, Lines);
        false-> Prcs++lists:duplicate(Lines-length(Prcs), [])
    end.

mk_print_prc(FD) ->
    fun(Prc) -> print_prc(FD, Prc) end.

print_prc(FD, Prc) ->
    try
        io:fwrite(FD,
                  format(),
                  [pidf(to_list(Prc#prc.pid)),
                   funf(Prc#prc.name),
                   funf(Prc#prc.current_function),
                   human(Prc#prc.message_queue_len),
                   human(Prc#prc.memory),
                   to_list(Prc#prc.cpu)])
    catch
        C:R:S -> io:fwrite(FD, "~p ~p~n~p~n", [C, R, S])
    end.

pidf(Pid) when is_pid(Pid) ->
    [_, A, B] = string:tokens(Pid, "."),
    lists:append(["<0.", A, ".", B]);
pidf(X) ->
    X.

funf({M, F, A}) -> to_list(M)++":"++to_list(F)++"/"++to_list(A);
funf(Term) -> io_lib:fwrite("~p", [Term]).

%%--------------------------------------------------------------------------
%% calculate process toplist

%% return [#prc{}], with length =< integer(Lines), sorted on atom(Sort)

toplist(#ld{lines = Lines, sort = Sort}, PrcData) ->
    Sorter = mk_toplist_gt(Sort),
    pipe(PrcData,
         [fun(X) -> lists:sort(Sorter, X) end,
          fun(X) -> lists:sublist(X, Lines) end]).

mk_toplist_gt(Sort) ->
    GET = mk_getter(Sort),
    fun(A, B) -> not (GET(A) =< GET(B)) end.

mk_getter(cpu)  -> fun(X) -> X#prc.cpu end;
mk_getter(msgq) -> fun(X) -> X#prc.message_queue_len end;
mk_getter(mem)  -> fun(X) -> X#prc.memory end.

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
%%----------------------------------------------------

%%----------------------------------------------------

%% fill #sys{}
fill_sys_data(LD) ->
    ?UPDATE_LD_DATA(LD, LD#ld.data, sys, fill_and_diff_sys(LD)).

fill_and_diff_sys(LD) ->
    diff_sys(fill_sys(LD), (LD#ld.data)#data.sys).

fill_sys(#ld{sysapi = SysApi, constants = Constants}) ->
    Now = erlang:timestamp(),
    {Ctxt, 0}                        = erlang:statistics(context_switches),
    {GCs, GCwords, 0}                = erlang:statistics(garbage_collection),
    {{input, IoIn}, {output, IoOut}} = erlang:statistics(io),
    {Reds, _}                        = erlang:statistics(reductions),
    OS                               = os_info(SysApi),
    #sys{now             = Now,
        cpu_per_red      = 1.0E-8,   %% This is just an initial value. 10 ns per reduction
        prcs             = erlang:system_info(process_count),
        context_switches = Ctxt,
        gcs              = GCs,
        gc_reclaimed     = GCwords,
        io_in            = IoIn,
        io_out           = IoOut,
        reductions       = Reds,
        run_queue        = erlang:statistics(run_queue),
        total_mem        = erlang:memory(total),
        processes_mem    = erlang:memory(processes),
        processes_used   = erlang:memory(processes_used),
        system           = erlang:memory(system),
        atom             = erlang:memory(atom),
        atom_used        = erlang:memory(atom_used),
        binary           = erlang:memory(binary),
        code             = erlang:memory(code),
        ets              = erlang:memory(ets),
        user             = OS#sys_os.user,
        nice             = OS#sys_os.nice,
        kernel           = OS#sys_os.kernel,
        idle             = OS#sys_os.idle,
        iowait           = OS#sys_os.iowait,
        beam_user        = OS#sys_os.beam_user,
        beam_kernel      = OS#sys_os.beam_kernel,
        beam_vsz         = OS#sys_os.vsz,
        beam_rss         = OS#sys_os.rss,
        beam_minflt      = OS#sys_os.minflt,
        beam_majflt      = OS#sys_os.majflt,
        cores            = Constants#sys_const.cores,
        total_ram        = Constants#sys_const.total_ram}.

%% calculate rates
diff_sys(New, Old) ->
    ?SYS_CPU(Nn, Rn, BKn, BUn, Kn, Un) = New,
    ?SYS_CPU(No, Ro, BKo, BUo, Ko, Uo) = Old,
    Dtime = timer:now_diff(Nn, No)/1_000_000,
    update_sys(Rn-Ro, BKn-BKo, BUn-BUo, Kn-Ko, Un-Uo, Dtime, New).

update_sys(Dred, Dbeam_k, Dbeam_u, Dkernel, Duser, Dtime, Sys) ->
    CpuBeam = safediv(max(0.001, Dbeam_k+Dbeam_u), Dtime),
    CpuTot = safediv(max(0.001, Dkernel+Duser), Dtime),
    CpuPerRed = safediv(CpuBeam, Dred),
    Sys#sys{cpu_per_red = CpuPerRed, cpu_beam = CpuBeam, cpu_total = CpuTot}.

safediv(X, Y) ->
    try X/Y catch _:_ -> 0 end.

%% OS info
os() ->
    case os:type() of
        {unix, linux}  -> linux;
        {unix, darwin} -> darwin;
        _              -> unsupported
    end.

constants() ->
    constants(os()).

constants(linux) ->
    #sys_const{total_ram = total_ram(linux)};
constants(_) ->
    #sys_const{}.

sysapi() ->
    case os() of
        linux  -> init_procfs();
        darwin -> init_ps();
        unsupported -> unsupported
    end.

init_procfs() ->
    {ok, ProcStat} = file:open("/proc/stat", [read, raw]),
    {ok, ProcSelfStat} = file:open("/proc/self/stat", [read, raw]),
    #sysapi{type = procfs,
        proc_stat = ProcStat,
        proc_self_stat = ProcSelfStat}.

init_ps() ->
    #sysapi{type = ps,
        port = open_port({spawn, "/bin/sh"}, [stream]),
        ps_p = "ps -o pid,utime,time,vsz,rss,majflt,minflt -p "++os:getpid()++"\n",
        ps_A = "ps -A -o cputime=,utime=\n"}.

os_info(#sysapi{type = procfs, proc_stat = ProcStat, proc_self_stat = ProcSelfStat}) ->
    proc_stat(ProcStat, proc_self_stat(ProcSelfStat, #sys_os{}));
os_info(#sysapi{type = ps, port = Port, ps_p = Ps_p, ps_A = Ps_A}) ->
    ps_A(Port, Ps_A, ps_p(Port, Ps_p, #sys_os{}));
os_info(_) ->
    #sys_os{}.

proc_stat(FD, OS) ->
    %%user nice kernel idle iowait irq softirq steal
    {ok, Str} = file:pread(FD, 0, 200),
    [User, Nice, Kernel, Idle, Iowait] =
        case string:tokens(Str, " \n") of
            ["cpu", I1, I2, I3, I4, I5|_] -> [I1, I2, I3, I4, I5];
            _                             -> [0, 0, 0, 0, 0]
        end,
    OS#sys_os{
        user   = jiffy_to_sec(User),
        nice   = jiffy_to_sec(Nice),
        kernel = jiffy_to_sec(Kernel),
        idle   = jiffy_to_sec(Idle),
        iowait = jiffy_to_sec(Iowait)}.

proc_self_stat(FD, OS) ->
    {ok, Str} = file:pread(FD, 0, 200),
    {Minflt, Majflt, Utime, Stime, Vsz, Rss} = proc_self_stat(Str),
    OS#sys_os{
        beam_user   = jiffy_to_sec(Utime),
        beam_kernel = jiffy_to_sec(Stime),
        vsz         = to_int(Vsz),   %% in bytes
        rss         = to_int(Rss),   %% in pages...
        minflt      = to_int(Minflt),
        majflt      = to_int(Majflt)}.

%%% pid, comm, state, ppid, pgrp, session, tty_nr, tpgid, flags,
%%% minflt, cminflt, majflt, cmajflt, utime, stime, cutime, cstime,
%%% priority, nice, num_threads, itrealvalue, starttime, vsize, rss
proc_self_stat(Str) ->
    case string:tokens(Str, " ") of
        [_, _, _, _, _, _, _, _, _, I10, _, I12, _,
         I14, I15, _, _, _, _, _, _, _, I23, I24|_] ->
            {I10, I12, I14, I15, I23, I24};
        _ ->
            {0, 0, 0, 0, 0, 0}
    end.

jiffy_to_sec(J) ->
    to_int(J)/100. %should use a better transform jiffies->secs

total_ram(linux) ->
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

%% ps -p
ps_p(Port, Cmd, SysOS) ->
    case get_ps_data(Port, Cmd) of
        [["PID", "UTIME", "TIME", "VSZ", "RSS", "MAJFLT", "MINFLT"],
         %%% ["1", "0:00.20", "0:00.30", "2488932", "12600", "-", "-"]]
         [_, Utime, Time, Vsz, Rss, MajFault, MinFault]] ->
            UtimeSec = timestr_to_sec(Utime), % user CPU time for this proc
            TimeSec = timestr_to_sec(Time),  % system+user time for this proc
            SysOS#sys_os
                {beam_user = UtimeSec,
                beam_kernel = TimeSec-UtimeSec,
                vsz = to_int(Vsz)*1024,        % to bytes
                rss = to_int(Rss),             % in kB pages
                minflt = to_int(MinFault),
                majflt = to_int(MajFault)};
        _ ->
            SysOS
    end.

%% ps -A
ps_A(Port, Cmd, SysOS) ->
    {User, Kernel} = get_user_and_kernel(Port, Cmd),
    SysOS#sys_os{user = User, kernel = Kernel}.

get_user_and_kernel(Port, Cmd) ->
    lists:foldl(fun get_user_kernel/2, {0, 0}, get_ps_data(Port, Cmd)).

get_user_kernel(Strs, {User, Kernel}) ->
    try
        [Ustr, Kstr] = Strs,
        {User+timestr_to_sec(Ustr), Kernel+timestr_to_sec(Kstr)}
    catch
        _:_ -> {User, Kernel}
    end.

get_ps_data(Port, Cmd) ->
    case port_command(Port, Cmd, []) of
        true ->
            receive
                {Port, {data, Data}} ->
                    [string:tokens(L, " ") || L <- string:tokens(Data, "\n")]
            end;
        false ->
            []
    end.

%% "8:11.15"
timestr_to_sec(Str) ->
    case string:tokens(Str, ":.") of
        [M, S, Centi] -> 60*to_int(M) + to_int(S) + to_int(Centi)/100;
        [H, M, S, Centi] -> 3600*to_int(H) + 60*to_int(M) + to_int(S) + to_int(Centi)/100;
        _ -> 0
    end.

%%%-------------------------------------------------------------------
%% collect info about an erlang process.
%% uses erlang:process_info/2

%% maps processes() to a map #{pid() => #prc{}}
%% bail if process_count is too high

fill_prc_data(LD) ->
    Data = LD#ld.data,
    New = fill_prc_data(LD#ld.max_procs, Data),
    LD#ld{data = Data#data{prc = New}}.

fill_prc_data(MaxProcs, Data) ->
    case MaxProcs < erlang:system_info(process_count) of
        true  -> #{};
        false -> fill_prcs(Data)
    end.

fill_prcs(Data) ->
    lists:foldl(mk_fill_prc(Data), #{}, processes()).

mk_fill_prc(#data{prc = Prcs, sys = #sys{cpu_per_red = CpR}}) ->
    fun(Pid, O) -> fill_prc(Pid, CpR, Prcs, O) end.

fill_prc(Pid, CpR, Prcs, O) ->
    P = #prc{pid = Pid,
        name = prc_info(Pid, name),
        memory = prc_info(Pid, memory),
        reductions = prc_info(Pid, reductions),
        current_function = prc_info(Pid, current_function),
        message_queue_len = prc_info(Pid, message_queue_len)},
    case Prcs of
        #{Pid := #prc{reductions = Reds}} ->
            O#{Pid => P#prc{cpu = 100*(P#prc.reductions-Reds)*CpR}}; %% in percent
        _ ->
            O#{Pid => P}
    end.


prc_info(Pid, name) -> name(Pid);
prc_info(Pid, Tag) ->
    case erlang:process_info(Pid, Tag) of
        undefined      -> get_default(Tag, Pid);
        {_, undefined} -> get_default(Tag, Pid);
        []             -> mod_val(Pid, Tag, "");
        {Tag, Val}     -> mod_val(Pid, Tag, Val)
    end.

name(Pid) ->
    case prc_info(Pid, registered_name) of
        [] ->
            case prc_info(Pid, {dictionary,'$process_label'}) of
                [] -> prc_info(Pid, initial_call);
                Val -> Val
            end;
        Val -> Val
    end.

get_default(Tag, Pid) ->
    {_, Default} = prcinfo(Tag, Pid),
    Default.

mod_val(Pid, Tag, Val) ->
    {Modder, _} = prcinfo(Tag, Pid),
    Modder(Val).

%% A table, maps Tag -> {ModifierFun, DefaultVal}
prcinfo({dictionary,'$process_label'}, _) ->
    {fun(Val) -> Val end,
     []};
prcinfo(stack_size, _) ->
    {fun(Val) -> 8*Val end,
     0};
prcinfo(heap_size, _) ->
    {fun(Val) -> 8*Val end,
     0};
prcinfo(total_heap_size, _) ->
    {fun(Val) -> 8*Val end,
     0};
prcinfo(reductions, _) ->
    {fun(Val) -> Val end,
     0};
prcinfo(last_calls, Pid) ->
    {fun(Val) ->
            case Val of
                false -> try process_flag(Pid, save_calls, 16)
                         catch _:_ -> ok end,
                    [];
                Calls -> lists:usort(Calls)
            end
     end,
     []};
prcinfo(registered_name, _) ->
    {fun(Val) -> Val end,
     []};
prcinfo(initial_call, Pid) ->
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
prcinfo(_, _) ->
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

%% setters

set_net({recv_avg, V}, Net) -> Net#net{recv_avg = V};
set_net({recv_cnt, V}, Net) -> Net#net{recv_cnt = V};
set_net({recv_dvi, V}, Net) -> Net#net{recv_dvi = V};
set_net({recv_max, V}, Net) -> Net#net{recv_max = V};
set_net({recv_oct, V}, Net) -> Net#net{recv_oct = V};
set_net({send_avg, V}, Net) -> Net#net{send_avg = V};
set_net({send_cnt, V}, Net) -> Net#net{send_cnt = V};
set_net({send_max, V}, Net) -> Net#net{send_max = V};
set_net({send_oct, V}, Net) -> Net#net{send_oct = V};
set_net({send_pend,V}, Net) -> Net#net{send_pend = V}.

fill_net_data(LD) ->
    Data = LD#ld.data,
    New = lists:map(fun port_info/1, lists:sort(erlang:ports())),
    LD#ld{data = Data#data{net = New}}.

%%returns #net{}
port_info(P) ->
    try maybe_inet(#net{port = P,
        name = port_name(P),
        input = erlang:port_info(P, input),
        output = erlang:port_info(P, output)})
    catch _:_ -> #net{port = P}
    end.

maybe_inet(Net = #net{name = "tcp"++_}) -> add_inet(Net);
maybe_inet(Net = #net{name = "udp"++_}) -> add_inet(Net);
maybe_inet(Net) -> Net.

add_inet(Net = #net{port = Port}) ->
    try {ok, Stats} = inet:getstat(Port),
        lists:foldl(fun set_net/2, Net, Stats)
    catch _:_ -> Net
    end.

port_name(P) ->
    case erlang:port_info(P, name) of
        {name, "udp_inet"} ->
            {ok, Port} = inet:port(P),
            flat("udp:~p", [Port]);
        {name, "tcp_inet"} ->
            {ok, {IP, Port}} = inet:peername(P),
            flat("tcp:~p:~p-~s", [IP, Port, tcp_name(IP, Port)]);
        {name, Name} ->
            Name
    end.

tcp_name(IP, Port) ->
    case inet:gethostbyaddr(IP) of
        {ok, #hostent{h_name = HostName}} ->
            try
                {ok, Socket} = gen_tcp:connect(IP, 4369, [inet], 100),
                inet:close(Socket),
                {ok, Names} = erl_epmd:names(IP),
                {value, {NodeName, Port}} = lists:keysearch(Port, 2, Names),
                NodeName++"@"++HostName
            catch
                _:_ -> HostName
            end;
        _ ->
            ""
    end.

%%----------------------------------------------------------------------------------------
%% mnesia activity

fill_mnesia_data(LD) ->
    ?UPDATE_LD_DATA(LD, LD#ld.data, mnesia, mnesia_info()).

mnesia_info() ->
    try mnesia:system_info(is_running) of
        no  -> #mnesia{};
        yes -> mnesia()
    catch
        _:_ -> #mnesia{}
    end.

mnesia() ->
    #mnesia{
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
-type table_type()  :: dets | ets | undefined.
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
object_count(_Table, undefined)   -> undefined;
object_count(Table,  dets)        -> dets_object_count(Table);
object_count(Table,  ets)         -> ets_object_count(Table).

-spec table_size(table_name()) -> bytes() | undefined.
table_size(Table) ->
    table_size(Table, get_term_storage_type(Table)).

-spec table_size(table_name(), table_type()) -> bytes() | undefined.
table_size(_Table, undefined)   -> undefined;
table_size(Table,  dets)        -> dets_size(Table);
table_size(Table,  ets)         -> ets_size(Table).

-spec get_term_storage_type(table_name()) -> table_type().
get_term_storage_type(Table) ->
    case mnesia:table_info(Table, storage_type) of
        disc_copies      -> ets;
        disc_only_copies -> dets;
        _                -> undefined
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
        M when 15 =< M -> human(M-15,"P");
        M when 12 =< M -> human(M-12,"T");
        M when  9 =< M -> human(M-9,"G");
        M when  6 =< M -> human(M-6,"M");
        M when  3 =< M -> human(M-3,"k");
        _              -> flat("~w",[I])
    end;
human(_) -> "0".

human(E, M) ->
    flat("~.1f~s", [math:pow(10,E), M]).

pipe(X0, Fs) ->
    lists:foldl(fun(F, X) -> F(X) end, X0, Fs).
