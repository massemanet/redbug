%%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% File    : redbg.erl
%%% Author  : Mats Cronqvist <masse@cronqvi.st>
%%% Description :
%%%
%%% Created : 24 Jan 2007 by Mats Cronqvist <locmacr@mwlx084>
%%%-------------------------------------------------------------------
-module(redbug).

-export([help/0]).
-export([start/1, start/2]).
-export([stop/0, stop/1]).
-export([dtop/0, dtop/1]).
-export([redbug_name/1]).

-define(
   log(T),
   error_logger:info_report(
     [{function, {?MODULE, ?FUNCTION_NAME}}, {line, ?LINE}|T])).

%% erlang:get_stacktrace/0 was made obsolete in OTP21
-ifdef(OTP_RELEASE). %% this implies 21 or higher
-define(EXCEPTION(Class, Reason, StackToken), Class:Reason:StackToken).
-define(GET_STACK(StackToken), StackToken).
-else.
-define(EXCEPTION(Class, Reason, _), Class:Reason).
-define(GET_STACK(_), erlang:get_stacktrace()).
-endif.

%% the redbug server data structure
%% most can be set in the input proplist
-record(cnf,
        {
         %% general
         time         = 15000,       % stop trace after this time [ms]
         msgs         = 10,          % stop trace after this # msgs [unit]
         target       = node(),      % target node
         cookie       = '',          % target node cookie
         blocking     = false,       % run blocking; return a list of msgs
         procs        = all,         % list of procs (or 'all')
         max_queue    = 5000,        % max # of msgs before suicide
         max_msg_size = 50000,       % max message size before suicide
         debug        = false,       % big error messages
         trace_child  = false,       % children gets traced (set_on_spawn)
         arity        = false,       % arity instead of args
         discard      = false,       % discard messages (when counting)
         %% print-related
         buffered     = false,       % output buffering
         records      = [],          % list of module names to get records from
         print_calls  = true,        % print calls
         print_file   = "",          % file to print to (standard_io)
         print_msec   = false,       % print milliseconds in timestamps?
         print_depth  = 999999,      % Limit for "~P" formatting depth
         print_re     = "",          % regexp that must match to print
         print_return = true,        % print return value
         print_fun    = '',          % custom print handler
         %% trc file-related
         file         = "",          % file to write trace msgs to
         file_size    = 1,           % file size (per file [Mb])
         file_count   = 8,           % number of files in wrap log
         %% internal
         trc          = [],          % cannot be set by user
         shell_pid    = [],          % cannot be set by user
         print_pid    = [],          % cannot be set by user
         trc_pid      = []           % cannot be set by user
        }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Prints help.
help() ->
  Text =
    ["redbug - the (sensibly) Restrictive Debugger"
    , ""
    , "  redbug:start(Trc) -> start(Trc, [])."
    , "  redbug:start(Trc, Opts)."
    , ""
    , "  redbug is a tool to interact with the Erlang trace facility."
    , "  It will instruct the Erlang VM to generate so called "
    , "  'trace messages' when certain events (such as a particular"
    , "  function being called) occur."
    , "  The trace messages are either printed (i.e. human readable)"
    , "  to a file or to the screen; or written to a trc file."
    , "  Using a trc file puts less stress on the system, but"
    , "  there is no way to count the messages (so the msgs opt"
    , "  is ignored), and the files can only be read by special tools"
    , "  (such as 'bread'). Printing and trc files cannot be combined."
    , "  By default (i.e. if the 'file' opt is not given), messages"
    , "  are printed."
    , ""
    , "Trc: list('send'|'receive'|string(RTP))"
    , "RTP:  restricted trace pattern"
    , "  the RTP has the form: \"<mfa> when <guards> -> <actions>\""
    , "  where <mfa> can be;"
    , "  \"mod\", \"mod:fun\", \"mod:fun/3\" or \"mod:fun('_', atom, X)\""
    , "  <guard> is something like;"
    , "  \"X==1\" or \"is_atom(A)\""
    , "  and <action> is;"
    , "  \"return\" and/or \"stack\" (separated by \";\")"
    , ""
    , "  E.g."
    , "  ets:lookup(T, hostname) when is_integer(T) -> stack"
    , ""
    , "Opts: list({Opt, Val})"
    , "  general opts:"
    , "time         (15000)       stop trace after this many ms"
    , "msgs         (10)          stop trace after this many msgs"
    , "target       (node())      node to trace on"
    , "cookie       (host cookie) target node cookie"
    , "blocking     (false)       block start/2, return a list of messages"
    , "arity        (false)       print arity instead of arg list"
    , "trace_child  (false)       children gets traced (set_on_spawn)"
    , "records      ([])          list of module names to get records from"
    , "buffered     (false)       buffer messages till end of trace"
    , "discard      (false)       discard messages (when counting)"
    , "max_queue    (5000)        fail if internal queue gets this long"
    , "max_msg_size (50000)       fail if seeing a msg this big"
    , "procs        (all)         (list of) Erlang process(es)"
    , "                             all|pid()|atom(RegName)|{pid, I2, I3}"
    , "  print-related opts"
    , "print_calls  (true)        print calls"
    , "print_file   (standard_io) print to this file"
    , "print_msec   (false)       print milliseconds on timestamps"
    , "print_depth  (999999)      formatting depth for \"~P\""
    , "print_re     (\"\")          print only strings that match this RE"
    , "print_return (true)        print the return value"
    , "print_fun    ()            custom print handler, fun/1 or fun/2;"
    , "                             fun(TrcMsg) -> <ignored>"
    , "                             fun(TrcMsg, AccOld) -> AccNew"
    , "  trc file related opts"
    , "file         (none)        use a trc file based on this name"
    , "file_size    (1)           size of each trc file"
    , "file_count   (8)           number of trc files"
    , ""
    ],
  lists:foreach(fun(S) -> io:fwrite(standard_io, "~s~n", [S])end, Text).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% dtop facade

%% @equiv dtop(#{})
dtop() ->
  dtop(#{}).

%% @docfile "doc/dtop.edoc"
dtop(Cfg) ->
  try
    redbug_dtop:start(),
    dtop_cfg(Cfg)
  catch
    exit:already_started ->
      case maps:size(Cfg) of
        0 -> redbug_dtop:stop();
        _ -> dtop_cfg(Cfg)
      end
  end.

dtop_cfg(Cfg) ->
  [redbug_dtop:sort(S) || (S = maps:get(sort, Cfg, "")) =/= ""],
  [redbug_dtop:max_prcs(M) || (M = maps:get(max_procs, Cfg, "")) =/= ""].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API from erlang shell
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc
%% Stops a trace.
%% @end
stop() ->
  stop(erlang:node()).

stop(Target) ->
  case whereis(redbug_name(Target)) of
    undefined -> not_started;
    Pid -> Pid ! stop, stopped
  end.

%% @equiv start(RTPs, [])
start(RTPs) ->
  start(RTPs, []).

%% @spec start(RTPs::list(), Opts::map()) -> {Procs::integer(), Functions::integer()}
%% @docfile "doc/start.edoc"
start('send', Props)    -> start([send], Props);
start('receive', Props) -> start(['receive'], Props);
start(Trc, Props) when is_map(Props) ->
  start(Trc, maps:to_list(Props));
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% the real start function!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start(Trc, Props) when is_list(Props) ->
  RedbugName = redbug_name(Props),
  case whereis(RedbugName) of
    undefined ->
      try
        Cnf = assert_print_fun(make_cnf(Trc, [{shell_pid, self()}|Props])),
        assert_cookie(Cnf),
        register(RedbugName, spawn(fun() -> init(Cnf) end)),
      
        maybe_block(Cnf, block_a_little(RedbugName))
      catch
        R   -> R;
        C:R -> {oops, {C, R}}
      end
      ;
    _ ->
      redbug_already_started
  end.

assert_print_fun(Cnf) ->
  case is_function(Cnf#cnf.print_fun) of
    true -> Cnf;
    false-> Cnf#cnf{print_fun=mk_print_fun(Cnf)}
  end.

mk_print_fun(Cnf) ->
  case Cnf#cnf.blocking of
    false-> mk_outer(Cnf);
    true -> mk_blocker()
  end.

assert_cookie(#cnf{cookie=''}) -> ok;
assert_cookie(Cnf) -> erlang:set_cookie(Cnf#cnf.target, Cnf#cnf.cookie).

block_a_little(ProcessName) ->
  Ref = erlang:monitor(process, ProcessName),
  receive
    {running, NoP, NoF}  -> erlang:demonitor(Ref), {ProcessName, NoP, NoF};
    {'DOWN', Ref, _, _, R} -> R
  end.

maybe_block(#cnf{blocking=true}, {ProcessName, I, _}) when is_integer(I) -> block(ProcessName);
maybe_block(_, R) -> R.

block(ProcessName) ->
  Ref = erlang:monitor(process, ProcessName),
  receive
    {'DOWN', Ref, _, _, R} -> R
  end.

%% turn the proplist inta a #cnf{}
make_cnf(Trc, Props) ->
  make_cnf(proplists:unfold(Props), #cnf{trc=Trc}, record_info(fields, cnf)).

make_cnf([], Cnf, _) -> Cnf;
make_cnf([{Tag, Val}|Props], Cnf, Tags) ->
  make_cnf(Props, setelement(findex(Tag, Tags)+1, Cnf, Val), Tags).

findex(Tag, [])       -> throw({no_such_option, Tag});
findex(Tag, [Tag|_])  -> 1;
findex(Tag, [_|Tags]) -> findex(Tag, Tags)+1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% the main redbug process
%%% a state machine. init, starting, running, stopping, wait_for_trc.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(Cnf) ->
  process_flag(trap_exit, true),
  try
    starting(do_start(Cnf))
  catch
    throw:R ->
      exit({argument_error, R});
    ?EXCEPTION(C, R, S) when Cnf#cnf.debug andalso not Cnf#cnf.blocking ->
      ?log([{C, R}, {stack, ?GET_STACK(S)}]);
    _:R ->
      exit(R)
  end.

starting(Cnf = #cnf{trc_pid=TrcPid}) ->
  receive
    {{starting, TrcPid, P, F}} -> running(run(Cnf, P, F));
    {'EXIT', TrcPid, R} -> throw(R)
  end.

running(Cnf = #cnf{trc_pid=TrcPid, print_pid=PrintPid}) ->
  receive
    stop ->
      TrcPid ! stop,
      wait_for_trc(Cnf),
      PrintPid ! stop,
      done(Cnf, {stopped, wait_for_printer(Cnf)});
    {'EXIT', TrcPid, R} ->
      PrintPid ! stop,
      done(Cnf, {R, wait_for_printer(Cnf)});
    {'EXIT', PrintPid, R} ->
      TrcPid ! stop,
      wait_for_trc(Cnf),
      done(Cnf, {printer_crash, R});
    X ->
      ?log([{unknown_message, X}])
  end.

wait_for_trc(#cnf{trc_pid=TrcPid}) ->
  receive
    {'EXIT', TrcPid, stop} ->
      ok;
    {'EXIT', TrcPid, R} ->
      ?log([{trace_control_died, R}])
  end.

wait_for_printer(#cnf{print_pid=PrintPid}) ->
  receive
    {'EXIT', PrintPid, R} -> R
  end.

done(#cnf{blocking=false}, {Reason, Answer}) ->
  io:fwrite("~s", [done_string(Reason)]),
  io:fwrite("redbug done, ~p - ~p~n", [Reason, Answer]);
done(#cnf{blocking=true}, Answer) ->
  exit(Answer).

done_string(Reason) ->
  case is_tuple(Reason) andalso element(1, Reason) of
    msg_queue ->
      "you might want to set the max_queue option (see redbug:help/0)\n";
    stack_size ->
      "you might want to set the max_msg_size option (see redbug:help/0)\n";
    arg_length ->
      "you might want to set the max_msg_size option (see redbug:help/0)\n";
    arg_size ->
      "you might want to set the max_msg_size option (see redbug:help/0)\n";
    _ ->
      ""
  end.

run(Cnf, P, F) ->
  [Cnf#cnf.shell_pid ! {running, P, F} || is_pid(Cnf#cnf.shell_pid)],
  Cnf.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_start(OCnf) ->
  Cnf = spawn_printer(wrap_print_fun(OCnf), maybe_new_target(OCnf)),
  Cnf#cnf{trc_pid=redbug_targ:start(Cnf#cnf.target, pack(Cnf))}.

maybe_new_target(Cnf = #cnf{target=Target}) ->
  case lists:member($@, Str=atom_to_list(Target)) of
    true -> Cnf;
    false-> Cnf#cnf{target=list_to_atom(Str++"@"++element(2, inet:gethostname()))}
  end.

wrap_print_fun(#cnf{print_fun=PF}) ->
  case erlang:fun_info(PF, arity) of
    {arity, 1} -> fun(M, N) -> PF(M), maybe_update_count(M, N) end;
    {arity, 2} -> PF
  end.

maybe_update_count(M, N) ->
  case element(1, M) of
    call -> N+1;
    send -> N+1;
    recv -> N+1;
    _    -> N
  end.

spawn_printer(PrintFun, Cnf) ->
  Cnf#cnf{print_pid=spawn_link(fun() -> print_init(PrintFun) end)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% the default print fun for blocking mode (i.e. return a term).
%% the printer proc will execute this (or a user-provided fun) for
%% each message from the target.
%% we throw away call_time, call_count, and meta messages.

mk_blocker() ->
  fun({call_time, {_, false}, _, _}, A)  -> A;
     ({call_time, {_, []}, _, _}, A)     -> A;
     ({call_count, {_, false}, _, _}, A) -> A;
     ({call_count, {_, 0}, _, _}, A)     -> A;
     ({meta, {recs, Recs}, _, _}, A)     -> put_recs(Recs), A;
     ({meta, stop, _, _}, 0)             -> [];
     ({meta, stop, _, _}, A)             -> lists:reverse(A);
     (X, 0)                              -> [expand(X)];
     (X, A)                              -> [expand(X)|A]
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% the default print fun in non-blocking mode (i.e. to the shell).
%% the printer proc will execute this (or a user-provided fun) for
%% each message from the target.

mk_outer(#cnf{file=[_|_]}) ->
  fun(_) -> ok end;
mk_outer(#cnf{print_depth=Depth, print_msec=MS, print_return=Ret, print_calls=Calls} = Cnf) ->
  OutFun = mk_out(Cnf),
  fun({Tag, Data, PI, TS}) -> outer(Tag, Data, PI, TS, Depth, MS, Ret, Calls, OutFun) end.

outer(Tag, Data, PI, TS, Depth, MS, Ret, Calls, OutFun) ->
  MTS = fix_ts(MS, TS),
  case {Tag, Data} of
    {'meta', {recs, Recs}} ->
      put_recs(Recs);
    {'meta', _} ->
      ok;
    {'call_time', {_, false}} ->
      ok;
    {'call_time', {{M, F, A}, PerProcCT}} ->
      {Count, Time} = lists:foldl(fun per_proc/2, {0, 0}, PerProcCT),
      [OutFun("% ~6s : ~6s : ~6s : ~w:~w/~w",
              [human(Count), human(Time), human(Time/Count), M, F, A]) || 0 < Count];
    {'call_count', {_, false}} ->
      ok;
    {'call_count', {{M, F, A}, Count}} ->
      [OutFun("~n% ~6s : ~w:~w/~w", [human(Count), M, F, A]) || 0 < Count];
    {'call', {{M, F, A}, Bin}} ->
      case Calls of
        true ->
          case is_integer(A) of
            true ->
              OutFun("~n% ~s ~s~n% ~w:~w/~w", [MTS, to_str(PI), M, F, A]);
            false->
              Args = [flat("~P", [expand(A0), Depth]) || A0 <- A],
              AL = string:join(Args, ", "),
              OutFun("~n% ~s ~s~n% ~w:~w(~s)", [MTS, to_str(PI), M, F, AL])
          end,
          lists:foreach(fun(L) -> OutFun("%   ~s", [L]) end, stak(Bin));
        false->
          ok
      end;
    {'retn', {{M, F, A}, Val0}} ->
      Val = case Ret of
              true  -> expand(Val0);
              false -> '...'
            end,
      OutFun("~n% ~s ~s~n% ~p:~p/~p -> ~P",
             [MTS, to_str(PI), M, F, A, Val, Depth]);
    {'send', {MSG, ToPI}} ->
      OutFun("~n% ~s ~s~n% ~s <<< ~P",
             [MTS, to_str(PI), to_str(ToPI), expand(MSG), Depth]);
    {'recv', MSG} ->
      OutFun("~n% ~s ~s~n% <<< ~P",
             [MTS, to_str(PI), expand(MSG), Depth])
  end.

per_proc({_, Count, Sec, Usec}, {AC, AT}) ->
  {Count+AC, Sec*1000000+Usec+AT}.

to_str({Pid, Reg}) ->
  flat("~w(~p)", [Pid, Reg]);
to_str(RegisteredName) ->
  flat("~p", [RegisteredName]).

%% expand records, if any
expand(T) when is_list(T) -> improper_map(fun expand/1, T);
expand(T) when is_map(T) -> maps:map(fun(_K, V) -> expand(V) end, T);
expand(T) when is_tuple(T) andalso 0 < size(T) ->
  case get({element(1, T), size(T)-1}) of
    undefined -> list_to_tuple(expand(tuple_to_list(T)));
    Fields -> maps:from_list(lists:zip(['_RECORD'|Fields], expand(tuple_to_list(T))))
  end;
expand(T) -> T.

%% handle improper lists
improper_map(_, []) -> [];
improper_map(F, [E]) -> [F(E)];
improper_map(F, [A|B]) when not is_list(B) -> [F(A)|F(B)];
improper_map(F, [A|B]) -> [F(A)|improper_map(F, B)].

%% we assert that the file can be created here (in the redbug proc),
%% but we don't actually open the file until we run the fun (in the
%% printer proc).
mk_out(#cnf{print_re=RE, print_file=File}) ->
  assert_dir(File),
  fun(F, A) ->
      Str=flat(F, A),
      case RE =:= "" orelse re:run(Str, RE) =/= nomatch of
        true  -> io:fwrite(get_fd(File), "~s~n", [Str]);
        false -> ok
      end
  end.

-include_lib("kernel/include/file.hrl").
-define(FILEINFO(T, A), #file_info{type = T, access = A}).

assert_dir([]) -> ok;
assert_dir(File) ->
  case filelib:ensure_dir(File) of
    ok ->
      case file:read_file_info(filename:dirname(File)) of
        {ok, ?FILEINFO(directory, read_write)} -> ok;
        {ok, ?FILEINFO(directory, write)} -> ok;
        {ok, ?FILEINFO(T, A)} -> throw({file_error, {File, {T, A}}});
        {error, R} ->  throw({file_error, {File, R}})
      end;
    {error, R} -> throw({file_error, {File, R}})
  end.

fix_ts(MS, TS) ->
  case MS of
    true -> ts_ms(TS);
    false-> ts(TS)
  end.

ts({H, M, S, _Us}) ->
  flat("~2.2.0w:~2.2.0w:~2.2.0w", [H, M, S]).
ts_ms({H, M, S, Us}) ->
  flat("~2.2.0w:~2.2.0w:~2.2.0w.~3.3.0w", [H, M, S, Us div 1000]).

%%% call stack handler
stak(Bin) ->
  L = string:tokens(binary_to_list(Bin), "\n"),
  lists:reverse(lists:foldl(fun munge/2, [], L)).

munge(I, Out) ->
  case lists:reverse(I) of
    "..."++_ -> [truncated|Out];
    _ ->
      case string:str(I, "Return addr") of
        0 ->
          case string:str(I, "cp = ") of
            0 -> Out;
            _ -> [mfaf(I)|Out]
          end;
        _ ->
          case string:str(I, "erminate process normal") of
            0 -> [mfaf(I)|Out];
            _ -> Out
          end
      end
  end.

mfaf(I) ->
  [_, C|_] = string:tokens(I, "()+"),
  C.

put_recs(Recs) ->
  [put({Name, length(Fields)}, Fields) || {Name, Fields} <- Recs].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% pack data into a proplist for target consumption
%%% Proplist = list({Tag, Val})
%%% Tag = time | flags | asts | procs | where
%%% Where = {buffer, Pid, Count, MaxQueue, MaxSize} |
%%%         {stream, Pid, Count, MaxQueue, MaxSize} |
%%%         {discard, Pid, Count, MaxQueue, MaxSize} |
%%%         {file, File, Size, Count} |
%%%         {ip, Port, Queue}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pack(Cnf) ->
  Flags0 = [call, timestamp],
  {Flags, ASTs} = lists:foldl(fun chk_trc/2, {Flags0, []}, slist(Cnf#cnf.trc)),
  [{time, chk_time(Cnf#cnf.time)},
   {flags, maybe_arity(Cnf, maybe_trace_child(Cnf, Flags))},
   {asts, ASTs},
   {procs, [chk_proc(P) || P <- mk_list(Cnf#cnf.procs)]},
   {records, chk_records(Cnf#cnf.records)},
   {where, where(Cnf)}].

mk_list([]) -> throw(no_procs);
mk_list([_|_] = L) -> L;
mk_list(E) -> [E].

where(Cnf) ->
  case Cnf#cnf.file of
    "" -> conf_term(Cnf);
    _  -> conf_file(Cnf)
  end.

conf_file(Cnf) ->
  {file, Cnf#cnf.file, Cnf#cnf.file_size, Cnf#cnf.file_count}.

conf_term(Cnf) ->
  {chk_buffered(Cnf#cnf.buffered, Cnf#cnf.discard),
   Cnf#cnf.print_pid,
   chk_msgs(Cnf#cnf.msgs),
   Cnf#cnf.max_queue,
   Cnf#cnf.max_msg_size}.

maybe_arity(#cnf{arity=true}, Flags) -> [arity|Flags];
maybe_arity(_, Flags)                -> Flags.

maybe_trace_child(#cnf{trace_child=true}, Flags) -> [set_on_spawn|Flags];
maybe_trace_child(_, Flags)                      -> Flags.

chk_time(Time) when is_integer(Time) -> Time;
chk_time(X) -> throw({bad_time, X}).

chk_buffered(_, true)  -> discard;
chk_buffered(true, _)  -> buffer;
chk_buffered(false, _) -> stream.

chk_proc(Pid) when is_pid(Pid) -> Pid;
chk_proc(Atom) when is_atom(Atom) -> Atom;
chk_proc({pid, I1, I2}) when is_integer(I1), is_integer(I2) -> {pid, I1, I2};
chk_proc(X) -> throw({bad_proc, X}).

chk_records([]) -> [];
chk_records(Mod) when is_atom(Mod) -> [Mod];
chk_records([Mod|Mods]) when is_atom(Mod) -> [Mod|chk_records(Mods)];
chk_records(What) -> throw({bad_module_name, What}).

chk_msgs(Msgs) when is_integer(Msgs) -> Msgs;
chk_msgs(X) -> throw({bad_msgs, X}).

chk_trc('send', {Flags, Trc})    -> {['send'|Flags], Trc};
chk_trc('receive', {Flags, Trc}) -> {['receive'|Flags], Trc};
chk_trc(Trc, {Flags, ASTs})      -> {Flags, [mk_ast(Trc)|ASTs]}.

mk_ast(Str) -> redbug_compiler:parse(Str).

-define(is_string(Str), (Str=="" orelse (9=<hd(Str) andalso hd(Str)=<255))).

slist(S) when ?is_string(S) -> [S];
slist(L) when is_list(L) -> lists:usort(L);
slist(X) -> [X].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% the print_loop process
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_init(PrintFun) ->
  print_loop(PrintFun, 0, running).

print_loop(PrintFun, Acc, State) ->
  maybe_exit(State, PrintFun, Acc),
  receive
    Ms = [_|_] -> print_loop(PrintFun, lists:foldl(PrintFun, Acc, Ms), State);
    stop -> print_loop(PrintFun, Acc, stopping)
  end.

maybe_exit(State, PrintFun, Acc) ->
  case State == stopping andalso process_info(self(), message_queue_len) of
    {_, 0} -> exit(PrintFun({meta, stop, dummy, {0, 0, 0, 0}}, Acc));
    _ -> ok
  end.

%% this is called from the default PrintFun
get_fd("") ->
  standard_io;
get_fd(FN) ->
  case get(redbug_fd) of
    undefined ->
      case file:open(FN, [write]) of
        {ok, FD} -> put(redbug_fd, FD), FD;
        _ -> throw({cannot_open, FN})
      end;
    FD -> FD
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%make ints human readable

human(X) when not is_number(X) -> X;
human(I) when I < 0 -> "-"++human(-I);
human(I) when 0 < I ->
  case math:log10(I) of
    M when 15=<M -> human(M-15, "P");
    M when 12=<M -> human(M-12, "T");
    M when  9=<M -> human(M-9, "G");
    M when  6=<M -> human(M-6, "M");
    M when  3=<M -> human(M-3, "k");
    _            -> flat("~w", [I])
  end;
human(_) -> "0".

human(E, M) ->
  flat("~.1f~s", [math:pow(10, E), M]).

flat(Format, Args) ->
  lists:flatten(io_lib:fwrite(Format, Args)).

redbug_name(Opts) when is_list(Opts) ->
  Node = proplists:get_value(target, Opts, erlang:node()),
  redbug_name(Node);

redbug_name(Node) when is_atom(Node) ->
  list_to_atom("redbug_" ++ atom_to_list(Node)).
