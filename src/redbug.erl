%%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% File    : redbg.erl
%%% Author  : Mats Cronqvist <masse@cronqvi.st>
%%% Description :
%%%
%%% Created : 24 Jan 2007 by Mats Cronqvist <locmacr@mwlx084>
%%%-------------------------------------------------------------------
-module(redbug).

-export([main/1]).

-export([help/0]).
-export([start/1,start/2,start/3,start/4,start/5]).
-export([stop/0]).

-define(log(T),error_logger:info_report(
                 [process_info(self(),current_function),
                  {line,?LINE}|T])).

%% erlang:get_stacktrace/0 was made obsolete in OTP21
-ifdef('OTP_RELEASE'). %% implies >= OTP21
-define(try_with_stack(F),
        try {ok,F} catch __C:__R:__S -> {__C,__R,__S} end).
-else.
-define(try_with_stack(F),
        try {ok,F} catch __C:__R -> {__C,__R,erlang:get_stacktrace()} end).
-endif.

%% the redbug server data structure
%% most can be set in the input proplist
-record(cnf,{
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
          trc_pid      = [],          % cannot be set by user
          cons_pid     = []           % cannot be set by user
         }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Called as an escript
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
main(Args) ->
  %% In the record definition, the default target node is node(),
  %% but that doesn't make sense for our escript, so make it
  %% 'undefined' and fail if it isn't filled in.
  Cnf0 = #cnf{target = undefined, print_fun = mk_outer(#cnf{})},
  Cnf1 =
    try handle_args(Args, Cnf0)
    catch throw:Error ->
        io:fwrite("Error: ~s~n~n", [Error]),
        help(escript),
        halt_and_flush(1)
    end,
  Cnf = maybe_new_target(Cnf1),
  start_distribution(Cnf),
  case ?try_with_stack(init(Cnf)) of
    {ok, _} ->
      halt_and_flush(0);
    {C,R,S} ->
      io:fwrite("~p~n",[{C,R,S}]),
      halt_and_flush(1)
  end.

halt_and_flush(Status) ->
  %% As far as I can tell, this is the best (but still not perfect)
  %% way to try to ensure that all output has been written before we
  %% exit. See:
  %% http://erlang.org/pipermail/erlang-questions/2011-April/057479.html
  init:stop(Status),
  timer:sleep(infinity).

handle_args([], #cnf{target = undefined}) ->
  throw("TargetNode not specified");
handle_args([], #cnf{trc = []}) ->
  throw("No trace patterns specified");
handle_args([], Config = #cnf{}) ->
  Config;

handle_args(["-setcookie" | Rest], Config) ->
  %% "-setcookie" is a synonym for "-cookie"
  handle_args(["-cookie" | Rest], Config);
handle_args(["-" ++ Option | Rest], Config) ->
  %% Everything else maps to fields in the cnf record
  Options = #{time => integer,
              msgs => integer,
              target => atom,
              cookie => atom,
              procs => term,
              max_queue => integer,
              max_msg_size => integer,
              debug => boolean,
              trace_child => boolean,
              arity => boolean,
              discard => boolean,
              %% print-related
              buffered => boolean,
              print_calls => boolean,
              print_file => string,
              print_msec => boolean,
              print_depth => integer,
              print_re => string,
              print_return => boolean,
              %% trc file-related
              file => string,
              file_size => integer,
              file_count => integer},
  OptionAtom = list_to_atom(Option),
  case maps:get(OptionAtom, Options, undefined) of
    undefined ->
      throw("Invalid option -" ++ Option);
    Type ->
      try to_option_value(Type, hd(Rest)) of
        Parsed ->
          Index = findex(OptionAtom, record_info(fields, cnf)) + 1,
          NewCnf = setelement(Index, Config, Parsed),
          handle_args(tl(Rest), NewCnf)
      catch
        error:badarg ->
          throw("Invalid value for -" ++ Option ++ "; expected " ++ atom_to_list(Type))
      end
  end;
handle_args([Node | Rest], Config = #cnf{target = undefined}) ->
  %% The first non-option argument is the target node
  handle_args(Rest, Config#cnf{target = to_atom(Node)});
handle_args([Trc | Rest], Config = #cnf{trc = Trcs}) ->
  %% Any following non-option arguments are trace patterns.
  NewTrc =
    case Trc of
      "send" -> send;
      "receive" -> 'receive';
      _ -> Trc
    end,
  NewTrcs = Trcs ++ [NewTrc],
  handle_args(Rest, Config#cnf{trc = NewTrcs}).

start_distribution(Cnf = #cnf{target = Target}) ->
  %% Check if the target node has a "long" or "short" name,
  %% since we need to match that.
  TargetS = atom_to_list(Target),
  NameType =
    case lists:dropwhile(fun(C) -> C =/= $@ end, TargetS) of
      "@" ++ HostPart ->
        case lists:member($., HostPart) of
          true ->
            longnames;
          false ->
            shortnames
        end;
      _ ->
        %% No host part?  maybe_new_target/1 is going to turn it
        %% into a short name.
        shortnames
    end,
  NodeName = random_node_name(),
  %% We want to start as a hidden node, but we can't affect that from
  %% here.  rebar.config contains an entry to pass the "-hidden"
  %% argument to escript.
  {ok, _} = net_kernel:start([list_to_atom(NodeName), NameType]),
  assert_cookie(Cnf).

-ifdef(USE_NOW).
random_node_name() ->
  {A, B, C} = now(),
  lists:concat(["redbug-", A, "-", B, "-", C]).
-else.
%% now/0 was deprecated in Erlang/OTP 18.0, which is the same release
%% that added the rand module, so let's use that.
random_node_name() ->
  "redbug-" ++ integer_to_list(rand:uniform(1000000000)).
-endif.

to_option_value(integer, String) ->
  list_to_integer(String);
to_option_value(atom, String) ->
  list_to_atom(String);
to_option_value(term, String) ->
  to_term(String);
to_option_value(boolean, String) ->
  case String of
    "true" -> true;
    "false" -> false;
    _ -> error(badarg)
  end;
to_option_value(string, String) ->
  String.

to_term(Str) ->
  {done, {ok, Toks, 1}, []} = erl_scan:tokens([], "["++Str++"]. ", 1),
  case erl_parse:parse_term(Toks) of
    {ok, [Term]} -> Term;
    {ok, L} when is_list(L) -> L
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
help() ->
  help(shell).

help(Where) ->
  Text =
    ["redbug - the (sensibly) Restrictive Debugger"
     ,""] ++
    case Where of
      shell ->
        ["  redbug:start(Trc) -> start(Trc, [])."
        ,"  redbug:start(Trc, Opts)."];
      escript ->
        ["  " ++ escript:script_name() ++ " [-Opt Value [...]] TargetNode Trc [Trc...]"
        ,"  (you may need to quote trace patterns containing spaces etc)"]
    end ++
     [""
     ,"  redbug is a tool to interact with the Erlang trace facility."
     ,"  It will instruct the Erlang VM to generate so called "
     ,"  'trace messages' when certain events (such as a particular"
     ,"  function being called) occur."
     ,"  The trace messages are either printed (i.e. human readable)"
     ,"  to a file or to the screen; or written to a trc file."
     ,"  Using a trc file puts less stress on the system, but"
     ,"  there is no way to count the messages (so the msgs opt"
     ,"  is ignored), and the files can only be read by special tools"
     ,"  (such as 'bread'). Printing and trc files cannot be combined."
     ,"  By default (i.e. if the 'file' opt is not given), messages"
     ,"  are printed."
     ,""
     ,"Trc: list('send'|'receive'|string(RTP))"
     ,"RTP:  restricted trace pattern"
     ,"  the RTP has the form: \"<mfa> when <guards> -> <actions>\""
     ,"  where <mfa> can be;"
     ,"  \"mod\", \"mod:fun\", \"mod:fun/3\" or \"mod:fun('_', atom, X)\""
     ,"  <guard> is something like;"
     ,"  \"X==1\" or \"is_atom(A)\""
     ,"  and <action> is;"
     ,"  \"return\" and/or \"stack\" (separated by \";\")"
     ,""
     ,"  E.g."
     ,"  ets:lookup(T, hostname) when is_integer(T) -> stack"
     ,""
     ,"Opts: list({Opt, Val})"
     ,"  general opts:"
     ,"time         (15000)       stop trace after this many ms"
     ,"msgs         (10)          stop trace after this many msgs"
     ,"target       (node())      node to trace on"
     ,"cookie       (host cookie) target node cookie"
     ,"blocking     (false)       block start/2, return a list of messages"
     ,"arity        (false)       print arity instead of arg list"
     ,"trace_child  (false)       children gets traced (set_on_spawn)"
     ,"buffered     (false)       buffer messages till end of trace"
     ,"discard      (false)       discard messages (when counting)"
     ,"max_queue    (5000)        fail if internal queue gets this long"
     ,"max_msg_size (50000)       fail if seeing a msg this big"
     ,"procs        (all)         (list of) Erlang process(es)"
     ,"                             all|pid()|atom(RegName)|{pid, I2, I3}"
     ,"  print-related opts"
     ,"print_calls  (true)        print calls"
     ,"print_file   (standard_io) print to this file"
     ,"print_msec   (false)       print milliseconds on timestamps"
     ,"print_depth  (999999)      formatting depth for \"~P\""
     ,"print_re     (\"\")          print only strings that match this RE"
     ,"print_return (true)        print the return value"
     ,"print_fun    ()            custom print handler, fun/1 or fun/2;"
     ,"                             fun(TrcMsg) -> <ignored>"
     ,"                             fun(TrcMsg, AccOld) -> AccNew"
     ,"  trc file related opts"
     ,"file         (none)        use a trc file based on this name"
     ,"file_size    (1)           size of each trc file"
     ,"file_count   (8)           number of trc files"
     ,""
    ],
  lists:foreach(fun(S) -> io:fwrite(standard_io,"~s~n",[S])end,Text).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API from erlang shell
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
stop() ->
  case whereis(redbug) of
    undefined -> not_started;
    Pid -> Pid ! stop,stopped
  end.

%% a bunch of aliases for start/2
start(Trc) -> start(Trc,[]).

start(T,M,Trc) -> start(Trc,[{time,T},{msgs,M}]).

start(T,M,Trc,P) -> start(Trc,[{time,T},{msgs,M},{procs,P}]).

start(T,M,Trc,P,N)  -> start(Trc,[{time,T},{msgs,M},{procs,P},{target,N}]).

start(M,F) when is_atom(M),is_atom(F) -> start({M,F});
start(send,Props)                     -> start([send],Props);
start('receive',Props)                -> start(['receive'],Props);
start(M,Props) when is_atom(M)        -> start([{M,'_'}],Props);
start(Trc,{Tag,Val})                  -> start(Trc,[{Tag,Val}]);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% the real start function!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start(Trc,Props) when is_list(Props) ->
  case whereis(redbug) of
    undefined ->
      try
        Cnf = assert_print_fun(make_cnf(Trc,[{shell_pid,self()}|Props])),
        assert_cookie(Cnf),
        register(redbug,spawn(fun() -> init(Cnf) end)),
        maybe_block(Cnf,block_a_little())
      catch
        R   -> R;
        C:R -> {oops,{C,R}}
      end;
    _ ->
      redbug_already_started
  end.

assert_print_fun(Cnf) ->
  case is_function(Cnf#cnf.print_fun) of
    true -> Cnf;
    false-> Cnf#cnf{print_fun=make_print_fun(Cnf)}
  end.

make_print_fun(Cnf) ->
  case Cnf#cnf.blocking of
    false-> mk_outer(Cnf);
    true -> mk_blocker()
  end.

assert_cookie(#cnf{cookie=''}) -> ok;
assert_cookie(Cnf) -> erlang:set_cookie(Cnf#cnf.target,Cnf#cnf.cookie).

block_a_little() ->
  Ref = erlang:monitor(process,redbug),
  receive
    {running,NoP,NoF}  -> erlang:demonitor(Ref),{NoP,NoF};
    {'DOWN',Ref,_,_,R} -> R
  end.

maybe_block(#cnf{blocking=true},{I,_}) when is_integer(I) -> block();
maybe_block(_,R) -> R.

block() ->
  Ref = erlang:monitor(process,redbug),
  receive
    {'DOWN',Ref,_,_,R} -> R
  end.

%% turn the proplist inta a #cnf{}
make_cnf(Trc,Props) ->
  make_cnf(proplists:unfold(Props),#cnf{trc=Trc},record_info(fields,cnf)).

make_cnf([],Cnf,_) -> Cnf;
make_cnf([{Tag,Val}|Props],Cnf,Tags) ->
  make_cnf(Props,setelement(findex(Tag,Tags)+1,Cnf,Val),Tags).

findex(Tag,[])       -> throw({no_such_option,Tag});
findex(Tag,[Tag|_])  -> 1;
findex(Tag,[_|Tags]) -> findex(Tag,Tags)+1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% the main redbug process
%%% a state machine. init, starting, running, stopping, wait_for_trc.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(Cnf) ->
  process_flag(trap_exit,true),
  case ?try_with_stack(starting(do_start(Cnf))) of
    {ok,Result} ->
      Result;
    {throw,R,_} ->
      exit({argument_error,R});
    {C,R,S} when Cnf#cnf.debug andalso not Cnf#cnf.blocking ->
      ?log([{C,R},{stack,S}]);
    {_,R,_} ->
      exit(R)
  end.

starting(Cnf = #cnf{print_pid=PrintPid}) ->
  receive
    stop                           -> Cnf#cnf.trc_pid ! stop;
    {redbug_targ,{starting,P,F,C}} -> running(run(Cnf#cnf{cons_pid=C},P,F));
    {'EXIT',_,{redbug_targ,R}}     -> throw(R);
    {'EXIT',PrintPid,R}            -> ?log([printer_died,{reason,R}]);
    {'EXIT',R}                     -> ?log([exited,{reason,R}]);
    X                              -> ?log([{unknown_message,X}])
  end.

running(Cnf = #cnf{trc_pid=TrcPid,print_pid=PrintPid}) ->
  receive
    stop                       -> TrcPid ! stop,
                                  wait_for_trc(Cnf),
                                  done(Cnf,wait_for_printer(Cnf));
    {redbug_targ,{stopping,_}} -> done(Cnf,wait_for_printer(Cnf));
    {'EXIT',TrcPid,R}          -> ?log([{trace_control_died,R}]),
                                  done(Cnf,wait_for_printer(Cnf));
    {'EXIT',PrintPid,R}        -> wait_for_trc(Cnf),
                                  done(Cnf,R);
    X                          -> ?log([{unknown_message,X}])
  end.

wait_for_trc(#cnf{trc_pid=TrcPid}) ->
  receive
    {'EXIT',TrcPid,normal} -> ok;
    {'EXIT',TrcPid,R} -> ?log([{trace_control_died,R}])
  end.

wait_for_printer(#cnf{print_pid=PrintPid}) ->
  receive
    {'EXIT',PrintPid,R} -> R
  end.

done(#cnf{blocking=false},{Reason,Answer}) ->
  io:fwrite("~s",[done_string(Reason)]),
  io:fwrite("redbug done, ~p - ~p~n",[Reason,Answer]);
done(#cnf{blocking=true},Reason) ->
  exit(Reason).

done_string(Reason) ->
  case is_tuple(Reason) andalso element(1,Reason) of
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

run(Cnf,P,F) ->
  Cnf#cnf.print_pid ! {trace_consumer,Cnf#cnf.cons_pid},
  [Cnf#cnf.shell_pid ! {running,P,F} || is_pid(Cnf#cnf.shell_pid)],
  Cnf.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_start(OCnf) ->
  Cnf = spawn_printer(wrap_print_fun(OCnf),maybe_new_target(OCnf)),
  Cnf#cnf{trc_pid=redbug_targ:start(Cnf#cnf.target,pack(Cnf))}.

maybe_new_target(Cnf = #cnf{target=Target}) ->
  case lists:member($@,Str=atom_to_list(Target)) of
    true -> Cnf;
    false-> Cnf#cnf{target=to_atom(Str++"@"++element(2,inet:gethostname()))}
  end.

to_atom(L) -> list_to_atom(L).

spawn_printer(PrintFun,Cnf) ->
  Cnf#cnf{print_pid=spawn_link(fun() -> print_init(PrintFun) end)}.

wrap_print_fun(#cnf{print_fun=PF}) ->
  case erlang:fun_info(PF,arity) of
    {arity,1} -> fun(M,N) -> PF(M),maybe_update_count(M,N) end;
    {arity,2} -> PF
  end.

maybe_update_count(M,N) ->
  case element(1,M) of
    call -> N+1;
    send -> N+1;
    recv -> N+1;
    _    -> N
  end.

mk_blocker() ->
  fun({_,{_,false},_,_},A)      -> A;
     ({call_time,{_,[]},_,_},A) -> A;
     ({call_count,{_,0},_,_},A) -> A;
     (X,0)                      -> [X];
     (X,A)                      -> [X|A]
  end.

mk_outer(#cnf{file=[_|_]}) ->
  fun(_) -> ok end;
mk_outer(#cnf{print_depth=Depth,print_msec=MS,print_return=Ret} = Cnf) ->
  OutFun = mk_out(Cnf),
  fun({Tag,Data,PI,TS}) ->
      MTS = fix_ts(MS,TS),
      case {Tag,Data} of
        {call_time,{_,false}} ->
          ok;
        {call_time,{{M,F,A},PerProcCT}} ->
          PerProc =
            fun({_,Count,Sec,Usec},{AC,AT}) ->
                {Count+AC,Sec*1000000+Usec+AT}
            end,
          {Count,Time} = lists:foldl(PerProc,{0,0},PerProcCT),
          [OutFun("~n% ~6s : ~6s : ~w:~w/~w",
                  [human(Count),human(Time),M,F,A]) || 0 < Count];
        {'call_count',{_,false}} ->
          ok;
        {'call_count',{{M,F,A},Count}} ->
          [OutFun("~n% ~6s : ~w:~w/~w",[human(Count),M,F,A]) || 0 < Count];
        {'call',{{M,F,A},Bin}} ->
          case Cnf#cnf.print_calls of
            true ->
              case is_integer(A) of
                true ->
                  OutFun("~n% ~s ~s~n% ~w:~w/~w",[MTS,to_str(PI),M,F,A]);
                false->
                  As = string:join([flat("~P",[E,Depth]) || E <- A],", "),
                  OutFun("~n% ~s ~s~n% ~w:~w(~s)",[MTS,to_str(PI),M,F,As])
              end,
              lists:foreach(fun(L) -> OutFun("  ~s",[L]) end,stak(Bin));
            false->
              ok
          end;
        {'retn',{{M,F,A},Val0}} ->
          Val = case Ret of
                  true  -> Val0;
                  false -> '...'
                end,
          OutFun("~n% ~s ~s~n% ~p:~p/~p -> ~P",
                 [MTS,to_str(PI),M,F,A,Val,Depth]);
        {'send',{MSG,ToPI}} ->
          OutFun("~n% ~s ~s~n% ~s <<< ~P",
                 [MTS,to_str(PI),to_str(ToPI),MSG,Depth]);
        {'recv',MSG} ->
          OutFun("~n% ~s ~s~n% <<< ~P",
                 [MTS,to_str(PI),MSG,Depth])
      end
  end.

to_str({Pid,Reg}) ->
  flat("~w(~p)",[Pid,Reg]);
to_str(RegisteredName) ->
  flat("~p",[RegisteredName]).

mk_out(#cnf{print_re=RE,print_file=File}) ->
  FD = get_fd(File),
  fun(F,A) ->
      Str=flat(F,A),
      case RE =:= "" orelse re:run(Str,RE) =/= nomatch of
        true  -> io:fwrite(FD,"~s~n",[Str]);
        false -> ok
      end
  end.

get_fd("") -> standard_io;
get_fd(FN) ->
  case file:open(FN,[write]) of
    {ok,FD} -> FD;
    _ -> throw({cannot_open,FN})
  end.

fix_ts(MS,TS) ->
  case MS of
    true -> ts_ms(TS);
    false-> ts(TS)
  end.

ts({H,M,S,_Us}) ->
  flat("~2.2.0w:~2.2.0w:~2.2.0w",[H,M,S]).
ts_ms({H,M,S,Us}) ->
  flat("~2.2.0w:~2.2.0w:~2.2.0w.~3.3.0w",[H,M,S,Us div 1000]).

%%% call stack handler
stak(Bin) ->
  lists:foldl(fun munge/2,[],string:tokens(binary_to_list(Bin),"\n")).

munge(I,Out) ->
  case lists:reverse(I) of
    "..."++_ -> [truncated|Out];
    _ ->
      case string:str(I,"Return addr") of
        0 ->
          case string:str(I,"cp = ") of
            0 -> Out;
            _ -> [mfaf(I)|Out]
          end;
        _ ->
          case string:str(I,"erminate process normal") of
            0 -> [mfaf(I)|Out];
            _ -> Out
          end
      end
  end.

mfaf(I) ->
  [_,C|_] = string:tokens(I,"()+"),
  C.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% pack the cnf record into a proplist for target consumption
%%% Proplist = list({Tag,Val})
%%% Tag = time | flags | rtps | procs | where
%%% Where = {term_buffer,{Pid,Count,MaxQueue,MaxSize}} |
%%%         {term_stream,{Pid,Count,MaxQueue,MaxSize}} |
%%%         {term_discard,{Pid,Count,MaxQueue,MaxSize}} |
%%%         {file,File,Size,Count} |
%%%         {ip,Port,Queue}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pack(Cnf) ->
  Flags0 = [call,timestamp],
  {Flags,RTPs} = lists:foldl(fun chk_trc/2,{Flags0,[]},slist(Cnf#cnf.trc)),
  dict:from_list([{time,chk_time(Cnf#cnf.time)},
                  {flags,maybe_arity(Cnf,maybe_trace_child(Cnf,Flags))},
                  {rtps,RTPs},
                  {procs,[chk_proc(P) || P <- mk_list(Cnf#cnf.procs)]},
                  {where,where(Cnf)}]).

mk_list([]) -> throw(no_procs);
mk_list([_|_] = L) -> L;
mk_list(E) -> [E].

where(Cnf) ->
  case Cnf#cnf.file of
    "" -> conf_term(Cnf);
    _  -> conf_file(Cnf)
  end.

conf_file(Cnf) ->
  {file,Cnf#cnf.file,Cnf#cnf.file_size,Cnf#cnf.file_count}.

conf_term(Cnf) ->
  {chk_buffered(Cnf#cnf.buffered,Cnf#cnf.discard),
   {Cnf#cnf.print_pid,
    chk_msgs(Cnf#cnf.msgs),
    Cnf#cnf.max_queue,
    Cnf#cnf.max_msg_size}}.

maybe_arity(#cnf{arity=true},Flags) -> [arity|Flags];
maybe_arity(_,Flags)                -> Flags.

maybe_trace_child(#cnf{trace_child=true},Flags) -> [set_on_spawn|Flags];
maybe_trace_child(_,Flags)                      -> Flags.

chk_time(Time) when is_integer(Time) -> Time;
chk_time(X) -> throw({bad_time,X}).

chk_buffered(_,true)  -> term_discard;
chk_buffered(true,_)  -> term_buffer;
chk_buffered(false,_) -> term_stream.

chk_proc(Pid) when is_pid(Pid) -> Pid;
chk_proc(Atom) when is_atom(Atom) -> Atom;
chk_proc({pid,I1,I2}) when is_integer(I1),is_integer(I2) -> {pid,I1,I2};
chk_proc(X) -> throw({bad_proc,X}).

chk_msgs(Msgs) when is_integer(Msgs) -> Msgs;
chk_msgs(X) -> throw({bad_msgs,X}).

-define(is_string(Str),(Str=="" orelse (9=<hd(Str) andalso hd(Str)=<255))).

chk_trc('send',{Flags,RTPs})                   -> {['send'|Flags],RTPs};
chk_trc('receive',{Flags,RTPs})                -> {['receive'|Flags],RTPs};
chk_trc(RTP,{Flags,RTPs}) when ?is_string(RTP) -> {Flags,[chk_rtp(RTP)|RTPs]};
chk_trc(X,_)                                   -> throw({bad_trc,X}).

chk_rtp(Str) -> redbug_msc:transform(Str).

slist(S) when ?is_string(S) -> [S];
slist(L) when is_list(L) -> lists:usort(L);
slist(X) -> [X].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% the print_loop process
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
print_init(PrintFun) ->
  receive
    {trace_consumer,TC} ->
      erlang:monitor(process,TC),
      print_loop(PrintFun,0)
  end.

print_loop(PrintFun,Acc) ->
  receive
    {'DOWN',_,_,_,R} -> exit({R,Acc});
    X                -> print_loop(PrintFun,lists:foldl(PrintFun,Acc,X))
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

flat(Format,Args) ->
  lists:flatten(io_lib:fwrite(Format,Args)).
