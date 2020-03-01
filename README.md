[![Build Status](https://travis-ci.org/massemanet/redbug.svg?branch=master)](https://travis-ci.org/massemanet/redbug)

NAME

redbug - restricted debugging utility

SYNOPSIS

start in non-blocking mode

    redbug:start(Trc,[Opts]) -> {integer(NoOfProcs),integer(NoOfFuncs)} |
                                {atom(ErrorType),term(ErrorReason)}
start blocking mode

    redbug:start(Trc,[Opts]) -> {atom(StopReason),list(TraceMessages)} |
                                {atom(ErrorType),term(ErrorReason)}

stop

    redbug:stop() -> stopped | not_started

help

    redbug:help() -> ok

run from command line (portable escript archive)

    ./_build/default/bin/redbug [-Opt Value [...]] TargetNode Trc [Trc...]

DESCRIPTION

redbug is a tool to interact with the Erlang trace facility. It will instruct
the Erlang VM to generate so-called 'trace messages' when certain events
(such as a particular function being called) occur. It uses a safe subset of
the tracing functionality, and exits if it feels overloaded, e.g. if it gets
flooded by trace messages. It runs in the background, collecting trace
messages, until it reaches one of its termination criteria (number of
messages/file size or elapsed time). The trace messages are either printed
(i.e. human readable) to a file or to the screen; or written to a trc file.
Using a trc file puts less stress on the system, but there is no way to count
the messages (so the 'msgs' opt is ignored), and the files can only be read
by special tools (such as 'bread'). Printing and trc files cannot be
combined.  By default (i.e. if the 'file' opt is not given), messages are
printed.


Trc: trace type

    list('send'|'receive'|string(RTP))

RTP:  restricted trace pattern. the RTP has the form:

    "MFA when GUARDS -> ACTIONS"

where MFA can be;

    "mod", "mod:fun", "mod:fun/3" or "mod:fun('_',atom,X)"

GUARD is something like;

    "X==1" or "is_atom(A)"

and ACTION is;

    "return" and/or "stack" (separated by ",")

So, an RTP looks something like this;

    "ets:lookup(T,hostname) when is_integer(T) -> stack"

Note that bindings (like we're binding 'T' above) works as expected.
So this RTP;

    "maps:to_list(#{a:=T,c:=#{d:=T}})"

will not trigger on this call;

```maps:to_list(#{a=>b,c=>#{d=>e,f=>g}}).```

but will trigger on this;

```maps:to_list(#{a=>b,c=>#{d=>b,f=>g}})."```

Opts: list({Opt,Val})

general opts:

    time         (15000)       stop trace after this many milliseconds
    msgs         (10)          stop trace after this many messages
    target       (node())      node to trace on
    cookie       ('')          target node cookie
    blocking     (false)       block start/2, return a list of messages
    procs        (all)         atom(all|new|running) | list(Proc)
                               where Proc is pid() | atom(RegName) | {pid,I2,I3}
    max_queue    (5000)        exit if redbug-internal queue gets this long
    max_msg_size (50000)       exit if seeing a message bigger than this
    debug        (false)       bigger error messages

print-related opts

    arity        (false)       print arity instead of argument list
    buffered     (no)          buffer messages till end of trace
    print_pid    (false)       print pid instead of registered name
    print_calls  (true)        print calls
    print_file   (standard_io) print to this file
    print_msec   (false)       print milliseconds on time stamps
    print_depth  (999999)      formatting depth for "~P"
    print_re     ("")          print only strings that match this regexp
    print_return (true)        print return value
    print_fun    ('')          custom print fun. gets called once for each trace
                               message. It can be a fun/1, (called as F(Msg),
                               return value is ignored), or a fun/2 (called as
                               F(Msg,Acc), return is next Acc)

trc file related opts

    file         (none)        use a trc file based on this name
    file_size    (1)           size of each trc file
    file_count   (8)           number of trc files

EXAMPLES

    %% Basic call trace
    1> redbug:start("erlang:demonitor").
    {30,2}
    15:39:00 <{erlang,apply,2}> {erlang,demonitor,[#Ref<0.0.0.21493>]}
    15:39:00 <{erlang,apply,2}> {erlang,demonitor,[#Ref<0.0.0.21499>]}
    15:39:00 <{erlang,apply,2}> {erlang,demonitor,[#Ref<0.0.0.21500>]}
    redbug done, timeout - 3

    %% As above, print pids
    2> redbug:start("erlang:demonitor",[{msgs,1},print_pid]).
    {30,2}
    15:42:04 <0.31.0> {erlang,demonitor,[#Ref<0.0.0.21616>]}
    redbug done, msg_count - 1

    %% As above, print return value. The return value is a separate message.
    3> redbug:start("erlang:demonitor->return",[{msgs,2},print_pid]).
    {30,2}
    15:43:22 <0.31.0> {erlang,demonitor,[#Ref<0.0.0.21677>]}
    15:43:22 <0.31.0> erlang:demonitor/1 -> true
    redbug done, msg_count - 2

    %% As above, also print the call stack. Note that not all functions in the
    %% call chain are on the stack, only functions we will return to (this is a
    %% consequence of tail call optimization.)
    4> redbug:start("erlang:demonitor->return,stack",[{msgs,2},print_pid]).
    {30,2}
    15:44:35 <0.31.0> {erlang,demonitor,[#Ref<0.0.0.21726>]}
      shell:eval_loop/3
      shell:eval_exprs/7
      shell:exprs/7
      erl_eval:do_apply/6
      redbug:start/2
    15:44:35 <0.31.0> erlang:demonitor/1 -> true
    redbug done, msg_count - 2

    %% Trace on messages that the shell process receives.
    5> redbug:start('receive',[{procs,[self()]}]).
    {1,0}
    15:15:47 <{erlang,apply,2}> <<< {running,1,0}
    15:17:49 <{erlang,apply,2}> <<< timeout
    redbug done, timeout - 2

    %% As above, but also trace on sends from the shell process. note that in
    %% this case the 'print_pid' opt would hide that there is a send to the
    %% group server.
    7> redbug:start([send,'receive'],[{procs,[self()]}]).
    {1,0}
    15:36:36 <{erlang,apply,2}> <<< {running,1,0}
    15:36:36 <{erlang,apply,2}> <{group,server,3}> <<< {io_request,<0.31.0>,
                                                        <0.24.0>,
                                                        {get_geometry,columns}}
    redbug done, timeout - 2

    %% Call trace with a function head match. Note that the first call to
    %% ets:tab2list/1 does not trigger the tracer.
    8> redbug:start("ets:tab2list(inet_db)",[{msgs,2},print_pid]).
    {30,1}
    9> ets:tab2list(ac_tab),ok.
    ok
    10> ets:tab2list(inet_db),ok.
    ok
    15:47:15 <0.31.0> {ets,tab2list,[inet_db]}
    redbug done, timeout - 1

    %% As above, but use the 'blocking' opt. redbug:start/2 blocks until end of
    %% trace, and returns the stop reason and a list of trace messages.
    10> spawn(fun()->receive after 2000->ets:tab2list(inet_db) end end).
    <0.540.0>
    11> redbug:start("ets:tab2list(inet_db)",[blocking]).
    {timeout,[{call,{{ets,tab2list,[inet_db]},<<>>},
                    {<0.540.0>,{erlang,apply,2}},
                    {15,50,43,776041}}]}

Examples using the escript

First start a node that we're going to trace:

    erl -sname foo

We'll need to type some commands into the shell for some of the
following traces to trigger.

Start tracing, giving the node name as the first argument. (If the
node name doesn't contain a host name, redbug will create a short
node name by adding the host name.)

    $ redbug foo erlang:demonitor

    % 14:19:29 <5270.91.0>(dead)
    % erlang:demonitor(#Ref<5270.0.4.122>, [flush])

    % 14:19:29 <5270.40.0>({erlang,apply,2})
    % erlang:demonitor(#Ref<5270.0.4.130>, [flush])

    % 14:19:29 <5270.40.0>({erlang,apply,2})
    % erlang:demonitor(#Ref<5270.0.4.131>, [flush])

    % 14:19:29 <5270.40.0>({erlang,apply,2})
    % erlang:demonitor(#Ref<5270.0.4.132>, [flush])
    redbug done, timeout - 4

    %% Limit message count
    $ redbug foo erlang:demonitor -msgs 1

    % 14:22:09 <5276.103.0>(dead)
    % erlang:demonitor(#Ref<5276.0.4.144>, [flush])
    redbug done, msg_count - 1

    %% Print return value. The return value is a separate message.
    $ redbug foo 'erlang:demonitor -> return' -msgs 2

    % 14:23:47 <5276.115.0>(dead)
    % erlang:demonitor(#Ref<5276.0.4.166>, [flush])

    % 14:23:47 <5276.115.0>(dead)
    % erlang:demonitor/2 -> true
    redbug done, msg_count - 1

    %% Also print call stack.
    $ redbug foo 'erlang:demonitor -> return;stack' -msgs 2

    % 14:24:43 <5276.121.0>(dead)
    % erlang:demonitor(#Ref<5276.0.4.177>, [flush])
      shell:'-get_command/5-fun-0-'/1

    % 14:24:43 <5276.121.0>(dead)
    % erlang:demonitor/2 -> true
    redbug done, msg_count - 1

    %% Trace on messages that the 'user_drv' process receives.
    $ redbug foo receive -procs user_drv -msgs 1

    % 14:27:10 <6071.31.0>(user_drv)
    % <<< {#Port<6071.375>,{data,"a"}}
    redbug done, msg_count - 1

    %% As above, but also trace on sends. The two trace patterns
    %% are given as separate arguments.
    $ redbug foo receive send -procs user_drv -msgs 2

    % 17:43:28 <6071.31.0>(user_drv)
    % <<< {#Port<6071.375>,{data,"a"}}

    % 17:43:28 <6071.31.0>(user_drv)
    % <6071.33.0>({group,server,3}) <<< {<6071.31.0>,{data,"a"}}
    redbug done, msg_count - 2

    %% Call trace with a function head match.
    $ redbug foo 'ets:tab2list(inet_db)' -msgs 2

    % 17:45:48 <5276.40.0>({erlang,apply,2})
    % ets:tab2list(inet_db)
    redbug done, timeout - 1
