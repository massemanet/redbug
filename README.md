[![Build Status](https://travis-ci.org/massemanet/redbug.svg?branch=master)](https://travis-ci.org/massemanet/redbug)


# DESCRIPTION

redbug is a tool to interact with the Erlang trace facility. It is
intended to be run from the erlang shell, but it can also be run from
an OS shell as an escript (see below). It will instruct the Erlang VM
to generate so-called 'trace messages' when certain events (such as a
particular function being called) occur. It uses a safe subset of the
tracing functionality, and exits if it feels overloaded, e.g. if it
gets flooded by trace messages. It runs in the background, collecting
trace messages, until it reaches one of its termination criteria
(number of messages/file size or elapsed time).

The trace messages are either printed (i.e. human readable) to a file
or to the screen; or written to a trc file.  Using a trc file puts
less stress on the system, but there is no way to count the messages
(so the 'msgs' opt is ignored), and the files can only be read by
special tools (such as 'bread'). Printing and trc files cannot be
combined.  By default (i.e. if the 'file' opt is not given), messages
are printed.

# DOCUMENTATION

Run `erl -run redbug help -run erlang halt`, or `redbug:help().`.

# EXAMPLES

Basic call trace

      1> redbug:start("erlang:demonitor").

      % 18:27:21 <0.188.0>({erlang,apply,2})
      % erlang:demonitor(#Ref<0.2419348116.2832203778.12948>)

      % 18:27:21 <0.188.0>({erlang,apply,2})
      % erlang:demonitor(#Ref<0.2419348116.2832203777.10938>, [flush])

      % 18:27:21 <0.188.0>({erlang,apply,2})
      % erlang:demonitor(#Ref<0.2419348116.2832203777.10939>, [flush])

      % 18:27:21 <0.188.0>({erlang,apply,2})
      % erlang:demonitor(#Ref<0.2419348116.2832203777.10940>, [flush])
      redbug done, timeout - 4


As above, but also print return value. The return value is a separate message.

      2> redbug:start("erlang:demonitor->return",[{msgs,2}]).
      {75,2}

      % 18:31:15 <0.188.0>({erlang,apply,2})
      % erlang:demonitor(#Ref<0.2419348116.2832203780.10535>)

      % 18:31:15 <0.188.0>({erlang,apply,2})
      % erlang:demonitor/1 -> true
      redbug done, msg_count - 1

As above, also print the call stack. Note that not all functions in the
call chain are on the stack, only functions we will return to (this is a
consequence of tail call optimization.)

      3> redbug:start("erlang:demonitor->return,stack",[{msgs,2}]).
      {75,2}

      % 18:32:54 <0.188.0>({erlang,apply,2})
      % erlang:demonitor(#Ref<0.2419348116.2832203778.13012>)
      %   redbug:block_a_little/0 
      %   redbug:start/2 
      %   erl_eval:do_apply/6 
      %   shell:exprs/7 
      %   shell:eval_exprs/7 
      %   shell:eval_loop/3 

As above, but use the 'blocking' opt. redbug:start/2 blocks until end of
trace, and returns the stop reason and a list of trace messages.

    10> spawn(fun()->receive after 2000->ets:tab2list(inet_db) end end).
    <0.540.0>
    11> redbug:start("ets:tab2list(inet_db)",[blocking]).
    {timeout,[{call,{{ets,tab2list,[inet_db]},<<>>},
                    {<0.540.0>,{erlang,apply,2}},
                    {15,50,43,776041}}]}


# ESCRIPT

Run from command line (portable escript archive)

    ./_build/default/bin/redbug [-Opt Value [...]] TargetNode Trc [Trc...]

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
