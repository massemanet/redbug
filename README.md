[![Build Status](https://travis-ci.org/massemanet/redbug.svg?branch=master)](https://travis-ci.org/massemanet/redbug)


DESCRIPTION

redbug is a tool to interact with the Erlang trace facility. It will instruct
the Erlang VM to generate so-called 'trace messages' when certain events
(such as a particular function being called) occur. It uses a safe subset of
the tracing functionality, and exits if it feels overloaded, e.g. if it gets
flooded by trace messages. It runs in the background, collecting trace
messages, until it reaches one of its termination criteria (number of
messages/file size or elapsed time).

The trace messages are either printed (i.e. human readable) to a file or to the
screen; or written to a trc file.  Using a trc file puts less stress on the
system, but there is no way to count the messages (so the 'msgs' opt is
ignored), and the files can only be read by special tools (such as
'bread'). Printing and trc files cannot be combined.  By default (i.e. if the
'file' opt is not given), messages are printed.

DOCUMENTATION

Run `rebar3 edoc` or check the online docs at [hexdocs](https://hexdocs.pm/redbug/).

EXAMPLES

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
