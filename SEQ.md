Sequential trace token
====

## Use case

```
      A             B
m1-> f(1) -> n1 -> g(1)
m2-> f(2) -> n2 -> g(2)
m3-> f(3) -> n3 -> g(3)
m4-> f(4) -> m4 -> g(4)
```

## Proof of Concept

We have two `trace patterns`. The first has a match spec that fails
unless there is a `sequential trace token`. The second (the `trigger`)
sets a`sequential trace token` when matched.

The below is a very small version of `redbug`;

```erlang
RDBG = fun(Target, Opts) ->
  DefaultOpts  = #{time=>3000, msgs=>10, trigger=>{erlang,time,0}},
  #{time := Time, mags := Msgs, trigger := Trigger} = maps:merge(DefaultOpts, Opts),
  TIME = fun(Offset) -> erlang:system_time(millisecond)+Offset end,
  MFA = fun({M, F, A})when is_list(A) -> {M, F, length(A)};
            (MFA) -> MFA
        end,
  ARGS = fun({_, _, A}) -> A end,
  TRC = fun(G,A,E) ->
    TO = -TIME(-E),
    [exit({msgs, lists:reverse(A)}) || Msgs =< length(A)],
    receive X -> G(G, [X|A], E)
    after TO -> exit({time, lists:reverse(A)})
    end,
  end,
  PRT = fun(P) ->
    monitor(process, P),
    receive {'DOWN', _, process, _, Msg} -> io:fwrite("~p~n", [Msg])
    end
  end,
  MStrigger = [{
    ARGS(Trigger),
    [],
    [{set_seq_token, label, {caller}},
     {message,{{set_token, {get_seq_token}}}}]
  }],
  MStarget = [{
    '_',
    [{'==',{is_seq_trace},true}],
    [{message,{{seq_token,{get_seq_token}}}}]
  }],
  erlang:trace_pattern({'_','_','_'}, false, [local]),
  erlang:trace_pattern(MFA(Trigger), MStrigger, [local]),
  erlang:trace_pattern(MFA(Target), MStarget, [local]),
  Tracer = spawn(fun() -> TRC(TRC, [], TIME(Time)) end),
  spawn(fun() -> PRT(Tracer) end),
  erlang:trace(all, true, [call, timestamp, {tracer, Tracer}])
end.
```

To test this, we use we set up three processes (A, B, and C) that
forwards messages.

```
 C                     B                               A
   -- {hello,1} --> foo:forward()  -- {hello,1} --> foo:show()
```

We want to trace on `foo:show/2`, but only if `foo:forward/3` has been
called with certain args earlier in the sequence.

```
> RDBG({foo,show,2}, #{time=>7000,trigger=>{foo,forward,['_',{hello,3},'_']}}).
> foo:go(#{}).
{a,{msg,{hello,0}}}
{a,{msg,{hello,1}}}
{a,{msg,{hello,2}}}
{a,{msg,{hello,3}}}
{a,{msg,{hello,4}}}
{a,{msg,{hello,5}}}
{a,{msg,{hello,6}}}
{a,{msg,{hello,7}}}
{a,{msg,{hello,8}}}

{time,[
  {trace_ts,<0.106.0>,call,
    {foo,forward, [pid_a, {hello,3}, #{pid_a => <0.105.0>,self => <0.106.0>}]},
    {set_token,{0,{foo,b_loop,1},0,<0.106.0>,0}},
    {1674,51334,51606}},
  {trace_ts,<0.105.0>,call,
    {foo,show,[{hello,3},#{}]},
    {seq_token,{0,{},1,<0.106.0>,0}},
    {1674,51334,51696}}
]}

>
```
