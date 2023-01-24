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
RDBG = fun(MFA, Opts) ->
  TIME = fun(Offset) -> erlang:system_time(millisecond)+Offset end,
  TRC = fun(G, M, Msgs, E) ->
    TO = -TIME(-E),
    [exit({msgs, M}) || Msgs =< M],
    receive X -> io:fwrite("~p~n", [X]), G(G, M+1, Msgs, E)
    after TO -> exit({time, M})
    end
  end,
  TRC0 = fun(Time,Msgs,Flags) ->
    erlang:trace(all, true, [call, timestamp|Flags]),
    io:fwrite("done, ~p~n", [catch TRC(TRC, 0, Msgs, Time)])
  end,
  ETP = fun(Mp, Fp, Ap, Hp, Gp, Bp) ->
    io:fwrite("trace pattern - ~p~n", [{{Mp, Fp, Ap}, [{Hp, Gp, Bp}]}]),
    erlang:trace_pattern({Mp, Fp, Ap}, [{Hp, Gp, Bp}], [local])
  end,
  TP = fun(MFAp, Gp, Bp) ->
    case MFAp of
      {Mp, Fp, Ap} when is_list(Ap) -> ETP(Mp, Fp, length(Ap), Ap, Gp, Bp);
      {Mp, Fp, Ap} when is_integer(Ap) -> ETP(Mp, Fp, Ap, '_', Gp, Bp);
      {Mp, '_', '_'} -> ETP(Mp, '_', '_', '_', Gp, Bp);
      {Mp, Fp} -> ETP(Mp, Fp, '_', '_', Gp, Bp);
      {Mp} -> ETP(Mp, '_', '_', '_', Gp, Bp);
      _ -> io:fwrite("no trigger~n", [])
    end
  end,

  DefaultOpts = #{time => 10000,
                  msgs => 10,
                  flags => [],
                  trigger => undefined,
                  trigger_guard => [],
                  trigger_body => [{set_seq_token, label, 1337}, {message,{{set_token, {get_seq_token}}}}],
                  target_guard => [{'==',{is_seq_trace},true}],
                  target_body => [{message,{{seq_token,{get_seq_token}}}}]},
  Os = maps:merge(DefaultOpts, Opts),

  erlang:trace_pattern({'_','_','_'}, false, [local]),
  seq_trace:reset_trace(),

  #{trigger := Trigger, trigger_guard := Gt, trigger_body := Bt} = Os,
  Ttrigger = TP(Trigger, Gt, Bt),

  #{target_guard := Gd, target_body := Bd} = Os,
  Ttarget = TP(MFA, Gd, Bd),

  #{time := Time, msgs := Msgs, flags := Flags} = Os,
  Tracer = spawn(fun() -> TRC0(TIME(Time), Msgs, Flags) end),

  {Ttarget, Ttrigger, Tracer}
end.
```

Normal trace
```
RDBG({gopnic_grpc_client,do_from_outside, ['_', #{their_tid => '$1', operation => send_routing_info_for_sm, ppi => m3ua}, envoy_outgoing, '_']}, #{target_guard=>[]}).
```

Sequential trace
```
RDBG({leper,'_','_'}, #{flags=>[arity], msgs=>100, trigger => {gopnic_grpc_client,do_from_outside, ['_', #{operation => send_routing_info_for_sm, ppi => m3ua}, envoy_outgoing, '_']}}).
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
