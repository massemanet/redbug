%% -*- mode: erlang; erlang-indent-level: 2 -*-
%% @doc
%% @end

-module(redbug_parser_eunit).

-export([go/0]).

go() ->
  [unit(1),
   unit("f:c(0.1)"),
   unit("f:c(<0.0.1>)"),
   unit("erlang:binary_to_list(<<1:3,_:5>>)"),
   unit("m'"),
   unit("m:f when '"),
   unit("x(s)"),
   unit("x:c(Aw)hen [A,A] == [A]++[A]"),
   unit("x:c(Aw)when ["),
   unit("x-s"),
   unit("a:b(X,y)when is_atom(Y)"),
   unit("m:f(A++B)"),
   unit("x:y(z)->bla"),
   unit("f:c(<<>>)"),
   unit(erlang),
   unit("a when element(1,'$_')=/=b"),
   unit("erlang when tl(hd('$_'))=={}"),
   unit("a:b when element(1,'$_')=/=c"),
   unit("a"),
   unit("a->stack"),
   unit("a:b"),
   unit("a:b->return "),
   unit("a:b/2"),
   unit("a:b/2->return"),
   unit("a:b(X,Y)"),
   unit("a:b(_,_)"),
   unit("a:b(X,X)"),
   unit("a:b(X,y)"),
   unit("a:foo()when a==b"),
   unit("a:foo when a==b"),
   unit("a:b(X,1)"),
   unit("a:b(X,\"foo\")"),
   unit("x:y({A,{B,A}},A)"),
   unit("x:y(A,[A,{B,[B,A]},A],B)"),
   unit("x:c([string])"),
   unit("x:c(S)when S==x;S==y"),
   unit("x:c(S)when (S==x)or(S==y)"),
   unit("a:b(X,Y)when is_record(rec,X) and (Y==0), (X==z)"),
   unit("a:b(X,y)when not is_atom(X)"),
   unit("a:b(X,Y)when X==1,Y=/=a"),
   unit("a:b(X,y)when not is_atom(X) -> return"),
   unit("a:b(X,y)when element(1,X)==foo, (X==z)"),
   unit("a:b(X,X) -> return;stack"),
   unit("x:y(A,[A,B,C])when A==B,is_atom(C)"),
   unit("x:y([A,B,C])when A=/=B,is_atom(C)"),
   unit("a:b([A,B,T])when B==T"),
   unit("x:y([C|{D}])when is_atom(C)"),
   unit("lists:reverse(\"ab\"++_)"),
   unit("lists:reverse(\"\"++A) when A==\"\""),
   unit("lists:reverse(\"ab\"++\"\")"),
   unit("lists:reverse(\"ab\"++C)when 3<length(C)"),
   unit("a:b([$a,$b|C])"),
   unit("a:b([22#22|C])"),
   unit("a:_(a)"),
   unit("a:_")].

unit(Str) ->
  {Str, catch redbug_compiler:string(Str)}.
