%% -*- mode: erlang; erlang-indent-level: 2 -*-
%% @doc
%% @end

-module(redbug_parser_eunit).

-export([go/0]).

go() ->
  [assertEqual(
      {syntax_error,{bad_type,{float,0.1}}},
      unit("f:c(0.1)")),

   assertEqual(
      {syntax_error,{parse_error,"syntax error before: '<'"}},
      unit("f:c(<0.0.1>)")),

   assertEqual(
      {syntax_error,{illegal_input,1}},
      unit(1)),

   assertEqual(
      {syntax_error,{unbound_var,'Y'}},
      unit("a:b(X,y)when is_atom(Y)")),

   assertEqual(
      {syntax_error,{parse_error,[{call,1,{atom,1,x},[{atom,1,s}]}]}},
      unit("x(s)")),

   assertEqual(
      {syntax_error,{parse_error,[{op,1,'-',{atom,1,x},{atom,1,s}}]}},
      unit("x-s")),

   assertEqual(
      {syntax_error,{unknown_action,"bla"}},
      unit("x:y(z)->bla")),

   assertEqual(
      {syntax_error,{parse_error,"syntax error before: hen"}},
      unit("x:c(Aw)hen [A,A] == [A]++[A]")),

   assertEqual(
      {syntax_error,{parse_error,"syntax error before: '.'"}},
      unit("x:c(Aw)when [")),

   assertEqual(
      {syntax_error,{bad_binary,{unbound_var,'_'}}},
      unit("erlang:binary_to_list(<<1:3,_:5>>)")),

   assertEqual(
      {syntax_error,{illegal_plusplus,{{var,1,'A'},{var,1,'B'}}}},
      unit("m:f(A++B)")),

   assertEqual(
      {syntax_error,{scan_error,"m'"}},
      unit("m'")),

   assertEqual(
      {syntax_error,{scan_error,"'"}},
      unit("m:f when '")),

   assertEqual(
      {{f,c,1},
       [{[<<>>],[],[]}],
       [local]},
      unit("f:c(<<>>)")),

   assertEqual(
      {{erlang,'_','_'},
       [{'_',[],[]}],
       [local]},
      unit(erlang)),

   assertEqual(
      {{a,'_','_'},
       [{'_',[{'=/=',{element,1,'$_'},b}],[]}],
       [local]},
      unit("a when element(1,'$_')=/=b")),

   assertEqual(
      {{erlang,'_','_'},
       [{'_',[{'==',{tl,{hd,'$_'}},{{}}}],[]}],
       [local]},
      unit("erlang when tl(hd('$_'))=={}")),

   assertEqual(
      {{a,b,'_'},
       [{'_',[{'=/=',{element,1,'$_'},c}],[]}],
       [local]},
      unit("a:b when element(1,'$_')=/=c")),

   assertEqual(
      {{a,'_','_'},
       [{'_',[],[]}],
       [local]},
      unit("a")),

   assertEqual(
      {{a,'_','_'},
       [{'_',[],[{message,{process_dump}}]}],
       [local]},
      unit("a->stack")),

   assertEqual(
      {{a,b,'_'},
       [{'_',[],[]}],
       [local]},
      unit("a:b")),

   assertEqual(
      {{a,b,'_'},
       [{'_',[],[{exception_trace}]}],
       [local]},
      unit("a:b->return ")),

   assertEqual(
      {{a,b,2},
       [{['_','_'],[],[]}],
       [local]},
      unit("a:b/2")),

   assertEqual(
      {{a,b,2},
       [{['_','_'],[],[{exception_trace}]}],
       [local]},
      unit("a:b/2->return")),

   assertEqual(
      {{a,b,2},
       [{['$1','$2'],[],[]}],
       [local]},
      unit("a:b(X,Y)")),

   assertEqual(
      {{a,b,2},
       [{['_','_'],[],[]}],
       [local]},
      unit("a:b(_,_)")),

   assertEqual(
      {{a,b,2},
       [{['$1','$1'],[],[]}],
       [local]},
      unit("a:b(X,X)")),

   assertEqual(
      {{a,b,2},
       [{['$1',y],[],[]}],
       [local]},
      unit("a:b(X,y)")),

   assertEqual(
      {{a,foo,0},
       [{[],[{'==',a,b}],[]}],
       [local]},
      unit("a:foo()when a==b")),

   assertEqual(
      {{a,foo,'_'},
       [{'_',[{'==',a,b}],[]}],
       [local]},
      unit("a:foo when a==b")),

   assertEqual(
      {{a,b,2},
       [{['$1',1],[],[]}],
       [local]},
      unit("a:b(X,1)")),

   assertEqual(
      {{a,b,2},
       [{['$1',"foo"],[],[]}],
       [local]},
      unit("a:b(X,\"foo\")")),

   assertEqual(
      {{x,y,2},
       [{[{'$1',{'$2','$1'}},'$1'],[],[]}],
       [local]},
      unit("x:y({A,{B,A}},A)")),

   assertEqual(
      {{x,y,3},
       [{['$1',['$1',{'$2',['$2','$1']},'$1'],'$2'],[],[]}],
       [local]},
      unit("x:y(A,[A,{B,[B,A]},A],B)")),

   assertEqual(
      {{x,c,1},
       [{[[string]],[],[]}],
       [local]},
      unit("x:c([string])")),

   assertEqual(
      {{x,c,1},
       [{['$1'],[{'orelse',{'==','$1',x},{'==','$1',y}}],[]}],
       [local]},
      unit("x:c(S)when S==x;S==y")),

   assertEqual(
      {{x,c,1},
       [{['$1'],[{'or',{'==','$1',x},{'==','$1',y}}],[]}],
       [local]},
      unit("x:c(S)when (S==x)or(S==y)")),

   assertEqual(
      {{a,b,2},
       [{['$1','$2'],
         [{'and',{is_record,'$1',rec},{'==','$2',0}},{'==','$1',z}],[]}],
       [local]},
      unit("a:b(X,Y)when is_record(X,rec) and (Y==0), (X==z)")),

   assertEqual(
      {{a,b,2},
       [{['$1',y],[{'not',{is_atom,'$1'}}],[]}],
       [local]},
      unit("a:b(X,y)when not is_atom(X)")),

   assertEqual(
      {{a,b,2},
       [{['$1','$2'],[{'==','$1',1},{'=/=','$2',a}],[]}],
       [local]},
      unit("a:b(X,Y)when X==1,Y=/=a")),

   assertEqual(
      {{a,b,2},
       [{['$1',y],[{'not',{is_atom,'$1'}}],[{exception_trace}]}],
       [local]},
      unit("a:b(X,y)when not is_atom(X) -> return")),

   assertEqual(
      {{a,b,2},
       [{['$1',y],[{'==',{element,1,'$1'},foo},{'==','$1',z}],[]}],
       [local]},
      unit("a:b(X,y)when element(1,X)==foo, (X==z)")),

   assertEqual(
      {{a,b,2},
       [{['$1','$1'],[],[{exception_trace},{message,{process_dump}}]}],
       [local]},
      unit("a:b(X,X) -> return;stack")),

   assertEqual(
      {{x,y,2},
       [{['$1',['$1','$2','$3']],[{'==','$1','$2'},{is_atom,'$3'}],[]}],
       [local]},
      unit("x:y(A,[A,B,C])when A==B,is_atom(C)")),

   assertEqual(
      {{x,y,1},
       [{[['$1','$2','$3']],[{'=/=','$1','$2'},{is_atom,'$3'}],[]}],
       [local]},
      unit("x:y([A,B,C])when A=/=B,is_atom(C)")),

   assertEqual(
      {{a,b,1},
       [{[['$1','$2','$3']],[{'==','$2','$3'}],[]}],
       [local]},
      unit("a:b([A,B,T])when B==T")),

   assertEqual(
      {{x,y,1},
       [{[['$1'|{'$2'}]],[{is_atom,'$1'}],[]}],
       [local]},
      unit("x:y([C|{D}])when is_atom(C)")),

   assertEqual(
      {{lists,reverse,1},
       [{[[97,98|'_']],[],[]}],
       [local]},
      unit("lists:reverse(\"ab\"++_)")),

   assertEqual(
      {{lists,reverse,1},
       [{['$1'],[{'==','$1',[]}],[]}],
       [local]},
      unit("lists:reverse(\"\"++A) when A==\"\"")),

   assertEqual(
      {{lists,reverse,1},
       [{["ab"],[],[]}],
       [local]},
      unit("lists:reverse(\"ab\"++\"\")")),

   assertEqual(
      {{lists,reverse,1},
       [{[[97,98|'$1']],[{'<',3,{length,'$1'}}],[]}],
       [local]},
      unit("lists:reverse(\"ab\"++C)when 3<length(C)")),

   assertEqual(
      {{a,b,1},
       [{[[97,98|'$1']],[],[]}],
       [local]},
      unit("a:b([$a,$b|C])")),

   assertEqual(
      {{a,b,1},
       [{[[46|'$1']],[],[]}],
       [local]},
     unit("a:b([22#22|C])")),

   assertEqual(
      {{a,'_','_'},
       [{[a],[],[]}],
       [global]},
      unit("a:_(a)")),

   assertEqual(
      {{a,'_','_'},
       [{'_',[],[]}],
       [global]},
      unit("a:_")),

   assertEqual(
      {{a,'_','_'},
       [{'_',[],[{exception_trace}]}],
       [global]},
      unit("a:_->return")),

   assertEqual(
      {{erlang,'_','_'},
       [{[{'$1'}],[{'==',{hd,'$1'},{{}}}],[]}],
       [global]},
      unit("erlang:_({A}) when hd(A)=={}")),

   assertEqual(
      {{a,'_','_'},
       [{[[]],[],[{exception_trace},{message,{process_dump}}]}],
       [global]},
      unit("a:X([]) -> return,stack")),

   assertEqual(
      {{lists,'_','_'},
       [{[[a]],[],[]}],
       [global]},
      unit("lists:X([a])")),

   assertEqual(
      {{lists,'_','_'},
       [{['$1'],[{is_list,'$1'}],[]}],
       [global]},
      unit("lists:X(A) when is_list(A)")),

   assertEqual(
      {{lists,'_','_'},
       [{'_',[],[]}],
       [global]},
      unit("lists:X")),

   assertEqual(
      {{x,c,1},
       [{['$1'],[{'==',['$1','$1'],{'++',['$1'],['$1']}}],[]}],
       [local]},
      unit("x:c(A)when [A,A] == [A]++[A]")),

   assertEqual(
      {{f,m,1},
       [{[<<1,$a,$b,$c>>],[],[]}],
       [local]},
      unit("f:m(<<1,\"abc\">>)")),

   assertEqual(
      {{erlang,binary_to_list,1},
       [{['$1'],[{'==','$1',<<"0">>}],[]}],
       [local]},
      unit("erlang:binary_to_list(A)when A==<<48>>")),

   assertEqual(
      {{erlang,binary_to_list,1},
       [{[<<"0">>],[],[]}],
       [local]},
      unit("erlang:binary_to_list(<<48>>)")),

   assertEqual(
      {{erlang,binary_to_list,1},
       [{['$1'],[{'==','$1',"abc"}],[]}],
       [local]},
      unit("erlang:binary_to_list(D) when D==\"abc\"")),

   assertEqual(
      {{maps,to_list,1},
       [{['$1'],[{'==','$1',#{a=>b}}],[]}],
       [local]},
      unit("maps:to_list(D) when D==#{a:=b}")),

   assertEqual(
      {{erlang,binary_to_list,1},
       [{[<<"0">>],[],[]}],
       [local]},
      unit("erlang:binary_to_list(<<\"0\">>)")),

   assertEqual(
      {{erlang,binary_to_list,1},
       [{[<<"!">>],[],[]}],
       [local]},
      unit("erlang:binary_to_list(<<1:3,1:5>>)")),

   assertEqual(
      {{maps,to_list,1},
       [{[#{a=>b,c=>d}],[],[]}],
       [local]},
      unit("maps:to_list(#{a=>b,c=>d})")),

   assertEqual(
      {{maps,to_list,1},
       [{[#{a=>b,c=>'$1'}],[{'==','$1',e}],[]}],
       [local]},
      unit("maps:to_list(#{a=>b,c=>D})when D==e")),

   assertEqual(
      {{maps,to_list,1},
       [{[#{a=>b,c=>'$1'}],[{'==','$1',e}],[]}],
       [local]},
      unit("maps:to_list(#{a:=b,c:=D})when D==e")),

   assertEqual(
      {{maps,to_list,1},
       [{['$1'],[{'is_map','$1'}],[]}],
       [local]},
      unit("maps:to_list(D)when is_map(D)"))].

assertEqual(Expected, {Input, Output}) ->
  {Input, Output, Expected}.

unit(Str) ->
  {Str, catch redbug_compiler:compile(Str)}.
