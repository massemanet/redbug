%% -*- mode: erlang; erlang-indent-level: 2 -*-
%% @doc
%% @end

-module(redbug_msc_eunit).

-include_lib("eunit/include/eunit.hrl").

unit(Str) ->
  try redbug_msc:transform(Str)
  catch _:R -> R
  end.

msc_test_() ->
  [?_assertEqual(
      {syntax_error,{bad_type,{float,0.1}}},
      unit("f:c(0.1)")),

   ?_assertEqual(
      {syntax_error,{parse_error,"syntax error before: '<'"}},
      unit("f:c(<0.0.1>)")),

   ?_assertEqual(
      {syntax_error,{illegal_input,1}},
      unit(1)),

   ?_assertEqual(
      {syntax_error,{unbound_var,'Y'}},
      unit("a:b(X,y)when is_atom(Y)")),

   ?_assertEqual(
      {syntax_error,{parse_error,[{call,1,{atom,1,x},[{atom,1,s}]}]}},
      unit("x(s)")),

   ?_assertEqual(
      {syntax_error,{parse_error,[{op,1,'-',{atom,1,x},{atom,1,s}}]}},
      unit("x-s")),

   ?_assertEqual(
      {syntax_error,{unknown_action,"bla"}},
      unit("x:y(z)->bla")),

   ?_assertEqual(
      {syntax_error,{parse_error,"syntax error before: hen"}},
      unit("x:c(Aw)hen [A,A] == [A]++[A]")),

   ?_assertEqual(
      {syntax_error,{parse_error,"syntax error before: '.'"}},
      unit("x:c(Aw)when [")),

   ?_assertEqual(
      {syntax_error,{bad_binary,{unbound_var,'_'}}},
      unit("erlang:binary_to_list(<<1:3,_:5>>)")),

   ?_assertEqual(
      {syntax_error,{illegal_plusplus,{{var,1,'A'},{var,1,'B'}}}},
      unit("m:f(A++B)")),

   ?_assertEqual(
      {syntax_error,{scan_error,"m'"}},
      unit("m'")),

   ?_assertEqual(
      {syntax_error,{scan_error,"'"}},
      unit("m:f when '")),

   ?_assertEqual(
      {{f,c,1},
       [{[<<>>],[],[]}],
       [local]},
      unit("f:c(<<>>)")),

   ?_assertEqual(
      {{erlang,'_','_'},
       [{'_',[],[]}],
       [local]},
      unit(erlang)),

   ?_assertEqual(
      {{a,'_','_'},
       [{'_',[{'=/=',{element,1,'$_'},b}],[]}],
       [local]},
      unit("a when element(1,'$_')=/=b")),

   ?_assertEqual(
      {{erlang,'_','_'},
       [{'_',[{'==',{tl,{hd,'$_'}},{{}}}],[]}],
       [local]},
      unit("erlang when tl(hd('$_'))=={}")),

   ?_assertEqual(
      {{a,b,'_'},
       [{'_',[{'=/=',{element,1,'$_'},c}],[]}],
       [local]},
      unit("a:b when element(1,'$_')=/=c")),

   ?_assertEqual(
      {{a,'_','_'},
       [{'_',[],[]}],
       [local]},
      unit("a")),

   ?_assertEqual(
      {{a,'_','_'},
       [{'_',[],[{message,{process_dump}}]}],
       [local]},
      unit("a->stack")),

   ?_assertEqual(
      {{a,b,'_'},
       [{'_',[],[]}],
       [local]},
      unit("a:b")),

   ?_assertEqual(
      {{a,b,'_'},
       [{'_',[],[{exception_trace}]}],
       [local]},
      unit("a:b->return ")),

   ?_assertEqual(
      {{a,b,2},
       [{['_','_'],[],[]}],
       [local]},
      unit("a:b/2")),

   ?_assertEqual(
      {{a,b,2},
       [{['_','_'],[],[{exception_trace}]}],
       [local]},
      unit("a:b/2->return")),

   ?_assertEqual(
      {{a,b,2},
       [{['$1','$2'],[],[]}],
       [local]},
      unit("a:b(X,Y)")),

   ?_assertEqual(
      {{a,b,2},
       [{['_','_'],[],[]}],
       [local]},
      unit("a:b(_,_)")),

   ?_assertEqual(
      {{a,b,2},
       [{['$1','$1'],[],[]}],
       [local]},
      unit("a:b(X,X)")),

   ?_assertEqual(
      {{a,b,2},
       [{['$1',y],[],[]}],
       [local]},
      unit("a:b(X,y)")),

   ?_assertEqual(
      {{a,foo,0},
       [{[],[{'==',a,b}],[]}],
       [local]},
      unit("a:foo()when a==b")),

   ?_assertEqual(
      {{a,foo,'_'},
       [{'_',[{'==',a,b}],[]}],
       [local]},
      unit("a:foo when a==b")),

   ?_assertEqual(
      {{a,b,2},
       [{['$1',1],[],[]}],
       [local]},
      unit("a:b(X,1)")),

   ?_assertEqual(
      {{a,b,2},
       [{['$1',"foo"],[],[]}],
       [local]},
      unit("a:b(X,\"foo\")")),

   ?_assertEqual(
      {{x,y,2},
       [{[{'$1',{'$2','$1'}},'$1'],[],[]}],
       [local]},
      unit("x:y({A,{B,A}},A)")),

   ?_assertEqual(
      {{x,y,3},
       [{['$1',['$1',{'$2',['$2','$1']},'$1'],'$2'],[],[]}],
       [local]},
      unit("x:y(A,[A,{B,[B,A]},A],B)")),

   ?_assertEqual(
      {{x,c,1},
       [{[[string]],[],[]}],
       [local]},
      unit("x:c([string])")),

   ?_assertEqual(
      {{x,c,1},
       [{['$1'],[{'orelse',{'==','$1',x},{'==','$1',y}}],[]}],
       [local]},
      unit("x:c(S)when S==x;S==y")),

   ?_assertEqual(
      {{x,c,1},
       [{['$1'],[{'or',{'==','$1',x},{'==','$1',y}}],[]}],
       [local]},
      unit("x:c(S)when (S==x)or(S==y)")),

   ?_assertEqual(
      {{a,b,2},
       [{['$1','$2'],
         [{'and',{is_record,'$1',rec},{'==','$2',0}},{'==','$1',z}],[]}],
       [local]},
      unit("a:b(X,Y)when is_record(X,rec) and (Y==0), (X==z)")),

   ?_assertEqual(
      {{a,b,2},
       [{['$1',y],[{'not',{is_atom,'$1'}}],[]}],
       [local]},
      unit("a:b(X,y)when not is_atom(X)")),

   ?_assertEqual(
      {{a,b,2},
       [{['$1','$2'],[{'==','$1',1},{'=/=','$2',a}],[]}],
       [local]},
      unit("a:b(X,Y)when X==1,Y=/=a")),

   ?_assertEqual(
      {{a,b,2},
       [{['$1',y],[{'not',{is_atom,'$1'}}],[{exception_trace}]}],
       [local]},
      unit("a:b(X,y)when not is_atom(X) -> return")),

   ?_assertEqual(
      {{a,b,2},
       [{['$1',y],[{'==',{element,1,'$1'},foo},{'==','$1',z}],[]}],
       [local]},
      unit("a:b(X,y)when element(1,X)==foo, (X==z)")),

   ?_assertEqual(
      {{a,b,2},
       [{['$1','$1'],[],[{exception_trace},{message,{process_dump}}]}],
       [local]},
      unit("a:b(X,X) -> return;stack")),

   ?_assertEqual(
      {{x,y,2},
       [{['$1',['$1','$2','$3']],[{'==','$1','$2'},{is_atom,'$3'}],[]}],
       [local]},
      unit("x:y(A,[A,B,C])when A==B,is_atom(C)")),

   ?_assertEqual(
      {{x,y,1},
       [{[['$1','$2','$3']],[{'=/=','$1','$2'},{is_atom,'$3'}],[]}],
       [local]},
      unit("x:y([A,B,C])when A=/=B,is_atom(C)")),

   ?_assertEqual(
      {{a,b,1},
       [{[['$1','$2','$3']],[{'==','$2','$3'}],[]}],
       [local]},
      unit("a:b([A,B,T])when B==T")),

   ?_assertEqual(
      {{x,y,1},
       [{[['$1'|{'$2'}]],[{is_atom,'$1'}],[]}],
       [local]},
      unit("x:y([C|{D}])when is_atom(C)")),

   ?_assertEqual(
      {{lists,reverse,1},
       [{[[97,98|'_']],[],[]}],
       [local]},
      unit("lists:reverse(\"ab\"++_)")),

   ?_assertEqual(
      {{lists,reverse,1},
       [{['$1'],[{'==','$1',[]}],[]}],
       [local]},
      unit("lists:reverse(\"\"++A) when A==\"\"")),

   ?_assertEqual(
      {{lists,reverse,1},
       [{["ab"],[],[]}],
       [local]},
      unit("lists:reverse(\"ab\"++\"\")")),

   ?_assertEqual(
      {{lists,reverse,1},
       [{[[97,98|'$1']],[{'<',3,{length,'$1'}}],[]}],
       [local]},
      unit("lists:reverse(\"ab\"++C)when 3<length(C)")),

   ?_assertEqual(
      {{a,b,1},[{[[97,98|'$1']],[],[]}],
       [local]},
      unit("a:b([$a,$b|C])")),

   ?_assertEqual(
      {{a,'_','_'},
       [{[a],[],[]}],
       [global]},
      unit("a:_(a)")),

   ?_assertEqual(
      {{a,'_','_'},[{'_',[],[]}],
       [global]},
      unit("a:_")),

   ?_assertEqual(
      {{a,'_','_'},
       [{'_',[],[{exception_trace}]}],
       [global]},
      unit("a:_->return")),

   ?_assertEqual(
      {{erlang,'_','_'},
       [{[{'$1'}],[{'==',{hd,'$1'},{{}}}],[]}],
       [global]},
      unit("erlang:_({A}) when hd(A)=={}")),

   ?_assertEqual(
      {{a,'_','_'},
       [{[[]],[],[{exception_trace},{message,{process_dump}}]}],
       [global]},
      unit("a:X([]) -> return,stack")),

   ?_assertEqual(
      {{lists,'_','_'},
       [{[[a]],[],[]}],
       [global]},
      unit("lists:X([a])")),

   ?_assertEqual(
      {{lists,'_','_'},
       [{['$1'],[{is_list,'$1'}],[]}],
       [global]},
      unit("lists:X(A) when is_list(A)")),

   ?_assertEqual(
      {{lists,'_','_'},
       [{'_',[],[]}],
       [global]},
      unit("lists:X")),

   ?_assertEqual(
      {{x,c,1},
       [{['$1'],[{'==',['$1','$1'],{'++',['$1'],['$1']}}],[]}],
       [local]},
      unit("x:c(A)when [A,A] == [A]++[A]")),

   ?_assertEqual(
      {{f,m,1},
       [{[<<1,$a,$b,$c>>],[],[]}],
       [local]},
      unit("f:m(<<1,\"abc\">>)")),

   ?_assertEqual(
      {{erlang,binary_to_list,1},
       [{['$1'],[{'==','$1',<<"0">>}],[]}],
       [local]},
      unit("erlang:binary_to_list(A)when A==<<48>>")),

   ?_assertEqual(
      {{erlang,binary_to_list,1},
       [{[<<"0">>],[],[]}],
       [local]},
      unit("erlang:binary_to_list(<<48>>)")),

   ?_assertEqual(
      {{erlang,binary_to_list,1},
       [{['$1'],[{'==','$1',"abc"}],[]}],
       [local]},
      unit("erlang:binary_to_list(D) when D==\"abc\"")),

   ?_assertEqual(
      {{maps,to_list,1},
       [{['$1'],[{'==','$1',#{a=>b}}],[]}],
       [local]},
      unit("maps:to_list(D) when D==#{a:=b}")),

   ?_assertEqual(
      {{erlang,binary_to_list,1},
       [{[<<"0">>],[],[]}],
       [local]},
      unit("erlang:binary_to_list(<<\"0\">>)")),

   ?_assertEqual(
      {{erlang,binary_to_list,1},
       [{[<<"!">>],[],[]}],
       [local]},
      unit("erlang:binary_to_list(<<1:3,1:5>>)")),

   ?_assertEqual(
      {{maps,to_list,1},
       [{[#{a=>b,c=>d}],[],[]}],
       [local]},
      unit("maps:to_list(#{a=>b,c=>d})")),

   ?_assertEqual(
      {{maps,to_list,1},
       [{[#{a=>b,c=>'$1'}],[{'==','$1',e}],[]}],
       [local]},
      unit("maps:to_list(#{a=>b,c=>D})when D==e")),

   ?_assertEqual(
      {{maps,to_list,1},
       [{[#{a=>b,c=>'$1'}],[{'==','$1',e}],[]}],
       [local]},
      unit("maps:to_list(#{a:=b,c:=D})when D==e")),

   ?_assertEqual(
      {{maps,to_list,1},
       [{['$1'],[{'is_map','$1'}],[]}],
       [local]},
      unit("maps:to_list(D)when is_map(D)"))].
