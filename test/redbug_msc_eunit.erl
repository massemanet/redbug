%% -*- mode: erlang; erlang-indent-level: 2 -*-
%% @doc
%% @end

-module(redbug_msc_eunit).

-include_lib("eunit/include/eunit.hrl").

unit(Str) ->
  try redbug_msc:transform(Str)
  catch _:{MS,_} -> MS
  end.

msc_test_() ->
  [?_assertEqual(
      unit("f:c(<<>>)"),
      {{f,c,1},
       [{[<<>>],[],[]}],
       [local]}),

   ?_assertEqual(
      unit("f:c(0.1)"),
      bad_type),

   ?_assertEqual(
      unit("f:c(<0.0.1>)"),
      syntax_error),

   ?_assertEqual(
      unit(1),
      illegal_input),

   ?_assertEqual(
      unit(erlang),
      {{erlang,'_','_'},
       [{'_',[],[]}],
       [local]}),

   ?_assertEqual(
      unit("a when element(1,'$_')=/=b"),
      {{a,'_','_'},
       [{'_',[{'=/=',{element,1,'$_'},b}],[]}],
       [local]}),

   ?_assertEqual(
      unit("erlang when tl(hd('$_'))=={}"),
      {{erlang,'_','_'},
       [{'_',[{'==',{tl,{hd,'$_'}},{{}}}],[]}],
       [local]}),

   ?_assertEqual(
      unit("a:b when element(1,'$_')=/=c"),
      {{a,b,'_'},
       [{'_',[{'=/=',{element,1,'$_'},c}],[]}],
       [local]}),

   ?_assertEqual(
      unit("a"),
      {{a,'_','_'},
       [{'_',[],[]}],
       [local]}),

   ?_assertEqual(
      unit("a->stack"),
      {{a,'_','_'},
       [{'_',[],[{message,{process_dump}}]}],
       [local]}),

   ?_assertEqual(
      unit("a:b"),
      {{a,b,'_'},
       [{'_',[],[]}],
       [local]}),

   ?_assertEqual(
      unit("a:b->return "),
      {{a,b,'_'},
       [{'_',[],[{exception_trace}]}],
       [local]}),

   ?_assertEqual(
      unit("a:b/2"),
      {{a,b,2},
       [{['_','_'],[],[]}],
       [local]}),

   ?_assertEqual(
      unit("a:b/2->return"),
      {{a,b,2},
       [{['_','_'],[],[{exception_trace}]}],
       [local]}),

   ?_assertEqual(
      unit("a:b(X,Y)"),
      {{a,b,2},
       [{['$1','$2'],[],[]}],
       [local]}),

   ?_assertEqual(
      unit("a:b(_,_)"),
      {{a,b,2},
       [{['_','_'],[],[]}],
       [local]}),

   ?_assertEqual(
      unit("a:b(X,X)"),
      {{a,b,2},
       [{['$1','$1'],[],[]}],
       [local]}),

   ?_assertEqual(
      unit("a:b(X,y)"),
      {{a,b,2},
       [{['$1',y],[],[]}],
       [local]}),

   ?_assertEqual(
      unit("a:foo()when a==b"),
      {{a,foo,0},
       [{[],[{'==',a,b}],[]}],
       [local]}),

   ?_assertEqual(
      unit("a:foo when a==b"),
      {{a,foo,'_'},
       [{'_',[{'==',a,b}],[]}],
       [local]}),

   ?_assertEqual(
      unit("a:b(X,1)"),
      {{a,b,2},
       [{['$1',1],[],[]}],
       [local]}),

   ?_assertEqual(
      unit("a:b(X,\"foo\")"),
      {{a,b,2},
       [{['$1',"foo"],[],[]}],
       [local]}),

   ?_assertEqual(
      unit("x:y({A,{B,A}},A)"),
      {{x,y,2},
       [{[{'$1',{'$2','$1'}},'$1'],[],[]}],
       [local]}),

   ?_assertEqual(
      unit("x:y(A,[A,{B,[B,A]},A],B)"),
      {{x,y,3},
       [{['$1',['$1',{'$2',['$2','$1']},'$1'],'$2'],[],[]}],
       [local]}),

   ?_assertEqual(
      unit("a:b(X,y)when is_atom(Y)"),
      unbound_variable),

   ?_assertEqual(
      unit("x:c([string])"),
      {{x,c,1},[{[[string]],[],[]}],
       [local]}),

   ?_assertEqual(
      unit("x(s)"),
      syntax_error),

   ?_assertEqual(
      unit("x-s"),
      syntax_error),

   ?_assertEqual(
      unit("x:c(S)when S==x;S==y"),
      {{x,c,1},
       [{['$1'],[{'orelse',{'==','$1',x},{'==','$1',y}}],[]}],
       [local]}),

   ?_assertEqual(
      unit("x:c(S)when (S==x)or(S==y)"),
      {{x,c,1},
       [{['$1'],[{'or',{'==','$1',x},{'==','$1',y}}],[]}],
       [local]}),

   ?_assertEqual(
      unit("a:b(X,Y)when is_record(X,rec) and (Y==0), (X==z)"),
      {{a,b,2},
       [{['$1','$2'],
         [{'and',{is_record,'$1',rec},{'==','$2',0}},{'==','$1',z}],[]}],
       [local]}),

   ?_assertEqual(
      unit("x:y(z)->bla"),
      syntax_error),

   ?_assertEqual(
      unit("a:b(X,y)when not is_atom(X)"),
      {{a,b,2},
       [{['$1',y],[{'not',{is_atom,'$1'}}],[]}],
       [local]}),

   ?_assertEqual(
      unit("a:b(X,Y)when X==1,Y=/=a"),
      {{a,b,2},
       [{['$1','$2'],[{'==','$1',1},{'=/=','$2',a}],[]}],
       [local]}),

   ?_assertEqual(
      unit("a:b(X,y)when not is_atom(X) -> return"),
      {{a,b,2},
       [{['$1',y],[{'not',{is_atom,'$1'}}],[{exception_trace}]}],
       [local]}),

   ?_assertEqual(
      unit("a:b(X,y)when element(1,X)==foo, (X==z)"),
      {{a,b,2},
       [{['$1',y],[{'==',{element,1,'$1'},foo},{'==','$1',z}],[]}],
       [local]}),

   ?_assertEqual(
      unit("a:b(X,X) -> return;stack"),
      {{a,b,2},
       [{['$1','$1'],[],[{exception_trace},{message,{process_dump}}]}],
       [local]}),

   ?_assertEqual(
      unit("x:y(A,[A,B,C])when A==B,is_atom(C)"),
      {{x,y,2},
       [{['$1',['$1','$2','$3']],[{'==','$1','$2'},{is_atom,'$3'}],[]}],
       [local]}),

   ?_assertEqual(
      unit("x:y([A,B,C])when A=/=B,is_atom(C)"),
      {{x,y,1},
       [{[['$1','$2','$3']],[{'=/=','$1','$2'},{is_atom,'$3'}],[]}],
       [local]}),

   ?_assertEqual(
      unit("a:b([A,B,T])when B==T"),
      {{a,b,1},
       [{[['$1','$2','$3']],[{'==','$2','$3'}],[]}],
       [local]}),

   ?_assertEqual(
      unit("x:y([C|{D}])when is_atom(C)"),
      {{x,y,1},
       [{[['$1'|{'$2'}]],[{is_atom,'$1'}],[]}],
       [local]}),

   ?_assertEqual(
      unit("lists:reverse(\"ab\"++_)"),
      {{lists,reverse,1},
       [{[[97,98|'_']],[],[]}],
       [local]}),

   ?_assertEqual(
      unit("lists:reverse(\"\"++A) when A==\"\""),
      {{lists,reverse,1},
       [{[[[]|'$1']],[{'==','$1',[]}],[]}],
       [local]}),

   ?_assertEqual(
      unit("lists:reverse(\"ab\"++\"\")"),
      {{lists,reverse,1},
       [{["ab"],[],[]}],
       [local]}),

   ?_assertEqual(
      unit("lists:reverse(\"ab\"++C)when 3<length(C)"),
      {{lists,reverse,1},
       [{[[97,98|'$1']],[{'<',3,{length,'$1'}}],[]}],
       [local]}),

   ?_assertEqual(
      unit("a:b([$a,$b|C])"),
      {{a,b,1},[{[[97,98|'$1']],[],[]}],
       [local]}),

   ?_assertEqual(
      unit("a:_(a)"),
      {{a,'_','_'},
       [{[a],[],[]}],
       [global]}),

   ?_assertEqual(
      unit("a:_"),
      {{a,'_','_'},[{'_',[],[]}],
       [global]}),

   ?_assertEqual(
      unit("a:_->return"),
      {{a,'_','_'},
       [{'_',[],[{exception_trace}]}],
       [global]}),

   ?_assertEqual(
      unit("erlang:_({A}) when hd(A)=={}"),
      {{erlang,'_','_'},
       [{[{'$1'}],[{'==',{hd,'$1'},{{}}}],[]}],
       [global]}),

   ?_assertEqual(
      unit("a:X([]) -> return,stack"),
      {{a,'_','_'},
       [{[[]],[],[{exception_trace},{message,{process_dump}}]}],
       [global]}),

   ?_assertEqual(
      unit("lists:X([a])"),
      {{lists,'_','_'},
       [{[[a]],[],[]}],
       [global]}),

   ?_assertEqual(
      unit("lists:X(A) when is_list(A)"),
      {{lists,'_','_'},
       [{['$1'],[{is_list,'$1'}],[]}],
       [global]}),

   ?_assertEqual(
      unit("lists:X"),
      {{lists,'_','_'},
       [{'_',[],[]}],
       [global]}),

   ?_assertEqual(
      unit("x:c(A)when [A,A] == [A]++[A]"),
      {{x,c,1},
       [{['$1'],[{'==',['$1','$1'],{'++',['$1'],['$1']}}],[]}],
       [local]}),

   ?_assertEqual(
      unit("x:c(Aw)hen [A,A] == [A]++[A]"),
      syntax_error),

   ?_assertEqual(
      unit("f:m(<<1,\"abc\">>)"),
      {{f,m,1},
       [{[<<1,$a,$b,$c>>],[],[]}],
       [local]}),

   ?_assertEqual(
      unit("erlang:binary_to_list(A)when A==<<48>>"),
      {{erlang,binary_to_list,1},
       [{['$1'],[{'==','$1',<<"0">>}],[]}],
       [local]}),

   ?_assertEqual(
      unit("erlang:binary_to_list(<<48>>)"),
      {{erlang,binary_to_list,1},
       [{[<<"0">>],[],[]}],
       [local]}),

   ?_assertEqual(
      unit("erlang:binary_to_list(D) when D==\"abc\""),
      {{erlang,binary_to_list,1},
       [{['$1'],[{'==','$1',"abc"}],[]}],
       [local]}),

   ?_assertEqual(
      unit("maps:to_list(D) when D==#{a:=b}"),
      {{maps,to_list,1},
       [{['$1'],[{'==','$1',#{a=>b}}],[]}],
       [local]}),

   ?_assertEqual(
      unit("erlang:binary_to_list(<<\"0\">>)"),
      {{erlang,binary_to_list,1},
       [{[<<"0">>],[],[]}],
       [local]}),

   ?_assertEqual(
      unit("erlang:binary_to_list(<<1:3,1:5>>)"),
      {{erlang,binary_to_list,1},
       [{[<<"!">>],[],[]}],
       [local]}),

   ?_assertEqual(
      unit("erlang:binary_to_list(<<1:3,_:5>>)"),
      syntax_error),

   ?_assertEqual(
      unit("maps:to_list(#{a=>b,c=>d})"),
      {{maps,to_list,1},
       [{[#{a=>b,c=>d}],[],[]}],
       [local]}),

   ?_assertEqual(
      unit("maps:to_list(#{a=>b,c=>D})when D==e"),
      {{maps,to_list,1},
       [{[#{a=>b,c=>'$1'}],[{'==','$1',e}],[]}],
       [local]}),

   ?_assertEqual(
      unit("maps:to_list(#{a:=b,c:=D})when D==e"),
      {{maps,to_list,1},
       [{[#{a=>b,c=>'$1'}],[{'==','$1',e}],[]}],
       [local]}),

   ?_assertEqual(
      unit("maps:to_list(D)when is_map(D)"),
      {{maps,to_list,1},
       [{['$1'],[{'is_map','$1'}],[]}],
       [local]})].
