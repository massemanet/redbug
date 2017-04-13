%% -*- mode: erlang; erlang-indent-level: 2 -*-
%% @doc
%% @end

-module(redbug_msc_eunit).

-include_lib("eunit/include/eunit.hrl").

t_0_test() ->
  ?assert(
     unit(
       {"f:c(<<>>)",
        {{f,c,1},
         [{[<<>>],[],[]}],
         [local]}})).
t_1_test() ->
  ?assert(
     unit(
       {"f:c(0.1)",
        bad_type})).
t_2_test() ->
  ?assert(
     unit(
       {"f:c(<0.0.1>)",
        syntax_error})).
t_8_test() ->
  ?assert(
     unit(
       {1,
        illegal_input})).
t9_test() ->
  ?assert(
     unit(
       {erlang,
        {{erlang,'_','_'},
         [{'_',[],[]}],
         [local]}})).
t01_test() ->
  ?assert(
     unit(
       {"a when element(1,'$_')=/=b",
        {{a,'_','_'},
         [{'_',[{'=/=',{element,1,'$_'},b}],[]}],
         [local]}})).
t02_test() ->
  ?assert(
     unit(
       {"erlang when tl(hd('$_'))=={}",
        {{erlang,'_','_'},
         [{'_',[{'==',{tl,{hd,'$_'}},{{}}}],[]}],
         [local]}})).
t03_test() ->
  ?assert(
     unit(
       {"a:b when element(1,'$_')=/=c",
        {{a,b,'_'},
         [{'_',[{'=/=',{element,1,'$_'},c}],[]}],
         [local]}})).
t04_test() ->
  ?assert(
     unit(
       {"a",
        {{a,'_','_'},
         [{'_',[],[]}],
         [local]}})).
t05_test() ->
  ?assert(
     unit(
       {"a->stack",
        {{a,'_','_'},
         [{'_',[],[{message,{process_dump}}]}],
         [local]}})).
t06_test() ->
  ?assert(
     unit(
       {"a:b",
        {{a,b,'_'},
         [{'_',[],[]}],
         [local]}})).
t07_test() ->
  ?assert(
     unit(
       {"a:b->return ",
        {{a,b,'_'},
         [{'_',[],[{exception_trace}]}],
         [local]}})).
t08_test() ->
  ?assert(
     unit(
       {"a:b/2",
        {{a,b,2},
         [{['_','_'],[],[]}],
         [local]}})).
t09_test() ->
  ?assert(
     unit(
       {"a:b/2->return",
        {{a,b,2},
         [{['_','_'],[],[{exception_trace}]}],
         [local]}})).
t10_test() ->
  ?assert(
     unit(
       {"a:b(X,Y)",
        {{a,b,2},
         [{['$1','$2'],[],[]}],
         [local]}})).
t11_test() ->
  ?assert(
     unit(
       {"a:b(_,_)",
        {{a,b,2},
         [{['_','_'],[],[]}],
         [local]}})).
t12_test() ->
  ?assert(
     unit(
       {"a:b(X,X)",
        {{a,b,2},
         [{['$1','$1'],[],[]}],
         [local]}})).
t13_test() ->
  ?assert(
     unit(
       {"a:b(X,y)",
        {{a,b,2},
         [{['$1',y],[],[]}],
         [local]}})).
t14_test() ->
  ?assert(
     unit(
       {"a:foo()when a==b",
        {{a,foo,0},
         [{[],[{'==',a,b}],[]}],
         [local]}})).
t15_test() ->
  ?assert(
     unit(
       {"a:foo when a==b",
        {{a,foo,'_'},
         [{'_',[{'==',a,b}],[]}],
         [local]}})).
t16_test() ->
  ?assert(
     unit(
       {"a:b(X,1)",
        {{a,b,2},
         [{['$1',1],[],[]}],
         [local]}})).
t17_test() ->
  ?assert(
     unit(
       {"a:b(X,\"foo\")",
        {{a,b,2},
         [{['$1',"foo"],[],[]}],
         [local]}})).
t18_test() ->
  ?assert(
     unit(
       {"x:y({A,{B,A}},A)",
        {{x,y,2},
         [{[{'$1',{'$2','$1'}},'$1'],[],[]}],
         [local]}})).
t19_test() ->
  ?assert(
     unit(
       {"x:y(A,[A,{B,[B,A]},A],B)",
        {{x,y,3},
         [{['$1',['$1',{'$2',['$2','$1']},'$1'],'$2'],[],[]}],
         [local]}})).
t20_test() ->
  ?assert(
     unit(
       {"a:b(X,y)when is_atom(Y)",
        unbound_variable})).
t21_test() ->
  ?assert(
     unit(
       {"x:c([string])",
        {{x,c,1},[{[[string]],[],[]}],
         [local]}})).
t22_test() ->
  ?assert(
     unit(
       {"x(s)",
        syntax_error})).
t23_test() ->
  ?assert(
     unit(
       {"x-s",
        syntax_error})).
t24_test() ->
  ?assert(
     unit(
       {"x:c(S)when S==x;S==y",
        {{x,c,1},
         [{['$1'],[{'orelse',{'==','$1',x},{'==','$1',y}}],[]}],
         [local]}})).
t25_test() ->
  ?assert(
     unit(
       {"x:c(S)when (S==x)or(S==y)",
        {{x,c,1},
         [{['$1'],[{'or',{'==','$1',x},{'==','$1',y}}],[]}],
         [local]}})).
t26_test() ->
  ?assert(
     unit(
       {"a:b(X,Y)when is_record(X,rec) and (Y==0), (X==z)",
        {{a,b,2},
         [{['$1','$2'],
           [{'and',{is_record,'$1',rec},{'==','$2',0}},{'==','$1',z}],[]}],
         [local]}})).
t27_test() ->
  ?assert(
     unit(
       {"x:y(z)->bla",
        syntax_error})).
t28_test() ->
  ?assert(
     unit(
       {"a:b(X,y)when not is_atom(X)",
        {{a,b,2},
         [{['$1',y],[{'not',{is_atom,'$1'}}],[]}],
         [local]}})).
t29_test() ->
  ?assert(
     unit(
       {"a:b(X,Y)when X==1,Y=/=a",
        {{a,b,2},
         [{['$1','$2'],[{'==','$1',1},{'=/=','$2',a}],[]}],
         [local]}})).
t30_test() ->
  ?assert(
     unit(
       {"a:b(X,y)when not is_atom(X) -> return",
        {{a,b,2},
         [{['$1',y],[{'not',{is_atom,'$1'}}],[{exception_trace}]}],
         [local]}})).
t31_test() ->
  ?assert(
     unit(
       {"a:b(X,y)when element(1,X)==foo, (X==z)",
        {{a,b,2},
         [{['$1',y],[{'==',{element,1,'$1'},foo},{'==','$1',z}],[]}],
         [local]}})).
t32_test() ->
  ?assert(
     unit(
       {"a:b(X,X) -> return;stack",
        {{a,b,2},
         [{['$1','$1'],[],[{exception_trace},{message,{process_dump}}]}],
         [local]}})).
t33_test() ->
  ?assert(
     unit(
       {"x:y(A,[A,B,C])when A==B,is_atom(C)",
        {{x,y,2},
         [{['$1',['$1','$2','$3']],[{'==','$1','$2'},{is_atom,'$3'}],[]}],
         [local]}})).
t34_test() ->
  ?assert(
     unit(
       {"x:y([A,B,C])when A=/=B,is_atom(C)",
        {{x,y,1},
         [{[['$1','$2','$3']],[{'=/=','$1','$2'},{is_atom,'$3'}],[]}],
         [local]}})).
t35_test() ->
  ?assert(
     unit(
       {"a:b([A,B,T])when B==T",
        {{a,b,1},
         [{[['$1','$2','$3']],[{'==','$2','$3'}],[]}],
         [local]}})).
t36_test() ->
  ?assert(
     unit(
       {"x:y([C|{D}])when is_atom(C)",
        {{x,y,1},
         [{[['$1'|{'$2'}]],[{is_atom,'$1'}],[]}],
         [local]}})).
t37_test() ->
  ?assert(
     unit(
       {"lists:reverse(\"ab\"++_)",
        {{lists,reverse,1},
         [{[[97,98|'_']],[],[]}],
         [local]}})).
t38_test() ->
  ?assert(
     unit(
       {"lists:reverse(\"ab\"++C)when 3<length(C)",
        {{lists,reverse,1},
         [{[[97,98|'$1']],[{'<',3,{length,'$1'}}],[]}],
         [local]}})).
t39_test() ->
  ?assert(
     unit(
       {"a:b([$a,$b|C])",
        {{a,b,1},[{[[97,98|'$1']],[],[]}],
         [local]}})).
t40_test() ->
  ?assert(
     unit(
       {"a:_(a)",
        {{a,'_','_'},
         [{[a],[],[]}],
         [global]}})).
t41_test() ->
  ?assert(
     unit(
       {"a:_",
        {{a,'_','_'},[{'_',[],[]}],
         [global]}})).
t42_test() ->
  ?assert(
     unit(
       {"a:_->return",
        {{a,'_','_'},
         [{'_',[],[{exception_trace}]}],
         [global]}})).
t43_test() ->
  ?assert(
     unit(
       {"erlang:_({A}) when hd(A)=={}",
        {{erlang,'_','_'},
         [{[{'$1'}],[{'==',{hd,'$1'},{{}}}],[]}],
         [global]}})).
t44_test() ->
  ?assert(
     unit(
       {"a:X([]) -> return,stack",
        {{a,'_','_'},
         [{[[]],[],[{exception_trace},{message,{process_dump}}]}],
         [global]}})).
t45_test() ->
  ?assert(
     unit(
       {"lists:X([a])",
        {{lists,'_','_'},
         [{[[a]],[],[]}],
         [global]}})).
t46_test() ->
  ?assert(
     unit(
       {"lists:X(A) when is_list(A)",
        {{lists,'_','_'},
         [{['$1'],[{is_list,'$1'}],[]}],
         [global]}})).
t47_test() ->
  ?assert(
     unit(
       {"lists:X",
        {{lists,'_','_'},
         [{'_',[],[]}],
         [global]}})).
t48_test() ->
  ?assert(
     unit(
       {"x:c(A)when [A,A] == [A]++[A]",
        {{x,c,1},
         [{['$1'],[{'==',['$1','$1'],{'++',['$1'],['$1']}}],[]}],
         [local]}})).
t49_test() ->
  ?assert(
     unit(
       {"x:c(Aw)hen [A,A] == [A]++[A]",
        syntax_error})).

t50_test() ->
  ?assert(
     unit(
       {"f:m(<<1,\"abc\">>)",
        {{f,m,1},
         [{[<<1,$a,$b,$c>>],[],[]}],
         [local]}})).

t51_test() ->
  ?assert(
     unit(
       {"erlang:binary_to_list(A)when A==<<48>>",
        {{erlang,binary_to_list,1},
         [{['$1'],[{'==','$1',<<"0">>}],[]}],
         [local]}})).

t52_test() ->
  ?assert(
     unit(
       {"erlang:binary_to_list(<<48>>)",
        {{erlang,binary_to_list,1},
         [{[<<"0">>],[],[]}],
         [local]}})).

t53_test() ->
  ?assert(
     unit(
       {"erlang:binary_to_list(<<\"0\">>)",
        {{erlang,binary_to_list,1},
         [{[<<"0">>],[],[]}],
         [local]}})).

t54_test() ->
  ?assert(
     unit(
       {"erlang:binary_to_list(<<1:3,1:5>>)",
        {{erlang,binary_to_list,1},
         [{[<<"!">>],[],[]}],
         [local]}})).

t55_test() ->
  ?assert(
     unit(
       {"erlang:binary_to_list(<<1:3,_:5>>)",
        unbound_var})).

unit({Str,MS}) ->
  try
    MS = redbug_msc:transform(Str),true
  catch
    _:{MS,_} -> true
  end.
