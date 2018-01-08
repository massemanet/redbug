%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 10 Mar 2010 by Mats Cronqvist <masse@kreditor.se>

%% msc - match spec compiler
%% transforms a string to a call trace expression;
%% {MFA,MatchSpec} == {{M,F,A},{Head,Cond,Body}}


-module('redbug_msc').
-author('Mats Cronqvist').

-export([transform/1]).

-define(is_string(Str),
        (Str=="" orelse (9=<hd(Str) andalso hd(Str)=<255))).

transform(E) ->
  compile(parse(to_string(E))).

to_string(A) when is_atom(A)    -> atom_to_list(A);
to_string(S) when ?is_string(S) -> S;
to_string(X)                    -> exit({illegal_input,X}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% compiler
%% returns {{Module,Function,Arity},[{Head,Cond,Body}],[Flag]}
%% i.e. the args to erlang:trace_pattern/3

compile({Mod,F,As,Gs,Acts}) ->
  {Fun,Arg}   = chk_fa(F,As),
  {Vars,Args} = compile_args(As),
  Guards      = compile_guards(Gs,Vars),
  Actions     = compile_acts(Acts),
  Flags       = compile_flags(F,Acts),
  {{Mod,Fun,Arg},[{Args,Guards,Actions}],Flags}.

chk_fa(' ',_) -> {'_','_'};
chk_fa(F,'_') -> {F,'_'};
chk_fa(F,As)  -> {F,length(As)}.

compile_flags(F,Acts) ->
  LG =
    case F of
      ' ' -> global;
      _   -> local
    end,
  lists:foldr(fun(E,A)->try [fl_fun(E)|A] catch _:_ -> A end end,[LG],Acts).

fl_fun("count") -> call_count;
fl_fun("time")  -> call_time.

compile_acts(As) ->
  lists:foldr(fun(E,A)->try [ac_fun(E)|A] catch _:_ -> A end end,[],As).

ac_fun("stack") -> {message,{process_dump}};
ac_fun("return")-> {exception_trace}.

compile_guards(Gs,Vars) ->
  {Vars,O} = lists:foldr(fun gd_fun/2,{Vars,[]},Gs),
  O.

gd_fun({Op,As},{Vars,O}) when is_list(As) -> % function
  {Vars,[unpack_op(Op,As,Vars)|O]};
gd_fun({Op,V},{Vars,O}) ->                   % unary
  {Vars,[{Op,unpack_var(V,Vars)}|O]};
gd_fun({Op,V1,V2},{Vars,O}) ->               % binary
  {Vars,[{Op,unpack_var(V1,Vars),unpack_var(V2,Vars)}|O]}.

unpack_op(Op,As,Vars) ->
  list_to_tuple([Op|[unpack_var(A,Vars)||A<-As]]).

unpack_var({bin,Bs},_) ->
  {value,Bin,[]} = erl_eval:expr({bin,1,Bs},[]),
  Bin;
unpack_var({tuple,Es},Vars) ->
  {list_to_tuple([unpack_var(E,Vars)||E<-Es])};
unpack_var({list,Es},Vars) ->
  [unpack_var(E,Vars)||E<-Es];
unpack_var({string,S},_) ->
  S;
unpack_var({var,Var},Vars) ->
  case proplists:get_value(Var,Vars) of
    undefined -> exit({unbound_variable,Var});
    V -> V
  end;
unpack_var({Op,As},Vars) when is_list(As) ->
  unpack_op(Op,As,Vars);
unpack_var({Op,V1,V2},Vars) ->
  unpack_op(Op,[V1,V2],Vars);
unpack_var({Type,Val},_) ->
  assert_type(Type,Val),
  Val.

compile_args('_') ->
  {[{'$_','$_'}],'_'};
compile_args(As) ->
  lists:foldl(fun ca_fun/2,{[],[]},As).

ca_fun({bin,Bs},{Vars,O}) ->
  {value,Bin,[]} = erl_eval:expr({bin,1,Bs},[]),
  {Vars,O++[Bin]};
ca_fun({list,Es},{Vars,O}) ->
  {Vs,Ps} = ca_fun_list(Es,Vars),
  {Vs,O++[Ps]};
ca_fun({tuple,Es},{Vars,O}) ->
  {Vs,Ps} = ca_fun_list(Es,Vars),
  {Vs,O++[list_to_tuple(Ps)]};
ca_fun({var,'_'},{Vars,O}) ->
  {Vars,O++['_']};
ca_fun({var,Var},{Vars,O}) ->
  V =
    case proplists:get_value(Var,Vars) of
      undefined -> list_to_atom("\$"++integer_to_list(length(Vars)+1));
      X -> X
    end,
  {[{Var,V}|Vars],O++[V]};
ca_fun({Type,Val},{Vars,O}) ->
  assert_type(Type,Val),
  {Vars,O++[Val]}.

ca_fun_list(Es,Vars) ->
  cfl(Es,{Vars,[]}).

cfl([],O) -> O;
cfl([E|Es],{V0,P0}) when is_list(Es) ->
  {V,P} = ca_fun(E,{V0,[]}),
  cfl(Es,{lists:usort(V0++V),P0++P});
cfl([E1|E2],{V0,P0}) ->
  %% non-proper list / tail match
  {V1,[P1]} = ca_fun(E1,{V0,[]}),
  {V2,[P2]} = ca_fun(E2,{lists:usort(V0++V1),[]}),
  {lists:usort(V0++V1++V2),P0++[P1|P2]}.

assert_type(Type,Val) ->
  case lists:member(Type,[integer,atom,string,char]) of
    true -> ok;
    false-> exit({bad_type,{Type,Val}})
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% parser
%% accepts strings like;
%%   "a","a:b","a:b/2","a:b(X,y)",
%%   "a:b(X,Y)when is_record(X,rec) and Y==0, (X==z)"
%%   "a:b->stack", "a:b(X)whenX==2->return"
%% returns
%%   {atom(M),atom(F),list(Arg)|atom('_'),list(Guard),list(Action)}
parse(Str) ->
  {Body,Guard,Action} = assert(split_fun(Str),{split_string,Str}),
  {M,F,A}             = assert(body_fun(Body),{parse_body,Str}),
  Guards              = assert(guards_fun(Guard),{parse_guards,Str}),
  Actions             = assert(actions_fun(Action),{parse_actions,Str}),
  {M,F,A,Guards,Actions}.

%% split the input string in three parts; body, guards, actions
%% we parse them separately
split_fun(Str) ->
  fun() ->
      % strip off the actions, if any
      {St,Action} =
        case re:run(Str,"^(.+)->\\s*([a-z;,]+)\\s*\$",[{capture,[1,2],list}]) of
          {match,[Z,A]} -> {Z,A};
          nomatch       -> {Str,""}
        end,
      % strip off the guards, if any
      {Body,Guard} =
        case re:run(St,"^(.+[\\s)])+when\\s(.+)\$",[{capture,[1,2],list}]) of
          {match,[Y,G]} -> {Y,G};
          nomatch       -> {St,""}
        end,
      {Body,Guard,Action}
  end.

body_fun(Str) ->
  fun() ->
      {done,{ok,Toks,1},[]} = erl_scan:tokens([],Str++". ",1),
      case erl_parse:parse_exprs(Toks) of
        {ok,[{op,1,'/',{remote,1,{atom,1,M},{atom,1,F}},{integer,1,Ari}}]} ->
          {M,F,lists:duplicate(Ari,{var,'_'})}; % m:f/2
        {ok,[{call,1,{remote,1,{atom,1,M},{atom,1,F}},Args}]} ->
          {M,F,[arg(A) || A<-Args]};            % m:f(...)
        {ok,[{call,1,{remote,1,{atom,1,M},{var,1,'_'}},Args}]} ->
          {M,' ',[arg(A) || A<-Args]};          % m:_(...)
        {ok,[{call,1,{remote,1,{atom,1,M},{var,1,_}},Args}]} ->
          {M,' ',[arg(A) || A<-Args]};          % m:V(...)
        {ok,[{remote,1,{atom,1,M},{atom,1,F}}]} ->
          {M,F,'_'};                            % m:f
        {ok,[{remote,1,{atom,1,M},{var,1,'_'}}]} ->
          {M,' ','_'};                          % m:_
        {ok,[{remote,1,{atom,1,M},{var,1,_}}]} ->
          {M,' ','_'};                          % m:V
        {ok,[{atom,1,M}]} ->
          {M,'_','_'};                          % m
        {ok,C} ->
          exit({this_is_too_confusing,C})
     end
  end.

guards_fun(Str) ->
  fun() ->
      case Str of
        "" -> [];
        _ ->
          {done,{ok,Toks,1},[]} = erl_scan:tokens([],Str++". ",1),
          {ok,Guards} = erl_parse:parse_exprs(disjunct_guard(Toks)),
          [guard(G)||G<-Guards]
      end
  end.

%% deal with disjunct guards by replacing ';' with 'orelse'
disjunct_guard(Toks) ->
  [case T of {';',1} -> {'orelse',1}; _ -> T end||T<-Toks].

guard({call,1,{atom,1,G},Args}) -> {G,[arg(A) || A<-Args]};   % function
guard({op,1,Op,One,Two})        -> {Op,guard(One),guard(Two)};% unary op
guard({op,1,Op,One})            -> {Op,guard(One)};           % binary op
guard(Guard)                    -> arg(Guard).                % variable

arg({op,_,'++',{string,_,Str},Var}) -> {list,arg_list(consa(Str,Var))};
arg({call,1,F,Args}) -> guard({call,1,F,Args});
arg({nil,_})         -> {list,[]};
arg(L={cons,_,_,_})  -> {list,arg_list(L)};
arg({tuple,_,Args})  -> {tuple,[arg(A)||A<-Args]};
arg({T,_,Var})       -> {T,Var}.

consa([],T)     -> T;
consa([C],T)    -> {cons,1,{char,1,C},T};
consa([C|Cs],T) -> {cons,1,{char,1,C},consa(Cs,T)}.

arg_list({cons,_,H,T}) -> [arg(H)|arg_list(T)];
arg_list({nil,_})      -> [];
arg_list(V)            -> arg(V).

actions_fun(Str) ->
  fun() ->
      Acts = string:tokens(Str,";,"),
      [exit({unknown_action,A}) || A <- Acts,not lists:member(A,acts())],
      Acts
  end.

acts() ->
  ["stack","return","time","count"].

assert(Fun,Tag) ->
  try Fun()
  catch
    _:{this_is_too_confusing,C}  -> exit({syntax_error,{C,Tag}});
    _:{_,{error,{1,erl_parse,L}}}-> exit({syntax_error,{lists:flatten(L),Tag}});
    _:{unknown_action,A}         -> exit({syntax_error,{unknown_action,A}});
    _:R                          -> exit({R,Tag,erlang:get_stacktrace()})
  end.
