%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Author  : Mats Cronqvist <masse@cronqvi.st>
%%% Created : 10 Mar 2010 by Mats Cronqvist <masse@kreditor.se>

%% msc - match spec compiler
%% transforms a string to a call trace expression;
%% {MFA,MatchSpec} == {{M,F,A},{Head,Cond,Body}}


-module('redbug_msc').

-export([transform/1]).

-define(LEX_COMMA,        {',',1}).
-define(LEX_COLUMN,       {':',1}).
-define(LEX_HASH,         {'#',1}).
-define(LEX_EQUAL,        {'=',1}).
-define(LEX_OPEN_BRACE,   {'{',1}).
-define(LEX_CLOSE_BRACE,  {'}',1}).
-define(LEX_ANY,          {var,1,'_'}).

transform(E) ->
  try
    compile(parse(to_string(E)))
  catch
    throw:{R,Info} -> exit({syntax_error,{R,Info}})
  end.

-define(is_string(Str), (Str=="" orelse (9=<hd(Str) andalso hd(Str)=<255))).
to_string(A) when is_atom(A)    -> atom_to_list(A);
to_string(S) when ?is_string(S) -> S;
to_string(X)                    -> throw({illegal_input,X}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% compiler
%% returns {{Module,Function,Arity},[{Head,Cond,Body}],[Flag]}
%% i.e. the args to erlang:trace_pattern/3

compile({Mod,F,As,Gs,Acts}) ->
  {Fun,Arg}   = compile_function(F,As),
  {Vars,Args} = compile_args(As),
  Guards      = compile_guards(Gs,Vars),
  Actions     = compile_acts(Acts),
  Flags       = compile_flags(F,Acts),
  {{Mod,Fun,Arg},[{Args,Guards,Actions}],Flags}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% compile function name
compile_function(' ',_) -> {'_','_'};
compile_function(F,'_') -> {F,'_'};
compile_function(F,As)  -> {F,length(As)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% compile argument list
compile_args('_') ->
  {[{'$_','$_'}],'_'};
compile_args(As) ->
  lists:foldl(fun ca_fun/2,{[],[]},As).

ca_fun({map,Map},{Vars,O}) ->
  {Vs,Ps} = ca_map(Map,Vars),
  {Vs,O++[maps:from_list(Ps)]};
ca_fun({list,Es},{Vars,O}) ->
  {Vs,Ps} = ca_list(Es,Vars),
  {Vs,O++[Ps]};
ca_fun({tuple,Es},{Vars,O}) ->
  {Vs,Ps} = ca_list(Es,Vars),
  {Vs,O++[list_to_tuple(Ps)]};
ca_fun({var,'_'},{Vars,O}) ->
  {Vars,O++['_']};
ca_fun({var,Var},{Vars,O}) ->
  V = get_var(Var, Vars),
  {[{Var,V}|Vars],O++[V]};
ca_fun({Type,Val},{Vars,O}) ->
  assert_type(Type,Val),
  {Vars,O++[Val]}.

ca_map(Fs,Vars) ->
  cfm(Fs,{Vars,[]}).

cfm([],O) -> O;
cfm([{K,V}|Fs],{V0,P0}) ->
  {[],[PK]} = ca_fun(K,{[],[]}),
  {Vs,[PV]} = ca_fun(V,{V0,[]}),
  cfm(Fs,{lists:usort(V0++Vs),P0++[{PK,PV}]}).

ca_list(Es,Vars) ->
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

get_var(Var,Vars) ->
  case proplists:get_value(Var,Vars) of
    undefined -> list_to_atom("\$"++integer_to_list(length(Vars)+1));
    X -> X
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% compile guards
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

unpack_var({map,M},Vars) ->
  maps:from_list([{unpack_var(K,Vars),unpack_var(V,Vars)}||{K,V}<-M]);
unpack_var({tuple,Es},Vars) ->
  {list_to_tuple([unpack_var(E,Vars)||E<-Es])};
unpack_var({list,Es},Vars) ->
  [unpack_var(E,Vars)||E<-Es];
unpack_var({string,S},_) ->
  S;
unpack_var({var,Var},Vars) ->
  case proplists:get_value(Var,Vars) of
    undefined -> throw({unbound_var,Var});
    V -> V
  end;
unpack_var({Op,As},Vars) when is_list(As) ->
  unpack_op(Op,As,Vars);
unpack_var({Op,V1,V2},Vars) ->
  unpack_op(Op,[V1,V2],Vars);
unpack_var({Type,Val},_) ->
  assert_type(Type,Val),
  Val.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% compile trace flags
compile_flags(F,Acts) ->
  LG =
    case F of
      ' ' -> global;
      _   -> local
    end,
  lists:foldr(fun(E,A)->try [fl_fun(E)|A] catch _:_ -> A end end,[LG],Acts).

fl_fun("count") -> call_count;
fl_fun("time")  -> call_time.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% compile actions
compile_acts(As) ->
  lists:foldr(fun(E,A)->try [ac_fun(E)|A] catch _:_ -> A end end,[],As).

ac_fun("stack") -> {message,{process_dump}};
ac_fun("return")-> {exception_trace}.

assert_type(Type,Val) ->
  case lists:member(Type,[integer,atom,string,char,bin]) of
    true -> ok;
    false-> throw({bad_type,{Type,Val}})
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
  {Body,Guard,Action} = split(Str),
  {M,F,A}             = body(Body),
  Guards              = guards(Guard),
  Actions             = actions(Action),
  {M,F,A,Guards,Actions}.

%% split the input string in three parts; body, guards, actions
%% we parse them separately
split(Str) ->
  %% strip off the actions, if any
  {St,Action} =
    case re:run(Str,"^(.+)->\\s*([a-z;,]+)\\s*\$",[{capture,[1,2],list}]) of
      {match,[Z,A]} -> {Z,A};
      nomatch       -> {Str,""}
    end,
  %% strip off the guards, if any
  {Body,Guard} =
    case re:run(St,"^(.+[\\s)])+when\\s(.+)\$",[{capture,[1,2],list}]) of
      {match,[Y,G]} -> {Y,G};
      nomatch       -> {St,""}
    end,
  {Body,Guard,Action}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% parse body
body(Str) ->
  case erl_scan:tokens([],Str++". ",1) of
    {done,{ok,Toks0,1},[]} ->
      Toks = case Toks0 of
        [{atom,1,Module},?LEX_COLUMN | _] ->
          case catch scan_records(read_records(Module, []), []) of
                   R when is_list(R) -> replace_records(R, Toks0);
                   _ -> Toks0
          end;
        _ -> Toks0
      end,
      case erl_parse:parse_exprs(Toks) of
        {ok,[{op,_,'/',{remote,_,{atom,_,M},{atom,_,F}},{integer,_,Ari}}]} ->
          {M,F,lists:duplicate(Ari,{var,'_'})}; % m:f/2
        {ok,[{call,_,{remote,_,{atom,_,M},{atom,_,F}},Args}]} ->
          {M,F,[arg(A) || A<-Args]};            % m:f(...)
        {ok,[{call,_,{remote,_,{atom,_,M},{var,_,'_'}},Args}]} ->
          {M,' ',[arg(A) || A<-Args]};          % m:_(...)
        {ok,[{call,_,{remote,_,{atom,_,M},{var,_,_}},Args}]} ->
          {M,' ',[arg(A) || A<-Args]};          % m:V(...)
        {ok,[{remote,_,{atom,_,M},{atom,_,F}}]} ->
          {M,F,'_'};                            % m:f
        {ok,[{remote,_,{atom,_,M},{var,_,'_'}}]} ->
          {M,' ','_'};                          % m:_
        {ok,[{remote,_,{atom,_,M},{var,_,_}}]} ->
          {M,' ','_'};                          % m:V
        {ok,[{atom,_,M}]} ->
          {M,'_','_'};                          % m
        {ok,C} ->
          throw({parse_error,C});
        {error,{_,erl_parse,L}} ->
          throw({parse_error,lists:flatten(L)})
     end;
    _ ->
      throw({scan_error,Str})
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% parse guards
guards("") -> "";
guards(Str) ->
  case erl_scan:tokens([],Str++". ",1) of
    {done,{ok,Toks,1},[]} ->
      case erl_parse:parse_exprs(disjunct_guard(Toks)) of
        {ok,Guards} ->
          [guard(G)||G<-Guards];
        {error,{_,erl_parse,L}} ->
          throw({parse_error,lists:flatten(L)})
      end;
    _ ->
      throw({scan_error,Str})
  end.

%% deal with disjunct guards by replacing ';' with 'orelse'
disjunct_guard(Toks) ->
  [case T of {';',1} -> {'orelse',1}; _ -> T end||T<-Toks].

guard({call,_,{atom,_,G},As}) -> {G,[guard(A) || A<-As]};   % function
guard({op,_,Op,One,Two})      -> {Op,guard(One),guard(Two)};% binary op
guard({op,_,Op,One})          -> {Op,guard(One)};           % unary op
guard(Guard)                  -> arg(Guard).                % variable

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% parse actions
actions(Str) ->
  Acts = string:tokens(Str,";,"),
  [throw({unknown_action,A}) || A <- Acts,not lists:member(A,acts())],
  Acts.

acts() ->
  ["stack","return","time","count"].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% parse arguments
arg({op,_,'++',A1,A2}) -> plusplus(A1,A2);
arg({nil,_})           -> {list,[]};
arg({cons,_,H,T})      -> {list,arg_list({cons,1,H,T})};
arg({tuple,_,Args})    -> {tuple,[arg(A)||A<-Args]};
arg({map,_,Map})       -> {map,[{arg(K),arg(V)}||{_,_,K,V}<-Map]};
arg({bin,_,Bin})       -> {bin,eval_bin(Bin)};
arg({T,_,Var})         -> {T,Var}.

plusplus({string,_,[]},Var) -> arg(Var);
plusplus({string,_,St},Var) -> {list,arg_list(consa(St,Var))};
plusplus(A1,A2)             -> throw({illegal_plusplus,{A1,A2}}).

consa([C],T)    -> {cons,1,{char,1,C},T};
consa([C|Cs],T) -> {cons,1,{char,1,C},consa(Cs,T)}.

arg_list({cons,_,H,T}) -> [arg(H)|arg_list(T)];
arg_list({nil,_})      -> [];
arg_list(V)            -> arg(V).

eval_bin(Bin) ->
  try
    {value,B,[]} = erl_eval:expr({bin,1,Bin},[]),
    B
  catch
    _:R -> throw({bad_binary,R})
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% replace #records by tuples
replace_records(Defs, Toks) ->
  lists:reverse(rewrite_records(parse_records(Defs, Toks, []), [])).

%%% replace #records with a private tuple {record, {name, fields, values}} in the list of tokens

parse_records(_Defs, [], Toks) -> lists:reverse(Toks);

parse_records(Defs, [?LEX_HASH,{atom,1,RecordName},?LEX_OPEN_BRACE | T], Toks) ->
  case lists:keyfind(RecordName, 1, Defs) of
    {_Name, Fields} ->
      {Rec, Remainder} = parse_record(Defs, RecordName, Fields, T),
      parse_records(Defs, Remainder, [Rec | Toks]);
    _ ->
      throw({missing_record, RecordName})
  end;

parse_records(Defs, [H | T], Toks) ->
  parse_records(Defs, T, [H | Toks]).

parse_record(Defs, RecordName, Fields, T) ->
  {Values, Remainder} = parse_record_fields([], 0, T),
  Values1 = lists:map(fun({Name, Value}) -> {Name, parse_records(Defs, Value, [])} end, Values),
  {{record, {RecordName, Fields, Values1}}, Remainder}.

parse_record_fields(Values, 0, [{atom,1,Field},?LEX_EQUAL | T]) ->
  parse_record_value(Values, 0, Field, [], T);

parse_record_fields(Values, 0, [?LEX_CLOSE_BRACE | T]) -> {Values, T}.

parse_record_value(Values, 0, Field, Value, [?LEX_COMMA | T]) ->
  parse_record_fields([{Field, lists:reverse(Value)} | Values], 0, T);

parse_record_value(Values, 0, Field, Value, [?LEX_CLOSE_BRACE | T]) ->
  parse_record_fields([{Field, lists:reverse(Value)} | Values], 0, [?LEX_CLOSE_BRACE | T]);

parse_record_value(Values, Depth, Field, Value, [?LEX_OPEN_BRACE | T]) ->
  parse_record_value(Values, Depth + 1, Field, [?LEX_OPEN_BRACE | Value], T);

parse_record_value(Values, Depth, Field, Value, [?LEX_CLOSE_BRACE | T]) ->
  parse_record_value(Values, Depth - 1, Field, [?LEX_CLOSE_BRACE | Value], T);

parse_record_value(Values, Depth, Field, Value, [H | T]) ->
  parse_record_value(Values, Depth, Field, [H | Value], T).

%%% replace the private tuple {record, {name, fields, values}} with record as tuples

rewrite_records([{record, {Name, Fields, Values}} | T], L) ->
  {_, NL} = lists:foldl(fun
                          (Field, {Rank, Acc0}) ->
                            Acc = [?LEX_COMMA | Acc0],
                            case lists:keyfind(Field, 1, Values) of
                              {_Name, Values0} ->
                                {Rank + 1, rewrite_records(Values0, Acc)};
                              _ ->
                                {Rank + 1, [?LEX_ANY | Acc]}
                            end
                        end, {1, [{atom,1,Name},?LEX_OPEN_BRACE | L]}, Fields),
  rewrite_records(T, [?LEX_CLOSE_BRACE | NL]);

rewrite_records([], L) -> L;

rewrite_records([E | T], L) ->
  rewrite_records(T, [E | L]).

%%% create a property list with all records (in abstract code)

scan_records([], L) -> L;
scan_records([{attribute,_,record, {Name, Fields}} | T], L) ->
  scan_records(T, [scan_record(Name, Fields) | L]);

scan_records([_H | T], L) ->
  scan_records(T, L).

scan_record(Name, Fields) ->
  {Name, lists:reverse(scan_record_fields(Fields, []))}.

scan_record_fields([], L) -> L;

scan_record_fields([{typed_record_field, F, _T} | T], L) ->
  scan_record_fields(T, scan_record_fields([F], L));

scan_record_fields([Rec | T], L) when element(1, Rec) == record_field ->
  {atom, _, N} = element(3, Rec),
  scan_record_fields(T, [N | L]).

%%% Read record information from file(s)  (source code from "stdlib/shell.erl")

read_records(FileOrModule, Opts0) ->
  Opts = lists:delete(report_warnings, Opts0),
  case find_file(FileOrModule) of
    {files,[File]} ->
      read_file_records(File, Opts);
    {files,Files} ->
      lists:flatmap(fun(File) ->
        case read_file_records(File, Opts) of
          RAs when is_list(RAs) -> RAs;
          _ -> []
        end
      end, Files);
    Error ->
      Error
  end.

-include_lib("kernel/include/file.hrl").

find_file(Mod) when is_atom(Mod) ->
  case code:which(Mod) of
    File when is_list(File) ->
      {files,[File]};
    preloaded ->
      {_M,_Bin,File} = code:get_object_code(Mod),
      {files,[File]};
    _Else -> % non_existing, interpreted, cover_compiled
      {error,nofile}
  end;
find_file(File) ->
  case catch filelib:wildcard(File) of
    {'EXIT',_} ->
      {error,invalid_filename};
    Files ->
      {files,Files}
  end.

read_file_records(File, Opts) ->
  case filename:extension(File) of
    ".beam" ->
      case beam_lib:chunks(File, [abstract_code,"CInf"]) of
        {ok,{_Mod,[{abstract_code,{Version,Forms}},{"CInf",CB}]}} ->
          case record_attrs(Forms) of
            [] when Version =:= raw_abstract_v1 ->
              [];
            [] ->
              %% If the version is raw_X, then this test
              %% is unnecessary.
              try_source(File, CB);
            Records ->
              Records
          end;
        {ok,{_Mod,[{abstract_code,no_abstract_code},{"CInf",CB}]}} ->
          try_source(File, CB);
        Error ->
          %% Could be that the "Abst" chunk is missing (pre R6).
          Error
      end;
    _ ->
      parse_file(File, Opts)
  end.

%% This is how the debugger searches for source files. See int.erl.
try_source(Beam, CB) ->
  Os = case lists:keyfind(options, 1, binary_to_term(CB)) of
         false -> [];
         {_, Os0} -> Os0
       end,
  Src0 = filename:rootname(Beam) ++ ".erl",
  case is_file(Src0) of
    true -> parse_file(Src0, Os);
    false ->
      EbinDir = filename:dirname(Beam),
      Src = filename:join([filename:dirname(EbinDir), "src",
        filename:basename(Src0)]),
      case is_file(Src) of
        true -> parse_file(Src, Os);
        false -> {error, nofile}
      end
  end.

is_file(Name) ->
  case filelib:is_file(Name) of
    true ->
      not filelib:is_dir(Name);
    false ->
      false
  end.

parse_file(File, Opts) ->
  Cwd = ".",
  Dir = filename:dirname(File),
  IncludePath = [Cwd,Dir|inc_paths(Opts)],
  case epp:parse_file(File, IncludePath, pre_defs(Opts)) of
    {ok,Forms} ->
      record_attrs(Forms);
    Error ->
      Error
  end.

pre_defs([{d,M,V}|Opts]) ->
  [{M,V}|pre_defs(Opts)];
pre_defs([{d,M}|Opts]) ->
  [M|pre_defs(Opts)];
pre_defs([_|Opts]) ->
  pre_defs(Opts);
pre_defs([]) -> [].

inc_paths(Opts) ->
  [P || {i,P} <- Opts, is_list(P)].

record_attrs(Forms) ->
  [A || A = {attribute,_,record,_D} <- Forms].

%%% End of reading record information from file(s)

%%-------------------------------------------------------------------------
%%              UNIT Tests
%%-------------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

scan(Str) ->
  {done,{ok,Toks,1},[]} = erl_scan:tokens([], Str++". ",1), Toks.

replace_records_test() ->

  R1 = parse_records([
      {myrecord, [firstname,age,now]},
      {jid, [user,server,resource]}
    ],
    scan("#myrecord{now = #jid{server = <<\"r.com\">>}, firstname = <<\"Frank\">>, age = {over, 20}}"),
    []
  ),

  ?assertEqual([
      {record,
        {
          myrecord,
          [firstname,age,now],
          [
            {age,[?LEX_OPEN_BRACE, {atom,1,over}, ?LEX_COMMA, {integer,1,20}, ?LEX_CLOSE_BRACE]},
            {firstname,[{'<<',1},{string,1,"Frank"},{'>>',1}]},
            {now,[{record, {
                jid,
                [user,server,resource],
                [
                  {server, [{'<<',1}, {string,1,"r.com"}, {'>>',1}]}
                ]
              }}]}
          ]
        }
      },
      {dot,1}
    ],
    R1
  ),

  R2 = parse_records(
    [{myrecord, [firstname,age,now]}],
    scan("#myrecord{now = 2019, firstname = <<\"Frank\">>, age = {over, 20}}"),
    []
  ),

  ?assertEqual([
      {record,
        {
          myrecord,
          [firstname,age,now],
          [
            {age,[?LEX_OPEN_BRACE, {atom,1,over}, ?LEX_COMMA, {integer,1,20}, ?LEX_CLOSE_BRACE]},
            {firstname,[{'<<',1},{string,1,"Frank"},{'>>',1}]},
            {now,[{integer,1,2019}]}
          ]
        }
      },
      {dot,1}
    ],
    R2
  ),

  R3 = parse_record(
    [{myrecord, [firstname,age,now]}],
    myrecord,
    [firstname,age,now],
    scan("now = 2019, firstname = <<\"Frank\">>, age = {over, 20}}")
  ),

  ?assertEqual({
      {record,
        {
          myrecord,
          [firstname,age,now],
          [
            {age,[?LEX_OPEN_BRACE, {atom,1,over}, ?LEX_COMMA, {integer,1,20}, ?LEX_CLOSE_BRACE]},
            {firstname,[{'<<',1},{string,1,"Frank"},{'>>',1}]},
            {now,[{integer,1,2019}]}
          ]
        }
      }, [
        {dot,1}
      ]
    },
    R3
  ),

  R4 = replace_records([
    {myrecord, [firstname,age,now]},
    {jid, [user,server,resource]}
  ], scan("#myrecord{now = #jid{server = <<\"xmpp.com\">>}, firstname = <<\"Frank\">>, age = {over, 20}}")),

  ?assertEqual([
      ?LEX_OPEN_BRACE,
      {atom,1,myrecord},
      ?LEX_COMMA,
      {'<<',1},
      {string,1,"Frank"},
      {'>>',1},
      ?LEX_COMMA,
      ?LEX_OPEN_BRACE,
      {atom,1,over},
      ?LEX_COMMA,
      {integer,1,20},
      ?LEX_CLOSE_BRACE,
      ?LEX_COMMA,
      ?LEX_OPEN_BRACE,
      {atom,1,jid},
      ?LEX_COMMA,
      ?LEX_ANY,
      ?LEX_COMMA,
      {'<<',1},
      {string,1,"xmpp.com"},
      {'>>',1},
      ?LEX_COMMA,
      ?LEX_ANY,
      ?LEX_CLOSE_BRACE,
      ?LEX_CLOSE_BRACE,
      {dot,1}
    ],
    R4
  ),

  R5 = replace_records([
    {myrecord, [firstname,age,now]}
  ], scan("hello:run(#myrecord{now = 2019}, 90)")),
  ?assertEqual([
    {atom,1,hello},
    ?LEX_COLUMN,
    {atom,1,run},
    {'(',1},
    ?LEX_OPEN_BRACE,
    {atom,1,myrecord},
    ?LEX_COMMA,
    ?LEX_ANY,
    ?LEX_COMMA,
    ?LEX_ANY,
    ?LEX_COMMA,
    {integer,1,2019},
    ?LEX_CLOSE_BRACE,
    ?LEX_COMMA,
    {integer,1,90},
    {')',1},
    {dot,1}],
    R5
  )
.

-endif.
