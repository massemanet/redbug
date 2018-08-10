-module(redbug_compiler).
-export([compile/1, parse/1, scan/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API

compile(X) ->
    try generate(parse(to_str(X)))
    catch
        exit:not_string       -> exit({syntax_error, "bad input"});
        exit:{scan_error, R}  -> exit({syntax_error, R});
        exit:{parse_error, R} -> exit({syntax_error, R});
        exit:{gen_error, R}   -> exit({syntax_error, R})
    end.

parse(Str) ->
    case catch redbug_parser:parse(scan(Str)) of
        {ok, Tree}             -> Tree;
        {error, {_, _, Error}} -> exit({parse_error, lists:flatten(Error)});
        {'EXIT', Error}        -> exit(Error)
    end.

scan(Str) ->
    case catch redbug_lexer:string(Str) of
        {ok, Tokens, _}                    -> Tokens;
        {error, {_, _, {illegal, Tok}}, _} -> exit({scan_error, "at: "++Tok});
        {error, {_, _, Error}, _}          -> exit({scan_error, Error});
        {'EXIT', R}                        -> exit({scan_error, {bad_input, R}})
    end.

to_str(Atom) when is_atom(Atom)  -> atom_to_list(Atom);
to_str(Bin)  when is_binary(Bin) -> binary_to_list(Bin);
to_str(List) when is_list(List)  -> List;
to_str(_) -> exit(not_string).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% the code generator

generate({MFA, G, As}) ->
    {M, F, Arity, Args, Ctxt, Flags0} = mk_mfa(MFA),
    {Guard, Flags1} = mk_guard(G, Ctxt, Flags0),
    {Actions, Flags2} = mk_actions(As, Flags1),
    {{M, F, Arity}, [{Args, Guard, Actions}], Flags2}.

%% the lifting context. bindings maps RTP variable names to match spec vars

-record(ctxt,
        {context=args,
         count=0,
         bindings=[]}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% module, function, arity, arguments

mk_mfa({M, F, As}) ->
    Mod = mk_mod(M),
    {Fun, Flags} = mk_fun(F),
    {Arity, Args, Ctxt} = mk_args(As, Fun),
    {Mod, Fun, Arity, Args, Ctxt, Flags}.

mk_mod({module, Mod}) -> Mod.

mk_fun('_') ->
    {'_', [local]};
mk_fun({function, '_'}) ->
    {'_', [global]};
mk_fun({function, Fun}) ->
    {Fun, [local]}.

mk_args({arity, Arity}, '_') ->
    {'_', lists:duplicate(Arity, '_'), #ctxt{}};
mk_args({arity, Arity}, _) ->
    {Arity, '_', #ctxt{}};
mk_args('_', _) ->
    {'_', '_', #ctxt{}};
mk_args(As, '_') ->
    {Args, Ctxt} = lift_list(As, #ctxt{}),
    {'_', Args, Ctxt};
mk_args(As, _) ->
    {Args, Ctxt} = lift_list(As, #ctxt{}),
    {length(Args), Args, Ctxt}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% guards

mk_guard('_', _, Flags) ->
    {[], Flags};
mk_guard(G, Ctxt, Flags) ->
    {Guard, _} = lift(G, Ctxt#ctxt{context=guard}),
    {[Guard], Flags}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% actions

mk_actions('_', Flags) ->
    {[], Flags};
mk_actions(As, Flags) ->
    lists:foldr(fun chk_action/2, {[], Flags}, As).

chk_action({action, stack},  {As, Fs}) -> {[{message,{process_dump}}|As], Fs};
chk_action({action, return}, {As, Fs}) -> {[{exception_trace}|As], Fs};
chk_action({action, time},   {As, Fs}) -> {As, [call_time|Fs]};
chk_action({action, count},  {As, Fs}) -> {As, [call_count|Fs]};
chk_action({action, Act}, _)           -> die("illegal action", Act).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% make concrete values

%% values
lift({atom, Value}, Ctxt) -> {Value, Ctxt};
lift({int,  Value}, Ctxt) -> {Value, Ctxt};
lift({bin,  Value}, Ctxt) -> {Value, Ctxt};
%% variables
lift({variable, "_"}, Ctxt) -> {'_', Ctxt};
lift({variable, Var}, Ctxt) -> lift_var(Var, Ctxt);
%% composite terms
lift({tuple,  Es},  Ctxt) -> lift_tuple(Es, Ctxt);
lift({list,   Es},  Ctxt) -> lift_list(Es, Ctxt);
lift({map,    KVs}, Ctxt) -> lift_map(KVs, Ctxt);
lift({record, Rec}, Ctxt) -> lift_record(Rec, Ctxt);
lift({field,  KV},  Ctxt) -> lift_field(KV, Ctxt);
%% operators and functions
lift({{comparison_op, Op},  Args}, Ctxt) -> lift2(Op,  Args, Ctxt);
lift({{arithmetic_op, Op},  Args}, Ctxt) -> lift2(Op,  Args, Ctxt);
lift({{boolean_op1,   Op},  Args}, Ctxt) -> lift1(Op,  Args, Ctxt);
lift({{boolean_op2,   Op},  Args}, Ctxt) -> lift2(Op,  Args, Ctxt);
lift({{type_test1,    Tst}, Args}, Ctxt) -> lift1(Tst, Args, Ctxt);
lift({{type_isrec,    Tst}, Args}, Ctxt) -> liftR(Tst, Args, Ctxt);
lift({{bif1,          Bif}, Args}, Ctxt) -> lift1(Bif, Args, Ctxt);
lift({{bif2,          Bif}, Args}, Ctxt) -> lift2(Bif, Args, Ctxt);
lift({{bif3,          Bif}, Args}, Ctxt) -> lift3(Bif, Args, Ctxt).

%% different arity functions
lift1(Op, Args, Ctxt) ->
    {[Arg1], _} = lift_list(Args, Ctxt),
    {{Op, Arg1}, Ctxt}.

lift2(Op, Args, Ctxt) ->
    {[Arg1, Arg2], _} = lift_list(Args, Ctxt),
    {{Op, Arg1, Arg2}, Ctxt}.

lift3(Op, Args, Ctxt) ->
    {[Arg1, Arg2, Arg3], _} = lift_list(Args, Ctxt),
    {{Op, Arg1, Arg2, Arg3}, Ctxt}.

liftR(is_record, Args, Ctxt) ->
    {[Arg1, Arg2], _} = lift_list(Args, Ctxt),
    {{is_record, Arg2, element(1, Arg1), size(Arg1)-1}, Ctxt}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% coposite types; records, maps, lists, tuples

lift_record({{atom, Mod}, {atom, Rec}, KVs}, Ctxt) ->
    Fields = get_rec_fields(Mod, Rec),
    {KVls, Binds} = lift_list(KVs, Ctxt),
    {mk_rec(Rec, KVls, Fields), Binds}.

lift_map(KVs, Ctxt) ->
    {KVls, Binds} = lift_list(KVs, Ctxt),
    {maps:from_list(KVls), Binds}.

lift_field([K, V], B0) ->
    {KL, B1} = lift(K, B0),
    {VL, B2} = lift(V, B1),
    {{KL, VL}, B2}.

lift_tuple(Es, Ctxt) ->
    {Ts, Binds} = lift_list(Es, Ctxt),
    case Ctxt#ctxt.context of
        args -> {list_to_tuple(Ts), Binds};
        guard -> {{list_to_tuple(Ts)}, Binds}
    end.

lift_list(Es, Ctxt) ->
    lift_list(Es, [], Ctxt).

%% we need to handle improper lists, i.e. when the tail is not a list
%% such as [a|b]. (the 'when not is_list' clause)

lift_list([], O, Ctxt) ->
    {lists:reverse(O), Ctxt};
lift_list(E, O, Ctxt) when not is_list(E) ->
    {LE, Binds} = lift(E, Ctxt),
    {lists:foldl(fun(H, T) -> [H|T] end, LE, O), Binds};
lift_list([E|Es], O, Ctxt) ->
    {LE, Binds} = lift(E, Ctxt),
    lift_list(Es, [LE|O], Binds).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% variables. if we see a new var, we can either create a new binding
%% (while unpacking arg list), or crash with `unknown variable'
%% (while unpacking guards)

lift_var(Var, Ctxt) ->
    case get_binding(Var, Ctxt, nil) of
        nil -> mk_binding(Var, Ctxt);
        Binding -> {Binding, Ctxt}
    end.

%% ctxt. can be args (lifting args) or guard (lifting guard)

mk_binding(Var, #ctxt{context=guard}) ->
    die("unbound variable", list_to_atom(Var));
mk_binding(Var, Ctxt) ->
    #ctxt{count=Count, bindings=Bs} = Ctxt,
    Binding = list_to_atom("$"++integer_to_list(Count+1)),
    {Binding, Ctxt#ctxt{count=Count+1, bindings=[{Var, Binding}|Bs]}}.

get_binding(Var, Ctxt, Def) ->
    proplists:get_value(Var, Ctxt#ctxt.bindings, Def).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% records. records are identified by the {module name, record name} tuple.
%% we get the record info (i.e. the list of field names) from the beam file.

get_rec_fields(Mod, Rec) ->
    get_fields(Rec, get_dbgi(get_filename(Mod))).

get_filename(Mod) ->
    case code:which(Mod) of
        non_existing -> die("no such module", Mod);
        Filename -> Filename
    end.

get_dbgi(Filename) ->
    try
        {ok, {_, [{_, DbgiB}]}} = beam_lib:chunks(Filename, ["Dbgi"]),
        {debug_info_v1, _, {Dbgi, _}} = binary_to_term(DbgiB),
        Dbgi
    catch
        _:_ -> die("no debug info", Filename)
    end.

get_fields(Rec, Dbgi) ->
    case [Fs || {attribute,_,record,{R, Fs}} <- Dbgi, R == Rec] of
        [] -> die("no such record", Rec);
        [Fs] -> lists:map(fun get_field/1, Fs)
    end.

%% there are 4 kinds of record field info; typed/untyped and initialized/not

get_field({typed_record_field, RecordField, _}) -> get_field(RecordField);
get_field({record_field, _, {atom, _, F}}) -> F;
get_field({record_field, _, {atom, _, F}, _}) -> F.

mk_rec(Rec, KVs, Fields) ->
    Empty = setelement(1, erlang:make_tuple(length(Fields)+1, '_'), Rec),
    fill_tuple(Empty, KVs, Fields).

fill_tuple(Tuple, [], _) ->
    Tuple;
fill_tuple(Tuple, [{K, V}|KVs], Fields) ->
    fill_tuple(setelement(index(K, Fields)+1, Tuple, V), KVs, Fields).

index(K, []) -> die("no such field", K);
index(K, [K|_]) -> 1;
index(K, [_|Ks]) -> 1+index(K, Ks).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% problem handler
die(Str, Term) ->
    exit({gen_error, lists:flatten(io_lib:format(Str++": ~p", [Term]))}).
