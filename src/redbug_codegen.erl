-module(redbug_codegen).

-export([generate/1]).

-record(bindings,
        {mutable=true,
         count=0,
         bs=[]}).

generate({MFA, G, As}) ->
    {M, F, Arity, Args, Bindings, Flags0} = mk_mfa(MFA),
    {Guard, Flags1} = mk_guard(G, Bindings, Flags0),
    {Actions, Flags2} = mk_actions(As, Flags1),
    {{M, F, Arity}, [{Args, Guard, Actions}], Flags2}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% module, function, arity, arguments

mk_mfa({M, F, As}) ->
    Mod = mk_mod(M),
    {Fun, Flags} = mk_fun(F),
    {Arity, Args, Bindings} = mk_args(As, Fun),
    {Mod, Fun, Arity, Args, Bindings, Flags}.

mk_mod({'atom', _, Mod}) -> Mod.

mk_fun('_') ->
    {'_', [local]};
mk_fun({'variable', _, _}) ->
    {'_', [global]};
mk_fun({'atom', _, Fun}) ->
    {Fun, [local]}.

mk_args('_', _) ->
    {'_', '_', #bindings{}};
mk_args({int, _, Arity}, _) ->
    {Arity, '_', #bindings{}};
mk_args(As, Fun) ->
    {Args, Bindings} = lift_list(As, #bindings{}),
    case Fun of
        '_' -> {'_', Args, Bindings};
        _ -> {length(Args), Args, Bindings}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% guards

mk_guard('_', _, Flags) ->
    {[], Flags};
mk_guard(G, Bindings, Flags) ->
    {Guard, _} = lift(G, Bindings#bindings{mutable=false}),
    {[Guard], Flags}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% actions

mk_actions('_', Flags) ->
    {[], Flags};
mk_actions(As, Flags) ->
    lists:foldr(fun chk_action/2, {[], Flags}, As).

chk_action({atom, _, stack}, {As, Fs})  -> {[{message,{process_dump}}|As], Fs};
chk_action({atom, _, return}, {As, Fs}) -> {[{exception_trace}|As], Fs};
chk_action({atom, _, time}, {As, Fs})   -> {As, [call_time|Fs]};
chk_action({atom, _, count}, {As, Fs})  -> {As, [call_count|Fs]};
chk_action({atom, _, Act}, _)           -> die("illegal action", Act).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% make concrete values

%% values
lift({atom, _, Value}, Bindings) -> {Value, Bindings};
lift({int, _, Value}, Bindings)  -> {Value, Bindings};
lift({bin, _, Value}, Bindings)  -> {Value, Bindings};
%% variables
lift({variable, _, "_"}, Bindings) -> {'_', Bindings};
lift({variable, L, Var}, Bindings) -> lift_var(Var, L, Bindings);
%% composite terms
lift({tuple, Es}, Bindings)   -> lift_tuple(Es, Bindings);
lift({list, Es}, Bindings)    -> lift_list(Es, Bindings);
lift({map, KVs}, Bindings)    -> lift_map(KVs, Bindings);
lift({field, KV}, Bindings)   -> lift_field(KV, Bindings);
lift({record, Rec}, Bindings) -> lift_record(Rec, Bindings);
%% operators and functions
lift({{comparison_op, _, Op}, Args}, Bindings) -> lift2(Op, Args, Bindings);
lift({{arithmetic_op, _, Op}, Args}, Bindings) -> lift2(Op, Args, Bindings);
lift({{boolean_op1, _, Op}, Args}, Bindings)   -> lift1(Op, Args, Bindings);
lift({{boolean_op2, _, Op}, Args}, Bindings)   -> lift2(Op, Args, Bindings);
lift({{type_test1, _, Test}, Args}, Bindings)  -> lift1(Test, Args, Bindings);
lift({{type_test2, _, Test}, Args}, Bindings)  -> lift2(Test, Args, Bindings);
lift({{bif1, _, Bif}, Args}, Bindings)         -> lift1(Bif, Args, Bindings);
lift({{bif2, _, Bif}, Args}, Bindings)         -> lift2(Bif, Args, Bindings);
lift({{bif3, _, Bif}, Args}, Bindings)         -> lift3(Bif, Args, Bindings).

%% different arity functions
lift1(Op, Args, Bindings) ->
    {[Arg1], _} = lift_list(Args, Bindings),
    {{Op, Arg1}, Bindings}.

lift2(Op, Args, Bindings) ->
    {[Arg1, Arg2], _} = lift_list(Args, Bindings),
    {{Op, Arg1, Arg2}, Bindings}.

lift3(Op, Args, Bindings) ->
    {[Arg1, Arg2, Arg3], _} = lift_list(Args, Bindings),
    {{Op, Arg1, Arg2, Arg3}, Bindings}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% container types; records, maps, lists, tuples

lift_record({{atom, _, Mod}, {atom, _, Rec}, KVs}, Bindings) ->
    Fields = get_rec_fields(Mod, Rec),
    {KVls, Binds} = lift_list(KVs, Bindings),
    {mk_rec(Rec, KVls, Fields), Binds}.

lift_map(KVs, Bindings) ->
    {KVls, Binds} = lift_list(KVs, Bindings),
    {maps:from_list(KVls), Binds}.

lift_field([K, V], B0) ->
    {KL, B1} = lift(K, B0),
    {VL, B2} = lift(V, B1),
    {{KL, VL}, B2}.

lift_tuple(Es, Bindings) ->
    {Ts, Binds} = lift_list(Es, Bindings),
    case Bindings#bindings.mutable of
        true -> {list_to_tuple(Ts), Binds};
        false -> {{list_to_tuple(Ts)}, Binds}
    end.

lift_list(Es, Bindings) ->
    lift_list(Es, [], Bindings).

%% we need to handle improper lists, i.e. when the tail is not a list
%% such as [a|b]

lift_list([], O, Bindings) ->
    {lists:reverse(O), Bindings};
lift_list(E, O, Bindings) when not is_list(E) ->
    {LE, Binds} = lift(E, Bindings),
    {lists:foldl(fun(H, T) -> [H|T] end, LE, O), Binds};
lift_list([E|Es], O, Bindings) ->
    {LE, Binds} = lift(E, Bindings),
    lift_list(Es, [LE|O], Binds).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% variables. if we see a new var, we can either create a new binding
%% (while unpacking arg list), or crash with `unknown variable'
%% (while unpacking guards)

lift_var(Var, L, Bindings) ->
    case get_binding(Var, Bindings, nil) of
        nil -> mk_binding(Var, L, Bindings);
        Binding -> {Binding, Bindings}
    end.

%% bindings. can be fixed or mutable

mk_binding(Var, _, #bindings{mutable=false}) ->
    die("unbound variable", list_to_atom(Var));
mk_binding(Var, _, Bindings) ->
    #bindings{count=Count, bs=Bs} = Bindings,
    Binding = list_to_atom("$"++integer_to_list(Count+1)),
    {Binding, Bindings#bindings{count=Count+1, bs=[{Var, Binding}|Bs]}}.

get_binding(Var, Bindings, Def) ->
    proplists:get_value(Var, Bindings#bindings.bs, Def).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% records. records are identified by the {module name, record name} tuple.
%% we get the record info (i.e. the list of field names) from the beam file.

get_rec_fields(Mod, Rec) ->
    get_fields(Rec, get_dbgi(get_filename(Mod))).

get_filename(Mod) ->
    case code:which(Mod) of
        non_existing -> die("unknown module", Mod);
        Filename -> Filename
    end.

get_dbgi(Filename) ->
    try
        {ok, {_, [{_, DbgiB}]}} = beam_lib:chunks(Filename, ["Dbgi"]),
        {debug_info_v1, _, {Dbgi, _}} = binary_to_term(DbgiB),
        Dbgi
    catch
        _:_ -> die("no_debug_info", Filename)
    end.

get_fields(Rec, Dbgi) ->
    case [Fs || {attribute,_,record,{R, Fs}} <- Dbgi, R == Rec] of
        [] -> die("no such record", Rec);
        [Fs] -> [F || {_, {_, _, {atom, _, F}}, _} <- Fs]
    end.

mk_rec(Rec, KVs, Fields) ->
    Empty = setelement(1, erlang:make_tuple(length(Fields)+1, '_'), Rec),
    fill_tuple(Empty, KVs, Fields).

fill_tuple(Tuple, [], _) -> Tuple;
fill_tuple(Tuple, [{K, V}|KVs], Fields) ->
    fill_tuple(setelement(index(K, Fields)+1, Tuple, V), KVs, Fields).

index(K, []) -> die("no_such_field", K);
index(K, [K|_]) -> 1;
index(K, [_|Ks]) -> 1+index(K, Ks).

%% problem handler
die(Str, Term) ->
    exit({gen_error, lists:flatten(io_lib:format(Str++": ~p", [Term]))}).
