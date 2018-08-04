-module(redbug_compiler).
-export([compile/1, parse/1, scan/1]).

compile(X) ->
    try do_compile(parse(to_str(X)))
    catch
        throw:R -> exit({compiler_error, R});
        exit:R -> exit(R)
    end.

parse(Str) ->
    case catch redbug_parser:parse(scan(Str)) of
        {ok, Tree} -> Tree;
        {error, {_, _, Error}} -> exit({parse_error, lists:flatten(Error)});
        {'EXIT', Error} -> exit(Error)
    end.

scan(Str) ->
    case catch redbug_lexer:string(Str) of
        {ok, Tokens, _} -> Tokens;
        {error, {_, _, Error}, _} -> exit({scan_error, Error});
        {'EXIT', R} -> exit({scan_error, bad_input, R})
    end.

to_str(Atom) when is_atom(Atom) -> atom_to_list(Atom);
to_str(Bin) when is_binary(Bin) -> binary_to_list(Bin);
to_str(List) when is_list(List) -> List;
to_str(_) -> exit(not_string).


do_compile({MFA, Guard, Actions}) ->
    {MFA, Guard, Actions}.

%% mk_tuple(List) -> list_to_tuple(List).

%% mk_map(KVs) -> maps:from_list(KVs).

%% mk_record(Mod, Rec, KVs) -> {record, {Mod, Rec, KVs}}.

%% chk_action({atom, _, stack})  -> {action, {message,{process_dump}}};
%% chk_action({atom, _, return}) -> {action, exception_trace};
%% chk_action({atom, _, time})   -> {flag, call_time};
%% chk_action({atom, _, count})  -> {flag,call_count};
%% chk_action({atom, L, Act})    -> return_error(L, io_lib:format("illegal action; ~p", [Act])).

%% lift({'variable', _, "_"} -> '_';
%% lift({'variable', L, Var}) -> lift_var(Var, L);
%% lift({_, _, Value})        -> Value;
%% lift({Token, _})           -> Token.

%% lift_var(false, Var, _) -> Var, list_to_atom("$"++integer_to_list(bumpv(count))));
%% lift_var(true, Var, L) -> return_error(L, io_lib:format("unbound variable; ~p", [Var])).
