-module(redbug_compiler).
-export([string/1]).

-define(SPAWN(Expr),
        begin
            {__P, __R} = spawn_monitor(fun() -> exit(Expr) end),
            receive {'DOWN', __R, process, __P, __Ret} -> __Ret end
        end).

string(Str) ->
    parse(to_str(Str)).

parse(Str) ->
    case ?SPAWN(redbug_parser:parse(scan(Str))) of
        {ok, Tree} -> Tree;
        {error, {_, _, Error}} -> exit({parse_error, lists:flatten(Error)});
        Error -> exit(Error)
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

