-module(redbug_compiler).
-export([parse/1]).

parse(Str) ->
    case catch redbug_parser:parse(scan(Str)) of
        {ok, Tree} -> Tree;
        {error, {_, _, Error}} -> exit({parse_error, lists:flatten(Error)});
        {'EXIT', Error} -> exit(Error)
    end.

scan(Str) ->
    case catch redbug_lexer:string(to_str(Str)) of
        {ok, Tokens, _} -> Tokens;
        {error, {_, _, Error}, _} -> exit({scan_error, Error});
        {'EXIT', R} -> exit({scan_error, bad_input, R})
    end.

to_str(Atom) when is_atom(Atom) -> atom_to_list(Atom);
to_str(Bin) when is_binary(Bin) -> binary_to_list(Bin);
to_str(List) when is_list(List) -> List;
to_str(_) -> exit(not_string).

