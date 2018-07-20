-module(redbug_compiler).
-export([parse/1]).

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
        {'EXIT', _} -> exit({scan_error, bad_input})
    end.
