Definitions.

%whitespace
WS = [\000-\s]

% separators
S = ->|when|\(|\)|\[|\]|{|}|;|:|#|,|:=|=|_|#{

% types
T = atom|float|integer|list|number|pid|port|reference|tuple|map|binary|function

% comparison operators
C = >|>=|<|=<|=:=|==|=/=|/=

% arithmetic operators
A = \+|-|\*|div|rem|band|bor|bxor|bnot|bsl|bsr

% boolean operators
B = and|or|andalso|orelse|xor

% BIFs
F = abs|element|hd|length|node|round|size|tl|trunc

Rules.

{WS}+ :
  skip_token.

({S}) :
  {token, {to_atom(TokenChars), TokenLine}}.

is_({T}) :
  {token, {'type_test1', TokenLine, to_atom(TokenChars)}}.

is_record :
  {token, {'type_test2', TokenLine, to_atom(TokenChars)}}.

self :
  {token, {'bif0', TokenLine, to_atom(TokenChars)}}.

({F}) :
  {token, {'bif1', TokenLine, to_atom(TokenChars)}}.

({C}) :
  {token, {'comparison_op', TokenLine, to_atom(TokenChars)}}.

({A}) :
  {token, {'arithmetic_op', TokenLine, to_atom(TokenChars)}}.

(not) :
  {token, {'boolean_op1', TokenLine, to_atom(TokenChars)}}.

({B}) :
  {token, {'boolean_op2', TokenLine, to_atom(TokenChars)}}.

[0-9]+ :
  {token, {'int', TokenLine, to_int(TokenChars)}}.

[A-Z_][A-Za-z0-9_]* :
  {token, {'variable', TokenLine, TokenChars}}.

"([^"]|\\")*" :
  {token, {'string', TokenLine, trim(TokenChars)}}.

[a-z][A-Z0-9a-z_]* :
  {token, {'atom', TokenLine, to_atom(TokenChars)}}.

'([^'|\\'])*' :
  {token, {'atom', TokenLine, to_atom(trim(TokenChars))}}.

<<("([^"]|\\")*"|[^>]*)*>> :
  {token, {'bin', TokenLine, to_binary(TokenChars)}}.

Erlang code.

to_atom(Str) ->
    list_to_atom(Str).

to_int(Str) ->
    list_to_integer(Str).

to_binary(Str) ->
    case erl_eval:eval_str(Str++". ") of
        {ok, Bin} -> Bin;
        {error, _} -> throw({error, {0, ?MODULE, {malformed_binary, Str}}, 0})
    end.

trim(Str) ->
    lists:reverse(tl(lists:reverse(tl(Str)))).
