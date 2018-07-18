Definitions.

WS = [\000-\s]
T = atom|float|integer|list|number|pid|port|reference|tuple|map|binary|function

Rules.

{WS}+ :
  skip_token.

(->|when|\(|\)|\[|\]|{|}|;|:|#|,|=|_|#{) :
  {token, {to_atom(TokenChars), TokenLine}}.

is_({T}) :
  {token, {'type_test1', TokenLine, to_atom(TokenChars)}}.

is_record :
  {token, {'type_test2', TokenLine, to_atom(TokenChars)}}.

self :
  {token, {'bif0', TokenLine, to_atom(TokenChars)}}.

(abs|element|hd|length|node|round|size|tl|trunc) :
  {token, {'bif1', TokenLine, to_atom(TokenChars)}}.

(>|>=|<|=<|=:=|==|=/=|/=) :
  {token, {'comparison_op', TokenLine, to_atom(TokenChars)}}.

(\+|-|\*|div|rem|band|bor|bxor|bnot|bsl|bsr) :
  {token, {'arithmetic_op', TokenLine, to_atom(TokenChars)}}.

(not) :
  {token, {'boolean_op1', TokenLine, to_atom(TokenChars)}}.

(and|or|andalso|orelse|xor) :
  {token, {'boolean_op2', TokenLine, to_atom(TokenChars)}}.

[0-9]+ :
  {token, {'int', TokenLine, to_int(TokenChars)}}.

[A-Z_][A-Za-z0-9_]* :
  {token, {'variable', TokenLine, TokenChars}}.

"([^"]|\\")*" :
  {token, {'string', TokenLine, trim(1, TokenChars)}}.

[a-z][A-Z0-9a-z_]* :
  {token, {'atom', TokenLine, to_atom(TokenChars)}}.

'([^'|\\'])*' :
  {token, {'atom', TokenLine, to_atom(trim(1, TokenChars))}}.

<<"([^"]|\\")*">> :
  {token, {'bin', TokenLine, TokenChars}}.

Erlang code.

to_atom(Str) ->
    list_to_atom(Str).

to_int(Str) ->
    list_to_integer(Str).

trim(N, S) ->
    lists:reverse(lists:nthtail(N, lists:reverse(lists:nthtail(N, S)))).
