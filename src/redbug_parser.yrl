Nonterminals
  rtp
  mfa module function args arity
  terms term list tuple
  record map record_fields record_field map_fields map_field
  guards guard guard_value test
  actions.

Terminals
  '(' ')' '[' ']' '{' '}'
  '->' 'when' ':' ';' '#' ',' '=' ':=' '=>' '#{' '/' '|' '++'
  'variable' 'bin' 'int' 'atom' 'string'
  'comparison_op' 'arithmetic_op' 'boolean_op1' 'boolean_op2'
  'type_test1' 'type_test2' 'bif0' 'bif1' 'bif2'.

Rootsymbol rtp.

Left  4 arithmetic_op.
Left  3 comparison_op.
Right 2 boolean_op1.
Left  1 boolean_op2.

rtp -> mfa                                              : {'$1', '_', '_'}.
rtp -> mfa '->' actions                                 : {'$1', '_', '$3'}.
rtp -> mfa 'when' guards                                : {'$1', '$3', '_'}.
rtp -> mfa 'when' guards '->' actions                   : {'$1', '$3', '$5'}.

mfa -> module                                           : {'$1', '_', '_'}.
mfa -> module ':' function                              : {'$1', '$3', '_'}.
mfa -> module ':' function '/' arity                    : {'$1', '$3', '$5'}.
mfa -> module ':' function '(' args ')'                 : {'$1', '$3', '$5'}.

module -> 'atom'                                        : '$1'.

function -> 'atom'                                      : '$1'.
function -> 'variable'                                  : '$1'.

args -> terms                                           : '$1'.

arity -> 'int'                                          : '$1'.

terms -> '$empty'                                       : [].
terms -> term                                           : ['$1'].
terms -> terms ',' term                                 : '$1' ++ ['$3'].

term -> 'variable'                                      : '$1'.
term -> 'bin'                                           : '$1'.
term -> 'int'                                           : '$1'.
term -> 'atom'                                          : '$1'.
term -> list                                            : {list, '$1'}.
term -> tuple                                           : {tuple, '$1'}.
term -> record                                          : {record, '$1'}.
term -> map                                             : {map, '$1'}.

list -> 'string'                                        : [{int, 0, I} || I <- element(3, '$1')].
list -> '[' terms ']'                                   : '$2'.
list -> '[' terms '|' term ']'                          : mk_cons('$2', '$4').
list -> list '++' 'variable'                            : mk_cons('$1', '$3').
list -> list '++' list                                  : mk_cons('$1', '$3').

tuple -> '{' terms '}'                                  : '$2'.

record -> 'atom' '#' 'atom'                             : {'$1', '$3', []}.
record -> 'atom' '#' 'atom' '{' record_fields '}'       : {'$1', '$3', '$5'}.

record_fields -> '$empty'                               : [].
record_fields -> record_field                           : ['$1'].
record_fields -> record_fields ',' record_field         : '$1' ++ ['$3'].

record_field -> 'atom' '=' term                         : {field, ['$1', '$3']}.

map -> '#{' map_fields '}'                              : '$2'.

map_fields -> '$empty'                                  : [].
map_fields -> map_field                                 : ['$1'].
map_fields -> map_fields ',' map_field                  : '$1' ++ ['$3'].

map_field -> term ':=' term                             : {field, ['$1', '$3']}.
map_field -> term '=>' term                             : {field, ['$1', '$3']}.

guards -> '(' guards ')'                                : '$2'.
guards -> guard                                         : '$1'.
guards -> guards ',' guard                              : {{boolean_op2, 0, 'andalso'}, ['$1', '$3']}.
guards -> guards ';' guard                              : {{boolean_op2, 0, 'orelse'}, ['$1', '$3']}.
guards -> 'boolean_op1' guard                           : {'$1', ['$2']}.
guards -> guards 'boolean_op2' guard                    : {'$2', ['$1', '$3']}.

guard -> '(' guard ')'                                  : '$2'.
guard -> test                                           : '$1'.
guard -> 'boolean_op1' test                             : {'$1', ['$2']}.
guard -> test 'boolean_op2' test                        : {'$2', ['$1', '$3']}.

test -> '(' test ')'                                    : '$2'.
test -> 'type_test1' '(' 'variable' ')'                 : {'$1', ['$3']}.
test -> 'type_test2' '(' 'variable' ',' 'variable' ')'  : {'$1', ['$3', '$5']}.
test -> 'type_test2' '(' 'atom' ',' 'variable' ')'      : {'$1', ['$3', '$5']}.
test -> guard_value 'comparison_op' guard_value         : {'$2', ['$1', '$3']}.

guard_value -> '(' guard_value ')'                        : '$2'.
guard_value -> term                                       : '$1'.
guard_value -> 'bif0' '(' ')'                             : {'$1', []}.
guard_value -> 'bif1' '(' guard_value ')'                 : {'$1', ['$3']}.
guard_value -> 'bif2' '(' guard_value ',' guard_value ')' : {'$1', ['$3', '$5']}.
guard_value -> guard_value 'arithmetic_op' guard_value    : {'$2', ['$1', '$3']}.

actions -> 'atom'                                       : ['$1'].
actions -> actions ',' 'atom'                           : '$1' ++ ['$3'].
actions -> actions ';' 'atom'                           : '$1' ++ ['$3'].

Erlang code.

mk_cons(H, T) -> lists:foldr(fun(E,O) -> [E|O] end, T, H).
