Nonterminals
  rtp
  mfa module function args terms term list tuple
  record map record_fields record_field map_fields map_field
  guards guard guard_value test
  actions.

Terminals
  '->' 'when' '(' ')' '[' ']' '{' '}' ':' ';' '#' ',' '=' ':=' '_' '#{'
  'variable' 'bin' 'float' 'int' 'atom' 'string'
  'comparison_op' 'arithmetic_op' 'boolean_op'
  'type_test1' 'type_test2' 'bif0' 'bif1'.

Rootsymbol rtp.

Left 1 boolean_op.
Left 2 comparison_op.
Left 3 arithmetic_op.

rtp -> mfa                            : {'$1', '_', '_'}.
rtp -> mfa '->' actions               : {'$1', '_', '$3'}.
rtp -> mfa 'when' guards              : {'$1', '$3', '_'}.
rtp -> mfa 'when' guards '->' actions : {'$1', '$3', '$5'}.

mfa -> module                           : {call, '$1', '_', '_'}.
mfa -> module ':' function              : {call, '$1', '$3', '_'}.
mfa -> module ':' function '(' args ')' : {call, '$1', '$3', '$5'}.

module -> 'atom' : '$1'.

function -> 'atom' : '$1'.
function -> '_'    : '$1'.

args -> terms : '$1'.

terms -> '$empty'       : [].
terms -> term           : ['$1'].
terms -> terms ',' term : '$1' ++ ['$3'].

term -> '_'        : '$1'.
term -> 'variable' : '$1'.
term -> 'bin'      : '$1'.
term -> 'float'    : '$1'.
term -> 'int'      : '$1'.
term -> 'atom'     : '$1'.
term -> 'string'   : '$1'.
term -> list       : '$1'.
term -> tuple      : '$1'.
term -> record     : '$1'.
term -> map        : '$1'.

list -> '[' terms ']' : {list, '$2'}.

tuple -> '{' terms '}' : {tuple, '$2'}.

record -> 'atom' '#' 'atom'                       : {record, '$1', '$3', []}.
record -> 'atom' '#' 'atom' '{' record_fields '}' : {record, '$1', '$3', '$5'}.

record_fields -> '$empty'                       : [].
record_fields -> record_field                   : ['$1'].
record_fields -> record_fields ',' record_field : '$1' ++ ['$3'].

record_field -> 'atom' '=' term : {'$1', '$3'}.

map -> '#{' map_fields '}' : {map, '$2'}.

map_fields -> '$empty'                 : [].
map_fields -> map_field                : ['$1'].
map_fields -> map_fields ',' map_field : '$1' ++ ['$3'].

map_field -> term ':=' term : {'$1', '$3'}.

guards -> guard                     : '$1'.
guards -> guards ',' guard          : {'andalso', ['$1', '$3']}.
guards -> guards ';' guard          : {'orelse', ['$1', '$3']}.
guards -> guards 'boolean_op' guard : {'$2', ['$1', '$3']}.

guard -> test                   : '$1'.
guard -> test 'boolean_op' test : {'$2', ['$1', '$3']}.

test -> type_test1 '(' 'variable' ')'                : {'$1', ['$3']}.
test -> type_test2 '(' 'variable' ',' 'variable' ')' : {'$1', ['$3', '$5']}.
test -> type_test2 '(' 'atom' ',' 'variable' ')'     : {'$1', ['$3', '$5']}.
test -> guard_value 'comparison_op' guard_value      : {'$2', ['$1', '$3']}.

guard_value -> 'variable'                              : '$1'.
guard_value -> bif0 '(' ')'                            : {'$1', []}.
guard_value -> bif1 '(' guard_value ')'                : {'$1', ['$3']}.
guard_value -> guard_value 'arithmetic_op' guard_value : {'$2', ['$1', '$3']}.

actions -> 'atom'             : {actions, ['$1']}. % return, stack
actions -> actions ',' 'atom' : {actions, '$1' ++ ['$3']}. % return, stack
actions -> actions ';' 'atom' : {actions, '$1' ++ ['$3']}. % return, stack
