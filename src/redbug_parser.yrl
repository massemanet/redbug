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

rtp -> mfa. % : .
rtp -> mfa '->' actions. % : .
rtp -> mfa 'when' guards. % : .
rtp -> mfa 'when' guards '->' actions. % : .

mfa -> module. % : .
mfa -> module ':' function. % : .
mfa -> module ':' function '(' args ')'. % : .

module -> 'atom'. % : .

function -> 'atom'. % : .
function -> '_'. % : .

args -> terms.

terms -> '$empty'. % : .
terms -> term. % : .
terms -> terms ',' term. % : .

term -> 'variable'. % : .
term -> 'bin'. % : .
term -> 'float'. % : .
term -> 'int'. % : .
term -> 'atom'. % : .
term -> 'string'. % : .
term -> list. % : .
term -> tuple. % : .
term -> record. % : .
term -> map. % : .

list -> '[' terms ']'. % : .

tuple -> '{' terms '}'. % : .

record -> 'atom' '#' 'atom'. % : .
record -> 'atom' '#' 'atom' '{' record_fields '}'. % : .

record_fields -> '$empty'. % : .
record_fields -> record_field. % : .
record_fields -> record_fields ',' record_field. % : .

record_field -> 'atom' '=' term. % : .

map -> '#{' map_fields '}'. % : .

map_fields -> '$empty'. % : .
map_fields -> map_field. % : .
map_fields -> map_fields ',' map_field. % : .

map_field -> term ':=' term. % : .

guards -> guard. % : .
guards -> guards ',' guard. % : .
guards -> guards ';' guard. % : .
guards -> guards 'boolean_op' guard. % : .

guard -> test. % : .
guard -> test 'boolean_op' test. % : .

test -> type_test1 '(' 'variable' ')'. % : .
test -> type_test2 '(' 'variable' ',' 'variable' ')'. % : . 
test -> type_test2 '(' 'atom' ',' 'variable' ')'. % : . 
test -> guard_value 'comparison_op' guard_value. % : .

guard_value -> 'variable'. % : .
guard_value -> bif0 '(' ')'. % : . 
guard_value -> bif1 '(' guard_value ')'. % : . 
guard_value -> guard_value 'arithmetic_op' guard_value. % : .

actions -> 'atom'            . % : . % return, stack
actions -> actions ',' 'atom'. % : . % return, stack
actions -> actions ';' 'atom'. % : . % return, stack
