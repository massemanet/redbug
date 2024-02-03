Header "%% @hidden".

Nonterminals
  rtp
  mfa module function args arity
  terms term list tuple var
  atomic
  record map record_fields record_field map_fields map_field
  guards guard guard_value test
  actions action.

Terminals
  '(' ')' '[' ']' '{' '}'
  '->' 'when' ':' ';' '#' ',' '=' ':=' '=>' '#{' '/' '|' '++'
  'pid' 'ref' 'port'
  'wildcard' 'variable' 'bin' 'int' 'atom' 'string'
  'comparison_op' 'arithmetic_op' 'boolean_op1' 'boolean_op2'
  'type_test1' 'type_isrec' 'bif0' 'bif1' 'bif2'.

Rootsymbol rtp.

Nonassoc 100 '('.
Nonassoc 100 ')'.
Left      50 arithmetic_op.
Left      40 comparison_op.
Right     30 boolean_op1.
Left      20 boolean_op2.
Left      10 '++'.

rtp -> mfa                            : {'$1', '_', '_'}.
rtp -> mfa '->' actions               : {'$1', '_', '$3'}.
rtp -> mfa 'when' guards              : {'$1', '$3', '_'}.
rtp -> mfa 'when' guards '->' actions : {'$1', '$3', '$5'}.

mfa -> module                            : {'$1', '_', '_'}.
mfa -> module ':' function               : {'$1', '$3', '_'}.
mfa -> module ':' function '/' arity     : {'$1', '$3', '$5'}.
mfa -> module ':' function '(' args ')'  : {'$1', '$3', '$5'}.

module -> var    : {module, '_'}.
module -> atomic : class(module, '$1').

function -> var  : {function, '_'}.
function -> atomic : class(function, '$1').

atomic -> 'atom'       : e13('$1').
atomic -> 'bif0'       : class(atom, '$1').
atomic -> 'bif1'       : class(atom, '$1').
atomic -> 'bif2'       : class(atom, '$1').
atomic -> 'type_test1' : class(atom, '$1').
atomic -> 'type_isrec' : class(atom, '$1').

args -> terms : '$1'.

arity -> 'int' : class(arity, '$1').

terms -> '$empty'       : [].
terms -> term           : ['$1'].
terms -> terms ',' term : '$1' ++ ['$3'].

term -> 'bin'  : e13('$1').
term -> 'int'  : e13('$1').
term -> 'pid'  : e13('$1').
term -> 'ref'  : e13('$1').
term -> 'port' : e13('$1').
term -> var    : '$1'.
term -> atomic : '$1'.
term -> list   : {list, '$1'}.
term -> tuple  : {tuple, '$1'}.
term -> record : {record, '$1'}.
term -> map    : {map, '$1'}.

list -> 'string'               : [{'int', I} || I <- e3('$1')].
list -> '[' terms ']'          : '$2'.
list -> '[' terms '|' term ']' : mk_cons('$2', '$4').
list -> list '++' var          : mk_cons('$1', '$3').
list -> list '++' list         : mk_cons('$1', '$3').

tuple -> '{' terms '}' : '$2'.

record -> atomic '#' atomic                       : {'$1', '$3', []}.
record -> atomic '#' atomic '{' record_fields '}' : {'$1', '$3', '$5'}.

record_fields -> '$empty'                       : [].
record_fields -> record_field                   : ['$1'].
record_fields -> record_fields ',' record_field : '$1' ++ ['$3'].

record_field -> atomic '=' term : {field, ['$1', '$3']}.

map -> '#{' map_fields '}' : '$2'.

map_fields -> '$empty'                 : [].
map_fields -> map_field                : ['$1'].
map_fields -> map_fields ',' map_field : '$1' ++ ['$3'].

map_field -> term ':=' term : {field, ['$1', '$3']}.
map_field -> term '=>' term : {field, ['$1', '$3']}.

guards -> '(' guards ')'             : '$2'.
guards -> guard                      : '$1'.
guards -> guards ',' guard           : {{boolean_op2, 'andalso'}, ['$1', '$3']}.
guards -> guards ';' guard           : {{boolean_op2, 'orelse'}, ['$1', '$3']}.
guards -> 'boolean_op1' guard        : {e13('$1'), ['$2']}.
guards -> guards 'boolean_op2' guard : {e13('$2'), ['$1', '$3']}.

guard -> '(' guard ')'           : '$2'.
guard -> 'boolean_op1' test      : {e13('$1'), ['$2']}.
guard -> test 'boolean_op2' test : {e13('$2'), ['$1', '$3']}.
guard -> test                    : '$1'.

test -> '(' test ')'                            : '$2'.
test -> 'type_test1' '(' var ')'                : {e13('$1'), ['$3']}.
test -> 'type_isrec' '(' record ',' var ')'     : {e13('$1'), [{record, '$3'}, '$5']}.
test -> guard_value 'comparison_op' guard_value : {e13('$2'), ['$1', '$3']}.
test -> guard_value                             : '$1'.

var -> 'variable' : class(var, '$1').
var -> 'wildcard' : class(var, '$1').

guard_value -> '(' guard_value ')'                        : '$2'.
guard_value -> 'bif0' '(' ')'                             : {e13('$1'), []}.
guard_value -> 'bif1' '(' guard_value ')'                 : {e13('$1'), ['$3']}.
guard_value -> 'bif2' '(' guard_value ',' guard_value ')' : {e13('$1'), ['$3', '$5']}.
guard_value -> guard_value 'arithmetic_op' guard_value    : {e13('$2'), ['$1', '$3']}.
guard_value -> term                                       : '$1'.

actions -> action             : ['$1'].
actions -> actions ',' action : '$1' ++ ['$3'].
actions -> actions ';' action : '$1' ++ ['$3'].

action -> 'atom' : {action, e3('$1')}.

Erlang code.

e3({_, _, E}) -> E.

e13({C, _, E}) -> {C, E}.

class(Class, {_, E}) -> {Class, E};
class(Class, {_, _, E}) -> {Class, E}.

mk_cons(H, T) -> lists:foldr(fun(E,O) -> [E|O] end, T, H).
