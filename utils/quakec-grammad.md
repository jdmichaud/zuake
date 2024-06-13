## QuakeC Grammar

`{ something }` means repetition
`[ something ]` means optionality
`something | somethingelse` means a choice

```
program = <top_level> { <program> }
top_level =
  <function_declaration>
  | <variable_definition>
  | <field_definition>
  | <function_definition>
function_declaration = <type> '(' <parameters_list> ')' <identifier> ';'
function_definition = <type> '(' <parameters_list> ')' <identifier> '=' [ frame_specifier ] builtin_immediate | statement ';'
variable_definition = <type> <identifier> [ '=' <immediate> ] { ',' <type> <identifier> [ = <immediate> ] } ';'
field_definition = '.'<type> <identifier> [ '=' <immediate> ] { ',' '.'<type> <identifier> [ = <immediate> ] } ';'
immediate = <literal> | <statement>
literal = float_literal | vector_literal | string_literal
statement =
  '{' statement { statement } '}'
  | <variable_definition> ';'
  | 'local' <variable_definition> ';'
  | 'return' <expression> ';'
  | 'if' ( <expression> ) <statement> [ 'else' <statement> ]
  | 'while' '(' <expression> ')' <statement>
  | 'do' <statement> 'while' '(' <expression> ')'
  | <expression> ';'
expression =
  <identifier>
  | <frame_identifier>
  | <literal>
  | <unary_expression>
  | <update_expression>
  | <binary_expression>
  | <conditional_expression>
  | <assignment_expression>
  | <field_expression>
  | <funcall_expression>
  | <subscript_expression>
  | <parenthesized_expression>
unary_expression = ('-' | '+' | '~' | '!') <expression>
update_expression = <expression> ('--' | '++')
binary_expression =
 <expression> ('+' | '-' | '*' | '/' | '%' | '||' | '&&' | '|' | '&' | '==' | '!=' | '>' | '>=' | '<=' | '<') <expression>
conditional_expression = <expression> '?' <expression> ':' <expression>
assignment_expression =
  (<identifier> | <field_expression>)
  '='
  ('=' | '+=' | '-=' | '*=' | '/=' | '|=' | '&=' | '&~=' | '%=' | '^=')
  <expression>
funcall_expression = <identifier> '(' <expression> { ',' <expression> } ')'
field_expression = <identifier> '.' <identifier>
subscript_expression = <identifier> '[' <expression> ']'
parenthesized_expression = '(' <expression> { ',' <expression> } ')'
frame_specifier = '[' <frame_identifier> | <frame_literal> ',' <identifier> ']'
identifier: /[a-zA-Z_][a-zA-Z0-9_]*/
frame_identifier = '$' /[a-zA-Z_][a-zA-Z0-9_.]*/
frame_immediate = '$' /[0-9]+/
builtin_immediate = /#[0-9]+/
float_literal = /-?([0-9]+)?\.([0-9]+f?)?/
vector_literal = '\'' <float_literal> <float_literal> <float_literal> '\''
string_literal = '"' '^"' { '^"' } '"'
```