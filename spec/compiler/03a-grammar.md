# Parser Grammar

This is the normative grammar for the first Go parser. Lexical productions are
defined by the token and literal specifications.

Notation:

```text
item       required item
[item]     optional item
{item}     zero or more repetitions
(a | b)    choice
"text"     literal token spelling
```

A trailing comma is accepted only where the production explicitly includes
one. Whitespace and comments are handled by the lexer.

## Files and declarations

```ebnf
file = { declaration }, EOF ;

declaration =
    import_declaration
  | binding_declaration
  | type_declaration
  | function_declaration
  | extern_declaration
  ;

import_declaration = "import", string_literal, ";" ;

binding_declaration =
    binding_keyword, identifier, [ type ], [ "=", expression ], ";"
  ;

binding_keyword = "let" | "var" ;

type_declaration =
    "type", identifier, [ type_parameters ], "=", type, ";"
  ;

type_parameters =
    "[", identifier, { ",", identifier }, [ "," ], "]"
  ;
```

The grammar accepts a binding with a type, initializer, or both. The checker
decides which combinations are legal for mutable, immutable, local, and global
bindings. An empty type-parameter list is a syntax error.

## Functions

```ebnf
function_declaration =
    "fn", function_modifiers, identifier, [ type_parameters ],
    function_signature, function_declaration_body
  ;

function_modifiers = [ "inline" ], [ string_literal ] ;

function_signature = parameter_list, type ;

parameter_list =
    "(", [ parameter_group, { ",", parameter_group }, [ "," ] ], ")"
  ;

parameter_group = [ "..." ], identifier_list, type ;
identifier_list = identifier, { ",", identifier } ;

function_declaration_body =
    block
  | "=>", expression, ";"
  ;

function_literal =
    "fn", function_modifiers, [ type_parameters ], function_signature,
    (block | "=>", expression)
  ;
```

`inline` and the optional string are syntactic modifiers only. Their validity
and calling-convention meaning are checked later. A variadic marker applies to
its parameter group; semantic checking requires a supported placement and ABI.

## External declarations

```ebnf
extern_declaration =
    "extern", [ string_literal ], (extern_item | extern_block)
  ;

extern_block = "{", { extern_item }, "}" ;

extern_item =
    "fn", identifier, extern_signature, ";"
  | "type", identifier, ";"
  | "let", identifier, type, ";"
  | "var", identifier, type, ";"
  ;

extern_signature = parameter_list, type ;
```

The optional string names the external library or linkage source for the item
or enclosing block. Extern items do not have bodies or initializers.

## Statements

```ebnf
block = "{", { statement }, "}" ;

statement =
    block
  | binding_declaration
  | return_statement
  | if_statement
  | while_statement
  | range_loop_statement
  | for_statement
  | switch_statement
  | defer_statement
  | print_statement
  | jump_statement
  | assignment_or_expression_statement
  ;

return_statement = "return", [ expression ], ";" ;

if_statement =
    "if", expression, statement, [ "else", statement ]
  ;

while_statement = "while", expression, statement ;

range_loop_statement =
    "loop", expression, (".." | "..="), expression,
    [ ":", identifier ], statement
  ;

for_statement =
    "for", [ for_initializer ], ";", [ expression ], ";",
    [ for_update ], statement
  ;

for_initializer =
    binding_keyword, identifier, [ type ], [ "=", expression ]
  | assignment
  ;

for_update = assignment | expression ;

switch_statement =
    "switch", expression, "{", { switch_case }, [ switch_else ], "}"
  ;

switch_case =
    "case", expression, { ",", expression }, ":", statement
  ;

switch_else = "else", ":", statement ;

defer_statement = "defer", statement ;

print_statement =
    "print", expression, { ",", expression }, ";"
  ;

jump_statement = ("break" | "continue"), ";" ;

assignment_or_expression_statement =
    expression, [ assignment_operator, expression ], ";"
  ;

assignment = expression, assignment_operator, expression ;

assignment_operator = "=" | "+=" | "-=" | "*=" | "/=" | "%=" ;
```

Assignments are statements, not expressions. Whether a left-hand expression is
an assignable place is semantic. Empty `for` clauses are accepted; an omitted
condition is semantically true. A non-assignment update must resolve to postfix
`++` or `--`; that restriction is semantic rather than a second expression
grammar.

## Expression precedence

From lowest to highest:

| Level | Operators | Associativity |
| --- | --- | --- |
| logical or | `||` | left |
| logical and | `&&` | left |
| bitwise or | `|` | left |
| bitwise xor | `^` | left |
| bitwise and | `&` | left |
| equality | `==`, `!=` | non-associative |
| comparison | `<`, `<=`, `>`, `>=` | non-associative |
| shift | `<<`, `>>` | left |
| additive | `+`, `-` | left |
| multiplicative | `*`, `/`, `%` | left |
| cast | `as Type` | left |
| prefix | `-`, `!`, `&`, `*`, `~` | right |
| postfix | calls, brackets, slices, members, paths, `!`, `++`, `--` | left |

Equality and comparison operators cannot be chained without explicit grouping.
`as` binds more tightly than multiplication, so `a + b as i64` means
`a + (b as i64)`.

```ebnf
expression = logical_or_expression ;

logical_or_expression =
    logical_and_expression, { "||", logical_and_expression }
  ;

logical_and_expression =
    bitwise_or_expression, { "&&", bitwise_or_expression }
  ;

bitwise_or_expression =
    bitwise_xor_expression, { "|", bitwise_xor_expression }
  ;

bitwise_xor_expression =
    bitwise_and_expression, { "^", bitwise_and_expression }
  ;

bitwise_and_expression =
    equality_expression, { "&", equality_expression }
  ;

equality_expression =
    comparison_expression,
    [ ("==" | "!="), comparison_expression ]
  ;

comparison_expression =
    shift_expression,
    [ ("<" | "<=" | ">" | ">="), shift_expression ]
  ;

shift_expression =
    additive_expression, { ("<<" | ">>"), additive_expression }
  ;

additive_expression =
    multiplicative_expression, { ("+" | "-"), multiplicative_expression }
  ;

multiplicative_expression =
    cast_expression, { ("*" | "/" | "%"), cast_expression }
  ;

cast_expression = prefix_expression, { "as", type } ;

prefix_expression =
    ("-" | "!" | "&" | "*" | "~"), prefix_expression
  | postfix_expression
  ;

postfix_expression = primary_expression, { postfix_suffix } ;

postfix_suffix =
    call_suffix
  | bracket_suffix
  | slice_suffix
  | member_suffix
  | path_suffix
  | record_literal_suffix
  | "!"
  | "++"
  | "--"
  ;
```

## Postfix forms and bracket application

```ebnf
call_suffix =
    "(", [ expression, { ",", expression }, [ "," ] ], ")"
  ;

bracket_suffix =
    "[", bracket_argument, { ",", bracket_argument }, [ "," ], "]"
  ;

bracket_argument = expression | type ;

slice_suffix =
    "[", [ expression ], ":", [ expression ], "]"
  ;

member_suffix = ".", (identifier | integer_literal) ;
path_suffix = "::", identifier ;

record_literal_suffix = ".", record_body ;

record_body =
    "{", [ record_field, { ",", record_field }, [ "," ] ], "}"
  ;

record_field = identifier, "=", expression ;
```

A `bracket_argument` is parsed as a category-neutral syntax term. Its node may
later be validated as a type or a value expression after the base resolves.
The parser does not use a following call, capitalization, or identifier lookup
to classify it. A comma does not force the generic interpretation.

The shared syntax-term grammar contains every type and expression form that can
legally occur between brackets. Overlapping spellings such as `T`, `module::T`,
`*T`, and `(T, U)` retain their syntactic structure without a semantic category.
Forms that are only types or only values still receive their ordinary node
kind. This is a surface-tree representation rule, not delayed token reparsing.
The syntax-term parser must consume the complete argument up to its top-level
comma or closing bracket; failure to form either permitted grammar produces a
parser diagnostic immediately.

`[]` is not a bracket application because it has no argument. A bracket with a
colon is always a slice. Indexing accepts exactly one value argument after
semantic classification; generic instantiation may accept multiple type
arguments.

## Primary expressions

```ebnf
primary_expression =
    literal
  | interpolated_string
  | identifier
  | "context"
  | "nil"
  | "none"
  | "some", expression
  | "sizeof", type
  | grouped_or_tuple_expression
  | array_expression
  | function_literal
  | anonymous_record_literal
  | partial_member_expression
  ;

literal =
    integer_literal
  | float_literal
  | string_literal
  | character_literal
  | "true"
  | "false"
  ;

grouped_or_tuple_expression =
    "(", expression,
    [ ",", [ expression, { ",", expression }, [ "," ] ] ], ")"
  ;

array_expression =
    "[", "]"
  | "[", expression, ";", expression, "]"
  | "[", expression, { ",", expression }, [ "," ], "]"
  ;

anonymous_record_literal = ".", record_body ;
partial_member_expression = ".", identifier ;
```

`(value)` is grouping. `(value,)` is a one-element tuple. Empty parentheses are
not a value. Array repetition accepts an expression count syntactically; the
checker requires it to be a valid non-negative compile-time integer.

Interpolation token structure is defined by the lexer:

```ebnf
interpolated_string =
    InterpolationStart,
    { InterpolationText
    | InterpolationExprStart, expression, InterpolationExprEnd },
    InterpolationEnd
  ;
```

## Types

```ebnf
type =
    named_type
  | pointer_type
  | optional_type
  | array_or_slice_type
  | function_type
  | grouped_or_tuple_type
  | struct_type
  | union_type
  | enum_type
  ;

named_type = path, [ type_arguments ] ;
path = identifier, { "::", identifier } ;

type_arguments =
    "[", type, { ",", type }, [ "," ], "]"
  ;

pointer_type = "*", type ;
optional_type = "?", type ;

array_or_slice_type =
    "[", "]", type
  | "[", constant_expression, "]", type
  ;

function_type =
    "fn", [ string_literal ], "(",
    [ type, { ",", type }, [ "," ] ], ")", type
  ;

grouped_or_tuple_type =
    "(", type, [ ",", [ type, { ",", type }, [ "," ] ] ], ")"
  ;

struct_type =
    "struct", "{", { struct_member }, "}"
  ;

struct_member = field_declaration | function_declaration ;

field_declaration = identifier_list, type, ";" ;

union_type =
    "union", [ "enum" ], "{", { union_member }, "}"
  ;

union_member = variant_declaration | function_declaration ;
variant_declaration = identifier_list, type, ";" ;

enum_type =
    "enum", "{", [ identifier, { ",", identifier }, [ "," ] ], "}"
  ;

constant_expression = expression ;
```

The parser records an array length as syntax. Constant evaluation and permitted
operators are semantic concerns. Empty tuple types are not accepted; `void` is
the no-value function result type.

`union enum` is the existing tagged-union spelling. Plain `union` is untagged.
The checker defines their layout, safety, construction, and member rules.

## Syntax rejected by the first parser

- prototype generic separators such as `value.[T]`;
- numeric literal suffixes;
- implicit function result types;
- assignment expressions;
- comparison chains such as `a < b < c`;
- empty generic argument or parameter lists;
- raw strings and block comments, which are lexical rather than parser forms.
