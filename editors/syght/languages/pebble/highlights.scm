; Comments
(comment) @comment

; Literals
; NOTE: these come before `(identifier) @variable` because an interpolated
; string covers the whole `` `...` `` range including the expressions inside
; `{...}`. Since a later pattern wins in Zed, the identifier rules below
; correctly re-highlight the inner expressions.
(integer_literal) @number
(float_literal) @number
(boolean_literal) @boolean
(nil_literal) @constant
(none_literal) @constant
(character_literal) @string
(string_literal) @string
(escape_sequence) @string.escape
(interpolated_string) @string
(interpolation_text) @string

; Fallback for every other identifier.
; Must stay ABOVE all more specific identifier captures (later wins in Zed).
(identifier) @variable

; Keywords
[
  "as"
  "break"
  "case"
  "continue"
  "defer"
  "else"
  "enum"
  "extern"
  "fn"
  "for"
  "if"
  "import"
  "inline"
  "let"
  "loop"
  "print"
  "println"
  "return"
  "slice"
  "sizeof"
  "some"
  "struct"
  "switch"
  "type"
  "union"
  "var"
  "while"
] @keyword

; `context` is the whole body of its rule, so it has no standalone token to
; match; capture the rule node instead.
(context_expression) @keyword

; Types
; User-defined types in type position. `named_type` also covers the path
; segments of `pkg::Type` and the identifiers inside `type_arguments`.
(named_type
  (identifier) @type)

; Builtin type names in type position. These are not structurally distinct
; from ordinary identifiers in the grammar, so match on the spelling, scoped
; to `named_type` so `var int_count i32 = 5;` still highlights `int_count`
; as a plain variable.
(named_type
  (identifier) @type.builtin
  (#match? @type.builtin "^(void|bool|int|str|char|f32|f64|i8|i16|i32|i64|isize|u8|u16|u32|u64|usize)$"))

; Declared type names
(type_declaration
  (identifier) @type)

; Generic type parameters: `fn foo[T]` / `type Box[T]`
(type_parameter
  (identifier) @type)

; Enum variant names: `enum { red, green, blue }`
(enum_type
  (identifier) @variant)

; Tagged union variant names: `union enum { empty void; value i32; }`
(union_member
  (identifier) @variant)

; Struct/union member (field) names
(aggregate_member
  (identifier) @property)

; Function declarations.
; Anchored on the following `parameter_list` so a bare-identifier arrow body
; (`fn f() int => value;`) does not get captured as the function name.
(function_declaration
  (identifier) @function
  (parameter_list))

; Extern function declarations
(extern_item
  (identifier) @function
  (parameter_list))

; Methods declared inside struct/union bodies
(aggregate_member
  (function_declaration
    (identifier) @function.method
    (parameter_list)))

(union_member
  (function_declaration
    (identifier) @function.method
    (parameter_list)))

; Parameters
(parameter
  (identifier) @variable.parameter)

; Function calls
(call_expression
  function: (identifier) @function)

; `pkg::foo(...)`
(call_expression
  function: (path_expression
    (path_suffix
      (identifier) @function)))

; `foo[T](...)` and `pkg::foo[T](...)`
(call_expression
  function: (index_expression
    operand: (identifier) @function))

(call_expression
  function: (index_expression
    operand: (path_expression
      (path_suffix
        (identifier) @function))))

; Field access: `obj.field` (non-call)
; Placed BEFORE the method-call rule below so a call's function-position
; field_expression (which structurally also matches this pattern, since
; it's a field_expression nested inside the call_expression) gets
; overridden by the more specific @function.method capture (later wins).
(field_expression
  (member_suffix
    (identifier) @property))

; `obj.foo(...)` / `Choice.value(...)` — method-style calls
(call_expression
  function: (field_expression
    (member_suffix
      (identifier) @function.method)))

; `pkg::` qualifier
(path_expression
  (identifier) @module)

; Partial member expression: `.field`
(partial_member_expression
  (identifier) @property)

; Record field names: `.{ x = 1, y = 2 }` / `Point.{ x = 1 }`
; Anchored on `=` so the value expression's identifier is not captured.
(record_field
  (identifier) @property
  "=")

; Calling-convention strings are ABI-significant syntax, not arbitrary text
(function_declaration
  (string_literal) @keyword)

(function_literal
  (string_literal) @keyword)

(function_type
  (string_literal) @keyword)

; Library name on an extern declaration
(extern_declaration
  (string_literal) @string)

; Operators
[
  "+"
  "-"
  "*"
  "/"
  "%"
  "="
  "+="
  "-="
  "*="
  "/="
  "%="
  "=="
  "!="
  "<"
  "<="
  ">"
  ">="
  "&&"
  "||"
  "!"
  "&"
  "|"
  "^"
  "~"
  "<<"
  ">>"
  ".."
  "..="
  "=>"
  "?"
  "::"
  "..."
  "++"
  "--"
] @operator

; Punctuation
["(" ")" "{" "}" "[" "]"] @punctuation.bracket
["," ";" "." ":"] @punctuation.delimiter
