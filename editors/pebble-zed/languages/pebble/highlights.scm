; Comments
(comment) @comment

; Keywords
[
  "fn"
  "type"
  "let"
  "var"
  "if"
  "else"
  "while"
  "for"
  "loop"
  "return"
  "break"
  "continue"
  "extern"
  "as"
  "print"
  "switch"
  "case"
  "defer"
  "import"
  "inline"
] @keyword

; Types
[
  "void"
  "bool"
  "int"
  "str"
  "char"
  "f32" "f64"
  "i8" "i16" "i32" "i64" "isize"
  "u8" "u16" "u32" "u64" "usize"
  "struct" "enum" "union"
] @type

; Boolean literals
[
  "true"
  "false"
] @boolean

(nil_literal) @constant

(some_expression) @keyword
(none_expression) @constant
(context_expression) @keyword

; Template literals
(template_literal
  "`" @string
  "`" @string)

(template_string_fragment) @string

(template_interpolation
  "{" @punctuation.bracket
  "}" @punctuation.bracket)

; Qualified paths used as types - highlight all segments
(pointer_type
  (qualified_path
    head: (identifier) @type
    tail: (identifier) @type))

(array_type
  element_type: (qualified_path
    head: (identifier) @type
    tail: (identifier) @type))

(slice_type
  element_type: (qualified_path
    head: (identifier) @type
    tail: (identifier) @type))

(optional_type
  (qualified_path
    head: (identifier) @type
    tail: (identifier) @type))

(variable_declaration
  type: (type
    (qualified_path
      head: (identifier) @type
      tail: (identifier) @type)))

(parameter
  type: (qualified_path
    head: (identifier) @type
    tail: (identifier) @type))

(function_declaration
  return_type: (qualified_path
    head: (identifier) @type
    tail: (identifier) @type))

(function_expression
  return_type: (qualified_path
    head: (identifier) @type
    tail: (identifier) @type))

(type_declaration
  (qualified_path
    head: (identifier) @type
    tail: (identifier) @type))

(cast_expression
  (qualified_path
    head: (identifier) @type
    tail: (identifier) @type))

; Function declarations
(function_declaration
  name: (identifier) @function)

(function_declaration
  calling_convention: (string_literal) @keyword)

(extern_function_declaration
  name: (identifier) @function)

(extern_block
  library: (string_literal) @string)

(extern_declaration
  library: (string_literal) @string)

; Function calls
(call_expression
  function: (qualified_path
    tail: (identifier) @function))

; Field access
(field_expression
  field: (identifier) @property)

; Parameters
(parameter
  name: (identifier) @variable.parameter)

; Variables
(identifier) @variable

; Literals
(integer_literal) @number
(hex_literal) @number
(float_literal) @number
(string_literal) @string
(char_literal) @string
(escape_sequence) @string.escape

; Operators
[
  "+"
  "-"
  "*"
  "/"
  "="
  "=="
  "!="
  "<"
  ">"
  "<="
  ">="
  "&&"
  "||"
  "!"
  "&"
  ".."
  "..="
  "^"
  "~"
  "|"
  "<<"
  ">>"
  "?"
] @operator

; Punctuation
["(" ")" "{" "}" "[" "]"] @punctuation.bracket
["," ";" "." ":" "::"] @punctuation.delimiter
