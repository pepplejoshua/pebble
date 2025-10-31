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
  "struct" "enum"
] @type

; Boolean literals
[
  "true"
  "false"
] @boolean

(nil_literal) @constant

; Function declarations
(function_declaration
  name: (identifier) @function)

(extern_declaration
  name: (identifier) @function)

; Function calls
(call_expression
  function: (identifier) @function)

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
  "::"
  ".."
  "..="
  "^"
  "~"
  "|"
  "<<"
  ">>"
] @operator

; Punctuation
["(" ")" "{" "}" "[" "]"] @punctuation.bracket
["," ";" "." ":"] @punctuation.delimiter
