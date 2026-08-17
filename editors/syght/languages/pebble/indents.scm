; Indent a braceless statement body (`if x` / `while x` / `loop ...` / `for ...`
; / `defer` followed by a single statement). When the body is a `{}` block the
; generic brace rule below handles it, so there is no double indent.
[
  (if_statement)
  (while_statement)
  (for_statement)
  (range_loop_statement)
  (defer_statement)
] @indent

; Indent inside any brace/paren/bracket delimited construct: blocks, function
; bodies, struct/union/enum bodies, switch bodies, extern blocks, parameter
; lists, arrays, type arguments, etc.
(_
  "{"
  "}" @end) @indent

(_
  "("
  ")" @end) @indent

(_
  "["
  "]" @end) @indent
