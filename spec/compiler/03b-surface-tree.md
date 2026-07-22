# Surface Tree

## Representation

The parser returns a `Tree` owned by one source file. The implementation uses
opaque, compilation-local IDs rather than exposing mutable node pointers:

```go
type NodeID uint32

type Tree struct {
    // private source, token, node, and child-list storage
}
```

`NodeID(0)` is invalid. Nodes and child lists are appended while parsing and
become immutable before `Parse` returns. Tree accessors return values, copies,
or iterators; they never expose mutable backing slices. Callers cannot replace
children or mutate node payloads.

This gives the compiler one obvious ownership rule: the tree owns its syntax,
and all later phases refer to it without rewriting it. A `NodeID` is stable only
for the lifetime of that `Tree`; it is not a cross-run cache key.

## Common node data

Every node has:

- `Kind`;
- half-open `Span` covering its real source tokens;
- ordered child IDs or a compact child-list handle;
- kind-specific token or small scalar data.

Names and literals are referenced by token/source span. The parser does not
allocate rewritten names. Integer and floating spellings remain available for
arbitrary-precision and target-aware semantic conversion. It may also retain a
validated decoded string/character value from the lexer, but decoded data never
replaces the original span.

## Root and declarations

| Node | Syntactic contents |
| --- | --- |
| `File` | ordered declarations and EOF |
| `ImportDecl` | import keyword, path token, semicolon |
| `BindingDecl` | `let`/`var`, name, optional type, optional initializer |
| `TypeDecl` | name, type parameters, defining type |
| `FunctionDecl` | modifiers, name, type parameters, parameters, result, body |
| `ExternDecl` | optional library string and one extern item or block |
| `ExternBlock` | ordered extern items |
| `ExternFunction` | name, parameters, result |
| `ExternType` | name |
| `ExternBinding` | `let`/`var`, name, type |
| `Parameter` | variadic marker, ordered names, type |
| `TypeParameter` | name token |

The tree stores the parameter grouping written by the user. It does not expand
`a, b T` into two independently authored parameters until a later convenient
representation requests that view.

## Statements

| Node | Syntactic contents |
| --- | --- |
| `BlockStmt` | braces and ordered statements |
| `ReturnStmt` | optional expression |
| `IfStmt` | condition, then statement, optional else statement |
| `WhileStmt` | condition and body |
| `RangeLoopStmt` | start, range token, end, optional iterator, body |
| `ForStmt` | optional initializer, condition, update, body |
| `SwitchStmt` | subject, ordered cases, optional else case |
| `SwitchCase` | ordered conditions and body |
| `DeferStmt` | deferred statement |
| `PrintStmt` | ordered expressions |
| `BreakStmt` | keyword and semicolon |
| `ContinueStmt` | keyword and semicolon |
| `AssignmentStmt` | left syntax, assignment operator, right expression |
| `ExpressionStmt` | expression and semicolon |

Assignment does not desugar compound operators in the surface tree. `x += y`
retains `+=`; typed IR later makes evaluation and conversion behavior explicit.

## Expressions and shared syntax terms

| Node | Syntactic contents |
| --- | --- |
| `Name` | identifier token |
| `Path` | ordered `::`-separated names |
| `Literal` | literal or Boolean/nil/none keyword token |
| `InterpolatedString` | ordered text and embedded expressions |
| `ContextExpr` | `context` keyword |
| `SomeExpr` | wrapped expression |
| `SizeofExpr` | type syntax |
| `PrefixTerm` | operator token and operand |
| `PostfixExpr` | `!`, `++`, or `--` token and operand |
| `BinaryExpr` | left, operator token, right |
| `CastExpr` | expression, `as`, target type syntax |
| `CallExpr` | callee and ordered arguments |
| `BracketApply` | base and category-neutral bracket arguments |
| `SliceExpr` | base, optional start, optional end |
| `MemberExpr` | base and identifier or tuple-index token |
| `GroupedTerm` | explicit parentheses and inner syntax term |
| `TupleTerm` | ordered terms, including one-element tuples |
| `ArrayExpr` | ordered elements |
| `ArrayRepeatExpr` | value and count expression |
| `RecordExpr` | optional explicit base and ordered field initializers |
| `FunctionTerm` | modifiers, type parameters, signature, optional body |
| `PartialMemberExpr` | leading dot and member name |

`BracketApply` is deliberately not `IndexExpr` or `GenericExpr`. Its
arguments are structurally parsed syntax nodes, not an unparsed token range.
Overlapping syntax uses shared nodes: for example, `*T` is a `PrefixTerm` until
context or resolution determines whether it is a pointer type or dereference.
Names, paths, grouping, tuples, and function signatures likewise keep one
surface representation when their spelling is shared by type and value syntax.

Known type positions still validate against the type grammar during parsing.
The neutral representation is required only where the base has not yet been
resolved.

Calls and record bodies are independent postfix nodes. The tree for
`constructors[i](value)` is a call whose callee is a bracket application. The
tree for `Factory[T].{ value = x }` is a record expression whose explicit base
is a bracket application. Neither tree asserts what the bracket means.

## Type-only forms

| Node | Syntactic contents |
| --- | --- |
| `OptionalType` | `?` and base type |
| `SliceType` | `[]` and element type |
| `ArrayType` | length syntax and element type |
| `StructType` | fields and methods |
| `UnionType` | tagged marker, variants, and methods |
| `EnumType` | ordered variant names |
| `FieldDecl` | ordered names and type |
| `VariantDecl` | ordered names and payload type |

Named, qualified, generic, pointer, grouped, tuple, and function types reuse the
shared syntax nodes where their written form overlaps value syntax. A
`FunctionTerm` without a body is a function type in a required type position; a
body makes it a function literal. Semantic type identity is never stored in
these nodes.

## Error representation

| Node | Purpose |
| --- | --- |
| `Missing` | expected syntax inserted at a zero-width location |
| `Error` | consumed damaged syntax that could not form the expected construct |

Both carry an expected category and span. Later phases must explicitly detect
them and either propagate an error value or skip the enclosing construct. They
must not infer meaning from a missing/error node.

## Spans

- A complete node spans from its first real token start through its last real
  token end, including delimiters and terminators that belong to the node.
- A missing node has `Start == End` at its insertion location.
- An error node spans every token consumed during its local recovery.
- Parent spans include error children but do not extend through unrelated
  synchronization tokens.
- Grouping, tuple, call, bracket, block, and record spans include delimiters.
- The `File` span is `[0, file.Len())`, including trailing trivia represented by
  the source even though trivia has no token node.

## Parse API

The initial package API is:

```go
func Parse(
    file *source.File,
    diagnostics *diagnostic.DiagnosticSet,
) *Tree
```

`Parse` lexes the file, buffers tokens as needed for lookahead, and appends
lexer and parser diagnostics to the supplied set in source order. It always
returns a non-nil tree for a valid `file`, including an empty or damaged file.

Fragment entry points for expression/type conformance tests remain unexported.
Production compiler phases parse complete files through `Parse`.
