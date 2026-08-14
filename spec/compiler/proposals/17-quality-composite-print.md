# 17 — Quality composite printing for every value type

**Status:** decision made (implement it, quality bar is real — "every type
of value in this language printable out of the box," clear output, not a
grudging minimum). Investigated via a read-only Luna dispatch
(2026-08-10); this document is that investigation's findings plus the
design it proposed, kept as the plan of record. Implementation proceeds
slice by slice through `13-v1-parity-gap-analysis.md`, same discipline as
every other item in that tracker.

## Part 1 — V1 reference behavior

V1 (`src/codegen.c`) is a compile-time recursive formatter, not a runtime
metadata walker — it statically unrolls a format string and argument list
for the value's known type, then emits ONE combined `printf`.

- Scalar format selection: `src/codegen.c:1753-1781`
- Composite format generation: `src/codegen.c:1784-1864`
  (`build_composite_format_string`)
- Composite argument generation: `src/codegen.c:1866-2022`
  (`build_composite_args`)
- Print statement generation: `src/codegen.c:2046-2185`, `2187-2242`

Generated shapes: `StructType.{field = value, field = value}`,
`(value, value)` (tuple), `[value, value]` (array). Builds the formatted
composite into a temp via `snprintf(NULL, 0, ...)` sizing +
`alloca`-backed `sprintf`, then passes it to the final `printf`
(`codegen.c:2094-2159`).

V1's nesting is real but incomplete: structs/tuples/arrays recurse fully
into each other, but a nested SLICE is not traversed (prints the type name
as a literal string, `codegen.c:1879-1886`); optional/pointer/union fields
fall into a generic fallback that prints the type name as a quoted string
(`codegen.c:2168-2181`) rather than inspecting the value, which for an
optional can produce invalid generated C. No cycle/depth protection exists
in the print path, but it doesn't need one: V1 never recursively
dereferences pointers, so pointer cycles never enter the formatter.
Top-level plain enums print `EnumType.variant` via a generated name table
(`codegen.c:2160-2167`, `2221-2225`).

## Part 2 — V2 current state

`print` currently accepts only bool, char, str, integer, and float —
`valuePrintable` in `compiler/internal/check/control_flow_validation.go:111-122`
rejects everything else at `control_flow_validation.go:230-234`. Backend
dispatch is `buildPrint` in `compiler/internal/backend/statements.go:2511-2770`,
switching on resolved builtin type at `statements.go:2634-2752` with no
composite branch at all.

The exact TIR type is already available at the print site
(`child.Type`, `statements.go:2581-2585`) — no runtime type info is
needed, matching V1's architecture. Existing scalar leaf behavior to
reuse exactly: integers use exact-width `PRI*` macros
(`statements.go:2661-2679`), bool emits `"true"`/`"false"`
(`2680-2699`), char converts through `pebble_rt_char_to_utf8`
(`2700-2732`), str emits `PebbleStr.data` (`2733-2743`), `f32`/`f64` use
`%f` (`2744-2750`).

Reusable existing recursive-traversal patterns (type collection/typedef
walkers, not print walkers, but the right shape to mirror): fixed-array
collection (`collect.go:135-199`), tuple (`collect.go:329-428`), optional
(`collect.go:429-513`), slice (`collect.go:542-631`), struct incl. nested
field traversal (`collect.go:883-997`), tagged-union
(`collect.go:1199-1289`), dependency-first aggregate ordering
(`typedefs.go:25-165`). Aggregate C layouts are already explicit: optional
is `has_value`+`value` (`typedefs.go:287-318`), tagged union is
`tag`+`payload` (`typedefs.go:401-455`), slice is `data`+`len`
(`typedefs.go:590-606`), struct fields at `typedefs.go:338-378`.

## Part 3 — Design

### Formatting policy

Readable, source-oriented, not V1's exact spelling:

```
Point{ x: 1, y: 2 }
Line{ a: Point{ x: 1, y: 2 }, b: Point{ x: 3, y: 4 } }
(1, "hello")
(1,)                 -- one-element tuple, trailing comma avoids paren ambiguity
[1, 2, 3]
[]                   -- empty slice
some(42)
none
Color.red
Color<invalid: 7>    -- defensive, invalid discriminant
Result.ok(42)
Result.done          -- payload-less variant
nil                  -- null pointer
&0x7ffee1234560       -- non-null pointer, address only, NEVER auto-dereferenced
<fn add>              -- statically known function reference
<fn @0x1234abcd>       -- indirect function pointer
```

Struct fields use `:` (not V1's `=`). Every print statement still ends
with exactly one newline. Strings keep V2's current unquoted scalar
output for now (quoting, if ever wanted, is a separate scalar-policy
change, not a composite-traversal one). The core quality bar: every value
gets a truthful representation — nothing silently prints only its type
name the way V1's fallback branch does for union/optional/pointer.

Scalar leaves reuse the existing V2 builders exactly (`buildCharOperand`,
`buildStrOperand`, the width-aware integer builders) — the composite
formatter never bypasses them.

### Cycles — the key architectural decision

**Pointers are always leaves.** `print p` where `p` is a pointer prints
the address only, never the pointee, unless the source explicitly writes
`print *p` (which prints the pointee using the normal composite
formatter — and any pointer FIELDS reached from there are again
address-only leaves). Structs/tuples/arrays/slices/optionals/enums/union
payloads all recurse by value.

Since Pebble permits a runtime cycle only through pointer indirection
(V1's checker treats pointers as cycle-breaking indirection,
`src/checker.c:356-361`, and rejects a genuinely infinite-size type
outright, `checker.c:363-407`/`570-613`), making pointers leaves makes
ordinary composite printing cycle-safe BY CONSTRUCTION — no visited-set,
no depth counter needed as the primary mechanism. A depth cap is still a
reasonable defensive backstop for pathologically deep BY-VALUE nesting,
but not the real safety mechanism.

### Output storage — no allocator dependency

Direct sequential `fprintf(stdout, ...)` calls, no intermediate dynamic
string. Deliberately chosen over V1's `alloca`+`snprintf`-sizing+`sprintf`
approach because it requires no allocator at all — this must not create a
dependency on proposal 15's Allocator/Context redesign, which is a
separate, parallel, not-yet-implemented effort. Also naturally supports a
dynamic slice's runtime-determined element count and lets enum/union
branches emit output conditionally without pre-computing a total length.

### Generated C shape (concrete example)

For `type Point = struct { x int; y int; }; type Line = struct { a Point; b Point; }; print my_line;`:

```c
Line pebble_print_value = my_line;
fprintf(stdout, "Line{ a: ");
fprintf(stdout, "Point{ x: ");
fprintf(stdout, "%" PRId32, pebble_print_value.pebble_field_a.pebble_field_x);
fprintf(stdout, ", y: ");
fprintf(stdout, "%" PRId32, pebble_print_value.pebble_field_a.pebble_field_y);
fprintf(stdout, " }, b: ");
fprintf(stdout, "Point{ x: ");
fprintf(stdout, "%" PRId32, pebble_print_value.pebble_field_b.pebble_field_x);
fprintf(stdout, ", y: ");
fprintf(stdout, "%" PRId32, pebble_print_value.pebble_field_b.pebble_field_y);
fprintf(stdout, " }\n");
```

A slice needs a real runtime loop (element formatter is still statically
generated, only the iteration count is dynamic):

```c
fprintf(stdout, "[");
for (size_t i = 0; i < value.len; i++) {
    if (i != 0) fprintf(stdout, ", ");
    /* recursively emitted element formatter */
}
fprintf(stdout, "]");
```

The right internal abstraction is a recursive emitter, conceptually
`emitPrintValue(typeID, cExpression, context)` — dispatches on `TypeID`,
emits punctuation/labels as static text, calls the existing scalar
builders at leaves, generates a runtime loop only for a dynamic slice,
and a runtime switch only for an enum/union discriminant.

## Implementation slices (recommended order, least risk first)

1. ~~**Structs of scalars**~~ **RESOLVED (`c182e73`).** Checker's
   `valuePrintable` accepts a struct whose fields are all scalar; backend
   emits `TypeName{ field: value, field: value }` via direct sequential
   `fprintf` calls, real source field/type names, materialized once to
   avoid double-evaluating a struct-returning call operand. Verified,
   causation-checked.
2. ~~**Tuples and fixed arrays**~~ **RESOLVED (`5e6e786`).** Positional
   recursion, compile-time unrolled. One-element tuple gets a trailing
   comma (`(5,)`). Companion widening: tuple/array element builders
   previously only supported the entry width and bool; widened to str,
   char, float, and any fixed-width integer. Verified, causation-checked.
3. ~~**Nested aggregates**~~ **RESOLVED (`b80fbc4`).** `printableType`
   is a recursive checker closure; backend's `buildPrintValueCalls` is
   one shared recursive dispatcher all three top-level operand builders
   route through, at any nesting depth, with the outer operand still
   materialized exactly once. New finding surfaced while verifying:
   struct-of-array could not be tested — array-typed struct fields are
   unsupported in this backend entirely (even without print), a
   separate pre-existing gap, logged in proposal 14. Verified, causation-
   checked; a stale slice-1 negative fixture was found and moved to
   valid/.
4. ~~**Slices**~~ **RESOLVED (`21e54ec`).** `printFprintfCall` gained a
   `raw` field for one pre-rendered C for-loop block (a slice's length
   is a runtime value, so the element sequence can't be a compile-time
   call list); the loop body reuses slices 1-3's same recursive
   `buildPrintValueCalls` against `<expr>.data[i]`. Verified multi-
   element, empty (`[]`), and slice-of-struct (nested recursion)
   shapes; not-yet-printable element type still rejects. Causation-
   checked.
5. ~~**Plain enums**~~ **RESOLVED (`c1bf23b`).** `printableType` splits on
   `declaration.Nominal` — `NominalEnum` is a leaf case, `NominalTaggedUnion`
   stays rejected (unions are slice 6). Backend emits one raw C switch
   over the discriminant, a case per declared variant printing
   `Type.variant`, a defensive `Type<invalid: N>` default. Verified
   two variants (proving the tag-to-name mapping); a tagged union still
   rejects; a stale slice-4 negative fixture (slice-of-enum) was caught
   and moved to valid/, since enum printability now flows through
   slice 4's recursive element check. Causation-checked.
6. ~~**Tagged unions**~~ **RESOLVED (`9a0f27d`).** `printableType` gains a
   `NominalTaggedUnion` case, recursive over each declared variant's
   payload type (void payload trivially printable). Backend emits one
   raw C switch over `.tag`; each payload-carrying case recurses into
   `buildPrintValueCalls` against `<expr>.payload.pebble_field_<variant>`
   then closes the paren; a payload-less case emits the bare
   `Type.variant` with no parens; a defensive `default:` emits
   `Type<invalid-tag: N>`, mirroring slice 5. Verified: a payload
   variant (`Result.ok(42)`), a second payload variant proving the
   tag-to-name mapping (`Result.error(failed)`), and a payload-less
   variant (`Status.done`) — all end-to-end compile-and-run, and a
   structural test asserting the raw switch shape with both variants
   actually constructed (a variant's C union payload member only exists
   if it's constructed somewhere in the unit — an existing convention
   shared with narrowed union-variant payload access, not new to this
   slice). Checker rejection of a pointer-payload variant confirmed. A
   stale `C0612` fixture asserting the old "unions aren't printable"
   rejection was found and moved to valid/. Causation-checked (reverted
   both files, confirmed the exact pre-fix `C0612` rejection
   reproduces). Full suite clean.
7. ~~**Optionals**~~ **RESOLVED (`1987102`).** `printableType` gains a
   `types.Optional` case, recursive over the payload type via
   `key.Child()` (mirroring the slice/tuple element recursion above).
   Backend emits one raw C `if (<expr>.has_value) { ... } else { ... }`:
   the true branch prints `"some("` then recurses into
   `buildPrintValueCalls` against `<expr>.value`, then `")"`; the false
   branch prints the bare `"none"` literal. Widened
   `buildOptionalValueExpr` to accept a `SymbolValue` (an already-
   declared optional local/global), a `DirectCall` (an optional-
   returning helper call), and a `SourceAlias` (grouped-expression
   parens) as print-operand sources — previously only
   `SomeOptional`/`NoneOptional`/`OptionalInject` construction nodes
   were handled, but a print operand is typically a reference to an
   already-declared local, not a fresh construction. Also taught
   `buildStructValueExpr`/`buildTupleValueExpr` to transparently unwrap
   a `SourceAlias`-wrapped payload literal, needed for
   `some(Point.{...})`/`some((1, 2))` to round-trip through print (the
   payload literal arrives wrapped in a SourceAlias node). Verified: a
   scalar payload (`some(5)`, `none`), a struct payload
   (`some(Point{ x: 1, y: 2 })`), a tuple payload (`some((1, 2))`),
   each with its `none` counterpart, and a mixed-operand print
   (`print a, " ", b`) — all with byte-exact captured stdout, not just
   exit code. A pointer-payload optional stays cleanly rejected at the
   checker (pointers are slice 8, not yet printable). Causation-checked
   against `HEAD`. Full `internal/backend` and `internal/check` suites
   clean.
8. ~~**Pointers**~~ **RESOLVED (`a8c48b8`).** `printableType` gains a
   `types.Pointer` LEAF case (`return true`, no `key.Child()`
   recursion) — printing a pointer never dereferences the pointee, so
   printability is unconditional regardless of pointee type; this is
   exactly what makes a self-referential pointer cycle trivially safe.
   Backend emits `"&" + %p` for a non-nil pointer and the bare `"nil"`
   literal for a null one (nil-check via `== NULL`, the same idiom the
   runtime's checked dereference uses).
   `buildPointerPrintValueExpr` covers every real pointer-value operand
   shape (`SymbolValue`, `Load`, `FieldValue`, `AddressOf`,
   `NilPointer`, `DirectCall`, `PointerCast`, `SourceAlias`). Printing a
   self-referential struct (`type Node = struct { next *Node; };`)
   required the struct's own C typedef to carry a tag name so the
   pointer field can reference the enclosing type via C's tag namespace
   before the typedef completes — detected precisely via a
   pointer-field cycle graph (`structIsCyclic`/`structCycleSet`, DFS
   cycle detection) rather than tagging every struct unconditionally;
   an earlier, broader attempt at this regressed 22 existing tests
   asserting the plain untagged spelling for the overwhelming
   non-cyclic common case, caught and corrected by the full-suite
   checkpoint. Two now-obsolete negative tests (an optional and a
   tagged union with a pointer payload, previously rejected as
   unprintable) were converted to positive compile-and-run tests. A
   dedicated test constructs a self-referential `Node` and proves the
   print terminates normally rather than recursing unboundedly.
   Causation-checked against `HEAD`; full `internal/backend` and
   `internal/check` suites clean.
9. **Function values** — named-function formatting first, indirect
   pointer-address formatting second; lowest priority but not left
   silently rejected, since the goal is universal printability.
