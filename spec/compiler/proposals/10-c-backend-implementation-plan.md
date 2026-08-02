# 10 C backend and runtime ABI — inventory of the old C backend and first-slice proposal

**Status:** 10.1 is implemented, committed, and pushed (see "Completed
slices" below). This is the pre-implementation record for the rest of
phase 10, written the way 07's plan was: real file/line citations
against the old backend, a verdict per item, and slices scoped
incrementally as work proceeds.

## Completed slices

- **10.1 — versioned runtime ABI skeleton** (`runtime/`): pure C,
  independent of the Go compiler, per section 4 below.
  `runtime/include/pebble_rt.h` pins the interface — `PebbleContext`/
  `PebbleAllocator` passed by pointer (not the old backend's
  recursive-by-value shape), a documented zeroing contract on the
  default allocator, a single `pebble_rt_panic` entry every safety
  check will funnel through, a length-prefixed `PebbleStr`, and
  explicit `PEBBLE_RT_MODE_SAFE`/`PEBBLE_RT_MODE_RELEASE`/
  `PEBBLE_RT_FREESTANDING` config macros. `context.c`/`panic.c`/
  `platform_host.c` implement it for the hosted configuration. A
  hand-written smoke test (`runtime/test/smoke_test.c`) exercises the
  zeroing allocator, argv adaptation, and proves `pebble_rt_panic`
  actually aborts via a fork+pipe+waitpid check. Builds clean under
  `-Wall -Wextra -Werror` in both SAFE and RELEASE modes, and under
  `-DPEBBLE_RT_FREESTANDING`. No file under `compiler/` or `src/` was
  touched.
- **10.2 — the first Go-emitted C** (`compiler/internal/backend`): a
  new `Emit(unit, snapshot, entrySymbol, w) error` supporting exactly
  one program shape — `fn main() void {}`, a Pebble-convention,
  zero-parameter, void-result entry with a completely empty body.
  Validates the entry's `FunctionDeclaration` node (convention,
  parameter count, void result via the type snapshot), resolves the
  body to its distinct `Block` node, and accepts only zero children or
  the single synthesized `ImplicitReturn` an empty void body produces
  — anything else is rejected with a descriptive error, not
  best-effort lowered. Only after validation passes does it write the
  fixed adapter shape (a `pebble_user_main` taking `PebbleContext*`,
  and a hosted `main()` that builds a default context and calls it).
  The emitted C compiles clean under `-Wall -Wextra -Werror` against
  the 10.1 runtime and runs to exit 0 — this is the first program the
  Go rewrite has ever produced and run end-to-end.
- **10.3 — an integer literal as the process exit code**
  (`compiler/internal/backend`): additive second shape, `fn main()
  i32 { return <non-negative integer literal>; }` — the literal
  propagates through `pebble_user_main`'s return value and the hosted
  `main`'s own `return` to become the process's actual exit code.
  10.2's void/empty-body shape is untouched and still separately
  verified. Rejects unary negation (`return -1;` is a `CheckedNegate`
  node wrapping a positive literal, not a signed literal — confirmed
  by inspecting the real node graph), non-literal return values, extra
  statements, and an i32 entry with an empty body (a shape the checker
  itself never produces from source — the checker rejects non-void
  fall-through — so the test constructs it directly via `tir.Builder`
  to exercise the backend's own validation independent of the
  checker). End-to-end verified both by a `cc`-invoking test and by
  the supervisor manually compiling and running the emitted C outside
  any test harness, observing exit code 42.
- **10.4 — real checked i32 arithmetic** (`runtime/`,
  `compiler/internal/backend`): the typed IR's `CheckedArithmetic`/
  `CheckedNegate` nodes retain fault-category semantics with
  "release-mode response left to phase 10" (spec 06b) — this closes
  that, rather than silently emitting raw C `+`/`-`/`*` and dropping
  the language's defined overflow behavior. `pebble_rt.h` gains
  `PEBBLE_PANIC_ARITHMETIC_OVERFLOW` and four `pebble_rt_checked_*_i32`
  helpers; `runtime/src/arith.c` implements SAFE-mode overflow panics
  (via the compiler's own `__builtin_*_overflow`) and RELEASE-mode
  defined wraparound (via unsigned arithmetic, never signed-overflow
  UB) — including negation's one overflowing boundary, `-INT32_MIN`.
  The backend now accepts a small recursive i32 expression tree for an
  entry's return value (literals, checked negation, checked
  `+`/`-`/`*`) instead of only a bare literal, emitting the runtime
  helpers rather than raw operators; division/modulo are cleanly
  rejected as needing a separate divide-by-zero fault category. Both
  runtime configurations and the end-to-end Pebble-source-to-exit-code
  path (including a real overflow abort with the correct panic
  message) were independently re-verified outside the dispatched
  tests.
- **10.5 — checked i32 division and modulo** (`runtime/`,
  `compiler/internal/backend`): closes the divide-by-zero fault
  category 10.4 deferred. Unlike overflow, division by zero has no
  defined release-mode answer — no bit pattern to wrap to — so
  `pebble_rt_checked_div_i32`/`mod_i32` panic with
  `PEBBLE_PANIC_DIVIDE_BY_ZERO` in **every** configuration, not just
  `PEBBLE_RT_MODE_SAFE` (independently confirmed: divide-by-zero
  aborts in both a SAFE and a RELEASE runtime build). `INT32_MIN / -1`
  follows the overflow convention (SAFE panics, RELEASE wraps to
  `INT32_MIN`); `INT32_MIN % -1` is mathematically `0` and
  representable, not a fault in either mode. Both cases are
  special-cased before ever evaluating C's `a/b` or `a%b` for that
  pair, since the language itself calls that evaluation undefined
  behavior for this input, not just an out-of-range result. The
  backend maps `/` and `%` to the new helpers alongside `+`/`-`/`*`;
  all five source-level arithmetic operators are now supported for an
  i32 entry's return expression.
- **10.6 — local i32 variables in the entry body**
  (`compiler/internal/backend`): an i32 entry's body may now be zero
  or more `let <name> i32 = <expr>;` declarations followed by the
  final return, where any expression may reference a local declared
  earlier in the same body. Each local emits one `const int32_t
  pebble_local_<symbolID>` declaration, named deterministically from
  stable IR identity rather than an emission-time counter — the exact
  naming discipline the phase-10 inventory (§L2) flagged the old
  backend for getting wrong. Reassignment, non-i32 locals, and any
  statement other than a local declaration or the final return are
  rejected cleanly. Confirmed overflow checking survives through a
  local reference, not just literal operands, both by the dispatched
  test and by inspecting the emitted C directly.
- **10.7 — comparisons and if/else as the entry body's tail**
  (`compiler/internal/backend`): the final statement may now be a
  two-armed `if <comparison> { return <expr>; } else { return <expr>;
  }` instead of only a bare return. All six comparisons (`<`, `<=`,
  `>`, `>=`, `==`, `!=`) are supported and emit the plain C operator
  directly (comparing two integers cannot overflow); locals declared
  earlier in the body are visible in the condition and both arms.
  Deliberately narrow: only the two-armed, both-branches-return shape,
  each arm exactly one return (no locals or nested `if` inside a
  branch) — everything else is a clean rejection. Real finding along
  the way: a bare comparison between untyped integer literals (`if 1 <
  2`) types both operands as the platform `int` builtin, not `i32` —
  `return`/`let x i32 = ...` positions anchor literals to `i32` via
  the checker's expectation mechanism, but a comparison condition has
  no such anchor unless one side is already an i32 value. Handled by
  lowering an `int`-typed literal operand directly rather than
  loosening the emitter's i32-only discipline anywhere else. Confirmed
  independently that the emitted if/else compiles clean under `-Wall
  -Wextra -Werror` with no "control reaches end of non-void function"
  warning — no defensive fallback needed.
- **10.8 — generalize entry bodies to a recursive block grammar**
  (`compiler/internal/backend`): collapses 10.7's three ad hoc
  validators into one recursive rule — a block is zero or more locals
  followed by a tail that's either a return or a two-armed if/else
  whose arms are themselves blocks under the same grammar. An if arm
  can now contain its own local declarations and nested if/else,
  removing 10.7's "exactly one return per arm" restriction. Local
  scope isolation is real: every recursive block entry clones the
  locals set before extending it, verified two ways — an end-to-end
  test where both arms declare a same-named source local (distinct
  symbol IDs) proving the arms' C names don't collide, and a
  hand-built unit where one arm's return deliberately references the
  sibling arm's local, confirmed rejected. Nested indentation
  confirmed by eye on a three-level-deep fixture and independently
  compiled and run outside the dispatched tests. Every 10.2–10.7 test
  still passes unmodified except the two whose now-supported shape
  became the new positive cases.
- **10.9 — mutable `var` locals and reassignment**
  (`compiler/internal/backend`): extends the block grammar's leading
  statements with `tir.Store` (reassignment) alongside `Initialize` — a
  `var x i32 = ...;` declared local can now be reassigned via `x =
  <expr>;` anywhere it's in scope. The checker already refuses a
  `Store` targeting a `let` (C0606) before typed IR exists, confirmed
  directly, so any `Store` this backend sees necessarily targets a
  `var`. Deliberate simplification: every local now emits as plain
  `int32_t` rather than 10.6's `const int32_t`, since the `Initialize`
  node doesn't carry let-vs-var and the checker's own guarantee makes
  `const` pure defense-in-depth here — documented in place, not
  dropped silently. This is the prerequisite for `while` loops: a loop
  that can only declare fresh immutable locals each iteration can't
  accumulate anything, so mutation had to land before iteration is
  worth adding. Overflow checking confirmed to survive through a
  reassignment, both by the dispatched test and independently outside
  the test harness.
- **10.10 — `while` loops** (`compiler/internal/backend`): adds `while
  <comparison> { <loop body> }` as a block statement (never the tail).
  The loop body has its own grammar — `Initialize`/`Store` only, no
  required tail, no nested `if`/`while` yet, since a loop body just
  runs statements — scoped and cloned exactly like an if arm.
  `Initialize`/`Store` handling was refactored into a shared
  `buildLeadingStatement` so `buildBlock` and the new `buildLoopBody`
  don't duplicate it. First construct where a bug could hang the
  compiled program itself rather than just misbehave or cleanly
  reject: every end-to-end loop test runs the binary under a context
  timeout that distinguishes a real abort from a genuine
  non-termination, verified directly by an overflow-inside-a-loop test
  that proves the harness identifies the abort correctly rather than
  mistaking it for a timeout. A real accumulation loop (`sum of 0..4 =
  10`) compiles and runs correctly, confirmed both by the dispatched
  test and by compiling and running the emitted C independently
  outside any harness.
- **10.11 — `if` and nested `while` inside a loop body**
  (`compiler/internal/backend`): closes 10.10's deferred gap. A loop
  body may now contain `if` (else optional — a loop-body `if` doesn't
  need to guarantee a return, unlike `if`-as-tail) and nested `while`
  loops. `buildLoopIf` is a separate function from `buildIf` since the
  grammar genuinely differs (arms are loop bodies with no required
  tail); nested `while` reuses `buildWhile` unchanged since it already
  recurses into `buildLoopBody`. Each arm/nested loop gets its own
  cloned locals scope. A real nested double-loop (3×3) and a loop-body
  `if`/`else` both compile and run correctly, confirmed by the
  dispatched tests and independently outside the harness.
- **10.12 — `break` and `continue` inside a loop body**
  (`compiler/internal/backend`): adds `tir.Break`/`tir.Continue` as
  loop-body statements. The language has no labeled break/continue (no
  label syntax exists at all — confirmed by grep, not assumed), so a
  jump's `Target` always names the nearest enclosing loop, and plain C
  `break;`/`continue;` (which already target the nearest enclosing
  loop by C's own scoping) is a direct, correct translation — no need
  to consult `Target`'s value at all, confirmed against a real
  nested-loop fixture rather than assumed. A non-empty `DeferChain` is
  rejected cleanly rather than silently dropped, since this backend
  doesn't lower `defer` yet and the checker does accept `defer` inside
  a loop body. Verified end-to-end: a `break` exits a sum-until-5 loop
  at exit code 10, `continue` skips one term for exit code 12, and a
  `break` inside a nested loop correctly targets only the inner loop —
  confirmed by compiling and running the emitted C independently
  outside the harness.

**Baseline.** `main` at `4b1be4d` ("compiler: render Related labels in text
output, add JSON diagnostic renderer"). Phases 01–09 are complete and 07
(generics) is fully closed — including the 07.8 expression-body fix and the
recorded-but-deferred `T0501` string-literal-expression-body defect, neither
of which touches this phase's scope.

**Sources read for this inventory (all read-only; nothing under `src/`
or `compiler/` was modified):** `src/codegen.c` (4,318 lines, in full),
`src/codegen.h`, `src/type.c` (1,317 lines, in full), `src/type.h`,
`src/alloc.c`, `src/temp_alloc.c`, `src/options.c`/`src/options.h`,
`src/main.c`, `src/module.h`, `src/symbol.h` (grep), and targeted
sections of `src/checker.c` (monomorphization, implicit-cast insertion,
entry-point verification). The governing spec is
`spec/compiler/10-c-backend-and-runtime.md`, read in full (67 lines).

## 1. The load-bearing architectural findings first

The spec says the backend input is *"lowered, fully typed IR"* and that the
backend *"does not perform name resolution, infer types, insert semantic
conversions, or discover generic instances while printing"*
(`10-c-backend-and-runtime.md:3-7`). The old backend fails this on nearly
every axis, and in a specific way: **it does not consume a resolved
intermediate at all.** It consumes the live, *mutated* compiler state —
the checker's symbol tables, type tables, and ASTs — and makes semantic
decisions during emission. The four findings below are the load-bearing
ones; everything in section 3 follows from them.

### 1a. Emission re-walks the live checker state and mutates it

`emit_program` (`src/codegen.c:509-1122`) iterates the checker's own
`global_scope->symbols`, `module_table`, `canonical_type_table`, and
`anonymous_funcs->symbols` uthash tables with `HASH_ITER` to discover what
to emit — the emit pass is effectively a second traversal of the whole
program's name space, reconstructing structure that a typed IR would have
made explicit:

```c
// codegen.c:514-517
HASH_ITER(hh, global_scope->symbols, sym, tmp) {
  if (sym->kind == SYMBOL_VARIABLE || sym->kind == SYMBOL_CONSTANT || ...)
```

The same two blocks (global scope, then every module's scope) are written
out three times with near-identical bodies — extern declarations
(`codegen.c:509-649`), definitions (`codegen.c:651-730`), prototypes
(`codegen.c:732-808`), and definitions again (`codegen.c:882-987`). The
`main` module's function named `main` is detected by string comparison and
renamed to `__user_main` in each of the four copies
(`codegen.c:745-748`, `898-901`, `951-954`).

Emission also *side-effects* shared state. `emit_type_name` calls
`mark_type_used` (`codegen.c:1126`), which mutates a `used` flag on
every `Type` reachable from anything printed — the "only emit used types"
pass then filters `canonical_type_table` by that flag
(`codegen.c:994-1012`). And at the very end `emit_program` tears down the
checker's tables:

```c
// codegen.c:1117-1121
HASH_CLEAR(hh, global_scope->symbols);
HASH_CLEAR(hh, type_table);
HASH_CLEAR(hh, canonical_type_table);
HASH_CLEAR(hh, anonymous_funcs->symbols);
```

So codegen is not a clean consumer of a finished artifact; it is a
second phase that walks, marks, and finally destroys the checker's state.
In the new architecture the backend should consume the typed IR unit
(`handoff`/TIR from 06b/07) and produce output without reading any
`global_scope`, `module_table`, or shared type table — those tables are
a 04b/06a-era implementation detail that TIR replaces.

### 1b. Generic instances are produced by checker mutation, not consumed from IR

The old backend never prints a generic function; it skips
`TYPE_GENERIC_FUNCTION` symbols in all four loops
(`codegen.c:736-739`, `885-888`, etc.). Instances come from the checker:
`monomorphize_function` (`src/checker.c:4408-4524`) deep-clones the
generic's AST (`clone_ast_node`, `checker.c:4434`), textually substitutes
type parameters (`substitute_function_types`, `checker.c:4437`), **re-runs
the type checker on the clone** (`check_function_signature` /
`check_function_body`, `checker.c:4496-4506`), inserts the clone's symbol
into `checker_state.current_module->scope` (`checker.c:4475`), and rewrites
the call expression to reference the clone (`checker.c:5269-5284`). The
cache is `mono_instances`, keyed by a mangled name like `add__int__bool`
(`checker.c:3245-3246`, `type.c:43`).

So the "generic instance discovery" happens at check time by mutating the
program, and codegen's job is just to not accidentally print the templates
and to pick up the clones from the polluted scope. This is precisely what
07's typed-IR specialization (`specialization.go`, `specialization_build.go`,
`Instantiation` nodes in `tir/node.go`) replaces: the IR now *is* the set of
monomorphized functions, with `TypeArgs` on every declaration and call site.
The old mechanism — clone AST, re-check, register symbol, rewrite call — is
not a design to port; it is the pre-IR way of doing what the IR already
does, and the backend must simply iterate `unit.Functions` /
`FunctionDeclaration`s instead of re-walking symbol tables.

### 1c. Semantic conversions are inserted while printing

The checker does insert explicit cast nodes (`maybe_insert_cast`,
`checker.c:2080-2149`, producing `AST_EXPR_IMPLICIT_CAST`), but codegen
then *re-derives the conversion at print time* by inspecting the
resolved source and target types:

```c
// codegen.c:3556-3584 — AST_EXPR_IMPLICIT_CAST
if (src_type->kind == TYPE_ARRAY && target->kind == TYPE_SLICE) {
  // Special: construct slice from array
  ... write_expression("(type){ &src.data[0], src.len }");
} else {
  // General cast, e.g., (float)int
  write_expression("(target)"); write_expression(temp);
}
```

The array→slice coercion (a semantic decision about how a value of one
type becomes a value of another) is made in codegen at emit time, keyed
off the two `Type*`s. The explicit-cast path even does type-punning
(`*(T*)&temp`, `codegen.c:3593-3597`). The typed IR should already contain
explicit conversion/lowering nodes with all the information needed; the
backend should print them, not re-derive which composite literal to
fabricate.

### 1d. The output is built with ad hoc string buffers, and runtime text is pasted in

Generated C is assembled in four growable `char*` "sections"
(`forward_types` / `type_defs` / `forward_vars_funcs` / `defs`, see
`codegen.h:39-48` and `append_to_section`, `codegen.c:22-56`), which are
grep-broken `realloc`+`memset` buffers. On top of those sits a single
global, fixed-size expression buffer shared by all expression emission:

```c
// codegen.c:14-20, 67
ExpressionBuffer expression_buffer = {0};
const size_t expression_buffer_size = 2048;
...
assert(expression_buffer.length + len < expression_buffer_size);
```

An expression whose textual form exceeds ~2 KB aborts the compiler on an
`assert`. The whole backend interleaves *emitting C statements* into the
current section (temporary declarations, assertions, `sprintf` calls) with
*building an expression string* in the shared buffer, with string lifetimes
managed by a single global `temp_allocator` that is `temp_reset` at
arbitrary statement boundaries — one of the workaround comments says so
explicitly:

```c
// codegen.c:2411-2413 (switch statement, non-tagged path)
// NOTE: Since we're evaluating upfront, we need longer living memory than
// temp As the bodies will be evaluated, then temp will be freed and clear
// these
```

And the runtime is not separated at all: the context/allocator structs,
assert helper, malloc wrappers, and the C `main` are `fputs`'d as literal
strings into every generated file (`emit_sections`, `codegen.c:1309-1451` —
detail in §3 rows below). That is the exact behavior the spec's
"Proposed layout" (`runtime/include/pebble_rt.h`, `runtime/src/*.c`,
`10-c-backend-and-runtime.md:40-48`) says must become a versioned runtime.

### 1e. Summary of cross-cutting verdicts

| Cross-cutting mechanism | Verdict |
| --- | --- |
| Consuming live checker state at emit time (`§1a`) | **REMOVE** — TIR supplies the program structure |
| Checker-mutation generics (`§1b`) | **REMOVE** — 07's specialization is the IR-native replacement |
| Semantic conversion at print time (`§1c`) | **REMOVE** — IR must carry explicit conversions |
| String-buffer sections + 2 KB expression buffer (`§1d`) | **REDESIGN** — the section ordering *concept* is needed (C requires decl-before-use) but the mechanism is trivially a structured emitter in Go; the fixed-size shared expression buffer is gone with it |
| Temp naming (`__temporary_var_%ld`, static loop counter, hardcoded `_i`) | **REDESIGN** — deterministic names derived from IR node identity, not a global counter |
| Emitted-runtime-as-literal-text (`§1d`) | **REDESIGN** — becomes the versioned runtime per spec's proposed layout |

## 2. Inventory against the spec's two checklists

Each row: **what the old backend does** (with citations and a representative
quote), then a **verdict** and the reasoning. The two lists come straight
from `10-c-backend-and-runtime.md:9-18` (lowering) and
`10-c-backend-and-runtime.md:28-38` (runtime ABI). Rows 1–8 are lowering
items; rows 9–17 are ABI items; rows 18–20 are additional behaviors the
old backend has that the new architecture must dispose of. Line numbers
refer to `src/codegen.c` unless otherwise noted.

### Lowering list (spec §"Backend input")

#### L1. Expression evaluation order

**Current behavior.** None. Expressions are emitted by recursive descent
into the shared expression buffer (`emit_expr`, `codegen.c:3001-4318`); the
emitted C expression is evaluated in C's order, and where a sub-expression
must be evaluated exactly once the backend emits a temp declaration as a
statement in the middle of the enclosing block. Examples: binary ops emit
both operands into temps then combine (`codegen.c:3389-3394`); slice
construction emits array/start/end into temps then builds the slice
literal (`codegen.c:3712-3822`); array-repeat emits a whole `for` loop as a
statement and references the array temp afterwards (`codegen.c:3638-3710`).
There is no model of evaluation order anywhere — it is inherited from C, and
the order of temp-statement emission is whatever the recursion happens to do.

**Verdict: REMOVE (nothing sound to inherit).** The old mechanism is "C
decides, modulo when the backend gets scared and materializes a temp."
Typed IR already has an explicit value/control structure (regions, values,
orderings from 06b) that *is* an evaluation-order statement; the lowering
spec's job is to map that onto C, not to port "recursively walk the AST and
hope." There is no old-backend decision worth preserving here.

#### L2. Temporary creation and lifetime

**Current behavior.** `get_temporary_name` (`codegen.c:274-281`) hands out
`__temporary_var_%ld` from a monotonically increasing `temporary_count` on
the `Codegen` struct; `_loop_i%d`/`_loop_start%d`/etc. come from a `static
int loop_counter` (`codegen.c:2592-2599`); the array-repeat fill loop uses a
hardcoded `_i` (`codegen.c:3683`) that collides if the loop is nested inside
another `_i` loop. Temps are emitted as C declarations at the current
emission position, so their C lifetime is the enclosing block. The
expression strings those temps name live in the global `temp_allocator`,
reset per statement (`temp_reset(&temp_allocator)` at the end of most
statement cases, e.g. `codegen.c:2244, 2698, 2809, 2819, 2938, 2970`).

**Verdict: REDESIGN.** Two independent problems: (a) the naming is a
collision-prone, non-deterministic global counter (the hardcoded `_i` is an
actual collision bug); (b) the temp *string* lifetime is a hidden global
allocator discipline rather than the C block scoping that actually gives
the temp its lifetime. Done well: lowering assigns temps with deterministic
names derived from IR node identity (function/region/value id), and the
emitter declares them in C blocks that match the IR scope, so C's own
scoping is the lifetime mechanism. A name collision in generated C is a
correctness bug; the new backend must make it impossible by construction.

#### L3. Defer expansion on every exit edge

**Current behavior.** A `DeferStack` with `DEFER_SCOPE_BLOCK/LOOP/FUNCTION`
(`codegen.h:19-32`, `codegen.c:101-226`). `defer` pushes the deferred
statement's AST onto the current scope's stack (`codegen.c:2262-2266`).
At scope exit, `defer_stack_emit_current_scope` re-emits each deferred
statement in reverse (`codegen.c:133-156`); on `break`/`continue`,
`defer_stack_emit_until` emits up to the loop boundary (`codegen.c:158-186`,
used at `codegen.c:2032, 2040`); on `return`, `defer_stack_emit_all` emits
every scope up to the function (`codegen.c:188-215`, used at
`codegen.c:2249`). Each deferred statement is wrapped in `{ /* defer */ }`
and emitted *textually* at the exit point — the same AST is emitted once
per exit edge it must run on. A `locked` flag on the current stack
(`codegen.c:139, 192`) suppresses double emission.

**Verdict: REDESIGN.** The concept — defer is compiler lowering and must
expand on every exit edge (spec `10-c-backend-and-runtime.md:56`) — is
right and must be carried. The implementation is wrong in shape: it
re-emits the deferred statement's *AST* at every exit, which duplicates
the deferred code in the output, and because only the *current* stack is
`locked` (`codegen.c:192` sets `stack->locked` but walks parents only to
emit), a `return` inside a nested block leaves the parent scopes unlocked
so their defers get emitted a second time in dead code when the enclosing
block closes. The new lowering should expand defers into explicit
`Defer`/cleanup nodes on each exit edge *once*, in the IR, before printing —
so the printer sees the already-expanded exit edges and emits no
`{ /* defer */ }` wrapper hacks.

#### L4. Loop and switch lowering

**Current behavior.**
- `while` → C `while` (`codegen.c:2568-2590`).
- `loop start..end` → a `for` with a runtime step direction, using the
  `static` `loop_counter` names above, plus a `const int <iter> = _loop_iN;`
  iterator binding (`codegen.c:2591-2700`). Note the iterator and bounds are
  hardcoded to C `int` (`codegen.c:2609, 2619, 2628, 2676`) regardless of
  the loop variable's actual type.
- `for` → either `for (decl; cond; update)` when init is a declaration, or
  `{ init; for (; cond; update) }` when init is an assignment
  (`codegen.c:2702-2811`).
- `switch` → C `switch` for integral/enum conditions with a pre-evaluated
  temp (`codegen.c:2432-2488`); for strings, an `if/else if` chain of
  `strcmp` calls (`codegen.c:2490-2558`); for tagged unions, a `switch` on
  `.<tag>`/`->__tag` with `case <Type>__<Variant>:` labels and an
  explicit `break` after every case (`codegen.c:2336-2406`). All case
  expressions are pre-evaluated into an arena because the per-statement
  `temp_allocator` would be reset (see the `codegen.c:2411-2413` quote in
  §1d).

**Verdict: REDESIGN.** The dispatch *policies* (string switch → strcmp
chain; tagged-union switch → tag enum) are decisions the lowering spec must
own, and the current split of "which policy" across `cond_type->kind` at
print time is exactly the kind of semantic dispatch the new backend must
not do. Two concrete defects worth carrying forward as warnings: the `loop`
iterator hardcodes `int` even for e.g. `usize`/`i64` bounds
(`codegen.c:2609-2676`), and the tagged-union switch only works because the
tag enum layout (`enum { T__A, ... } __tag;`, see §L10) is baked into the
emitted struct — the tag representation itself is an ABI decision (§A5)
that the lowering must then rely on consistently.

#### L5. Aggregate construction

**Current behavior.** Struct literals emit C compound literals with
designators, field values pre-evaluated into temps
(`codegen.c:4253-4308`). Tuples emit `(tupletype){ a, b }`
(`codegen.c:4085-4103`). Arrays emit `(arraytype){ {elem,...}, N }`
(`codegen.c:3607-3636`). Array-repeat emits a statement-expression with a
fill loop, or `{0}` when the repeated value is a zero literal
(`is_zero_value`, `codegen.c:297-337`, used at `codegen.c:3638-3650`). The
C representations are: array = `struct { T data[N]; size_t len; }`
(`codegen.c:1695-1704`), slice = `struct { T *data; size_t len; }`
(`codegen.c:1704-1712`). Several constructions rely on GNU statement
expressions `({ ... })` and `alloca` (`codegen.c:4029-4064, 2138-2141`),
which are not standard C.

**Verdict: REDESIGN.** "Emit a compound literal" is a sound emission
target, but the aggregate *representations* (array carrying a `len`, slice
as ptr+len, tuple as an anonymous struct) are ABI decisions (§A2) and the
"decide to emit a fill loop vs `{0}`" at print time is lowering work that
the IR should already have done. The GNU-extension reliance
(statement expressions, `alloca`) should be eliminated or made an explicit,
documented target choice.

#### L6. Optional and tagged-union operations

**Current behavior.** Optional = `struct { T value; bool has_value; }`
(`codegen.c:1712-1720`). `some(x)` → `(optional_T){ x, true }`
(`codegen.c:3494-3505`). Force unwrap (`x!`) → temp, `__pebble_assert(tmp.has_value, "unwrap", file, line)`, then `.value`
(`codegen.c:3507-3542`). Tagged union = `struct { enum { T__A, ... } __tag;
union { ... } __data; }` (`codegen.c:1647-1683`). Member access on a tagged
union asserts the tag then reads `.__data.<member>` — *unconditionally*, not
gated on any mode (`codegen.c:4105-4240`, assertions at `4154-4168` and
`4205-4219`). Assigning a tagged-union member constructs a whole tagged
union compound literal with the tag set (`codegen.c:2832-2888`).

**Verdict: REDESIGN.** The two-slot optional (`value` + `has_value`) and the
tag/union tagged-union are reasonable *seeds* for the ABI's representation
section (§A2/A5), but the operations are ad hoc: the unwrap assert is
guarded only by `!freestanding` (so it calls a function that the
freestanding output never defines — see §A3 and §1d), the tagged-union
member asserts ignore the mode entirely, and "assign a member" is
implemented by fabricating an entire aggregate. In the new design the
representations go in the ABI and the operations are either lowering
(accessed through representation-stable IR nodes) or runtime (panic on
mismatched access), not per-site emitted text.

#### L7. Bounds and unwrap checks

**Current behavior.** Ad hoc, mode-dependent, and inconsistent:
- Indexing (string / array / slice) bounds-checks **only** when
  `mode_is_safe()` (`codegen.c:3830`, `3894`), where `mode_is_safe()` is
  true only for `RELEASE_DEBUG` and `RELEASE_SAFE`
  (`options.c:319-322`) — so `--release` (`RELEASE_DEFAULT`) and
  `--release-small` silently drop every index check.
- String index uses `strlen` on a `const char *` (`codegen.c:3854-3869`).
- Slice indexing additionally asserts the `data` pointer is non-NULL with
  the message `"slice pointer use-after-free"` (`codegen.c:3939-3951`).
- Force unwrap asserts unconditionally in non-freestanding builds
  (`codegen.c:3523-3536`), with a comment claiming the intent:
  `// Should always assert unwraps, even in release modes` (`codegen.c:3524`)
  — which is *not* true of the sibling checks (bounds are dropped in
  `--release`), so the policy is not even self-consistent.
- Tagged-union member access asserts unconditionally (`codegen.c:4154`,
  `4205`).
- Slice construction performs no bounds checks at all (`codegen.c:3712-3822`).

Every check funnels through the same `__pebble_assert` helper, which is
emitted only in non-freestanding output (`codegen.c:1338-1351`) and which
delegates to libc's `__assert_fail` / `__assert`:

```c
// codegen.c:1339-1349
static void __pebble_assert(bool condition, const char *what, const char *file, size_t line) {
  if (!condition) {
#ifdef __ASSERT_FUNCTION
    __assert_fail(what, file, line, __ASSERT_FUNCTION);
#else
    (__builtin_expect(!condition, 0) ? __assert (what, file, line) : (void)0);
#endif
  }
}
```

**Verdict: REDESIGN.** There are three real problems: (1) the *policy* —
which checks run in which configuration — is scattered across the backend
as independent `if (mode_is_safe())` / `if (!freestanding)` gates that
contradict each other (and, concretely, freestanding output calls
`__pebble_assert` which the freestanding preamble never defines — a
generated-C-does-not-compile bug for freestanding programs that use
indexing or tagged-union access; the unwrap assert at `codegen.c:3523`
happens to be correctly suppressed in freestanding, which only shows the
per-site gating is not uniform); (2) the checks are emitted as
inline statements whose text is regenerated at every site, so the check
itself is not one versioned thing; (3) the failure path is host-libc
`assert`, unsuitable for the runtime's own panic channel
(§A3). Done well: the ABI defines one panic entry, the safe/release/
freestanding configuration (§A6) determines *once* whether a check is
emitted, and lowering emits a small set of check patterns (index bounds,
unwrap, tag-match) in terms of that single entry.

#### L8. Pebble-to-C calling convention adaptation

**Current behavior.** `CALL_CONV_PEBBLE` means "has `__pebble_context
context` as first parameter": every prototype and definition prepends
`__pebble_context context` (`codegen.c:756, 793, 909, 962`, and
`codegen.c:820-831` for anonymous functions), and every call site
textually injects the identifier `context` as the first argument
(`codegen.c:3995-3998`). `CALL_CONV_C` functions get no context parameter
(`codegen.c:754-758`). The entry adapter (`codegen.c:1414-1451`, §A4)
constructs the context and calls the renamed `__user_main`. There is no
other adaptation — no marshalling, no return-value treatment; the generated
function *is* the C function.

**Verdict: REDESIGN.** Threading a magic identifier named `context` works
only because the backend controls every call site and never lets a user
binding shadow the name — that is a fragile implicit contract, not an ABI.
The typed IR already threads context as an ordinary lowered parameter where
needed, and the ABI section must pin down the calling convention (which
functions carry context, what the entry signature is). The *concept* —
a first-parameter context for Pebble-convention functions — is fine and is
the seed of the ABI's "calling convention interoperation" section (§A7).

### Runtime ABI list (spec §"Runtime ABI")

#### A1. Context and allocator layout

**Current behavior.** Emitted verbatim into every generated file
(`codegen.c:1353-1363`):

```c
typedef struct __pebble_context __pebble_context;
typedef struct Allocator {
  void *ptr;
  void *(*alloc)(__pebble_context, void *, size_t);
  void *(*realloc)(__pebble_context, void *, void *, size_t);
  void (*free)(__pebble_context, void *, void *);
} Allocator;
struct __pebble_context {
  Allocator default_allocator;
};
```

The same shapes are modeled in the compiler's own type system
(`type.c:604-655`, where `type_context->canonical_name = "__pebble_context"`
at `type.c:610` and the allocator function types are built at
`type.c:612-643`), so the *compiler and the emitted text agree by
construction* — which is exactly what the spec's versioned ABI is trying to
make explicit rather than implicit.

**Verdict: ACCEPT (the seed), REDESIGN (the packaging).** The underlying
decision — a context struct carrying an allocator vtable of
`alloc`/`realloc`/`free` function pointers — is a sound ABI seed and should
be carried into `runtime/include/pebble_rt.h`, not dropped. What must
change: (a) it becomes one versioned header + `runtime/src/*.c`, not a
`fputs` literal; (b) the function-pointer struct passes `__pebble_context`
**by value**, which makes the context a recursive-by-value type and forces
the forward-typedef dance — the ABI must decide by-value vs by-pointer
deliberately; (c) there is only `default_allocator` with no
initialization/shutdown (§A8) and the `ptr` field is never used by the
default impls (§A2/A9).

#### A2. String, slice, optional, enum, and tagged-union representation

**Current behavior** (all as emitted `typedef struct` layouts in
`emit_type_if_needed`, `codegen.c:1460-1733`):
- `str` → C `const char *` (`emit_type_name`, `codegen.c:1136`) — a
  NUL-terminated string; bounds checks therefore use `strlen`
  (`codegen.c:3858`).
- slice `[]T` → `struct { T *data; size_t len; }` (`codegen.c:1704-1712`).
- array `[N]T` → `struct { T data[N]; size_t len; }` (`codegen.c:1695-1704`)
  — arrays carry a `len` even though the length is statically known.
- optional `?T` → `struct { T value; bool has_value; }` (`codegen.c:1712-1720`).
- enum → C `enum { T_V0, T_V1, ... }` plus a `static const char *
  T__names[]` string table for printing (`codegen.c:1530-1577`).
- tagged union → `struct { enum { T__A, ... } __tag; union { ... } __data; }`
  (`codegen.c:1647-1683`).
- tuple → anonymous `struct { T _0; T _1; ... }` with single-char field
  names and an explicit acknowledged limit:
  `// NOTE: This will break past 10 elements 0-9` (`codegen.c:1689-1691`).

**Verdict: REDESIGN.** The per-type *shapes* are legitimate candidates for
the ABI's representation section, but they come with bad defaults and
leakage: `str` as `const char*` buys `strlen`-based bounds checking and no
length prefix; arrays carrying a redundant `len` field bloat every array
and make array-vs-slice almost identical in C; tuple field names `_0`..`_9`
have a hard limit; the enum `__names` table is print-support leaking into
the type layout. The ABI must choose these representations once and
deliberately (including the exact-width/word-size rules from
`10-c-backend-and-runtime.md:20-23`), and the compiler must emit *against*
that header rather than re-deriving layouts per program.

#### A3. Assertion, bounds-failure, unwrap-failure, and panic behavior

**Current behavior.** A single emitted `__pebble_assert` that bottoms out in
libc's `__assert_fail`/`__assert` (`codegen.c:1338-1351`, quoted in L7),
only in non-freestanding output; every safety check call site feeds it
(`codegen.c:3525, 3854, 3920, 3940, 4154, 4205`). There is no other panic or
failure path — no distinct bounds/unwrap/panic reporting, no freestanding
behavior, no abort/exit ownership.

**Verdict: REDESIGN.** The "one assert helper inlined into the program" is
replaced by the runtime's `panic.c`: a versioned panic entry
(`pebble_panic`/assert-failure reporter) that generated code and the
runtime both call, with a hosted implementation (print + abort) and a
freestanding contract. As noted in L7, today's version has the concrete
defect that freestanding output omits the definition while call sites still
reference it, so "route every check through a single runtime panic entry"
fixes both the inconsistency and the freestanding break.

#### A4. Entry-point adapter and argument representation

**Current behavior.** The checker verifies the entry signature
(`verify_entry_point`, `checker.c:7590-7735`): `main` must be Pebble-
convention, return `int`, and have 0, 1 (`argv []str`), or 2
(`argc int, argv []str`) parameters (`checker.c:7623-7702`). Codegen then
emits a hardcoded C `main` as a literal string (`codegen.c:1414-1451`):

```c
// codegen.c:1440-1448 (freestanding branch omitted)
if (entry_sym->type->data.func.param_count == 1) {
  fputs("  slice_str __argv = { argv, argc };\n"
        "  return __user_main(context, __argv);\n", cg->output);
} else if (entry_sym->type->data.func.param_count == 2) {
  fputs("  return __user_main(context, argc, argv);\n", cg->output);
} else {
  fputs("  return __user_main(context);\n", cg->output);
}
```

`main` is renamed `__user_main` in prototypes and definitions
(`codegen.c:745-748`, etc.). Non-`main` entry points are verified as
`void` and C-convention (`checker.c:7704-7722`).

**Verdict: REDESIGN.** Two concrete mismatches make this the clearest
"adapter needs a real ABI" row: (1) for 2-parameter `main`, the adapter
passes raw C `argc`/`argv` (`const char **`) straight through
(`codegen.c:1445`), while the checker's own verification says the second
parameter is `[]str` (`checker.c:7677-7691`) — the slice wrapping that the
1-parameter path does (`slice_str __argv = { argv, argc }`,
`codegen.c:1441`) is skipped in the 2-parameter path, and `{ argv, argc }`
assigns the `int argc` into a `size_t len`; (2) the whole thing is a
hardcoded string with param-count branching, not a defined ABI contract.
The concept — a synthesized C `main` that builds the context and adapts
host arguments into the language's argument representation — is exactly the
"entry-point adapter and argument representation" item the ABI must define
(`10-c-backend-and-runtime.md:32`), and should be carried into the runtime
platform layer (`runtime/src/platform_<target>.c` in the proposed layout)
rather than regenerated per program.

#### A5. Allocation hooks

**Current behavior.** Emitted per program (`codegen.c:1365-1382`):

```c
void *__pebble_c_alloc(__pebble_context ctx, void *ptr, size_t size) {
  void *data = malloc(size);
  memset(data, 0, size);   // zeroing allocator
  return data;
}
void *__pebble_c_realloc(__pebble_context ctx, void *ptr, void *data, size_t new_size) {
  return realloc(data, new_size);   // 'ptr' and 'ctx' ignored
}
void __pebble_c_free(__pebble_context ctx, void *ptr, void *data) { free(data); }
```

**Verdict: REDESIGN.** The concept (default allocator hooks wired into the
context's vtable) is sound and becomes `runtime/src/memory.c`, but the
defaults have problems to fix in the ABI: `malloc(0)`/zero-size behavior is
unspecified, `realloc`'s separate `ptr` argument is dead, and the zeroing
`malloc`+`memset` contract (does `alloc` guarantee zeroed memory?) is
implicit. Also, `ctx` and `ptr` being passed but unused means the hooks
can't be non-default implementations that need context — the ABI signature
should be set deliberately, not inherited from this first draft.

#### A6. Safe, release, and freestanding configurations

**Current behavior.** Two global switches consulted ad hoc throughout
codegen: `compiler_opts.freestanding` and `mode_is_safe()`
(`options.c:319-322`). The release mode also changes the *C compiler*
flags (`release_mode_string`, `options.c:354-389`: `-fsanitize` in debug,
`-Os`/`-O2` in release), i.e. safety is a mix of (a) whether codegen emits
check text, gated inconsistently (L7), and (b) whether the host compiler
adds sanitizers. Freestanding additionally changes the emitted preamble
(`codegen.c:247-257`) and drops the allocator/assert emission
(`codegen.c:1338-1351, 1365-1382`) but not the call sites.

**Verdict: REDESIGN.** The spec wants a defined set of configurations
(safe / release / freestanding, `10-c-backend-and-runtime.md:36`). The old
backend's notion is a pile of booleans whose interactions are contradictory
(see L7's freestanding-`__pebble_assert` bug and the `--release` drops-bounds
but-keeps-tag-asserts inconsistency). This becomes a small, explicit ABI
concept (a configuration enum/macros in `pebble_rt.h`) plus one place in
the compiler that decides which checks are emitted.

#### A7. C calling convention interoperation

**Current behavior.** `CALL_CONV_C` functions are emitted without the
context parameter (`codegen.c:754-758`), and `extern` C functions are
declared with `extern` and their C types (`codegen.c:553-578, 622-647`).
Variadic C functions get special handling at call sites: the extra
arguments are packed into an `alloca`-backed slice passed as the last
parameter, using a GNU statement expression (`codegen.c:4000-4071`).

**Verdict: REDESIGN.** The primitive exists (C functions don't thread
context; externs are declared), but the variadic path is a GNU-extensions
`({ ... })` construction (see L5) and the "which convention" decision is
made per call site from the function type at print time
(`codegen.c:3995-3998`). The ABI must define the interop contract
(`10-c-backend-and-runtime.md:37`) — including how variadics and slices
cross the boundary — and the compiler should emit against it, not
re-fabricate an `alloca` slice per call.

#### A8. Initialization and shutdown

**Current behavior.** None. The C `main` initializes the context inline
with the default allocator (`codegen.c:1429-1438`); freestanding uses an
empty context (`codegen.c:1424-1427`). There is no init/shutdown beyond
that, and no exit path.

**Verdict: REMOVE (nothing to inherit) / the ABI must add it.** The old
backend's absence of an init/shutdown story is not a design to port; the
ABI list explicitly includes "initialization and shutdown"
(`10-c-backend-and-runtime.md:35`), so the runtime and the entry adapter
must define one. Note this row is a genuine *gap* in the old backend — the
inventory's honest finding is "there is nothing here," which is itself
useful: don't let the new design inherit the absence.

#### A9. Runtime ABI version

**Current behavior.** None. Every program embeds its own copy of the
context/allocator/assert text (`codegen.c:1338-1382`), so there is nothing
to version — compiler and "runtime" agree only because the compiler emits
both sides.

**Verdict: REDESIGN.** The spec requires a versioned ABI
(`10-c-backend-and-runtime.md:28, 38`). The old backend's "agreement by
construction" (compiler emits both struct and program, `type.c:604-655`
matching `codegen.c:1353-1363`) is replaced by an explicit
`PEBBLE_RT_ABI_VERSION` (or similar) in `pebble_rt.h`, checked at compile
time when generated C is compiled against the runtime — the compiler
stamps the version it targets, the runtime rejects mismatches.

### Additional old-backend behaviors that must be disposed of

#### A10. Type-name and type-layout emission (cross-cutting)

**Current behavior.** C type names *are* the checker's structural canonical
names — `compute_canonical_name` builds strings like `slice_i32`,
`array_3_i32`, `tuple_i32_str`, `func_pebble_i32_ret_void`
(`type.c:684-946`); `emit_type_name` maps `TypeKind` to C spellings
(`codegen.c:1124-1214`) — notably **`TYPE_INT` → `int` and `TYPE_ISIZE` →
`ptrdiff_t`** (`codegen.c:1130, 1175`), i.e. implementation-defined C
widths, directly contradicting the new requirement that Pebble `int`/`uint`
be target-native word types and exact-width integers use `<stdint.h>`
(`10-c-backend-and-runtime.md:20-23`). Forward declarations, definitions,
dedup (uthash sets `declared_types`/`defined_types`), and a hand-rolled
dependency graph with topological sort order emitted type definitions
(`codegen.c:343-462, 989-1112`).

**Verdict: REDESIGN.** The two parts must be separated: (1) the
*dependency-ordered forward-decl + definition emission* is a genuine C
requirement and a sound concept to carry — but it should consume declared
dependencies from the typed IR's type store, not compute a dependency graph
by string-matching canonical names against a hash table at emit time; (2)
the *naming and spelling* is reworked wholesale: exact-width via
`<stdint.h>`, Pebble `int`/`uint` as word-sized `intptr_t`/`uintptr_t` per
the spec, and a deterministic name scheme that is an ABI-visible decision
(these names appear in generated C and must be stable across compiler
versions). Also worth fixing on the way out: `compute_canonical_name` for
`TYPE_OPTIONAL` reads `type->data.ptr.base` instead of
`type->data.optional.base` (`type.c:734-740`) — it only works because both
union members are first.

#### A11. Print / interpolated-string formatting

**Current behavior.** `print` and string interpolation are lowered by
*synthesizing `printf`/`sprintf` format strings and argument lists at print
time*, by walking the `Type` structs: `get_format_specifier`
(`codegen.c:1753-1782`), `build_composite_format_string`
(`codegen.c:1784-1864`), `build_composite_args` (`codegen.c:1866-2023`),
used by `print` (`codegen.c:2046-2246`) and interpolation
(`codegen.c:3064-3334`). Struct/tuple/array printing emits a two-pass
`snprintf(NULL, 0, ...)` sizing + `alloca` + `sprintf` into a buffer
(`codegen.c:2113-2156`); bools print as `true`/`false` ternaries; enums
index the emitted `T__names[]` table. This is roughly a quarter of the
file and the largest single chunk of hand-rolled runtime behavior living
inside the backend.

**Verdict: REDESIGN.** Two defensible end states: (a) a versioned runtime
formatting facility (the natural home, alongside `panic.c`/`memory.c`, so
the format logic is compiled once and versioned with the ABI), with the
compiler emitting only a format string + argument list against that
facility; or (b) a clearly-specified lowering that keeps compile-time
format construction but as a structured emitter, not the current string
assembly. Either way, "the compiler walks live `Type*` to produce a
`printf` format at emit time" — including the enum `__names` tables baked
into every type definition (`codegen.c:1557-1568`) — is not a mechanism
that survives into the new backend.

#### A12. `extern` and linkage handling

**Current behavior.** `SYMBOL_EXTERN_FUNCTION`/`EXTERN_VARIABLE`/
`EXTERN_CONSTANT` symbols are declared `extern` with their C types, gated
on `lib_name` (`codegen.c:519-541, 553-578, 588-647`); external `const` is
`extern`-declared but never defined. Global variables are emitted with
`extern` forward decls (`codegen.c:543-552`) then definitions with `{0}`
initializers or their init expression (`codegen.c:651-730`).

**Verdict: REDESIGN (thin).** The concept is ordinary C linkage and is
fine, but the new backend must express it from the IR's declaration nodes
(with the qualified/mangled names the 09/typed-IR layers own) rather than
re-deriving `full_qualified_name` strings off `sym->decl` AST fields. This
row is carried for completeness; it is the least objectionable part of the
old backend.

## 3. Verdict summary (one line each)

| # | Item | Verdict | One-line reason |
| --- | --- | --- | --- |
| L1 | Expression evaluation order | REMOVE | No mechanism exists; C order inherited; IR supplies the structure |
| L2 | Temporary creation and lifetime | REDESIGN | Global counter naming + hidden temp-string lifetime; `_i` hardcoded collision |
| L3 | Defer expansion on exit edges | REDESIGN | Concept right (lowering, per exit edge) but re-emits AST textually and double-emits via lock gap |
| L4 | Loop and switch lowering | REDESIGN | Dispatch policy chosen at print time; loop iterator hardcoded to `int` |
| L5 | Aggregate construction | REDESIGN | Compound literals fine as target; representations and zero-vs-loop choice are ABI/lowering decisions |
| L6 | Optional/tagged-union operations | REDESIGN | Representations are a seed; ops are ad hoc, unwrap gated inconsistently |
| L7 | Bounds and unwrap checks | REDESIGN | Policy scattered and self-contradictory; freestanding emits calls to undefined `__pebble_assert` |
| L8 | Pebble→C calling convention | REDESIGN | Magic `context` identifier threading; needs ABI definition |
| A1 | Context and allocator layout | ACCEPT-seed / REDESIGN-packaging | Layout is a sound ABI seed; must move to versioned header, decide by-value/ptr |
| A2 | String/slice/optional/enum/tagged-union repr | REDESIGN | Shapes are candidates but `str`=const char*, array `len`, `_0.._9` limits leak |
| A3 | Assertion/panic behavior | REDESIGN | Becomes runtime `panic.c`; today host-libc `assert` + freestanding break |
| A4 | Entry adapter and args | REDESIGN | 2-param `main` passes raw `argv` despite `[]str` verification; hardcoded string |
| A5 | Allocation hooks | REDESIGN | Becomes `memory.c`; zero-size/realloc-`ptr`/zeroing contract undefined |
| A6 | Safe/release/freestanding configs | REDESIGN | Boolean pile with contradictory interactions; needs one ABI concept |
| A7 | C calling-convention interop | REDESIGN | Exists primitively; variadic path is GNU `({})` + alloca |
| A8 | Init and shutdown | REMOVE-gap | Nothing to inherit; the ABI must define it fresh |
| A9 | Runtime ABI version | REDESIGN | Absent; agreement today is "compiler emits both sides" |
| A10 | Type-name/layout emission | REDESIGN | Canonical-name strings are the C names; `int`/`ptrdiff_t` violate spec §widths; topo-sort concept kept |
| A11 | Print/interpolation formatting | REDESIGN | ~25% of the backend is compile-time printf synthesis; move to runtime facility |
| A12 | Extern/linkage | REDESIGN-thin | Concept fine; consume from IR declarations, not `sym->decl` strings |

Notably, **nothing in the old backend earns a plain "ACCEPT, port as-is."**
The single closest item (A1) is accepted only as a *seed* for the ABI's
context/allocator section and still gets redesigned packaging. This matches
the explicit instruction that the old backend is not to be transcribed.

## 4. Proposed first slice — 10.1: the versioned runtime ABI skeleton

The inventory supports starting exactly where the spec's "Proposed layout"
points: **the runtime itself**, as pure new C, independent of the Go
compiler. Nothing else can be tested against a runtime that doesn't exist
yet: every later slice (emitting context construction, calling-convention
threading, safety checks, the entry adapter) is defined *against* the ABI,
so the ABI has to exist first — and because it is pure C, it is the one
piece of phase 10 that is fully verifiable before any Go lowering exists.
This matches how every prior phase in this repo actually ran (07 alone took
eight-plus increments, each a self-contained, independently verifiable
slice); later slices are scoped incrementally from here.

### Scope of 10.1

Create, under `runtime/`:

- **`runtime/include/pebble_rt.h`** — the versioned ABI header, carrying
  forward the old backend's sound seeds from §A1/A2/A5 with the problems
  noted there addressed:
  - an ABI version macro (A9), checked by the runtime at compile time;
  - the context and allocator layout (A1), with the by-value/pointer and
    zeroing contracts made explicit;
  - the target representations for `str`/slice/optional/tagged-union (A2)
    — these are the *emission targets* the future Go backend will print,
    so the header is the single place they're defined (exact choices are
    deliberate decisions to be recorded in `10-c-backend-and-runtime.md`
    as the ABI section is written, not ported silently);
  - a single panic/assert-failure entry (A3) and the config concept
    (A6): safe/release/freestanding as macros the *compiler* will set on
    the compiler command line and the header honors;
  - the entry-adapter contract (A4): the signature the runtime expects
    the generated `__pebble_main`-style user entry to have, and how the
    platform layer builds the context and adapts host arguments.
- **`runtime/src/context.c`** — default context construction (hosted) and
  the zeroing allocator from A5, cleaned up.
- **`runtime/src/panic.c`** — the panic/assert-failure reporter for the
  hosted configuration.
- **`runtime/src/platform_host.c`** — the hosted entry adapter
  (`main` → build context → adapt argc/argv → call the user entry),
  which is the first real implementation of A4 and the place the
  old backend's hardcoded `main` string (`codegen.c:1414-1451`) migrates to.
- A build recipe and a **smoke test**: a small hand-written C program
  that `#include`s `pebble_rt.h`, links the three `.c` files, and exercises
  context construction, an allocation via the default allocator, and a
  deliberate panic, with the test asserting the observable outcome (exit
  code / message). This is the "independently verifiable" bar: the slice
  compiles and runs with plain `cc`/`clang`, no compiler involvement.

### Deliberately excluded from 10.1 (and why)

- **Any Go code, any change under `compiler/`** — the runtime is a
  prerequisite for the emitter, not its consumer.
- **The emitter itself** (even a hello-world emission) — it would have to
  hardcode assumptions the ABI hasn't pinned down yet; emitting first
  would smuggle the old backend's implicit contracts back in.
- **`str`/slice/optional/tagged-union operation helpers** beyond what the
  header declares — representations are declared, operations arrive with
  the lowering slices that need them.
- **Freestanding platform files** — the header must not *require* the
  hosted runtime, but implementing `platform_<target>.c` for a bare target
  is its own later slice.

### Verification for 10.1

The 07-style bar: build the runtime with `clang -Wall -Wextra -Werror`
against `pebble_rt.h`, run the smoke test, and paste the real output
(`GOCACHE=/tmp/pebble-orc-gocache` style discipline applies to the repo's
Go verification once 10.2 lands; this slice has no Go to verify). No phase
01–09 file is touched.

### Follow-on slices (scoped as work proceeds, not now)

Rough direction only, in the same spirit as 07's incremental slicing:
10.2 wire `pebble_rt.h` into the first emitted program (context
construction + a Pebble-convention `main` adapter emitted against the
ABI); 10.3 the type-representation emission against the header; 10.4
safety checks through the panic entry; then expressions/statements/lowering
work in small verifiable increments. Each will get its own brief when its
turn comes.

## 5. How to continue this work

Future sessions should treat this document as the pre-implementation
record: the inventory (sections 2–3) is the answer to the spec's "each
item must be accepted into the ABI, redesigned, or removed"
(`10-c-backend-and-runtime.md:63-67`); the ABI decisions made while writing
`pebble_rt.h` in 10.1 should be recorded back into
`10-c-backend-and-runtime.md` as they're resolved, the way 07 recorded its
resolutions into `07-generics.md`. The old backend (`src/`) is reference
only and must not be ported piecemeal — section 3's verdicts are the
justification for each disposal.
