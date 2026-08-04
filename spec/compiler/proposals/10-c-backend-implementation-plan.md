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
- **10.13 — widen the backend to i64** (`runtime/`,
  `compiler/internal/backend`): proves the checked-integer pattern
  built for i32 (10.3–10.12) actually generalizes rather than having
  been accidentally i32-specific. `runtime/src/arith.c` gains the six
  i64 checked-arithmetic functions mirroring the i32 ones exactly at
  the wider width. The backend resolves the entry's integer width once
  (i32 or i64, from its own result type) and threads it through every
  builder instead of hardcoding i32; a body never mixes widths — an
  i32 local inside an i64 entry (or vice versa) is a clean rejection,
  since there's no cast/coercion lowering to fall back on. The i32
  path is untouched (`pebble_user_main` keeps the legacy `static int`
  return type byte-for-byte, confirmed against every existing i32
  test); i64 uses `static int64_t` so a 64-bit value isn't truncated
  before the hosted `main` narrows it to the process exit code.
  Verified end-to-end, including independently outside the harness:
  a real i64 accumulation loop, and `INT64_MAX + 1` aborting with the
  i64-specific overflow message. (One dispatch note: the first attempt
  at this slice terminated prematurely mid-investigation with an
  incomplete worklog; the supervisor found the actual diff was
  substantial and correct despite that, and completed full independent
  verification — runtime SAFE/RELEASE builds, the full Go suite and
  race, and manual reproduction — before accepting it.)
- **10.14 — `bool` locals and bare-bool conditions**
  (`compiler/internal/backend`): `bool` no longer exists only
  transiently as a comparison result — a local may now be declared
  `bool` alongside the entry's integer width, reassigned, and used
  directly as an `if`/`while` condition (a literal, a bool local
  reference, or a `!` negation of one). `buildCondition` dispatches on
  the condition node's shape (a `BinaryValue` comparison keeps the
  existing path; anything else routes through the new
  `buildBoolExpr`), so a body may mix `bool` and integer locals side
  by side. The locals scope map now records each local's own resolved
  type rather than just presence, so a reference validates and emits
  against the right grammar. Real edge case found and correctly
  rejected rather than mishandled: `!(i < 5)` wraps a `SourceAlias`
  around the comparison rather than being a bare bool value, so
  negating a comparison directly stays outside this slice's grammar —
  confirmed against a real fixture. Verified end-to-end: a mixed
  bool+integer accumulation loop compiles and runs to exit code 10,
  confirmed both by the dispatched tests and independently outside the
  harness.
- **10.15 — `&&` and `||` combining bool values**
  (`compiler/internal/backend`): closes the gap 10.7/10.11/10.14 all
  deliberately left rejected. `buildBoolExpr` gains
  `ShortCircuitValue` (`&&`/`||`), a `BinaryValue` case (a comparison
  can serve as an operand), and a `SourceAlias` unwrap case (needed to
  reach a parenthesized comparison operand). Plain C `&&`/`||` are the
  correct lowering — both languages short-circuit, and every operand
  in this backend's grammar is side-effect-free — and precedence
  needed no work, confirmed against the parser's own grammar (AND
  binds tighter than OR) that the typed IR tree already encodes it;
  the emitter parenthesizes every combination explicitly so nesting
  stays unambiguous. Incidentally supersedes 10.14's `!(i < 5)`
  rejection (the `SourceAlias` unwrap makes it reachable). Verified a
  three-way precedence fixture compiles to the correctly-grouped C and
  exits as expected — confirmed independently outside the harness.
- **10.16 — bool equality/inequality comparisons**
  (`compiler/internal/backend`): closes 10.15's own confirmed
  remaining gap, `(1 < 2) == (3 < 4)` — comparing two `bool` values
  with `==`/`!=`. `buildComparison` decides the operand grammar from
  the operands' own resolved types rather than assuming integers, and
  parenthesizes each bool operand so a nested bool comparison can't
  chain associatively with the outer operator. Ordering comparisons
  between bool operands are rejected but confirmed unreachable from
  real source (checker rejects that shape as C0603 before typed IR
  exists), so it's defense for hand-built IR, not a real gap. Verified
  end-to-end and independently outside the harness.
- **10.17 — a second function, callable from the entry**
  (`compiler/internal/backend`): the first genuinely new architectural
  piece — `Emit` no longer emits exactly one function. It discovers
  every function actually reachable by a call from the entry (a DFS
  over `DirectCall` edges), validates each against the same
  constraints the entry satisfies (Pebble convention, zero parameters,
  result exactly the entry's resolved width), and emits each as its
  own `static pebble_fn_<symbolID>(PebbleContext *ctx)` function,
  reusing `buildBlock` unchanged for the body. Emitting exactly the
  reachable set guarantees by construction that every emitted helper
  has at least one call site, so the mandated `-Wall -Wextra -Werror`
  build never warns about an unused static function — confirmed with a
  real unreachable-function test. Emission order is the walk's
  post-order (callees before callers), since there's no
  forward-declaration mechanism yet; a cycle (self- or
  mutual-recursion) is a clean rejection naming the call chain, not
  attempted. Context threading is not an explicit IR child — confirmed
  against a real `DirectCall` node, `ContextAction` records
  `ContextForward`, and this backend prepends `ctx` as the first C
  argument itself, mirroring `pebble_user_main`'s own signature.
  Real, independently-confirmed finding along the way: the shared
  end-to-end test harness never actually compiled under `-Wall -Wextra
  -Werror` before this slice. Retrofitting it surfaced a genuine
  `-Wunused-variable` case (a declared-but-unread `bool` local from
  10.14); fixed at the root — every emitted local now gets a `(void)`
  cast immediately after its declaration, the same pattern already
  used for `ctx`, so the emitter is immune to the warning regardless
  of whether a program reads the local. The harness now compiles
  every end-to-end test under strict flags uniformly. First dispatch
  attempt at this slice was a genuine silent no-op (near-empty
  worklog, only a leftover investigation probe, no real diff) — caught
  and resolved by retrying with the identical brief. Verified a
  two-level call chain compiles in the correct forward-definition
  order and exits correctly, confirmed independently outside the
  harness.
- **10.18 — parameters for called functions**
  (`compiler/internal/backend`): lifts 10.17's zero-parameter
  restriction. `validateHelperSignature` now walks
  `decl.Parameters`, resolving and requiring each one to be the
  entry's resolved width or `bool` — the same two grammars a local
  already supports — rejecting anything else with an error naming the
  offending parameter position. `buildHelperFunctions` seeds each
  helper's own locals scope with its parameters before building its
  body, so a parameter reference inside the body resolves through the
  existing `SymbolValue`/locals machinery unchanged — a parameter
  behaves exactly like a pre-declared local once inside the function.
  The `DirectCall` case in `buildExpr` now builds each call-site
  argument (via `buildExpr` or `buildBoolExpr`, chosen by the callee's
  declared parameter type) through a new `buildCallArguments` helper,
  validating the argument count against the callee's declared
  parameter count. Each emitted helper's C signature declares its
  parameters using the same `pebble_local_<symbolID>` naming a local
  already uses, so a parameter and a local are textually identical
  inside the body. Real, independently-confirmed finding: a genuine
  `cc` compile (not assumed) confirms `-Wunused-parameter` fires under
  `-Wall -Wextra -Werror` for a declared-but-unread C parameter, so
  every parameter gets the same `(void)pebble_local_<id>;` cast a
  local already gets. Verified end-to-end (including a two-parameter
  `add(a, b)` call, a `bool` parameter, a parameter used inside a
  loop/if, and a nested-call argument) and independently outside the
  harness — manually compiled and ran the emitted C for `add(20, 22)`
  with `-Wall -Wextra -Werror`, confirming exit code 42.
- **10.19 — tuple values, the first aggregate type**
  (`compiler/internal/backend`): tuples chosen over structs/records
  because `compiler/internal/types` already models `Tuple` as a real
  `Kind` with `TypeKey.Elements()`, and `compiler/internal/tir` already
  has real node kinds for it — structs have no type-system
  representation at all yet. Each distinct tuple type is emitted as
  one C struct typedef, named `pebble_tuple_<typeID>_t` from the
  tuple's own stable `types.TypeID`, with positional fields `_0`, `_1`,
  ... in element order (the old backend's own convention, without its
  9-field cap). A new `collectTupleTypes` pass walks the entry body
  and every reachable helper's body (reusing the same traversal
  `discoverReachableHelpers` already uses) to discover every tuple
  type referenced, so every typedef is written before any function
  that uses it. A tuple-typed local is declared from a tuple literal
  (`let t (i32, i32) = (20, 22);`), each element built by the grammar
  its own type selects, emitted as a C struct initializer. Element
  types are restricted to the entry's resolved width or `bool` — the
  same two grammars a scalar local already supports; nested tuple
  elements, `str` elements, whole-tuple reassignment, tuple
  parameters/results, and indexing a tuple literal directly are all
  clean rejections. The existing `locals` map was widened to a small
  `localInfo{kind, tuple}` struct rather than adding a parallel map, so
  no builder call site needed a second argument. Real,
  independently-confirmed finding: reading one element of a tuple
  local (`t.1`) lowers to `Load(TuplePlace)`, not a bare
  `TupleElementValue` (that shape is only produced for indexing a
  tuple literal directly, out of scope here) — confirmed against a
  real fixture before assuming the node shape, exactly the discipline
  this phase has followed throughout. Also confirmed: `TupleCoerce` is
  unreachable in this slice's scope (in-scope literals anchor directly
  to their element type) and was left unimplemented. Real limitation
  surfaced, not fixed in this slice (out of scope, since it lives in
  `compiler/internal/tir`): tuple element 0 (`t.0`) could not be read
  from any source in this compiler at the time, because
  `tir.Node.Ordinal` uses 0 as its zero-sentinel and the typed-IR
  verifier rejected an Ordinal of 0 on a `TuplePlace`/
  `TupleElementValue`; every test in this slice used ordinals ≥ 1.
  **Fixed immediately after** (same day, `compiler/internal/tir`):
  `Ordinal` is a zero-based element index, so 0 is a legitimate value,
  not an absent-field sentinel — the checker itself was already
  correct (`internal/check/place_facts.go`'s `validPlaceProjection`
  deliberately omits a `TupleOrdinal == 0` check for the tuple case),
  so this was purely a structural-verifier bug. The two erroneous
  `Ordinal == 0` checks in `verify.go` were removed and the contract
  documented directly on the `Node.Ordinal` field comment; the
  per-tag damage test table was updated to damage these two node
  kinds by breaking the child-count invariant instead, since Ordinal
  0 is no longer damage. A permanent regression test
  (`TestEmitTupleElementZeroReadCompilesAndRuns`) now proves `t.0`
  compiles end-to-end. Verified end-to-end (two- and three-element
  tuples, a bool element driving an `if`, a tuple element as a call
  argument, a tuple local inside a helper, an i64 tuple, element 0 of
  a tuple, and the
  element-type/whole-tuple-store/tuple-parameter/tuple-literal-index
  rejections) and independently outside the harness — manually
  compiled and ran the emitted C for
  `let t (i32, i32) = (20, 22); return t.1;` (exit 22) and, after the
  fix, `return t.0;` (exit 20), both with `-Wall -Wextra -Werror`.
- **10.20 — fixed-length array values** (`compiler/internal/backend`,
  plus `runtime/` and `compiler/internal/check`): the second
  backend-supported aggregate type. Unlike a tuple, an array needs no
  C struct typedef — C's own array declaration syntax is
  self-contained — so an array-typed local is declared directly as
  `<ctype> pebble_local_<sym>[N] = { e0, ... };`. Element types are
  restricted to the entry's width or `bool`, matching every prior
  aggregate slice. Only an `ArrayValue` literal (`[10, 20, 30]`) is a
  supported initializer; `ArrayRepeat` (`[v; N]`) is explicitly
  rejected — safely evaluating a repeat value exactly once while
  filling N slots is a real design question deferred to a later
  slice. Reading an element (`a[i]`) lowers, confirmed against a real
  fixture, to `Load(CheckedIndexPlace)` — the array analog of 10.19's
  `Load(TuplePlace)` finding — and is bounds-checked at every read.
  Ahead of the backend slice, added the runtime bounds-check helper it
  needs (`pebble_rt_checked_index_i32`/`i64`, new
  `runtime/src/bounds.c`, declared in `pebble_rt.h`): an out-of-bounds
  access panics with `PEBBLE_PANIC_INDEX_OUT_OF_BOUNDS` in *every*
  configuration (SAFE and RELEASE), the same reasoning already applied
  to division by zero, since there is no defined "wrapped" result for
  an out-of-bounds access; verified via the runtime's own Makefile
  smoke test in both modes before the backend slice was dispatched.
  `localInfo` (from 10.19) gained a mutually-exclusive `array
  types.TypeID` field alongside `tuple`. Whole-array reassignment,
  array-of-tuple/nested-array elements, array parameters/results, and
  bare `CheckedIndex` (string/non-place indexing) are all clean
  rejections. **Real, significant finding along the way**: while
  investigating this slice, two independent dispatch attempts stalled
  trying to build the exact fixture the brief required
  (`let a [2]i32 = [10, 20];`) — tracing it down personally found a
  real checker bug in `compiler/internal/check/aggregate_facts.go`'s
  `prepareArray`: it unconditionally created an inference-solver
  variable cell and then, only when a known destination array type
  existed, discarded it in favor of a known-type term instead — the
  abandoned cell was never bound to anything and was reported as a
  spurious `T0510` for *every* array literal checked against an
  explicit destination type, blocking real Pebble programs regardless
  of phase 10. Fixed and landed separately (`4a479e8`) before resuming
  this slice — including catching a second version of the same class
  of bug in the first fix attempt itself (unconditionally calling the
  *other* term constructor instead) by re-running the full probe
  matrix, not just the originally-failing cases, before trusting the
  fix. This slice needed three dispatch attempts after that: two more
  flash attempts (one hit the checker bug before it was fixed; one
  stalled on investigation-tooling friction, writing a probe dump to
  `/tmp` it then couldn't read back), escalated to
  `openai/gpt-5.6-luna` per the established escalation policy, which
  completed the slice cleanly. Verified end-to-end (element read, bool
  element driving an `if`, an expression-built index, an out-of-bounds
  panic, an array element as a call argument, an i64 array, and the
  `ArrayRepeat`-rejection) and independently outside the harness —
  manually compiled and ran the emitted C for
  `let a [3]i32 = [10, 20, 30]; return a[1];` with
  `-Wall -Wextra -Werror`, confirming exit code 20.
- **10.21 — optional values** (`compiler/internal/backend`, plus
  `runtime/` and `compiler/internal/check`): the third
  backend-supported aggregate type after tuples (10.19) and arrays
  (10.20). An optional type is emitted as one C struct typedef per
  distinct optional type, `pebble_optional_<typeID>_t`, with `bool
  has_value` and a `value` field typed as the payload's own C type.
  Payload types restricted to the entry's width or `bool`. Both `some
  <expr>` (`SomeOptional`) and `none` (`NoneOptional`) are supported
  local initializers; force-unwrap (`x!`, `CheckedOptionalUnwrap`) is
  bounds-checked via the runtime helper added ahead of this slice
  (`pebble_rt_checked_unwrap_i32`/`i64`/`bool`, new
  `runtime/src/optional.c`, declared in `pebble_rt.h`, which also
  gained a `<stdbool.h>` include it didn't need before): an absent
  optional panics with `PEBBLE_PANIC_UNWRAP_FAILED` in *every*
  configuration, the same reasoning already applied to array bounds
  and division by zero; verified via the runtime's own Makefile smoke
  test in both modes before the backend slice was dispatched.
  `localInfo` gained a fourth mutually-exclusive field (`optional
  types.TypeID`) alongside `kind`/`tuple`/`array`. Reassigning an
  optional local, optional-typed parameters/results, and unsupported
  payload types are clean rejections. **Real finding, caught during
  independent verification, not left as-is**: the dispatched worklog
  (`opencode-go/mimo-v2.5`, which completed the slice cleanly on its
  first attempt) claimed `none` was "unreachable from real source
  today (the checker rejects it with T0510)" and documented
  `NoneOptional` as defensive-only code for hand-built IR. That claim
  did not survive reproduction: building the exact `let x ?i32 =
  none;` fixture reproduced a real T0510 failure, but tracing it found
  a genuine checker bug — fixed separately (`6c08b8b`,
  `compiler/internal/check/expression_facts.go`'s `shapeLeaf`) — the
  same root-cause class as 10.20's `prepareArray` fix (`4a479e8`): an
  inference-solver `Variable` cell created unconditionally, then
  discarded (never bound to anything) whenever a known destination
  shape existed instead. This time the bug lived in a *shared* helper
  used by both the `nil` literal (against a known pointer destination)
  and the `none` literal (against a known optional destination), so
  the fix closed a broader, genuinely language-level gap, not a
  backend-only one. With that fixed, `none` compiles and runs
  correctly through the exact code this slice had already written for
  it — `buildOptionalLocalDeclaration`'s `NoneOptional` case was
  implemented correctly, just mislabeled as unreachable defense;
  added the missing end-to-end test coverage and corrected four stale
  comments. Verified end-to-end (`some`+unwrap at both integer widths,
  a bool payload driving an `if`, an unwrapped value as a call
  argument, an optional local inside a helper, `none` never unwrapped,
  and force-unwrapping `none` aborting the process) and independently
  outside the harness — manually compiled and ran the emitted C for
  `let x ?i32 = some 42; return x!;` with `-Wall -Wextra -Werror`,
  confirming exit code 42.
- **10.22 — struct (record) values** (`compiler/internal/backend`):
  the fourth backend-supported aggregate type after tuples (10.19),
  arrays (10.20), and optionals (10.21). A struct type is emitted as
  one C struct typedef per distinct struct type,
  `pebble_struct_<typeID>_t`, with each C field named
  `pebble_field_<memberSymbolID>` from the field's own stable symbol
  ID (mirroring the `pebble_local_<symbolID>` naming discipline), in
  the struct's *declared* field order. Field types restricted to the
  entry's width or `bool`. Construction only supports a
  `RecordConstruct` literal (`Point.{ x = 1, y = 2 }`); since a
  construction site's field order need not match the struct's
  declared order, the local's initializer is emitted as a C99
  designated-initializer brace list
  (`{ .pebble_field_25 = ..., .pebble_field_26 = ... }`), which
  sidesteps the ordering problem entirely regardless of either order —
  verified with a dedicated test that writes fields out of declared
  order and confirms each still lands in the right place, not just a
  test that happens to write them in order. Reading a field
  (`point.x`) lowers, per a real fixture, to `Load(FieldPlace)` — the
  struct analog of 10.19/10.20's `Load(TuplePlace)`/
  `Load(CheckedIndexPlace)` findings. `localInfo` gained a sixth
  mutually-exclusive field (`structType`). A struct's declared field
  order comes from the `TypeDeclaration`'s container (`TypeDecl.Members`),
  not the `TypeDeclaration` node itself (which carries no
  children/type) — the same "the node isn't the whole story" finding
  10.18 made for `FunctionDeclaration.Parameters`. Reassigning a
  struct local, struct-typed parameters/results, `FieldValue` (reading
  a field off a struct literal directly), nested field access, and
  unsupported field types are all clean rejections. Bonus finding, no
  special-casing needed: a generic struct's monomorphized instance is
  `Nominal` exactly like a non-generic struct (the concrete type
  arguments live in a part of the type key this backend never
  inspects), so it works automatically. No checker/tir bug found this
  time — the brief explicitly instructed stopping and reporting rather
  than routing around one, given two were found in the previous two
  slices (4a479e8, 6c08b8b); none turned up here. Verified end-to-end
  (two-field read, out-of-declared-order construction, a bool field
  driving an `if`, a field as a call argument, a three-field struct
  with two fields read and added, an i64 field, and a struct local
  inside a helper) and independently outside the harness — manually
  compiled and ran the emitted C for a struct constructed with fields
  out of declared order (`Point.{ y = 22, x = 20 }; return point.x +
  point.y;`) with `-Wall -Wextra -Werror`, confirming exit code 42.
- **10.23 — str values, literal locals and equality**
  (`compiler/internal/backend`, plus `runtime/`): the first
  backend-supported type not represented by a C primitive or a
  program-specific typedef — `str` is the runtime's own `PebbleStr`
  (data pointer + length, already declared in `pebble_rt.h`).
  Deliberately narrow, unlike the aggregate slices before it: only a
  `str` local declared from a string literal, and `==`/`!=` between
  two `str` values (a local, a literal, or either). Indexing,
  concatenation, interpolation, length access, `str`
  parameters/results, and `str` fields inside another aggregate are
  each their own real design question, deferred. Equality routes
  through the runtime helper added ahead of this slice
  (`pebble_rt_str_eq`, new `runtime/src/str.c`): `==` emits the call
  directly, `!=` its negation; false immediately on a length mismatch,
  never NUL-termination-dependent. Ordering comparisons between `str`
  operands are confirmed reachable from real source and rejected
  cleanly (not assumed unreachable). **The one real correctness trap
  in this slice**: embedding a decoded literal's bytes as a C string
  literal safely. A naive `\xHH` hex escape is unsafe — C's escape
  rules greedily consume following hex/octal digit characters
  ("maximal munch"), so escaping a non-printable byte immediately
  followed by a literal hex digit can silently produce the wrong
  bytes. Fixed by always emitting a fixed-width 3-digit octal escape
  (`\NNN`) for every byte outside printable ASCII, which C can never
  over-consume — verified for real, not just compiled: a dedicated
  test compares two differently-spelled Pebble string literals that
  decode to the same nine bytes (one spelled with `\n`/`\t`/`\"`/`\\`,
  the other with `\xHH` byte escapes, including a control byte
  immediately followed by a digit) and confirms they compare equal
  after the C round-trip, plus an independent standalone C program
  confirming the exact byte sequence. `str` indexing (`s[i]`) is
  confirmed reachable, lowers to a separate `CheckedIndex` mechanism
  this backend doesn't build for `str`, and is rejected cleanly. No
  checker/tir bug found this time. Verified end-to-end (equal/unequal
  literals, a local compared against a literal, `!=`, equality as a
  bool value/logical operand/`while` condition, and the escape
  round-trip) and independently outside the harness — manually
  compiled and ran the emitted C for the escape-round-trip fixture
  with `-Wall -Wextra -Werror`, confirming exit code 7.
- **10.24 — tuple- and struct-typed function parameters**
  (`compiler/internal/backend`): 10.18 gave helper functions
  parameters, restricted to the entry's width or `bool`; 10.19-10.23
  then added five more types as locals but none could cross a
  function boundary. This slice lifts that restriction for tuple and
  struct parameters specifically — the two aggregate types whose
  internal shape this backend already fully understands.
  `validateHelperSignature` accepts a tuple/struct parameter type
  alongside width/bool; `buildHelperFunctions` seeds the parameter's
  locals-scope entry exactly as an `Initialize` of that type already
  does, so element/field reads inside the callee's body resolve
  through the existing `Load(TuplePlace)`/`Load(FieldPlace)` machinery
  unchanged, with the C parameter declared using the aggregate's own
  typedef name. `buildCallArguments` routes these through a new
  `buildAggregateArgument`, which requires the call-site argument be a
  plain `SymbolValue` naming an already-declared aggregate-typed local
  of matching type — passing the whole value by value is trivially
  valid C once the typedef exists. **Real, confirmed-not-guessed
  finding**: constructing a fresh tuple/struct value inline at a call
  site (`f((1, 2))`, `f(Point.{ x = 1, y = 2 })`) is reachable from
  real source, so it's a clean rejection naming what was found, not an
  assumed-unreachable case — building one requires a general
  build-an-aggregate-value expression, saved for later. Two prior
  rejection tests (10.19's `TestEmitRejectsTupleParameter`, 10.22's
  `TestEmitRejectsStructTypedParameter`) were updated to this new,
  narrower rejection reason. **Real gap closed**:
  `collectTupleTypes`/`collectStructTypes` previously only discovered
  a type from local construction/declaration, missing a type used
  *only* as a parameter type (never constructed in any reachable
  body); both now also scan each reachable helper's own `Parameters`
  list, verified with a dedicated test where the type appears nowhere
  else. Tuple/struct-typed function *return* types remain explicitly
  out of scope — confirmed reachable from real source, still rejected
  cleanly by the existing result-type check; changing a helper's C
  return type to an aggregate typedef touches enough distinct pieces
  (signature formatting, typedef-before-definition ordering for the
  helper's own signature, building a full aggregate return value) to
  deserve its own future slice. No checker/tir bug found this time.
  Verified end-to-end (a tuple parameter reading both elements
  including element 0, a mixed `(i32, bool)` tuple parameter driving
  an `if`, a struct parameter, a struct parameter's bool field driving
  an `if`, and a parameter-only type still getting its typedef) and
  independently outside the harness — manually compiled and ran the
  emitted C for `fn sumT(t (i32, i32)) i32 { return t.0 + t.1; } fn
  main() i32 { let t (i32, i32) = (20, 22); return sumT(t); }` with
  `-Wall -Wextra -Werror`, confirming exit code 42.
- **10.25 — aggregate values as compound-literal expressions**
  (`compiler/internal/backend`): closes 10.24's "inline aggregate
  construction as a call argument is not supported yet" gap. 10.19/10.22
  built tuple/struct construction only as a local's declaration
  initializer; this slice adds the general primitive — building a
  `TupleValue`/`RecordConstruct` as an ordinary C expression, not just
  a full declaration statement. `buildTupleBraceList`/
  `buildStructBraceList` are extracted from the existing
  local-declaration builders (same element/field validation and
  `buildExpr`/`buildBoolExpr` dispatch, returning just the brace-list
  text — verified to emit byte-identical output to before for every
  pre-existing test); `buildTupleValueExpr`/`buildStructValueExpr`
  wrap the same brace list in a C99 compound-literal cast
  (`(pebble_tuple_<id>_t){ 20, 22 }`, positional; `(pebble_struct_<id>_t){
  .pebble_field_<m> = e, ... }`, designated, so out-of-declared-order
  construction still needs no reordering). `buildAggregateArgument`
  (10.24) now accepts either an existing aggregate-typed local or a
  bare inline construct of matching type. **Real,
  confirmed-not-guessed finding**: a paren-wrapped aggregate argument
  (`f(((1, 2)))`) arrives as a `SourceAlias`, which this backend
  already rejects consistently for every type (the scalar analog
  `f((1))` was already rejected before this slice) — kept as-is, not
  specially handled. The two 10.24 rejection tests whose fixtures are
  now the positive case were removed and replaced with a rejection
  test covering the genuinely-still-rejected paren-wrapped shape.
  Verified end-to-end (inline tuple argument, inline struct argument,
  an inline struct argument with fields out of declared order) and
  independently outside the harness — manually compiled and ran the
  emitted C for `fn f(t (i32, i32)) i32 { return t.0 + t.1; } fn
  main() i32 { return f((20, 22)); }` with `-Wall -Wextra -Werror`,
  confirming exit code 42.
- **10.26 — tuple- and struct-typed function return types**
  (`compiler/internal/backend`): the biggest single change to the
  backend's core plumbing since 10.17's original multi-function
  architecture. Every builder threads one `width` parameter meaning
  two conflated things — the integer grammar to build at, and
  (implicitly) "this function's own return type." A new `resultInfo`
  value (mirroring `localInfo`'s `kind`/`tuple`/`structType` shape)
  threads alongside `width` through `buildBlock`/`buildIf`
  specifically, recording what the *enclosing function* returns,
  distinct from `width`'s unchanged meaning everywhere else. The
  entry's own path passes `resultInfo{kind: result}` — byte-identical
  to before, confirmed by all 200+ prior tests passing unmodified. A
  helper whose `ResultType` is a tuple/struct is declared with the
  aggregate's own typedef as its C return type; its tail `return` is
  built by a new `buildAggregateReturnValue` (a forwarded local of
  matching type, or a fresh inline construction via 10.25's compound-
  literal builders) instead of `buildExpr`. Calling such a helper is
  supported in exactly one position — the direct initializer of a
  matching aggregate-typed local declaration; `buildDirectCall` was
  factored out of `buildExpr`'s `DirectCall` case (byte-identical
  behavior) and shared with the new `buildAggregateCallInitializer`.
  **Real, confirmed-not-guessed findings**: calling such a helper as a
  call argument, as an operand, or as another helper's own return
  value are all reachable from real source and rejected cleanly; an
  enum-typed helper result is `Nominal` like a struct and rejected
  cleanly through the same existing path, no special-casing needed; a
  tuple/struct-returning helper called as an if/else arm's or a
  while-loop-body's local initializer "just works" through the
  existing recursive path with zero special-casing, since `resultInfo`
  only governs the enclosing function's own return. Typedef discovery
  extended to scan each reachable helper's own `ResultType` (mirrors
  10.24's `Parameters` scan). Verified end-to-end (tuple/struct
  flagship returns, forwarding an existing local vs. fresh
  construction, a bool element, an if/else-tail return, a
  result-only-typed helper, and returns used inside an if-arm/loop-body
  local initializer) and independently outside the harness — manually
  compiled and ran the emitted C for `fn makeT() (i32, i32) { return
  (20, 22); } fn main() i32 { let t (i32, i32) = makeT(); return t.0 +
  t.1; }` with `-Wall -Wextra -Werror`, confirming exit code 42.
- **10.27 — ArrayRepeat array initializers** (`compiler/internal/backend`):
  closes 10.20's deferred `[v; N]` gap. A naive brace-list duplication
  (`{ v, v, v }`) would evaluate `v` N times, wrong if `v` has any
  observable side effect. This slice evaluates the repeat value
  exactly once and fills every slot from that one evaluation, emitting
  three C statements instead of one declaration line (a bare array
  declaration, a one-time-evaluated `pebble_repeat_<sym>` temp, and a
  `size_t` fill loop) — the synthetic names derived from the local's
  own declaration symbol, collision-free by construction since
  `ArrayRepeat` only ever appears as that one local's own initializer.
  **Real, confirmed-not-guessed finding**: `ArrayRepeat`'s second
  child is always a checker-synthesized `uint` `IntegerLiteral` exactly
  equal to the array type's own declared length — never a runtime
  expression, never differing — confirmed via real fixture dumps at
  three array lengths/widths. Every out-of-scope `ArrayRepeat` shape
  (a call argument, nested in another aggregate, direct indexing) is
  reachable from real source but already rejected cleanly by
  pre-existing gates, so no new rejection code was needed. Single
  evaluation proved structurally: a dedicated test confirms a repeat
  value that calls a helper function appears exactly once in the
  emitted C text. Verified end-to-end (sum of three repeated elements,
  a non-trivial repeat-value expression, a bool element, an i64 array)
  and independently outside the harness — manually compiled and ran
  the emitted C for `let a [3]i32 = [5; 3]; return a[0] + a[1] +
  a[2];` with `-Wall -Wextra -Werror`, confirming exit code 15.
- **10.28 — one level of nested aggregates, with typedef dependency
  ordering** (`compiler/internal/backend`): closes Phase 1's last gap.
  Every prior aggregate slice restricted its elements/fields to scalar
  types only; this slice lifts that by exactly one level — a tuple
  element, struct field, array element, or optional payload may now
  itself be a tuple/struct/optional (not an array — see below). **A
  real structural correctness problem**: C requires a type fully
  defined before use as another type's field, but every prior
  typedef-collection pass emitted types in first-encountered walk
  order, coincidentally correct only because nothing depended on
  anything else before now. Fixed with dependency-first ordering
  across the tuple/optional/struct typedef families (reproduced the
  wrong-order failure before the fix, confirmed correct after),
  preserving first-encountered order for unrelated types so no
  existing typedef ordering changed. **A real correctness trap found
  and fixed**: for an inline nested aggregate literal used directly as
  a struct field's value, the checker leaves that literal's own `Type`
  unanchored; field-type resolution now prefers the struct's own
  declared field type over the raw value node's `Type`. **Real,
  confirmed checker-level limitations** (not backend gaps, left as
  clean rejections): arrays cannot be tuple elements or struct fields
  at all today — the checker itself fails to construct or resolve them
  (`C0619`/`T0501`); an inline `some` as a struct field value is
  separately checker-rejected (`C0601`). This slice needed two
  dispatch attempts: the first (`opencode-go/deepseek-v4-flash`)
  investigated thoroughly and correctly identified the real problems
  above, but its ~1400-line implementation was never verified — zero
  new tests, and independently found to have regressed two
  pre-existing rejection tests whose fixtures the new nesting support
  should have turned into positive cases. Discarded entirely (not
  patched); its confirmed findings were folded into a refined brief
  for the retry (`openai/gpt-5.6-luna`), which produced a smaller,
  correct, fully-verified implementation. Verified end-to-end (238
  backend tests, 0 failures, including both previously-regressed tests
  now passing as intended positive cases) and independently outside
  the harness — manually compiled and ran the emitted C for a
  struct-in-tuple fixture with `-Wall -Wextra -Werror`, confirming
  exit code 42 and correct struct-before-tuple typedef order.

  **Phase 1 (aggregate loose ends) is now complete** — 10.25 through
  10.28.
- **10.29 — range loops** (`compiler/internal/backend`, plus a
  prerequisite `compiler/internal/check`/`compiler/internal/tir` fix):
  Phase 2's first slice (control flow completion). A range loop
  (`loop start..end : name { body }`, `..=` for inclusive) lowers
  directly to a C `for` loop whose loop counter *is* the bound
  iterator, its body built by the same `buildLoopBody` a `while`
  loop's body already uses, after seeding a cloned locals scope with
  the iterator as an ordinary local of the entry's width — the same
  seeding pattern a helper's parameters already use.
  `break`/`continue` already worked unmodified (their `Target` already
  named the range loop's own `Region`, confirmed against a real
  fixture). **This slice depended on a prerequisite fix landed
  separately first**: `check/tir: attach the range-loop iterator's
  symbol to its RangeLoop node` — the iterator's `symbol.SymbolID` was
  resolved during checking but never attached to the `RangeLoop` TIR
  node itself (the general binding-node pass deliberately skips it,
  mirroring how it skips parameters — each is meant to be attached by
  its own owning node's builder instead — but `buildRangeLoop` never
  did), so the iterator's declaration was structurally unrecoverable
  from typed IR before the fix. **Real finding, confirmed against real
  fixtures**: when the iterator is never used in a width-anchoring
  position (only in a comparison, an array index, or another loop's
  bound), the checker leaves it as the unanchored `int` builtin rather
  than the entry's width — handled by reusing the exact precedent this
  backend already has for unanchored integer literals at every
  int-literal-tolerant position (comparisons, array indexing), now
  also recognizing an int-typed `SymbolValue`. **Real deviation from
  the original scoping**: nested range loops needed a `tir.RangeLoop`
  case added to `buildLoopBody`'s own statement switch (previously
  only `tir.While` was handled there), not "zero changes" as assumed
  going in. The unbound form (`loop start..end { ... }`, no `: name`)
  is confirmed reachable and rejected cleanly (no way to observe such
  a loop's iteration from inside it). Reassigning the iterator is
  checker-blocked (`C0606`) and therefore unreachable. Verified
  end-to-end (accumulation, exclusive vs. inclusive, break/continue,
  nested range loops, a range loop inside a `while`, non-literal
  bounds, an iterator used only in a comparison, an unused iterator, a
  nested loop using the outer iterator as its own bound, an array
  index, an i64 entry, and a helper-call bound) and independently
  outside the harness — manually compiled and ran the emitted C for
  `fn main() i32 { var sum i32 = 0; loop 0..3 : i { sum = sum + i; }
  return sum; }` with `-Wall -Wextra -Werror`, confirming exit code 3.
- **10.30 — classic C-style for loops** (`compiler/internal/backend`,
  plus a `compiler/internal/tir` fix): Phase 2's second slice.
  `for init; cond; update { body }` lowers directly to a C `for` loop
  with the same three individually-optional clauses, each reusing the
  exact machinery its block-level counterpart already has — the
  initializer shares `buildScalarInitializeCore`, the update shares
  `buildStoreCore`, and the condition uses the same `buildCondition`
  an `if`/`while` already uses (`buildLeadingStatement`'s
  Initialize/Store cases were refactored to call these same shared
  cores, confirmed byte-identical output for every pre-existing test).
  `For.Children` is variable-length — the checker appends only
  whichever clauses are present, in the fixed relative order
  initializer/condition/update, then the body always last —
  disambiguated purely by node category (the condition, when present,
  is the unique `CategoryValue` child among the non-body children),
  confirmed against real fixtures for every clause-presence
  combination. **Real, confirmed limitation, documented not silently
  mis-handled**: a no-condition `Store` used as the initializer
  (`for step = 0;; { }`, out of scope) is structurally
  indistinguishable from the in-scope update-only shape
  (`for ; ; step = step + 1 { }`) — the `For` node carries only
  `Region` and `Children`, nothing naming which clause a lone `Store`
  actually is; a lone no-condition `Store` is always treated as the
  update. **Real bug found and fixed separately before this slice
  could finish**: the typed-IR verifier wrongly required `For`'s first
  child to always be `CategoryNonvalue`, rejecting any well-formed
  for-loop with a condition but no initializer (`for ; cond; { }` /
  `for ; cond; update { }`, both reachable from real source) before
  the backend was ever reached — the dispatch that found this
  correctly stopped and reported it rather than routing around it, per
  instructions; fixed in `check/tir: attach the range-loop iterator's
  symbol...`-adjacent commit "`tir: fix verifier rejecting well-formed
  classic for-loops missing an initializer`", with two end-to-end
  tests added for the previously-blocked shapes once the fix landed.
  Verified end-to-end (accumulation, every clause-presence
  combination including the two the tir fix unblocked, break/continue,
  nesting inside `while`/range/another `for`, an i64 entry, a bool
  initializer+condition, a `&&` condition, a for loop inside a helper,
  a helper call in the condition, and four rejection tests for
  out-of-scope initializer/update shapes) and independently outside
  the harness — manually compiled and ran the emitted C for
  `fn main() i32 { var total i32 = 0; for var step i32 = 0; step < 3;
  step = step + 1 { total = total + step; } return total; }` with
  `-Wall -Wextra -Werror`, confirming exit code 3.
- **10.31 — switch statements** (`compiler/internal/backend`): Phase
  2's third and final control-flow slice.
  `switch <subject> { case v1, v2: <body> case v3: <body> else: <body> }`
  lowers directly to a C `switch` statement, as a block's tail
  statement (mirroring how a two-armed if/else is the only other
  non-return tail shape `buildBlock` accepts). Multiple `SwitchCase`
  nodes sharing the same body node ID (a multi-value `case v1, v2:`
  clause, confirmed against a real fixture) are grouped and emitted as
  stacked C case labels sharing one body, rather than duplicating the
  body text. An `else` arm maps to C's `default:`. A `SwitchCase`'s
  body may be a `Block` (multi-statement, braces required) or a bare
  statement directly (single-statement, no braces, confirmed against a
  real fixture) — the only supported bare shape is a `Return`, since
  every case body must end in a return or a two-armed if/else whose
  arms each end in return (confirmed: a non-exhaustive switch with no
  `else` is checker-rejected), the same tail-statement grammar
  `buildBlock` already enforces everywhere else — so no `break;` is
  ever needed. A `Block`-shaped case body reuses `buildBlock`
  completely unchanged, correctly threading `resultInfo` (10.26) so a
  case's return value builds correctly for both scalar- and
  aggregate-returning functions. A `CaseValue`-based case (an enum
  variant) is rejected cleanly — no enum support exists yet. No
  checker/tir bug found this slice (unlike 10.29/10.30, which each
  needed one). This dispatch needed two model attempts for reasons
  unrelated to the task: the default-tier model failed to even start
  twice in a row (OpenCode's own infrastructure was down, confirmed by
  an alternate model completing cleanly on its first attempt
  immediately after). Verified end-to-end (multi-value case, both
  matching values, single-value case, else, block- and bare-return
  case bodies, bool subject both values, switch inside a helper, a
  helper call in the subject, an i64 entry, multiple cases with their
  own locals, and rejection tests for an enum case and a
  non-exhaustive switch) and independently outside the harness —
  manually compiled and ran the emitted C for
  `fn main() i32 { switch 1 { case 1, 2: return 10; case 3: return 30;
  else: return 0; } }` with `-Wall -Wextra -Werror`, confirming exit
  code 10.

  **Phase 2's control-flow trio (range loops, classic for loops,
  switch) is now complete** — 10.29 through 10.31. `defer` (Phase 2's
  fourth originally-planned item) remains, needing real design work
  (per-exit-edge lowering) before it can be scoped into a slice.
- **10.32 — defer statements** (`compiler/internal/backend`): Phase
  2's fourth and final slice, completing Phase 2. The design
  investigation this slice depended on (done directly, not delegated)
  found `defer`'s C lowering needs no runtime bookkeeping at all:
  Pebble's `defer` is purely static/lexical, not runtime-conditional
  like Go's. The checker's own `deferChainFor`
  (`compiler/internal/check/ir_builder_control.go`) has already
  resolved, at check time, exactly which `DeferRegister` nodes must
  run at every exit — `Return`, `Break`, `Continue` — via that exit
  node's own `DeferChain []NodeID` field, in LIFO (last-registered-
  first) order; confirmed against real fixtures that a defer
  registered inside an `if`/loop region is correctly excluded from an
  exit's chain once the exit is lexically outside that region,
  regardless of the runtime branch taken. The backend's job is close
  to mechanical: a `DeferRegister` node encountered at its own
  position in program order (in a block's or loop body's
  leading-statement sequence) emits nothing — it's a pure registration
  marker; at every exit point, `buildDeferredStatements` walks the
  exit node's `DeferChain` and emits each entry's single deferred
  statement, in chain order, immediately before the actual C
  `return`/`break`/`continue`. Wired into `buildBlock`'s tail `Return`
  case, `buildLoopJump` (`Break`/`Continue`), and
  `buildSwitchCaseBody`'s bare-`Return` case. Only `Store`
  (reassignment) is supported as a deferred statement's kind for now;
  a deferred `Initialize` or `Print` is rejected cleanly, naming the
  unsupported kind, rather than guessed at — the checker already
  guarantees a deferred statement is never itself an exit or a nested
  `defer` (C0613), so the backend never needs to handle those shapes.
  Verified end-to-end (a single defer observably firing before a
  return; two defers in one scope proving LIFO order; a defer inside
  an if-arm whose exit is in the same arm, firing; a defer inside a
  while-loop body whose exit is after the loop, correctly not firing;
  a defer before a break; a defer before a continue; nested scopes —
  an outer function-level defer plus an inner loop-level defer plus a
  break, proving the break's chain includes only the inner defer and
  the outer defer fires separately at the function's own return; a
  defer inside a helper function) and independently outside the
  harness — manually compiled and ran the emitted C for the
  nested-scopes fixture (`fn main() i32 { var x i32 = 0;
  defer x = x + 100; var i i32 = 0; while i < 5 { defer x = x + 1;
  if i == 0 { break; } i = i + 1; } return x; }`) with
  `-Wall -Wextra -Werror`, confirming exit code 101 (inner defer fires
  on break: 0+1=1; outer defer fires on the function's return:
  1+100=101). No checker/tir bug found this slice.

  **Phase 2 (control flow completion) is now fully complete** —
  10.29 through 10.32.
- **10.33 — void-returning call used as a statement**
  (`compiler/internal/backend`): closes a gap discovered while
  implementing 10.32 — this backend could only call a function when
  the result was used as an expression value; a bare `helper();`
  statement (discarding the result, or calling a void-returning
  function at all) was entirely unsupported anywhere, including as a
  deferred statement. Confirmed against real fixtures: a bare
  discarded-expression statement is a `tir.ExpressionStatement`
  (`CategoryNonvalue`, one child — the discarded expression), produced
  by the checker's `controlExpression` case with no `StatementForm`
  set. The only supported shape is a `tir.DirectCall` to a
  void-returning function, emitted as a bare
  `pebble_fn_<symbol>(ctx, <args>);` by a new shared
  `buildExpressionStatement`, wired into `buildLeadingStatement`
  (covering both `buildBlock`'s and `buildLoopBody`'s leading-statement
  sequences) and into `buildDeferredStatements` (`defer helper();` now
  works). `validateHelperSignature` now accepts a void result;
  `buildHelperFunctions` declares such a helper with C return type
  `void` and `resultInfo{kind: types.Void}`; `buildBlock` gained a
  `tir.ImplicitReturn` tail case (the synthesized fall-through every
  void function's body ends in, confirmed against fixtures — emits
  nothing, after any deferred statements, and is a clean rejection if
  it somehow reaches a non-void-result block). A call to a
  **non-void**-returning function discarded as a bare statement
  (`f();` where `f` returns `i32`) is confirmed checker-reachable but
  deliberately out of scope — rejected cleanly, naming the callee's
  actual result type, rather than guessing how to drop a non-void
  result. `CompoundStore` (`x += 1;` as a bare statement) and
  `tir.Print` remain unimplemented, out of scope for this slice.
  A real latent bug was found and fixed during verification: a
  deferred void call registered inside a region no exit's `DeferChain`
  ever reaches (the defer never fires, by 10.32's own static/lexical
  design) was still being emitted as a helper function, since the
  reachability walk (`collectDirectCalls`) followed a `DeferRegister`'s
  children at its *registration* position — tripping
  `-Wunused-function` under the mandated `-Wall -Wextra -Werror`
  build. Fixed by having `collectDirectCalls` skip a `DeferRegister`'s
  children entirely at registration (a firing defer's call is always
  reached separately, through the exit's own `DeferChain` walk); this
  also retroactively fixes the same latent gap for a 10.32-style
  deferred `Store` whose right-hand side is a helper call that never
  fires. Verified end-to-end (a void helper called as a statement; a
  void helper with a parameter and a non-trivial self-contained body;
  a void call inside a loop body; a void helper calling another void
  helper as its own statement; a void call from an i64 entry; a
  deferred void call firing before a return, paired with a deferred
  Store to make the LIFO firing independently observable; a deferred
  void call firing before a break; a deferred void call that does
  *not* fire, confirming its callee is correctly absent from the
  emitted C entirely; a rejection test for a non-void discarded call)
  and independently outside the harness — manually compiled and ran
  the emitted C for a fixture combining a deferred void call taking an
  argument with a same-scope deferred `Store`, crossing a `break`,
  with `-Wall -Wextra -Werror`, confirming exit code 11 with no
  `-Wunused-function` warning.
- **10.34 — plain (payload-less) enum locals and switch matching**
  (`compiler/internal/backend`): Phase 3's first slice. Pebble has two
  enum forms — a plain enum (`type Color = enum { red, green, blue
  };`, no variant carries a payload) and a tagged union (`type Choice
  = union enum { empty void; value i32; };`, at least one variant
  carries a payload). This slice is plain enums only; tagged unions
  need a real C representation decision (a tagged struct with a union
  member) and remain a clean rejection. A real, load-bearing finding
  from the investigation: `types.TypeKey`'s `Nominal` shape carries
  only the declaration symbol — a plain enum and a struct are
  otherwise indistinguishable in the type snapshot, and the checker's
  own enum/union distinction (`infer.NominalKind`) isn't reachable
  from the backend at all (no `infer` import). The backend
  distinguishes them from the unit's own node graph instead
  (`isEnumType`): a Nominal type's declared members are enum variants
  exactly when none of them ever appears as a `FieldPlace.Member` or a
  `RecordConstruct` field anywhere in the *entire* compilation unit —
  sound because any struct that survives `collectStructTypes` must
  already have that evidence somewhere (`resolveStructInfo` hard-fails
  a member with no resolvable field type), so a genuine struct can
  never be starved of evidence and misread as an enum. A plain enum is
  emitted as one C `enum` typedef (`pebble_enum_<typeID>_t`) with one
  named constant per variant (`pebble_variant_<memberSymbol>`) in the
  enum's declared order (`TypeDecl.Members`) — that declared order
  *is* the discriminant (`Members[i]` gets C value `i`), so switch
  case labels and stored values agree with the typedef by
  construction. Supported: a variant literal (`Color.green`, an
  `EnumVariantValue`, or the zero-payload call form `Color.red()`, a
  payload-less `VariantConstruct` — both confirmed reachable from real
  source) as an enum-typed local's initializer or a reassignment's new
  value; a `CaseValue`-based switch case (`buildSwitch`/
  `buildCaseLabel`, previously a hard rejection since 10.31) for an
  enum-typed subject, multi-value cases and `else`/`default:` unchanged
  from 10.31; and — a genuine surprise the investigation confirmed via
  real fixtures rather than assumed unreachable — **enum comparison**,
  both equality *and* the ordering operators (`<`, `<=`, `>`, `>=`),
  all six confirmed checker-reachable and lowered to the plain C
  operator on the underlying discriminant. Enum-typed function
  parameters/results, and enum-typed tuple/struct/array/optional
  elements or fields, remain clean rejections (deliberately out of
  scope, threaded through every `*CType` helper so each names the enum
  type explicitly rather than falling through to a generic struct
  error). A genuine **checker limitation** was found and reported (not
  fixed, out of this slice's scope): the full check pipeline cannot
  build *any* tagged-union program at all today — `var c Choice =
  Choice.value(5);` fails `C0601`, and even a top-level `let` fails
  `C0616` — so the tagged-union rejection test had to hand-build its
  IR directly through `tir.Builder` rather than via `buildFixture`. A
  stale pre-existing test (`TestEmitSwitchRejectsCaseValue`, whose own
  comment claimed enum cases "cannot be constructed from real
  source") was replaced with a comment pointing at the now-passing
  coverage, since the shape it skipped is exactly what this slice
  implements. Verified end-to-end (each of the three variants
  independently selecting its own switch case; a multi-value case on
  an enum subject, both member values; an `else` arm, both the
  fallthrough and a direct-case-hit path; block- and bare-return case
  bodies; reassignment; the zero-payload call-construction form;
  equality both true and false; an ordering comparison; an enum
  comparison as a `while` condition; an unused enum local under strict
  warnings; an enum switch inside a helper; an enum local inside a
  loop body; a hand-built-IR rejection test for a tagged-union
  payload) and independently outside the harness — manually compiled
  and ran the emitted C for a fixture combining an ordering comparison
  gating a multi-value-case switch, with `-Wall -Wextra -Werror`,
  confirming exit code 10 and a typedef whose constants are ordered
  exactly as predicted.
- **10.35 — tagged-union (`union enum`) locals and discriminant-only
  switch matching** (`compiler/internal/backend`): Phase 3's second
  slice, completing enum/tagged-union support. Depends on a separate
  checker fix landed in this session (commit `7feaf0c`,
  `compiler/internal/check/declaration_facts.go`): a tagged-union
  variant's construction (`Choice.value(5)`) previously had its
  inferred type wrongly published as its own *payload* type instead of
  the declaring union type, so the full checker pipeline could not
  build any program assigning one to a union-typed destination at all;
  with that fixed, tagged unions became newly reachable from real
  source. A second scoping finding — confirmed by reading the language
  spec (`spec/compiler/OPEN-DECISIONS.md`) and the parser
  (`parseSwitchCase`, which parses a case value as a bare expression
  with no pattern-binding syntax) — is that **nothing in Pebble can
  read a tagged union's payload back out of a matched case**, so this
  slice is construction + storage + discriminant-only matching only;
  there is no payload-read path to implement because none exists in
  the language. A tagged union with at least one non-void variant
  whose construction reaches this backend lowers to a tagged struct
  (`pebble_union_<typeID>_t`): a `tag` field typed as the same
  discriminant enum a plain enum emits (`pebble_enum_<typeID>_t`,
  reused verbatim via `buildEnumTypedef`) plus a `payload` union with
  one member per non-void variant *actually constructed somewhere in
  the reachable program* (a variant with no construction site needs no
  union member, since nothing ever reads or writes it), each member
  named `pebble_field_<memberSymbol>` — the same naming convention a
  struct field uses, deliberately distinct from
  `pebble_variant_<memberSymbol>` (the *enum constant*, i.e. the tag
  value), so the two names can never collide. Construction
  (`Choice.value(5)`) lowers to a C99 compound literal:
  `(pebble_union_<typeID>_t){ .tag = pebble_variant_<member>, .payload
  = { .pebble_field_<member> = <payload> } }`; a payload-less
  construction leaves the `payload` union unspecified (legal C — the
  tag alone determines which member, if any, is meaningful). A
  tagged-union switch subject reads `.tag` (a local reference or an
  inline construction used directly as the subject, confirmed
  checker-reachable) and its `CaseValue` case labels are byte-identical
  to a plain enum's — the discriminant ordinal scheme is shared.
  Payloads are restricted to exactly the entry's resolved width or
  bool (mirroring every other aggregate slice's own scalar-only
  scope); a tuple/struct/array/optional/str/nested-enum payload, or an
  unanchored-int literal-arithmetic payload, is a clean rejection
  naming what's unsupported. A `union enum` whose every variant is
  payload-less needs no new code at all — it was already reachable
  through 10.34's plain-enum path unmodified, confirmed by a test
  rather than assumed. Comparison between two tagged-union values is
  confirmed checker-unreachable (`C0603`), so nothing needed
  rejecting there. The obsolete 10.34 hand-built-IR rejection test
  (`TestEmitRejectsTaggedUnionPayload`, whose fixture the checker
  fix now makes buildable through the ordinary pipeline) was replaced
  with 14 new tests. Verified end-to-end (a payload-carrying and a
  payload-less variant each firing their own switch case; a
  multi-value case; an `else` arm; reassignment from a payload-less to
  a payload-carrying construction; a bool payload; two non-void
  variants with differing payload types, both selecting correctly; a
  variant construction used directly as a switch subject; a payload
  round-trip proved through the only observable channel that exists —
  an anchored overflowing payload aborting at construction, since the
  language has no way to read the value back directly; the all-void
  `union enum` case; a union switch inside a helper; an unused union
  local under strict warnings; a rejection test for a non-scalar
  payload) and independently outside the harness — manually compiled
  and ran the emitted C for a fixture with two non-void variants of
  different payload types (one `i32`, one `bool`) constructed into two
  separate locals and switched on, with `-Wall -Wextra -Werror`,
  confirming exit code 1 and a tagged-struct typedef with both
  distinct union members declared exactly as predicted.

  **Phase 3 (enums/tagged unions) is now fully complete** — 10.34 and
  10.35, plus the prerequisite checker fix.
- **10.36 — str reassignment and str-typed function parameters/
  results** (`compiler/internal/backend`): Phase 4's (strings/slices)
  first slice — deliberately the smallest, lowest-risk piece,
  closing two gaps the file's own doc comment already named as
  out of scope, neither needing any new runtime primitive (confirmed:
  `runtime/src/str.c` has exactly one function, `pebble_rt_str_eq`,
  unchanged by this slice). A str-typed local may now be reassigned —
  `s = "hi";` — but only from a string literal, the same single
  initializer shape a str local's declaration already accepts;
  reassigning from a str-typed local, a call result, or string
  concatenation is confirmed checker-reachable and a clean rejection
  naming what was found, not implemented. A helper function may now
  declare str-typed parameters and a str result, each declared as the
  runtime ABI's fixed `PebbleStr` (no typedef, exactly like a str
  local). A str-returning helper's result is supported in exactly
  three positions, each confirmed checker-reachable: a matching
  str-typed local's declaration initializer, a `==`/`!=` comparison
  operand, and another str-returning helper's own return value — all
  routed through a single shared `buildStrOperand`, extended with a
  `DirectCall` case, so a str value builds identically regardless of
  which of the three positions it appears in. A new
  `buildStrLiteralValue` helper was extracted so a str local's
  declaration, a bare-literal comparison operand, and a reassignment
  all emit byte-identical `PebbleStr` construction text from the same
  literal — one source of truth rather than three copies that could
  drift. Verified end-to-end (reassignment observed indirectly via a
  subsequent `==` comparison, since this backend has no way to
  directly return/print a str's contents; an escaped-literal
  reassignment; a str parameter passed a literal and a local at
  different call sites; a str-returning helper's result used in a
  local declaration, forwarded by reference, compared directly with no
  intermediate local, chained into another helper's return, and passed
  as another call's argument; rejection tests for reassigning a str
  local from a local, a call, and a concatenation) and independently
  outside the harness — manually compiled and ran the emitted C for a
  fixture combining a str parameter, an in-body reassignment, and a
  str-returning helper's result compared in the caller, with
  `-Wall -Wextra -Werror`, confirming exit code 1. str indexing and
  ordering comparisons between strs remain out of scope, unrelated to
  the decision below.

  **Decision — str concatenation and interpolation are deferred to a
  user-level `String` type, not a compiler/runtime primitive.**
  Reached in discussion after this slice landed, before scoping any
  further string work. `str` (`PebbleStr`) stays exactly what it is: a
  fixed, non-owning view — it will never grow a `+` operator or any
  other mechanism that allocates behind an innocuous-looking
  expression. The reasoning: the moment concatenation exists, someone
  has to decide who owns and eventually frees the new buffer it
  allocates — and Pebble's slice/string ownership and lifetime model
  is an explicit, acknowledged **open language design question**
  (`spec/compiler/proposals/open-language-decisions.md` §2.4,
  `OPEN-DECISIONS.md` highest-priority #4), not something this backend
  work should quietly settle by picking a runtime representation.
  Deciding it properly (Rust-style ownership/borrowing, C-style manual
  free, an arena, refcounting) is a real language-design project on
  its own, not an incidental detail of adding one operator. Separately
  — and independent of that unresolved question — hiding an allocation
  behind `+` is a design smell on its own merits (Rust's own `String +
  &str` is widely considered a wart for exactly this reason; Go's
  `strings.Builder` exists because plain concatenation hides its
  cost); an explicit method call is the more honest shape regardless
  of how ownership eventually gets decided.

  A `String` type already exists at [`std/string.peb`](../../../std/string.peb),
  apparently written for the old (pre-C-rewrite) backend: a plain
  struct (`data *char; len usize; capacity usize;`) with methods
  (`push_str`, `reserve`, `substr`, `insert`, `remove`, ...), backed by
  `extern` libc calls (`strlen`, `memcmp`) and a `mem` module
  (`mem::realloc`/`mem::copy`/`mem::delete`) — ownership is manual and
  explicit (`new()` allocates, `delete()` frees), the plain-C answer,
  chosen on purpose over any compiler-enforced scheme. This is the
  intended home for concatenation (`push_str`, or a future `concat`/`+`
  method returning a new `String`) once it can actually run.

  It cannot run today: `std/string.peb` depends on backend features
  that do not exist yet in the new C backend at all — methods on
  struct types (no method-call lowering exists, only plain function
  calls), raw pointers (`*char`, `*void`, pointer arithmetic — no
  pointer lowering exists), and multi-module imports
  (`import "std:mem"`, `mem::realloc` — `Emit` takes exactly one
  compilation unit today). Concatenation/interpolation work is
  therefore blocked on those three prerequisite features landing as
  their own phases, not scoped as part of Phase 4. When those land,
  extend `std/string.peb` (or a similar user-level `String`) to
  support building a `String` from two `str` values, rather than
  reopening this decision.

  With concatenation/interpolation off the table, Phase 4 (strings/
  slices) is retargeted at general slice types (`[]T`, `types.Slice`
  in the type snapshot — confirmed a distinct kind from `Array`/
  `Pointer`/`Optional`, and currently entirely unlowered by this
  backend). This also directly unblocks Phase 6's `main([]str)` entry
  adapter, which needs slice-of-str argument passing regardless.
- **10.37 — slice-typed locals sliced from a fixed array, and
  indexing** (`compiler/internal/backend`, plus a prerequisite
  `runtime/` and `compiler/internal/tir`/`compiler/internal/check`
  fix): Phase 4's first slice-type slice. A slice (`[]T`) is only ever
  created by slicing an existing fixed-size array (there is no
  slice-literal construction) and is a fixed, non-owning view exactly
  like `str` — the checker's own stance on the slice ownership/
  lifetime open question (§2.4) is to let it through untouched ("06b
  neither rejects an otherwise well-typed slice for escape nor invents
  a lifetime proof"), so this needed no new design decision, only the
  same "fixed view, no ownership machinery" pattern `str` already
  uses. Lowers to a small C struct
  (`pebble_slice_<typeID>_t { <elementCType> *data; size_t len; }`,
  field names matching `PebbleStrSlice`'s own naming in
  `pebble_rt.h`), constructed from `a[start:end]` as
  `{ .data = pebble_local_<array> + <checked start>, .len =
  (size_t)(end - <checked start>) }`, with the checked start computed
  by a new runtime primitive (see below) and stored in a temp (the
  pointer offset can't itself be a sub-expression of the compound
  literal it initializes). Indexing a slice
  (`s[i]`) reuses the *exact same* `Load(CheckedIndexPlace)` machinery
  a fixed array's indexing already uses — confirmed via a real fixture
  dump before scoping the slice — extended to read `.data`/`.len`
  instead of subscripting the base directly. Element types are
  restricted to exactly the entry's resolved width or bool, mirroring
  every other aggregate slice's initial scope; a tuple/struct/array/
  optional/str/enum element is a clean rejection.

  **Prerequisite runtime primitive** (commit `50c0236`, landed before
  this slice was dispatched): `pebble_rt_checked_slice_start_i32`/
  `_i64` in `runtime/include/pebble_rt.h` / `runtime/src/bounds.c`,
  mirroring `pebble_rt_checked_index_i32`'s own convention exactly —
  validates `0 <= start <= end <= length` and returns `start`
  unchanged on success (panicking otherwise, in every configuration),
  so the check embeds inline the same way checked indexing already
  does. Verified independently via the existing
  `runtime/test/smoke_test.c` harness in both SAFE and RELEASE modes
  before any backend work began.

  **A real, load-bearing typed-IR bug was found and fixed before this
  slice could be implemented at all** (commit `a3e6721`): a first
  dispatch attempt correctly stopped rather than guess, having found
  that a 2-child `CheckedSlice` node was genuinely ambiguous — `a[1:]`
  (start-only) and `a[:3]` (end-only) produced byte-identical node
  shapes (`Children=[base, bound]`), with no field on `tir.Node` or
  any `Unit` API able to recover which bound the lone trailing child
  was. Confirmed independently via real fixture dumps before
  accepting the report. Fixed directly: `tir.Node` gained
  `SliceStartPresent`/`SliceEndPresent bool` fields, set from the
  exact same `StartPresent`/`EndPresent` signal the checker's own
  `indexRecord` already computes at the point `CheckedSlice` is built
  (`ir_builder_value.go`'s `expressionSlice` and `expressionBracket`
  cases) — no new computation, just retaining information already in
  hand — with `tir/verify.go` now enforcing the child count matches
  what the two flags imply. The backend slice was then re-dispatched
  against the fix.

  The second implementation attempt also needed a fix during review:
  the checked-start result was stored in a temp hardcoded to
  `int32_t` regardless of the entry's own width, so an i64 entry's
  `pebble_rt_checked_slice_start_i64` call (returning `int64_t`) was
  silently narrowed — masked in the dispatched tests because their
  bound values were small enough to survive truncation unchanged.
  Fixed directly (one line, `cType(width)` instead of the hardcoded
  `int32_t`) rather than a full re-dispatch, given the rest of the
  diff was solid; confirmed with an i64 fixture compiled and run
  outside the harness afterward. Verified end-to-end (both bounds
  explicit; each of the three bound-omission shapes, each confirming
  the resolved default actually behaves correctly at runtime, not
  just that it compiles; a bool-element slice; an i64-entry slice; an
  out-of-range slice-construction bound aborting at runtime; an
  out-of-range index into a valid slice aborting at runtime; a
  rejection test for a tuple-element slice) and independently outside
  the harness — manually compiled and ran the emitted C for the
  i64-entry fixture with `-Wall -Wextra -Werror`, confirming exit code
  200 and the corrected `int64_t` temp declaration.
- **10.38 — slice-typed function parameters and return values**
  (`compiler/internal/backend`): closes the two remaining gaps 10.37
  deliberately left open (locals only). A slice-typed parameter reuses
  10.37's own typedef unchanged (`pebble_slice_<typeID>_t`) and seeds
  the callee's scope exactly like a slice local, so an index inside
  the body routes through the same `Load(CheckedIndexPlace)` machinery
  with no new code. A slice-typed **return** needed real handling,
  confirmed via a real fixture before any code was written: a
  slice-returning helper's tail `return a[1:3];` is a bare
  `CheckedSlice` construction, not a reference to a pre-declared
  local — and that construction needs 10.37's own two-statement shape
  (a temp holding the checked-start result, then the compound literal
  using it), which doesn't fit into a single-expression `return`.
  Solved by following the exact precedent `DeferChain` (10.32) already
  established for the same shape of problem — `buildBlock`'s `Return`
  case (and `buildSwitchCaseBody`'s bare-`Return` case) already thread
  an extra statement in *before* the final `return` line for deferred
  cleanup; a new `buildSliceReturnValue` returns the temp-declaration
  text and the return expression separately, threaded into the exact
  same pre-return-statement slot. The construction logic itself was
  extracted into a shared `buildSliceConstruction`, parameterized on
  the temp's name, so both the local-declaration and return call sites
  are one source of truth rather than two copies that could drift —
  the return-side temp is named from the return value node's own
  `NodeID` (`pebble_slice_ret_<nodeID>`) rather than a local symbol
  (a return has none), confirmed distinct from a local's
  `pebble_slice_start_<symbol>` naming even in principle. A
  slice-typed local declared from a call result
  (`var s []i32 = helper();`) was also confirmed reachable and added,
  mirroring 10.36's own `str`-returning-call local-declaration case.
  Call-site argument passing accepts only a reference to an
  already-declared slice-typed local — an inline construction used
  directly as a call argument (`f(a[1:3])`) is confirmed
  checker-reachable but a deliberate, explicit clean rejection: a C
  function argument is a pure expression position with nowhere to
  place the temp-declaration statement the construction needs, and
  this backend does not reach for a GNU statement-expression or any
  other workaround to make it fit. Re-slicing a slice remains rejected
  by 10.37's existing "slice base is not an array-typed local" check,
  confirmed unchanged. Verified end-to-end (a slice parameter indexed
  inside its helper, both `i32` and bool-element and `i64` variants; a
  slice-returning helper's inline construction, forwarded parameter,
  and forwarded local, each independently proving the two return paths
  both actually execute correctly, not just compile; the i64 side of
  the return-side construction specifically, since a width bug was
  found in exactly this construction shape during 10.37's own review;
  two slice constructions in an `if`/`else` tail's two arms; three
  slice constructions across a `switch`'s case bodies, each getting
  its own uniquely-named temp; a rejection test for an inline
  construction used as a call argument) and independently outside the
  harness — manually compiled and ran the emitted C for the
  three-case `switch` fixture with `-Wall -Wextra -Werror`, confirming
  exit code 2 and that all three case-local temps
  (`pebble_slice_ret_19`/`_25`/`_31`) are distinct and correctly
  scoped inside their own case braces.
- **10.39 — indexed element writes for arrays and slices**
  (`compiler/internal/backend`): while scoping "slice element writes"
  as a follow-up to 10.37/10.38, a real fixture revealed the gap was
  bigger and the fix simpler than expected — **array element writes
  were also unsupported**, not just slice ones (`buildStoreCore`
  unconditionally rejected any `Store` whose place wasn't a plain
  `StoragePlace`, confirmed via `a[0] = 9;` failing on a plain array
  with no slices involved). The fix is small because the hard part
  was already built: `buildPlaceLValue`'s own `CheckedIndexPlace` case
  (used, until now, only by the read side —
  `Load(CheckedIndexPlace)`) already built the correct bounds-checked
  lvalue text for *both* an array base and a slice base. This slice
  just routes a `CheckedIndexPlace` `Store` target through that same
  builder — unchanged — then dispatches the right-hand value against
  the resolved element type (the entry's width or bool), reusing
  every existing bounds-check call site rather than hand-building a
  new one. Confirmed reachable and correctly handled: writing to an
  element of a slice-typed *parameter* (not just a local) resolves
  identically to a local, since 10.38 already seeds a parameter into
  the same scope a local uses. Confirmed out of scope and left
  unchanged: compound assignment to an indexed place (`arr[i] += 1;`,
  a `tir.CompoundStore` the leading-statement dispatch already
  rejects). The non-width/bool element rejection branch is real
  (confirmed reachable via a tuple-element array write) but is
  currently caught earlier, at typedef-build time, before the store
  is ever reached — retained as defense, not dead code. Verified
  end-to-end (an array write and a slice write, each read back to
  confirm the value actually changed, not just that it compiled; a
  slice-typed parameter's element written inside a helper and
  observed by the caller reading its own array afterward — the real
  proof a slice's write reaches the same backing storage the caller
  owns, since a slice is a non-owning view; bool-element writes for
  both array and slice; an i64-entry write; an out-of-bounds array
  write and an out-of-bounds slice write, both aborting at runtime
  through the same `pebble_rt_checked_index_i32`/`_i64` calls the read
  side already uses) and independently outside the harness — manually
  compiled and ran the emitted C for the slice-parameter-write fixture
  with `-Wall -Wextra -Werror`, confirming the write inside the helper
  (`pebble_local_25.data[...] = 9;`) correctly mutated the caller's
  own array, read back as exit code 9 after the call returned.

  **Prerequisite runtime primitive** (commit `f79166f`, landed just
  before this slice, for the *next* piece of this batch — str ordering
  comparisons, not yet dispatched at the time 10.39 was written):
  `pebble_rt_str_cmp` in `runtime/include/pebble_rt.h` /
  `runtime/src/str.c`, the same `memcmp`/`strcmp` contract
  (negative/zero/positive; a shared-prefix tie breaks toward the
  shorter string). Verified independently via the existing
  `runtime/test/smoke_test.c` harness in both SAFE and RELEASE modes.
- **10.40 — ordering comparisons between str values**
  (`compiler/internal/backend`): the second piece of the batch above,
  using `pebble_rt_str_cmp` (commit `f79166f`). `buildComparison`'s
  str branch previously accepted only `==`/`!=`, explicitly rejecting
  an ordering comparison (`<`, `<=`, `>`, `>=`) even though the
  checker accepts it — the existing doc comment already flagged this
  as a real, confirmed-reachable gap. The fix reuses everything
  already in place: `op` (the operator's C spelling) is already
  validated up front by the shared `comparisonOperator` helper before
  any type-specific branch runs, so removing the str-specific
  ordering rejection was sufficient — `==`/`!=` keep their existing
  `pebble_rt_str_eq`-based lowering unchanged, and the four ordering
  operators now emit `pebble_rt_str_cmp(<left>, <right>) <op> 0`.
  Verified end-to-end (all four ordering operators, each proving both
  a true and a false outcome, not just that it compiles; a literal
  operand; a regression check confirming `==`/`!=` still use
  `pebble_rt_str_eq` and never `pebble_rt_str_cmp`) and independently
  outside the harness — manually compiled and ran the emitted C for
  the prefix-tie-break edge case (`"hi" < "hi!"`, the one case a naive
  comparison could get backwards) with `-Wall -Wextra -Werror`,
  confirming exit code 10 (true — the shorter string correctly sorts
  first).
- **10.41 — `char` as a scalar local/parameter/result type**
  (`compiler/internal/backend`, plus a prerequisite `runtime/`
  addition): the first prerequisite for closing Phase 4's last real
  gap, str indexing (`s[0]`) — confirmed via a real fixture that
  `s[0]`'s result type is `char`, so char had to exist as a supported
  scalar before indexing could return one. `char` was a real,
  already-typed builtin with zero backend handling before this slice;
  it is a full Unicode scalar value (confirmed via the parser spec and
  `tir.Literal.Char`'s own `rune`/`int32` field), not a single byte,
  so its C representation is a fixed `int32_t` — independent of the
  entry's own resolved i32/i64 width, the same way `str` and slice
  types are independent of it. Mirrors `bool`/`str`'s own support
  surface one for one: char-typed locals (from a literal, a char-typed
  local reference, or a call to a char-returning helper — all three
  confirmed checker-reachable, the same three shapes `str`'s own
  `buildStrOperand` already handles), reassignment (from any of the
  same three shapes), all six comparisons (`==`, `!=`, and — a genuine
  surprise confirmed via a real fixture rather than assumed
  unreachable, the same kind of finding 10.34's enum-ordering
  confirmation was — all four ordering operators too, since comparing
  Unicode scalar values numerically is well-defined), and char-typed
  parameters/results/call arguments. One shared `buildCharOperand`
  builds a char value in all six positions, mirroring `buildStrOperand`
  exactly.

  **Prerequisite runtime primitive** (commit `8fb8b21`, landed before
  this slice was dispatched, for the *next* piece — str indexing
  itself, not yet dispatched at the time 10.41 was written):
  `pebble_rt_str_char_at_i32`/`_i64` in `runtime/include/pebble_rt.h`
  / `runtime/src/str.c` — a real UTF-8 decoder, not a mechanical
  wrapper, since `s[i]` is a Unicode-scalar-value index (confirmed via
  spec `06b-validation-and-typed-ir.md`: "`str` | one integer | `char`,
  Unicode-scalar index"), not a byte offset, so finding "the i'th
  codepoint" requires walking and decoding the variable-width UTF-8
  byte sequence from the start — O(index) work, not O(1). Panics in
  every configuration on a negative index, an index past the last
  codepoint, or any malformed UTF-8 encountered along the way (an
  invalid lead byte, an invalid continuation byte, or a sequence
  truncated by the string's own length) — `PebbleStr`'s bytes are not
  guaranteed to be valid UTF-8. Verified standalone against real
  multi-byte UTF-8 (1/2/3/4-byte sequences, decoding `"aé€😀b"`
  correctly at every index) and all five panic paths, before being
  added to `runtime/test/smoke_test.c` and re-verified there in both
  SAFE and RELEASE modes.

  Verified end-to-end for the char slice itself (equality both
  directions; `!=`; a non-ASCII literal (`'é'`, U+00E9) and an emoji
  literal (`'😀'`, U+1F600) each round-tripping their full scalar
  value through equality, not just ASCII; reassignment from a literal
  and from another char local; ordering comparison both directions;
  a local declared from another local; a char parameter and result
  called and compared, both a matching and a distinguishing outcome,
  proving the value that survives the call is the actual argument, not
  a fixed constant) and independently outside the harness — manually
  compiled and ran the emitted C for the emoji-through-a-helper-call
  fixture with `-Wall -Wextra -Werror`, confirming exit code 1 (the
  full 21-bit scalar value `128512` survived the `int32_t` parameter/
  return round trip intact). A pre-existing rejection test for str
  indexing was updated for its now-more-accurate error message: char
  is a supported local type since this slice, so the rejection now
  comes from `buildCharOperand` refusing the `CheckedIndex` initializer
  shape rather than from char itself being unsupported — str indexing
  remains rejected, unchanged, pending 10.42.
- **10.42 — str indexing (`s[i]` returning `char`)**
  (`compiler/internal/backend`): closes Phase 4's last real gap, using
  both prerequisites landed for exactly this purpose (10.41's `char`
  scalar type, commit `09ab763`; the `pebble_rt_str_char_at_i32`/`_i64`
  UTF-8 decoder, commit `8fb8b21`). `s[i]` lowers to a bare
  `tir.CheckedIndex` value node — not `Load(CheckedIndexPlace)`, the
  shape array/slice indexing uses — because a str's byte-level content
  is not addressable as a place, so the read is a pure
  decode-to-value operation. Added as a fourth shape to the existing
  `buildCharOperand` (alongside its `CharLiteral`/`SymbolValue`/
  `DirectCall` cases), reusing `buildStrOperand` for the base
  (confirmed reachable: a str-typed local reference, a bare string
  literal directly, or a call to a str-returning helper — the same
  three shapes `buildStrOperand` already builds unchanged) and the
  same int-literal/int-`SymbolValue`/`buildExpr` index dispatch
  `buildArrayPlaceRead` already established. A real edge case was
  confirmed and correctly handled rather than assumed away: indexing
  an array literal directly (`['h', 'i'][0]`) *also* produces a bare
  `CheckedIndex` with a `char`-typed result (an array literal has no
  addressable place either), so a non-str base is confirmed
  checker-reachable and is a clean rejection naming what was found,
  not silently routed through the str decoder. Verified end-to-end
  (ASCII indexing compared against a char literal; a bare string
  literal base; a multi-byte fixture proving the Unicode-scalar-value
  index semantics — indexing past a 2-byte character lands on the
  *next* codepoint, not partway through the first one's bytes; the
  same proof at 4-byte-sequence scale with an emoji; a
  runtime-computed index via checked arithmetic; a plain width-typed
  local-reference index; a range-loop iterator used directly as the
  index; an out-of-range index and a negative index each aborting at
  runtime; the i64-entry width-generic path; the non-str-base
  rejection) and independently outside the harness — manually
  compiled and ran the emitted C for a fixture indexing both the
  emoji and the character immediately after it in `"a😀b"` with
  `-Wall -Wextra -Werror`, confirming exit code 1 (both codepoints
  decoded correctly, proving the decoder walks codepoints, not bytes).

  **Phase 4 (strings/slices) is now complete.** Concatenation and
  interpolation remain deliberately deferred to a user-level `String`
  type (the documented decision after 10.36), pending methods,
  pointers, and multi-module imports landing as their own phases —
  everything else scoped for `str`, `char`, and `[]T` is done: str
  literals, comparison (equality and ordering), reassignment,
  parameters/results, and indexing; `char` as a first-class scalar;
  and slices — locals, indexed reads and writes, parameters, and
  returns.
- **10.43 — thread real source location into panic diagnostics, part
  1 of 2** (`compiler/internal/backend`, on top of the runtime half
  landed in commit `3d949d4`): the old backend embedded the *Pebble*
  source file and line at every generated safety-check call site
  (`expr->span.file`/`expr->span.start_line`); the new one never has —
  every `pebble_rt_checked_*` call this backend has ever emitted,
  across every slice this whole phase, has always passed nothing,
  reporting only the panic kind and never where. `Emit()` gains a
  `*source.FileSet` parameter (the checker's own `check.Inputs.Sources`
  the caller already builds and previously discarded after `Check()`
  — nothing new needed synthesizing), threaded through the ~53
  builder functions in `buildExpr`/`buildBoolExpr`'s transitive
  closure, and a new `buildSourceLoc(fileSet, span)` resolves one
  node's `Span` to a `(PebbleSourceLoc){"file", line, col}` C
  compound literal (`FileSet.File` → `File.Position`, both already
  real, precise APIs — confirmed nothing needed inventing here
  either). This slice wires real locations into checked arithmetic
  (`CheckedArithmetic`/`CheckedNegate`, using the node's own `Span`)
  and checked optional unwrap (all four call sites); every other
  checked-call category (array/slice indexing, checked slice range,
  str indexing) gets a placeholder `(PebbleSourceLoc){0}` so the
  emitted C stays valid against the runtime's new signatures, with
  real locations for those deferred to a second slice. Verified
  end-to-end (an arithmetic overflow and an absent-optional unwrap,
  each still panicking correctly and each confirmed via the emitted
  C text to carry a real, non-placeholder location; a placeholder
  category — checked array indexing — confirmed to still emit `{0}`
  and still compile/run correctly, proving this slice didn't disturb
  anything outside its own scope) and independently outside the
  harness — manually compiled and ran the emitted C for an
  absent-optional unwrap with `-Wall -Wextra -Werror`, producing the
  real panic report `pebble: unwrap of empty optional at
  main.peb:1:43`.
- **10.44 — thread real source location into panic diagnostics, part
  2 of 2** (`compiler/internal/backend`, on top of 10.43): replaces
  every remaining placeholder `(PebbleSourceLoc){0}` call site with a
  real, resolved location, reusing `buildSourceLoc` and the `fileSet`
  plumbing 10.43 already threaded — no new helper, no new parameter
  wiring. Checked array/slice indexing (`buildArrayPlaceRead`, both
  the array- and slice-base call sites, and `buildPlaceLValue`'s
  `CheckedIndexPlace` write-side case) now use the `CheckedIndexPlace`
  node's own `Span`; checked slice-range construction
  (`buildSliceConstruction`) now uses the `CheckedSlice` node's own
  `Span`; str indexing (`buildCharOperand`'s `tir.CheckedIndex` case)
  now uses the `CheckedIndex` node's own `Span`. Every checked-call
  category this backend emits now carries a real Pebble source
  location — the placeholder is gone. Verified end-to-end with four
  new proof tests (out-of-bounds array index, out-of-bounds slice
  element index, an invalid slice-construction range, and an
  out-of-range str index — each panicking correctly and each confirmed
  via the emitted C text to no longer contain
  `(PebbleSourceLoc){0}` anywhere), the full pre-existing backend
  suite (10.1 through 10.43, all passing unmodified except for ~14
  hardcoded-placeholder string assertions updated to match the new
  real-location text), and independently outside the harness —
  manually compiled and ran the emitted C for an out-of-bounds array
  index with `-Wall -Wextra -Werror`, producing the real panic report
  `pebble: index out of bounds at main.peb:1:68`.
- **10.45 — support `types.Int` as a third entry-point width**
  (`compiler/internal/backend`): closes a previously-unknown gap
  between `check.EntryRequired` (which only ever accepted a `main`
  returning `Void` or `Int` — the bare `int` type, confirmed the real,
  documented entry-point convention via `ADVANCED.md` and every fixture
  under `compiler/tests/module/valid/*/main.peb`) and `Emit`'s own
  entry validation (which only accepted `Void`, `I32`, or `I64` and
  explicitly rejected `Int`, by design). The two had never overlapped
  for any exit-code-returning program: every existing backend test
  proving an integer exit code used an `i32`/`i64` main with
  `requireEntry: false`, so `EntryRequired` combined with a real
  integer-returning entry had literally never been exercised anywhere
  in this codebase before. Fixed by adding `types.Int` as a third,
  fully independent exact width alongside `I32`/`I64` in the five
  width-dispatch sites (`validateEntrySignature`, `isWidth`, `cType`,
  `checkedSuffix`, plus confirming `entryReturnType` and
  `arrayLengthLiteral` already covered it via their existing
  non-I64-defaults-to-32-bit paths) — `Int` maps to C `int32_t` and
  reuses the existing `pebble_rt_checked_*_i32` runtime family
  unchanged, no new runtime primitives. `isWidth` deliberately treats
  `Int` as its own exact width, not aliased to `I32`: a body under an
  `int`-entry must use `int`-typed locals/arithmetic consistently,
  exactly like the pre-existing i32-vs-i64 no-mixing rule. Verified
  end-to-end with four new tests (a trivial expression-bodied `int`
  entry, a checked-addition `int` entry, a checked array read under
  `int` width, and an out-of-bounds abort under `int` width — all with
  `requireEntry: true`, actually exercising the checker's entry gate
  this time), the full pre-existing backend suite (one stale
  hardcoded-error-message assertion updated to include `int` in its
  expected text), and the full repo suite.
- **10.46 — `pebc`, the first real CLI driver (Phase 6)**
  (new `compiler/cmd/pebc` package): `backend.Emit` had never been
  called from anywhere except Go tests until this slice. `pebc
  [-o path] <entry.peb>` runs the real pipeline —
  `module.FileSystemProvider` (already existed, unused until now) →
  `symbol.Resolve` → `types.New` → `check.Check` with
  `check.EntryRequired` genuinely turned on → `backend.Emit` — and
  writes the emitted C to a file or stdout, rendering diagnostics via
  `diagnostic.RenderText` on any failure. Also settled an open question
  about multi-module imports: the plan's own text previously assumed
  `import` support was blocked on `Emit` accepting more than one
  compilation unit, but investigation found `check.Check` already
  merges an entire module graph into a single `*tir.Unit` before
  `Emit` ever sees it — nobody had ever actually run a multi-module
  program through the C backend before, though, since every existing
  backend fixture was single-file. A real two-file `import "./helper";`
  fixture, run end-to-end through `pebc` and compiled/run for real,
  confirms **multi-module import already works through this backend**
  — no additional lowering work needed for that specific gap. Verified
  with four tests (single-file happy path, a real type error reported
  on stderr, a missing-entry-point error, and the multi-module proof),
  the full repo suite, and independently outside the harness — built
  the actual `pebc` binary, ran it against a real `.peb` file on
  disk (`fn main() int { let a int = 19; let b int = 23; return a +
  b; }`), and compiled/ran its emitted C standalone, producing exit
  code 42.

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
