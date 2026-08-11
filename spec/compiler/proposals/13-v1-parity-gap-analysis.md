# 13 — v1 parity gaps

**Purpose.** This file is the working area for exactly ONE gap at a time,
sourced from `14-v2-v1-checker-backend-parity-audit.md`'s master findings
list. It is not a backlog. Proposal 14 is the backlog and the completion
record; this file is the scratch pad for whichever single item is currently
being reproduced, worked, and closed.

## Workflow

1. Pick the next unaddressed item from proposal 14 (its fourth-pass current
   gap table and implementation slicing notes).
2. Reproduce it for real. Document the item here, in "Active defect," with
   its exact reproduction, current failure, and known cause — this file must
   never hold an item that hasn't been reproduced.
3. Work it. Any sub-issue discovered while working it (a stale test, a
   related but distinct bug, a scope question) gets recorded here too, under
   the same active item, not spun off silently.
4. When satisfactorily complete (verified, tested, committed): wipe this
   file's "Active defect" section back to empty, mark the corresponding item
   complete in proposal 14, and pick the next item. Never carry more than one
   active item here.
5. When proposal 14's whole list is exhausted, ask Sol/Codex for another
   audit pass to find what's still missing.

## Dispatch rules (apply to the current active item only)

- Dispatch compiler and runtime logic through Orc.
- Use one small, decisive Orc slice at a time. Review and verify each slice
  before the next dispatch.
- Use `opencode-go/deepseek-v4-flash` by default. Do not give it a long,
  multi-layer task. `opencode-go/mimo-v2.5` is banned. If flash genuinely
  stalls or fails, check `orc list` for the user's own concurrent Luna usage,
  then escalate to `openai/gpt-5.6-luna`. When escalating a stuck session,
  prefer resuming the same session with the new `--model` over deleting and
  dispatching fresh.
- Before each dispatch, require a clean worktree and no active Orc or
  OpenCode worker for this repository.
- After each dispatch, inspect the diff and check for scratch files, debug
  output, scope growth, and stale tests. Run the full required verification
  and a causation check before commit and push.
- Delete failed/stalled/killed Orc sessions with `orc delete` immediately,
  not just their scratch files.

## Active defect

**Item: a non-literal bool switch subject emits an invalid native C
`switch(bool)`, failing under the mandated `-Wswitch-bool -Werror`.**

Sourced from Sol's fourth-pass audit (2026-08-10, commit `3047352`),
P1. Independently reproduced and isolated before dispatch.

**Reproduction** (confirmed against current HEAD):

```
fn choose(flag bool) int {
    switch flag {
        case true: return 1;
        else: return 0;
    }
}
fn main() int {
    return choose(true);
}
```

`go run ./cmd/pebc -run <file.peb>` fails at the `cc` step:
`error: switch condition has boolean value [-Werror,-Wswitch-bool]`.
The checker accepts this program fine (bool switch exhaustiveness is
already proven, per proposal 14's switch matrix) — this is purely a
backend C-shape bug. (A separate-looking `-Wreturn-type` error only
appeared in my FIRST reproduction attempt, which omitted the `else`
clause — that's an artifact of my own incomplete test, not a real
second bug; confirmed by re-testing with an explicit `else`, which
isolates cleanly to just `-Wswitch-bool`.)

**Known cause:** `buildSwitchStatement`
(`compiler/internal/backend/statements.go`, around line 711) routes a
bool-typed subject through `buildBoolExpr` into the SAME native-C-
`switch(...)` lowering as int/uint/char subjects. C forbids/warns on
switching directly on a `bool` expression under `-Wswitch-bool`. A
str-typed subject already has its own dedicated lowering (an if/else
chain via `buildStrSwitchStatement`, just fixed today for double-
evaluation) precisely because C switch labels must be integer
constants — bool needs similar special treatment, but the fix here is
much smaller: bool's underlying value IS already an integer (0 or 1),
so casting the subject expression to `int` before the native C switch
(`switch ((int)pebble_local_25)`) is the standard, minimal C idiom
that silences `-Wswitch-bool` without restructuring the lowering into
an if/else chain at all.

**Scope:** when the subject is bool-typed, wrap the built subject
expression in an `(int)` cast (or `int32_t`, matching whatever this
backend's existing int-cast convention is — check `cType`/similar) for
JUST the C `switch (...)` header — the case labels themselves (`case
1:`, `case 0:` or however `true`/`false` case constants are currently
spelled) don't need to change, since a `switch(int)` still compares
correctly against integer case labels. Verify the reproduction above
now compiles and runs (returns 1). Also verify: a LITERAL bool subject
(`switch true { ... }`) is unaffected (still works, was already
covered by existing tests per the ledger's "Verified V2 extension"
claim); a bool switch with both `case true`/`case false` (no `else`,
exhaustive by construction) compiles and runs correctly; existing bool
switch tests are unaffected.

<!-- Previous item, resolved 2026-08-10:

**Item: a range loop with a runtime-computed or negative-literal
descending bound silently runs zero iterations.**

Sourced from Sol's fourth-pass audit (2026-08-10, commit `3047352`),
P0. Independently reproduced before dispatch.

**Reproduction** (confirmed against current HEAD):

```
fn start_val() int { return 3; }
fn end_val() int { return 0; }
fn main() int {
    var total int = 0;
    loop start_val()..end_val() : i {
        total = total + 1;
    }
    return total;
}
```

```
fn main() int {
    var total int = 0;
    loop 0..-5 : i {
        total = total + 1;
    }
    return total;
}
```

Both `go run ./cmd/pebc -run <file.peb>` runs return `0` — `total`
never incremented, the loop body never ran, no error, no warning. A
genuinely silent semantic bug.

**Known cause:** `buildRangeLoop`
(`compiler/internal/backend/statements.go:1319`) only chooses the
descending direction (`>`/`--`) when BOTH bounds' emitted C text
parses via `strconv.Atoi` as plain decimal literals (lines 1375-1394)
— i.e. only when both bounds are compile-time-known, non-negative
integer LITERALS. A runtime bound (a call, a variable) never produces
`strconv.Atoi`-parseable text, so it silently falls through to the
"ascending" default. A negative literal ALSO fails this: `-5` in
Pebble source doesn't lower as a single `IntegerLiteral` node with
negative text — `buildRangeBound`'s literal branch
(`isNonNegativeDecimal`, line ~1449) only accepts non-negative decimal
text, so a negative bound is built via a different node shape entirely
(routed through `buildExpr`, likely emitting a checked-negation
runtime call, not plain decimal text) — which also fails the
`strconv.Atoi` literal check and falls through to ascending.

**V1's actual fix, already in production — mirror it exactly, don't
invent a new pattern.** `src/codegen.c:2568-2670` (`AST_STMT_LOOP`)
NEVER special-cases compile-time-literal bounds. It ALWAYS:
1. Evaluates both bounds once into C locals (`int loop_start0 = ...;
   int loop_end0 = ...;`).
2. Computes the step direction AT RUNTIME: `int loop_step0 =
   (loop_start0 <= loop_end0) ? 1 : -1;`.
3. Uses a ternary-conditioned loop test that works for either
   direction: `for (int loop_i0 = loop_start0; (loop_step0 > 0) ?
   (loop_i0 < loop_end0) : (loop_i0 > loop_end0); loop_i0 +=
   loop_step0) { ... }` (with `<=`/`>=` swapped in for an inclusive
   range).

This ONE uniform lowering handles compile-time-ascending,
compile-time-descending, negative-literal, and runtime-computed bounds
identically, with zero special-casing — it's strictly simpler than
V2's current two-path (literal vs. non-literal) logic, not more
complex, and it's V1 parity by construction since it's V1's own
emitted C, verbatim.

**Scope:** replace `buildRangeLoop`'s compile-time-literal direction
detection entirely with this always-runtime-direction lowering.
Verify both reproductions above now return `3` (three iterations).
Also verify: the existing compile-time-ascending case (`loop 0..3 :
i`) and compile-time-descending case (`loop 3..0 : i`, from the
earlier `8baeb8e` fix) both still produce the correct iteration count
and correct iterator values inside the body; inclusive ranges (`loop
0..=3`) in both directions; a zero-length range (`loop 3..3`) still
runs zero iterations correctly (not an off-by-one). Full suite clean,
causation-checked.

**Note — likely fixes a second tracked P0 as a byproduct.** Sol's
audit separately flagged that `buildRangeLoop` evaluates the END bound
before the START bound (a side-effecting bound records the wrong
order vs. V1). Since V1's pattern (above) evaluates start into a local
FIRST, then end into a local SECOND, as two separate sequential C
statements, adopting it here should also fix that ordering bug for
free. Confirm this explicitly with a side-effecting-bounds
reproduction (each bound a call that appends to a shared counter/log)
proving start runs before end, and report whether a separate dispatch
for that item is still needed.

**Resolution (`003141d`, 2026-08-10).** Replaced the compile-time-
literal direction detection entirely with V1's actual production
lowering (`src/codegen.c`'s `AST_STMT_LOOP`), mirrored verbatim: both
bounds evaluated once into C locals in source order, step computed at
runtime from comparing them, ternary-conditioned loop test. Verified a
runtime-bound descending range, a negative-literal range (`0..-5`
exclusive correctly visits 5 values: 0,-1,-2,-3,-4), all existing
ascending/descending/inclusive/zero-length/uint-bounded cases
unaffected. Confirmed the evaluation-order P0 (below) was also fixed
as a byproduct — no separate dispatch was needed. Causation-checked by
reverting and reproducing both original bugs.

-->

<!-- Previous item, resolved 2026-08-10:

**Item: a call-valued `str` switch subject is evaluated once per case
comparison instead of once.**

Sourced from Sol's fourth-pass audit (2026-08-10, commit `3047352`),
P0. Independently spot-verified before dispatch.

**Reproduction** (confirmed against current HEAD):

```
fn choose() str {
    print "called";
    return "b";
}
fn main() int {
    switch choose() {
        case "a": return 1;
        case "b": return 0;
        else: return 2;
    }
}
```

`go run ./cmd/pebc -run <file.peb>` prints `called` TWICE (once per
case comparison against `"a"` and `"b"`), instead of once. A genuine
silent semantic bug: any switch subject with a side effect (a call
that logs, mutates, or has any observable effect beyond its return
value) runs that side effect once per case label instead of once
total.

**Known cause:** `buildStrSwitchStatement`
(`compiler/internal/backend/statements.go:912`) builds the subject
expression ONCE via `buildStrOperand` (line 919), but a `str` switch
doesn't lower to a native C `switch` (C can't switch on a struct) — it
lowers to an if/else chain of `pebble_rt_str_eq(subjectExpr, lit)`
calls (line 972), and the same `subjectExpr` TEXT is spliced into
every one of those calls. A native C `switch(expr)` evaluates `expr`
exactly once by C's own semantics; this if/else-chain lowering gets no
such guarantee for free, and nothing here materializes the subject
into a temp first.

**Scope:** materialize the subject into a `PebbleStr` local temp once,
before the if/else chain, and reference that temp (not the raw
expression) in every `pebble_rt_str_eq` call — mirroring the
"materialize once into a per-operand temp" convention already used
elsewhere in this backend (e.g. composite print operands, the
tagged-union switch subject). Verify the reproduction above prints
`called` exactly once; verify a str-LITERAL subject (no side effect to
observe, but confirm it still compiles/runs correctly and the temp
doesn't break the no-side-effect case); verify existing str-switch
tests (from `49d0f23`) are unaffected; verify a `do { ... } while (0)`
break-wrapped chain still uses the temp correctly.

**Resolution (`b1a53e7`, 2026-08-10).** The subject is now materialized
into a `PebbleStr` temp once, before the if/else chain; every equality
check reads the temp. Verified the repro now prints once, not twice; a
str-literal/str-local subject and the break-wrapped shape are
unaffected; causation-checked by reverting and reproducing the exact
double-print.

-->



<!-- Previous item, resolved 2026-08-10:

**Item: `context`-as-value — bare `context` expression fails as a function
argument and as a `let` local's initializer.**

Sourced from proposal 15 slice 4's verification (2026-08-10) — the
`Allocator`/`Context` ordinary-struct redesign (`b54d79d`/`dee9b0f`/
`a404f14`) fully fixed `Allocator` crossing a function boundary as an
argument, a return value, and a struct-field assignment, but `Context`
specifically was never independently verified. The user asked "so we
can use context expr and allocator type as we like?", which prompted
testing `Context` on its own — it's not the same, and it's broken in
three of four value positions.

**Reproduction** (confirmed against current HEAD, `go run ./cmd/pebc -run <file.peb>`):

```
fn use_context(c: Context) void {}

fn make_local() void {
    let c = context;
}

fn returns_context() Context {
    return context;
}

struct Holder { c: Context }
fn field_works() Holder {
    return Holder.{ c = context };  // this ONE already works
}
```

- Argument: `use_context(context)` fails —
  `entry function body expression contains a call to symbol 24 whose
  argument 0 is a ContextValue, want a reference to a struct-typed
  local in scope or a struct literal (a RecordConstruct); only passing
  an already-declared struct-typed local or constructing a fresh struct
  literal inline is supported`
- Local initializer: `let c = context;` fails —
  `entry function body block declares a runtime-typed local initialized
  from a ContextValue`
- Return: `return context;` fails —
  `entry function body return statement returns a ContextValue, want a
  reference to a struct-typed local in scope, a struct literal (a
  RecordConstruct), or a call to a struct-returning helper (a
  DirectCall); only returning an already-declared struct-typed local,
  constructing a fresh struct literal inline, or forwarding a
  struct-returning helper call is supported`
- Struct-field construction value (`Holder.{ c = context }`) already
  works — confirmed via direct repro, exit 1 (no error).

**Known cause:** the bare `context` keyword expression lowers to a
distinct TIR node kind, `ContextValue` — not `SymbolValue` (the existing
runtime-identity reference handling) and not `RecordConstruct` (the
shape slice 3's Allocator-in-value-position fix added support for, see
`compiler/internal/backend/aggregates.go`'s `buildRuntimeAllocatorBraceList`
and `compiler/internal/backend/values.go`'s `buildRuntimeValue`/
`buildStructValueExpr`). `buildRuntimeValue` already has a
`node.Symbol == unit.Runtime().Context` check that returns `"(*ctx)"`
for one call path, but the argument-building, local-declaration-
building, and return-building code paths never consult it for a bare
`ContextValue` node — they only recognize `SymbolValue`/`RecordConstruct`
shapes. Needs a `ContextValue` case added to whichever functions build
call arguments, local initializers, and return values, mirroring the
existing single-site handling.

**Scope:** fix all three broken positions (argument, local initializer,
return) using the reproductions above as acceptance tests; reconfirm
the already-working struct-field-value case is unaffected; full suite
clean; causation-check against the exact errors quoted above.

-->

**Resolution (`64d2e2b`, 2026-08-10).** `buildAggregateArgument`,
`buildRuntimeLocalDeclaration`, and `buildAggregateReturnValue` each
gained a `ContextValue` case emitting `(*ctx)`, mirroring the existing
single-site handling. Verified end-to-end for all three positions via
a real alloc→write→read→free roundtrip through `default_allocator`;
the struct-field-value case and Allocator's own slice-3 paths
reconfirmed unaffected; causation-checked against the exact pre-fix
rejection messages.
