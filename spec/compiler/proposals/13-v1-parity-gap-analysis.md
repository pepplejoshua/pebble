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
