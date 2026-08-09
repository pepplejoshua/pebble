# 15 — Allocator/Context as ordinary structs, not special runtime types

**Status:** decision made, not yet implemented. Two orc dispatches
(`opencode-go/deepseek-v4-flash`, then escalated to `openai/gpt-5.6-luna`)
were sent to turn this into a full implementation plan; neither produced a
usable result (no deliverable, near-empty worklogs) — both attempts are
recorded here as failed, not repeated a third time tonight. This document is
the human-reasoned decision record and starting point for a real
investigation dispatch next session.

## The problem

Tracker item 1 of `13-v1-parity-gap-analysis.md`: a constructed `Allocator`
value cannot be passed as a function argument, returned from a function, or
assigned into a struct field after construction — only the literal
`Allocator.{ ptr=..., alloc=..., realloc=..., free=... }` itself compiles.

## Root cause

`Allocator` and `Context` are `SymbolRuntimeType` in v2 — a compiler-
synthesized symbol kind with no real `.peb` declaration, registered directly
in `compiler/internal/symbol/resolve.go`'s `installPrelude`. Every general
mechanism for moving a struct-shaped value through a program (argument
passing, returns, field assignment) was built assuming the value is backed
by a `TypeDeclaration` from the normal aggregate-collection walk over parsed
declarations — and `Allocator`/`Context` are deliberately excluded from that
walk (an earlier fix, in git history as `f5403b5`, added this exclusion to
route around a different, unrelated bug at the time). Nobody built a second,
parallel version of argument-passing/return/field-assignment for the excluded
case. The gap isn't a missing feature so much as a self-inflicted one: real
user structs already support all three operations correctly.

## Why not just patch the special path

That was the original plan (tracker item 1's original slices: fix
argument-passing, then field-assignment, then returns, one at a time, inside
the existing `SymbolRuntimeType` machinery). Rejected in favor of the
approach below because patching the special path only closes the three
failures already found — it doesn't prevent the next one, and this exact
"the special path doesn't support X yet" shape is what produced tonight's
bug in the first place.

## The actual fix, confirmed against two real precedents

**Pebble v1** (`src/type.c`, `type_init`, lines ~604–680) registers
`Allocator` and `Context` via `type_create_struct(...)` — the *same*
function `src/checker.c` calls for ordinary parsed struct declarations
(lines ~1812, ~1868, ~6527) — then `type_register`/`canonical_register`s the
result exactly like any builtin. There is no special runtime-type machinery
in v1 at all. Every general struct mechanism works on `Allocator`/`Context`
in v1 for free, because nothing downstream needs to know they're special.

**Odin** (a real, shipped systems language with the same `context`/
`Allocator` design intent) does the same split: `Context` and `Allocator`
are ordinary struct types defined in normal Odin source
(`core/runtime`/`base:runtime`). The only genuinely special thing in Odin is
that the compiler automatically threads a `context` value as a hidden
argument through every `proc` call — a calling-convention feature, entirely
separate from whether the *type* is special.

Pebble v2 already has the calling-convention half of this correctly built:
every emitted Pebble-convention C function already takes a hidden
`PebbleContext *ctx` parameter, threaded automatically (visible in every
emitted-C dump this session, e.g. `pebble_fn_24(PebbleContext *ctx, ...)`).
**That part is not broken and must not change.** The bug is entirely in
treating the *type* as special, not in the (correct, necessary) implicit
threading.

## The redesign

Write `Allocator` and `Context` as literal Pebble struct declarations in a
real `.peb` source file — a prelude — parsed and resolved before every other
module, with its declarations injected into global/prelude scope
automatically (no explicit `import` required, matching how `context` is
already implicitly available today). They flow through the *exact same*
parser → resolver → checker → backend pipeline as any other `.peb`-declared
struct. Zero special-casing anywhere in aggregate collection,
argument-passing, returns, or field-assignment.

The one thing that must remain special, kept as small as possible: something
still needs to mark which *one* struct type is "the" context type, so the
backend keeps threading it implicitly as the hidden `ctx` parameter — e.g. a
well-known name check or a single flag on the resolved type, not a parallel
type-registration system.

## Open questions for the next real investigation (neither dispatch reached these)

1. Does `compiler/internal/module`'s graph/resolution model support
   injecting a module before user modules are resolved, with its top-level
   declarations visible to every other module without an explicit `import`?
   If not, what's the smallest addition that gives it that?
2. Full inventory of every site that currently special-cases
   `SymbolRuntimeType`/`RuntimeAllocator`/`RuntimeContext` across
   `compiler/internal/symbol`, `compiler/internal/check`,
   `compiler/internal/infer`, `compiler/internal/backend` — which become
   unnecessary and can be deleted once Allocator/Context are ordinary
   parsed structs, vs. which encode the genuinely-special ctx-threading
   behavior and need a minimal replacement marker.
3. Confirm the backend's ctx-threading decision (search `emit.go` for where
   `PebbleContext *ctx` gets added to signatures/call sites) is driven by
   calling convention (`types.Pebble`) alone, not secretly by
   `SymbolRuntimeType` — if it's already convention-driven, this whole
   redesign is safe with no changes needed to that part at all.
4. Honest size estimate once 1–3 are answered — this may need splitting into
   multiple implementation slices rather than one.

## Relationship to other open items

- Tracker item 1 (`13-v1-parity-gap-analysis.md`) should be rewritten to
  point here as the planned direction, superseding its original
  patch-the-special-path slices, once a real investigation (not another
  shallow dispatch) answers the open questions above.
- Tracker item 2 (`arena.peb` rewrite) depends on this being fixed before
  `examples/arena_alloc.peb` can pass end-to-end — `arena::init`/
  `arena::allocator` cross exactly the function boundaries this document
  describes.
