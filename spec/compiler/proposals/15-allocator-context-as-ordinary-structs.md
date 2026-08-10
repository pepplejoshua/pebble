# 15 — Allocator/Context as ordinary structs, not special runtime types

**Status:** decision made, investigated, implementation starting. Two
earlier orc dispatches (`opencode-go/deepseek-v4-flash`, then escalated to
`openai/gpt-5.6-luna`) that tried to jump straight to a full implementation
plan produced no usable result. A THIRD dispatch (`openai/gpt-5.6-luna`,
2026-08-10), scoped as read-only investigation only — answer the four
open questions below, write no code — succeeded: 70 real tool calls,
concrete file/line citations for every claim, independently spot-checked
against the actual source and confirmed accurate. See "Investigation
findings" below. The honest size estimate (question 4) is mine, synthesized
from the investigation's raw findings after its own session ran out of
budget before writing it.

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

## Investigation findings (2026-08-10, `openai/gpt-5.6-luna`, spot-checked and confirmed accurate)

**Q1 — module prelude injection: nothing exists today.** `module.Build`
loads only `config.EntryPath`, then processes authored imports in
discovery order (`build.go:67-75`, `processImports` at `build.go:112-156`).
`std:` is purely an import route (`build.go:201-212`) — there is no
implicit-visibility or prelude-injection mechanism anywhere in the module
package. `DependencyOrder` is a DFS postorder (`cycle.go:10-55`). A real
addition is needed; see the implementation plan below for the proposed
shape.

**Q2 — full special-case inventory**, confirmed by direct spot-check
(`resolve.go:118-150` matches exactly):

| Site | What it does | Disposition once Allocator/Context are ordinary structs |
|---|---|---|
| `symbol.Resolve.installPrelude` (`resolve.go:118-150`) | Synthesizes the `Allocator`/`Context` symbols and their fields directly, no `.peb` source | **Delete** — replaced by real parsed declarations from the prelude module |
| `symbol.resolveRecord` (`visit.go:511-555`) | A runtime-member branch for resolving a member access on the synthesized types | **Delete** — ordinary member resolution already handles a real struct |
| `infer.prepareRuntimePrelude`/`runtimeSymbol` (`runtime_prelude.go:10-105`) | Synthesizes declarations and validates runtime identities | **Delete** — ordinary declaration inference already handles a real struct |
| `templateForSymbol` (`type_resolver.go:174-224`) | Rejects `Context`, maps `Allocator` specially | **Delete** the special-case branches — falls through to ordinary nominal-type handling |
| Checker `buildUnit` (`check/ir_builder.go:26-55`) | Extracts runtime identities/fields | **Delete** — ordinary struct field extraction already exists |
| Backend: `locals.go:14-31`, `aggregates.go:193-314` and `1142-1159`, `collect.go:1333-1369`, `types.go:1107-1141` | Allocator ABI/type-naming and adapter logic | **Keep, minimized** — this is the genuinely-special part (the C ABI shape for `Allocator`'s function-pointer fields); audit each site during implementation to confirm it's ABI-shape logic, not identity-special-casing, and shrink to the minimum needed |

**Q3 — ctx-threading is convention-driven only, confirmed.** `helperFunction`/
`helperPrototype` (`calls.go:1888-1918`) always emit `PebbleContext *ctx`
for a Pebble-convention function; `validateHelperSignature`
(`validate.go:166-169`) rejects any non-`types.Pebble` declaration.
`buildDirectCallArgs` requires `types.Pebble` + `ContextForward`
(`calls.go:1006-1010`, confirmed by direct read) and emits
`callee(ctx, ...)` (`calls.go:1048-1052`). **No**
`SymbolRuntimeType`/`RuntimeAllocator`/`RuntimeContext` appears anywhere in
this path — the backend's context-threading needs **zero changes**. This
is the single biggest risk-reducer this investigation found: the part of
the system most load-bearing for correctness (every function call in every
compiled program) is untouched by this redesign.

**Q4 — implementation plan (synthesized from the above, since the
investigation's own session ran out of budget before writing this part):**

1. ~~**Slice 1 — prelude-injection mechanism, proven in isolation.**~~
   **RESOLVED (`b54d79d`).** Opt-in `BuildConfig.PreludePath` (empty by
   default, zero behavior change otherwise); a prelude module is parsed
   and resolved first, tagged `RolePrelude`; every ordinary module's
   scope is reparented under the prelude module's scope instead of
   directly under the builtin prelude, reusing the existing scope-chain
   lookup — no new resolution machinery. Proven end-to-end (not just at
   the checker level): a new `-prelude` CLI flag plus a real compile-
   and-run test — a prelude module declares a struct, a main module
   references it with zero imports, compiles and runs, returns 42.
   Backward compatibility and an existing `std:` import both confirmed
   unaffected. Causation-checked.
2. ~~**Slice 2 — real `.peb` prelude source, shadow-verified.**~~
   **RESOLVED (`dee9b0f`).** `compiler/prelude/runtime.peb` declares
   `Allocator`/`Context` as ordinary structs with ABI-matching field
   types, not yet wired into the default compilation path. Two-layer
   proof: field-type spellings match exactly, and the same type
   expressions resolve to byte-identical `TypeID`s as the synthesized
   version (driven under non-reserved mirror names, since the
   resolver's `reservedBuiltin` guard currently rejects a source
   declaration literally named `Allocator` — itself one of the exact
   sites slice 3 removes). A real program's existing use of `Allocator`
   confirmed completely unaffected. Purely additive; full suite passes.
3. ~~**Slice 3 — the cutover.**~~ **RESOLVED (`a404f14`).** The embedded
   prelude (`go:embed`) is now the default source of `Allocator`/`Context`
   for every compilation; `installPrelude`'s synthesis, `resolveRecord`'s
   runtime-member branch, and `templateForSymbol`'s special-casing are
   deleted; `runtime_prelude.go` is reduced to reading the already-
   prepared parsed declaration instead of hand-synthesizing one;
   `check/buildUnit`'s extraction correctly kept (Part D's still-
   necessary marker). The backend ABI sites needed zero structural
   changes (confirmed: they route through `unit.Runtime()`, not
   identity checks) — but independent verification found and fixed a
   real bug the implementation's own tests missed: constructing an
   Allocator in a value position (return/argument/nested field, not
   just a local declaration) emitted an invalid cast to a
   nonexistent typedef. Every pre-existing test whose assumptions this
   intentionally changed was updated to assert the new correct
   behavior, not silenced. Full suite passes; the exact original C
   compile failure was causation-checked to confirm the fix is real.
4. ~~**Slice 4 — verification.**~~ **RESOLVED (2026-08-10).** Tracker
   item 1's original reproduction (a constructed `Allocator` crossing a
   function boundary as an argument, a return value, and a struct-field
   assignment, all together) passes end-to-end — confirmed already
   during slice 3's own verification, causation-checked against the
   real pre-cutover failure. `examples/arena_alloc.peb` re-attempted:
   still fails, but for entirely unrelated reasons — pointer-arithmetic
   type-unification errors (`T0505`, `T0507`), a separate, already-
   tracked gap (proposal 16), not anything Allocator/Context-related.
   Zero Allocator/Context errors remain in its output, confirming the
   redesign fully resolved that class of problem in this file; the
   remaining blocker is out of scope for proposal 15.

**Status: all 4 slices complete. The Allocator/Context redesign is
done.**

Given the architecture risk is concentrated entirely in slice 1 (novel,
compiler-wide infrastructure) and slice 3 (many coordinated deletions),
those two get the most scrutiny; slices 2 and 4 are comparatively
mechanical once 1 and 3 land.

## Relationship to other open items

- Tracker item 1 (`13-v1-parity-gap-analysis.md`) should be rewritten to
  point here as the planned direction, superseding its original
  patch-the-special-path slices, once a real investigation (not another
  shallow dispatch) answers the open questions above.
- Tracker item 2 (`arena.peb` rewrite) depends on this being fixed before
  `examples/arena_alloc.peb` can pass end-to-end — `arena::init`/
  `arena::allocator` cross exactly the function boundaries this document
  describes.
