# Task: allow record construction for a runtime/builtin nominal type (Allocator)

## Location

`compiler/internal/symbol/visit.go`, function `resolveRecord` (search
for `func (r *resolver) resolveRecord`).

## The bug

A record literal (`Type.{ field = value, ... }`) cannot be constructed
for the built-in `Allocator` type — every field is rejected:

```
extern... (no extern needed for repro)
fn my_alloc(ptr *void, size uint) *void { return nil; }
fn my_realloc(ptr *void, data *void, new_size uint) *void { return nil; }
fn my_free(ptr *void, data *void) void {}
fn main() int {
    var a = Allocator.{ ptr = nil, alloc = my_alloc, realloc = my_realloc, free = my_free };
    return 0;
}
```

```
error[N0001]: type has no member "ptr"
error[N0001]: type has no member "alloc"
error[N0001]: type has no member "realloc"
error[N0001]: type has no member "free"
```

This is wrong — `Allocator`'s real member names (`ptr`, `alloc`,
`realloc`, `free`) and their exact function-type signatures are
correctly defined in `internal/infer/runtime_prelude.go` (confirm by
reading it: `p.runtimeMembers(allocatorSymbol, []string{"ptr", "alloc",
"realloc", "free"})` and the `types.FunctionKey(...)` calls building
each member's expected function type). The repro's argument types
match exactly. This fully blocks `compiler/std/mem/arena.peb`'s
`allocator()` function (and any other user code constructing a custom
`Allocator`) — building an `Allocator` value is its entire purpose. Do
not modify `arena.peb` — it's correct; this is a checker/symbol
-resolution gap.

## Root cause (confirmed, read the code to verify before fixing)

Same file, function `resolveMember` (used for an ordinary member READ,
`a.ptr`) has a guard:

```go
if base.State == ResolutionResolved {
    symbol, _ := r.result.Symbols.Symbol(base.Symbol)
    if symbol.Kind == SymbolType || symbol.Kind == SymbolExternType {
        return r.resolveNamedMember(ctx, children[1], member, base.Symbol)
    }
}
```

This SKIPS early, syntax-level member validation for any symbol kind
OTHER than `SymbolType`/`SymbolExternType` — including `Allocator`'s
real kind, `SymbolRuntimeType` — deferring resolution to a LATER,
type-driven phase that already correctly understands `Allocator`'s
synthetic runtime-injected members (this is why `a.ptr` reads already
work correctly, confirmed by earlier session work fixing
`Allocator`-typed struct fields).

`resolveRecord` (the function handling record CONSTRUCTION) has NO
such guard — it calls `r.resolveNamedMember(ctx, parts[0], name,
owner)` UNCONDITIONALLY whenever `owner != 0`, regardless of the
owner's symbol kind. `resolveNamedMember` looks up
`r.memberBindings[owner][name]` — a map populated purely from PARSED
`.peb` struct declarations during symbol resolution. `Allocator` has
no such declaration (its members are synthesized entirely in Go,
injected at a LATER phase), so the lookup always misses, producing the
misleading "type has no member" error regardless of what's actually
being constructed.

## The fix

Add the same kind-based guard `resolveMember` already has to
`resolveRecord`: when `owner != 0`, look up the owner's own symbol
kind, and skip the early `resolveNamedMember` validation (deferring to
the later type-driven phase instead) for any kind that isn't
`SymbolType`/`SymbolExternType` — mirroring `resolveMember`'s existing
condition exactly, so the two functions treat member validation
consistently for the same set of symbol kinds.

Read `resolveRecord`'s FULL function body (not just the fragment
above) before editing — understand exactly what `resolveNamedMember`'s
call accomplishes for the NORMAL (ordinary struct) case (does it just
validate, or does it also produce some resolution/binding that later
compilation stages depend on?) so that skipping it for a runtime type
doesn't silently break something else. If skipping it entirely isn't
safe, investigate what the DEFERRED path needs instead — check how
`resolveMember`'s own deferred branch handles this (search for
`ResolutionDeferred` in this file) and mirror that shape for
`resolveRecord` too, rather than just deleting the validation call
outright.

## Do NOT

- Do not touch `resolveMember` itself — it already works correctly;
  reference it as the model, don't modify it.
- Do not modify `internal/infer/runtime_prelude.go` — `Allocator`'s
  member definitions there are already correct.
- Do not modify `compiler/std/mem/arena.peb` or any other `.peb` file.
- Do not attempt to make `Context` constructible via `Context.{ ... }`
  — confirmed separately that `Context` isn't even a nameable type in
  user scope at all (a different situation, likely intentional design,
  not investigated — out of scope for this task).
- Do not attempt any other tracked checker/backend item — unrelated,
  out of scope.

## Tests

Add test(s) to `compiler/internal/symbol/` (find the existing test
file testing record/member resolution — search for `resolveRecord` or
`resolveMember` in `*_test.go` files in that package, or a broader
integration test elsewhere in `internal/check/` if that's where
record-construction tests for this kind of shape actually live —
follow whichever pattern already covers a similar case). At minimum, a
test confirming `Allocator.{ ptr = ..., alloc = ..., realloc = ...,
free = ... }` now resolves without a `N0001` diagnostic.

Also verify end-to-end: build `pebc` (`cd compiler && go build -o
/tmp/pebc_verify ./cmd/pebc`, run from the `compiler/` directory) and
run it against the repro fixture above (as a scratch file inside the
repo tree, deleted before finishing) — confirm it checks AND emits AND
runs cleanly (a full round trip: construct an `Allocator`, don't need
to actually use it for a real allocation, just confirm construction
compiles and the program exits 0). Also confirm `compiler/std/mem/
arena.peb` itself now compiles further than before (it may still hit
other, separately-tracked issues past this fix — that's fine and
expected; the goal is confirming THIS specific error is gone).

## Acceptance criteria

- `go build ./...` and `go vet ./...` clean.
- `gofmt -l .` empty.
- New test(s) pass.
- Full `go test ./... -count=1` (from the `compiler/` dir) stays green.
- Do NOT commit. Leave changes in the working tree for review.

## Scratch files

If you need scratch/fixture `.peb` files while investigating, write
them inside the repo's own working tree (not `/tmp`), and delete them
before finishing.
