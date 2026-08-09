# 13 — v1 parity gaps

**Purpose.** This file is the sole issue tracker for Pebble compiler v2
parity work. It contains only open work. Git history and commit messages are
the permanent record for completed work.

## Tracker rules

- Delete an item after its fix and tracker update are committed.
- Add each new finding before work starts.
- Record a real reproduction, the current failure, and the known cause.
- Separate confirmed defects from items that still need verification.
- Keep design decisions separate from implementation defects.
- Dispatch compiler and runtime logic through Orc.
- Use one small, decisive Orc slice at a time. Review and verify each slice
  before the next dispatch.
- Use `opencode-go/deepseek-v4-flash` by default. Do not give it a long,
  multi-layer task. Escalate only after a real capability failure.
- Before each dispatch, require a clean worktree and no active Orc or
  OpenCode worker for this repository.
- After each dispatch, inspect the diff and check for scratch files, debug
  output, scope growth, and stale tests. Run the full required verification
  and a causation check before commit and push.

## Active defects

### 1. `Allocator` values cannot cross a function boundary

**Area:** backend generator

**Priority:** high; blocks all real Allocator-backed library code, not just
`arena.peb`

**Discovered while attempting the arena.peb rewrite below (uncommitted
session, not yet landed).** An `Allocator` value can only exist as a
`RecordConstruct` declaration initializer, a field read, or the special
allocator-call `ctx` position. It cannot be:

- passed as a function argument (`init(&a, 256, alloc)` fails: "argument ...
  passes ... not a struct-typed local"),
- returned from a function (`arena::allocator()` returning `Allocator.{...}`
  fails: "struct value ... no TypeDeclaration"), or
- assigned into a struct field (`arena.backing = backing` fails: "reassigns
  an element of type nominal").

This directly threatens the "Allocator record construction now compiles"
close recorded on this item historically (`da4559f`, `f5403b5`, `ac7aa32`,
`78e70de`) — those commits made the *literal construction* work, but never
verified a value actually surviving a function call, a return, or a field
write, because no real multi-function Allocator consumer had been compiled
end-to-end until tonight. `compiler/std/vec.peb`'s `self.backing = allocator;`
(in `reserve`) has the same field-assignment shape and needs to be checked
against this — it may be similarly broken and simply never exercised by an
end-to-end compile-and-run before.

Slices:

1. Investigation only. Reproduce each of the three failures above with a
   minimal standalone `.peb` file (not `arena.peb` — isolate one at a time).
   Confirm whether `vec.peb`'s `self.backing = allocator;` is actually
   reachable/compiled by any existing test, or whether it's dead code that
   was never emitted. Identify the exact backend code path(s) responsible for
   each of the three rejections (they may be one shared cause — "runtime
   nominal types only handled in declaration position" — or three separate
   gaps).
2. Fix argument-passing first (smallest surface), with a focused compile-run
   test.
3. Fix struct-field assignment, with a focused compile-run test. Re-check
   `vec.peb`'s `reserve` against it.
4. Fix return values, with a focused compile-run test.

### 2. Pointer arithmetic (`*T + uint`, `*T - uint`) does not type-check

**Area:** backend generator

**Priority:** high; blocks `compiler/std/mem/arena.peb`

**Decision made:** do not reverse the reaffirmed pointer-arithmetic ban
(`open-language-decisions.md` §1.5). Instead `compiler/std/mem/arena.peb` is
being rewritten to avoid needing pointer arithmetic at all (see item 3
below), which is also more consistent with the rest of `std` (`Vec`/
`HashMap` use safe slice indexing throughout; nothing else needs raw pointer
walking). This item's investigation
(`spec/compiler/proposals/14-pointer-arithmetic.md`, uncommitted) is kept as
a record of the path considered and not taken — do not act on it without a
fresh decision.

Original reproduction, for the record: `current.ptr + total_aligned`,
`curr_ptr + sizeof MemHeader`, `data - sizeof MemHeader`, `arena.current
.buffer + arena.current.used` in the pre-rewrite `arena.peb` all failed
`error[T0505]: cannot unify semantic type kind 2 with kind 1`.

Not currently being pursued — superseded by item 3's rewrite. Revisit only if
the rewrite proves impractical.

### 3. `arena.peb` rewrite (avoid pointer arithmetic) is checker-clean but two backend bugs block real compilation

**Area:** Pebble standard library (`compiler/std/mem/arena.peb`) and backend
generator

**Priority:** high; blocks `examples/arena_alloc.peb` and item 1 above

**Status:** a full rewrite of `arena.peb`'s `init`+`alloc` (fresh-allocation
path only; `relink_slot`/`add_free_slot`/`realloc`/`free`/`destroy` not yet
touched) is sitting uncommitted in the working tree. It replaces raw pointer
walking with a `[]u8` slice plus `uint` byte offsets, converting the offset
to an address only via safe indexing + address-of (`&buf[offset]`) at actual
read/write sites — no pointer arithmetic anywhere. It passes the Go-level
checker. **It does not actually compile to C** — verified directly, not just
by the dispatch's own self-report, which claimed success prematurely based
on the checker pass alone.

Real `cc` compilation of a standalone consumer test fails with two distinct
backend bugs, neither related to pointer arithmetic:

1. **Struct/slice C-typedef ID confusion.** The emitted C confuses
   `pebble_struct_29_t` with `pebble_slice_33_t` (and similar) — "incompatible
   pointer types", "no member named ... in ...", "unknown type name". Looks
   like the backend's struct-ordering/typedef-ID assignment gets confused by
   `Slab` being self-referential (`next *Slab`) combined with its new `[]u8`
   slice field; needs its own investigation, independent of the arena
   rewrite itself.
2. **Checked-arithmetic helper names emitted with a missing type suffix.**
   `pebble_rt_checked_mod_`, `pebble_rt_checked_div_`, `pebble_rt_checked_add_`
   — calls to C functions that don't exist (should presumably end in `_u32`/
   `_uint`/etc.). Also independent of pointer arithmetic.

Also surfaced along the way (already folded into the rewrite, not blocking):
`slice ptr, count` is std-only (`C0619` outside `std`); the backend has no
`u64`/`uint` shift helpers (`uint` bitwise `&`/`~`/`<<`/`>>` all rejected —
`mem::align_up` needed its own `+`/`/`/`*`-only reimplementation inside
arena.peb); passing a slice-typed *struct field* as a function argument is
rejected (only slice-typed locals work) — the rewrite works around this by
having helpers take `*Slab` and index `.buffer` directly, matching
`vec.peb`'s own style.

Slices:

1. Investigate and fix the struct/slice typedef-ID confusion (bug 1 above)
   with a minimal standalone repro (a self-referential struct with a slice
   field, no Allocator involved) — independent of arena.peb.
2. Investigate and fix the checked-arithmetic helper name generation gap
   (bug 2 above) with a minimal repro.
3. Once both backend bugs are fixed, re-verify the existing `init`+`alloc`
   rewrite compiles and runs (a standalone test exists in the session
   worklog for `ses_6a77ef1a9f5e09c47d9ff2dc` if needed as a starting point,
   though it was deleted as scratch — rewrite a small one).
4. Continue the rewrite: `relink_slot`, `add_free_slot`, `realloc`, `free`,
   `destroy` still use the old `*u8` pointer-arithmetic style and need the
   same slice+offset treatment. `realloc`/`free` additionally need to recover
   "which slab, what offset" from a bare incoming `*void` — do this via the
   already-implemented pointer-to-uint cast plus plain `uint` range
   comparison against each slab's buffer-start address, not new pointer
   arithmetic.
5. Compile and run `examples/arena_alloc.peb` end to end, then run full
   checks and causation checks. This slice also depends on item 1
   (`Allocator` cannot cross a function boundary) being fixed first, or
   `arena.peb`'s public API (`init`, `allocator`) cannot actually be called
   the way `examples/arena_alloc.peb` calls it.

### 4. Generic `Result[T, E]` methods do not narrow `self`

**Area:** checker, then backend generator

**Priority:** medium; blocks `std/result.peb` and `examples/count_lines.peb`

Ordinary tagged-union narrowing works. It does not cover a method receiver
that refers to its own generic union, such as `self Result[T, E]`.
`is_ok`, `unwrap_or`, `map`, and `set_error` cannot read `self.Ok` or
`self.Err` inside the matching switch arm. The case-label
`aggregateEnumVariant` loses its declaration identity and reaches
`Declaration=0` because the generic receiver template is not materialized
into `knownValues`, so `knownDestination` cannot recover the nominal
declaration. Read-side narrowing, narrowed writes, and later `str` payload
emission are separate gaps.

Narrowed writes are a separate checker gap. `set_error` needs to write
`self.Err`, but the existing widening applies only to read-side member
validation. After checker support, a later backend defect is expected:
`buildStrOperand` has no `Load(FieldPlace)` path for a `str` variant payload.

Slices:

1. Fix only read-side generic-self narrowing. Recover the declaration from the
   solved receiver type when the case-label aggregate has `Declaration=0`.
   Add positive matching-arm and
   negative wrong-arm/outside-arm checker tests.
2. Fix only narrowed writes. Add focused assignment/place tests. Do not touch
   backend emission.
3. If now reachable, add only `str` `FieldPlace` load emission and its focused
   compile-run test.
4. Compile and run real consumers of `std:result`; then run full checks and
   causation checks.

### 5. Qualified static methods do not exist

**Area:** checker facts, validation, inference, and typed IR

**Priority:** low; no current repository consumer requires the feature

**Reproduction:** `Box.mk(5)` fails for a non-generic type and
`Box[int].mk(5)` fails for a generic type.

A method without `self` is rejected with `C0604`. A qualified type path
does not create a receiver value. `finishCall` then fails its receiver lookup,
`validateCallRecords` rejects `Receiver == 0`, and
`ir_builder_calls.go` tries to build value zero. The feature has no complete
representation in the current method model. Top-level generic helper
functions are the working repository convention.

Slices, only if this low-priority feature is approved later:

1. Write a specification note for static-method identity, lookup, overload
   rules, and the typed-IR representation. No production code.
2. Implement only fact collection and inference. Add facts tests; retain the
   later validation rejection.
3. Implement only call-record validation for the accepted static form.
4. Implement only typed-IR construction.
5. Add backend and end-to-end tests for plain and generic owners.

### 6. `main(argv []str)` cannot read process arguments

**Area:** typed IR and backend generator

**Priority:** low; no current example depends on it

The checker accepts `fn main(argv []str) int`, but `pebble_user_main` and the
C entry templates still have zero parameters. C `argc` and `argv` are
discarded. A Pebble program that reads the parameter cannot emit correctly.
The two-parameter v1 form remains intentionally unsupported.

Slices:

1. Investigate and specify the smallest typed-IR representation for the one
   accepted `[]str` entry parameter. No production edit.
2. Carry only that parameter through IR construction.
3. Add the C `argc`/`argv` to `[]str` adapter and compile-run tests.

### 7. Inline slice construction fails in pure expression positions

**Area:** backend generator

**Priority:** low; named-local workaround works

Calls such as `print f(a[1:3])`, `return f(a[1:3])`, and
`f(g(a[1:3]))` need a leading statement for slice construction, but their
pure expression positions cannot carry one. Bare calls and local declaration
initializers already work.

Slices:

1. Investigation only. Inventory the exact call builders that discard or
   cannot return a leading statement. Select one smallest expression form.
2. Implement one form only, with one compile-run test.
3. Repeat for each remaining form. Do not combine all call sites in one task.

### 8. An array literal of non-primitive elements cannot initialize a slice-typed local

**Area:** checker

**Priority:** low; no current repository consumer requires the pattern

**Reproduction:**

```pebble
let items []str = ["a", "b", "c"];
```

and

```pebble
type Point = struct { x int; y int; };
let pts []Point = [Point.{ x = 1, y = 2 }, Point.{ x = 3, y = 4 }];
```

Both fail with `error[C0601]: cannot convert value for assignment` on the
`let` line. This is narrower than it looks: struct-field initialization from
an inline array literal already works (closed, see git history), and named
top-level `let`/`var` locals with array element types already work. The gap
is specifically an array literal directly initializing a slice-typed
(`[]T`) top-level local when `T` is not a primitive. Confirmed by direct
reproduction; not yet root-caused. Investigation only until the exact
conversion path that rejects it is identified.

This replaces the two now-confirmed verification-queue entries for `[]str`
element support and struct/tuple/optional/generic-struct slice elements —
both reproduce the same `C0601` failure and are almost certainly one root
cause, not two.

### 9. A generic struct method cannot inherit the owner type parameter

**Area:** checker or backend; exact layer needs confirmation

**Priority:** low

`fn describe(self Box[K]) int => 42;` fails when the method does not declare
its own `[K]`. Methods that redeclare `[K]` work and cover current stdlib use.

First slice: reproduce against current HEAD and identify the first failing
phase. Investigation only. Do not combine this with static methods.

### 10. Whole dereferenced structs cannot become values

**Area:** checker place tracking and backend struct rvalues

**Priority:** deferred

`(*p).x` and `let v Point = *p;` fail because place tracking does not extend
a `DereferencePlace` through this struct-value position. A skipped backend
test records the known reproduction. Investigate the checker and backend
boundaries before making an implementation plan.

### 11. `emit.go` and `emit_test.go` have grown too large for one file

**Area:** backend generator, codebase maintainability

**Priority:** low; does not block parity work, but is actively hurting the
ability to navigate and review backend changes

`compiler/internal/backend/emit.go` is 15,652 lines and `emit_test.go` is
13,386 lines — both roughly an order of magnitude larger than every other
file in the compiler (the next largest is 4,060 lines). Nearly every backend
feature this project has landed has added another case to one of a handful
of giant type-switches in this one file, and the file has never been split
along those seams.

Slices:

1. Investigation only: inventory `emit.go`'s top-level declarations and
   propose a split along existing natural seams (e.g. expression-position
   builders, place/lvalue builders, struct/enum/union record construction,
   slice/array construction, call-site builders, reachability/collection
   passes). No production edit. Confirm the split preserves all unexported
   symbol visibility within `package backend` (same package, multiple files
   — no API change).
2. Execute the split as one mechanical, behavior-preserving move per
   proposed seam, each as its own slice with a full build+test+diff review
   before the next. Do not change any logic while moving it.
3. Split `emit_test.go` to mirror the same seams once `emit.go` itself is
   split.

## Verification queue

These items are not confirmed defects. Test them with small source files.
If one fails, move it to Active defects with its exact error and cause before
dispatching implementation work.

1. `extern "libm.so" { ... }` library-named extern blocks.
2. `extern { type FILE; }` opaque extern types.
3. Three-level nested generic types such as
   `Vec[HashMap[str, Result[T, E]]]`.
4. `TupleCoerce` backend reachability and emission.
5. Confirm that `TypeUse` is compile-time-only and needs no backend case.
6. Re-audit `open-language-decisions.md` against the current compiler. Its
   old status and some individual claims are known to be stale.

## Design decisions, not implementation defects

- Implicit loop variable `iter` stays unsupported. Loop variables must be
  named explicitly.
- Integer-to-pointer conversion stays forbidden.
- Integer-to-`char` conversion stays forbidden until scalar validity has a
  defined check.
- Enum printing stays out of the plain `print` task. Rich debug printing is
  a separate future feature.
- Untagged-union construction, read, and write have no accepted safety
  design. Do not implement them until that design is decided.
- The v2 CLI stays smaller than the v1 CLI. Do not recreate old flags without
  a separate decision.
- The two-parameter v1 `main(argc, argv)` form stays unsupported.
- **Open:** should a CLI diagnostic that is only a warning (not an error)
  cause `pebc` to exit nonzero? Both `std/hmap.peb` and `std/set.peb` used to
  carry dead `return` statements after an exhaustive `while true` purely to
  avoid an unreachable-statement warning tripping the CLI's exit code; both
  dead returns are now removed (the backend gap that motivated keeping them
  is fixed — non-void bodies ending in an exhaustive `while true` now emit
  correctly). No consumer currently depends on warnings being fatal. Needs a
  real decision, not an implementation task.

## Deferred language and runtime work

- Generational-pointer use-after-free and double-free tracking.
- `any` with real type erasure.
- Ownership and borrow checking.
