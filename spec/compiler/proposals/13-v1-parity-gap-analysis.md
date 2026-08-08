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

### 1. `Allocator.{ ... }` record construction cannot compile

**Area:** backend generator

**Priority:** high; blocks `compiler/std/mem/arena.peb`

**Reproduction:** construct `Allocator.{ ptr = nil, alloc = my_alloc,
realloc = my_realloc, free = my_free }` with the runtime signatures:

```pebble
fn my_alloc(context *void, size uint) *void { return nil; }
fn my_realloc(context *void, ptr *void, size uint) *void { return nil; }
fn my_free(context *void, ptr *void) void {}
```

**Current status:** checker, typed IR, and backend Allocator construction now
pass. A real `std:mem/arena` consumer is still required for closure.

The checker and typed IR now succeed (`da4559f`). Aggregate collection skips
the runtime type (`f5403b5`), general pointer-bearing function types emit
correctly (`ac7aa32`), and Allocator `RecordConstruct` values now lower to
`PebbleAllocator` with ABI-safe callback bridges (`78e70de`). The remaining
open work is a real `std:mem/arena` consumer check.

Slices:

1. Change only aggregate collection so a runtime `RecordConstruct` does not
   enter parsed-struct typedef resolution. Keep recursion through its field
   values. Add a focused test that pins the next runtime-record rejection.
2. Add only runtime `RecordConstruct` C emission for `PebbleAllocator`.
   Preserve the existing function-pointer calling convention. Add focused
   emitted-C and compile-run tests. Do not widen general record construction.
   Completed in `78e70de`.
3. Compile and run a real `std:mem/arena` consumer, then run full checks and
   the causation checks.

### 2. Generic `Result[T, E]` methods do not narrow `self`

**Area:** checker, then backend generator

**Priority:** medium; blocks `std/result.peb` and `examples/count_lines.peb`

Ordinary tagged-union narrowing works. It does not cover a method receiver
that refers to its own generic union, such as `self Result[T, E]`.
`is_ok`, `unwrap_or`, `map`, and `set_error` cannot read `self.Ok` or
`self.Err` inside the matching switch arm. Prior investigation saw
`aggregateTaggedVariant` lose the declaration identity and reach
`Declaration=0`; this needs current confirmation before implementation.

Narrowed writes are a separate checker gap. `set_error` needs to write
`self.Err`, but the existing widening applies only to read-side member
validation. After checker support, a later backend defect is expected:
`buildStrOperand` has no `Load(FieldPlace)` path for a `str` variant payload.

Slices:

1. Investigation only. Produce a minimal generic-self read reproduction and
   trace the first lost declaration or specialization identity. Do not edit
   production code.
2. Fix only read-side generic-self narrowing. Add positive matching-arm and
   negative wrong-arm/outside-arm checker tests.
3. Fix only narrowed writes. Add focused assignment/place tests. Do not touch
   backend emission.
4. If now reachable, add only `str` `FieldPlace` load emission and its focused
   compile-run test.
5. Compile and run real consumers of `std:result`; then run full checks and
   causation checks.

### 3. Qualified static methods do not exist

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

### 4. `main(argv []str)` cannot read process arguments

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

### 5. Inline slice construction fails in pure expression positions

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

### 6. `std/hmap.peb` and `std/set.peb` stop the CLI on `C0618`

**Area:** Pebble standard library or CLI diagnostic policy

**Priority:** low

Each file has a trailing return after a `while true` loop. The checker reports
an unreachable-statement warning. `pebc` exits with failure for every
diagnostic, including warnings. A similar dead return was safely removed from
`std/io.peb`, but each remaining function must be checked on its own.

Removing the two `std/hmap.peb` returns exposes a separate backend gap:
non-void helper bodies whose final statement is an exhaustive `while true`
are rejected because the backend expects a trailing return node. The exact
error is `entry function body ... ends in a While statement`; the same shape
may exist in `std/io.peb`. The source cleanup is therefore not committed until
the backend can emit this valid control-flow shape.

Slices:

1. Verify and remove only the dead return in `std/hmap.peb` if its loop paths
   are exhaustive. Compile and run a real hmap consumer.
2. Verify and remove only the dead return in `std/set.peb` if its loop paths
   are exhaustive. Compile and run a real set consumer.
3. Separately decide whether CLI warnings should cause a nonzero exit. Do not
   change CLI policy as part of either standard-library slice.

### 7. Non-void helper bodies cannot end in an exhaustive `while true`

**Area:** backend generator

**Priority:** medium; blocks the hmap and set library consumer sweep

**Reproduction:** after removing the dead return from `HashMap.get_by_ref` or
`HashMap.remove`, the helper's typed-IR body ends directly in a `While` node.
Emission rejects it with `entry function body ... ends in a While statement`.
The control flow is exhaustive because every loop exit returns; the trailing
source return only existed to satisfy the current backend shape.

Slices:

1. Investigation only. Trace helper tail validation and identify the smallest
   backend representation for a terminal exhaustive loop. Do not edit code.
2. Add backend lowering for one terminal `while true` shape with a focused
   compile-run test. Preserve rejection of genuinely fall-through non-void
   bodies.
3. Remove the dead return from `std/hmap.peb`, run `std_hash.peb`, then repeat
   the source cleanup and consumer check for `std/set.peb`.

### 8. A generic struct method cannot inherit the owner type parameter

**Area:** checker or backend; exact layer needs confirmation

**Priority:** low

`fn describe(self Box[K]) int => 42;` fails when the method does not declare
its own `[K]`. Methods that redeclare `[K]` work and cover current stdlib use.

First slice: reproduce against current HEAD and identify the first failing
phase. Investigation only. Do not combine this with static methods.

### 9. Some checked numeric operations have no `u64` runtime helper

**Area:** runtime and backend generator

**Priority:** low

`u64` division, modulo, shifts, and float-to-integer conversion are explicit
clean rejections because their checked runtime operations do not exist.
Confirm each operation is intended by the language contract before work.

Slices: one operation family per dispatch. Add its runtime helper and smoke
tests first, then its backend selection and compile-run test.

### 10. Enum-to-integer conversion lacks backend lowering

**Area:** backend generator

**Priority:** low

The checker accepts the conversion, but the backend does not lower it. First
slice: reproduce against current HEAD and identify the exact TIR node and
missing builder case. Do not implement until the reproduction is recorded.

### 11. Whole dereferenced structs cannot become values

**Area:** checker place tracking and backend struct rvalues

**Priority:** deferred

`(*p).x` and `let v Point = *p;` fail because place tracking does not extend
a `DereferencePlace` through this struct-value position. A skipped backend
test records the known reproduction. Investigate the checker and backend
boundaries before making an implementation plan.

## Verification queue

These items are not confirmed defects. Test them with small source files.
If one fails, move it to Active defects with its exact error and cause before
dispatching implementation work.

1. `extern "libm.so" { ... }` library-named extern blocks.
2. `extern { type FILE; }` opaque extern types.
3. Three-level nested generic types such as
   `Vec[HashMap[str, Result[T, E]]]`.
4. `[]str` element support. Old tracker text may be stale after later slice
   and aggregate element work.
5. A slice whose element is a struct, tuple, optional, or generic struct.
   Old tracker text conflicts with later completed work and must be tested.
6. `TupleCoerce` backend reachability and emission.
7. Confirm that `TypeUse` is compile-time-only and needs no backend case.
8. Re-audit `open-language-decisions.md` against the current compiler. Its
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

## Deferred language and runtime work

- Generational-pointer use-after-free and double-free tracking.
- `any` with real type erasure.
- Ownership and borrow checking.
