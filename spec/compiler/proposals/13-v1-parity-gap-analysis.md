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

### 1. `print` cannot render a multi-byte `char`

**Area:** runtime, then backend generator

**Priority:** high

**Reproduction:** `fn main() int { print 'é'; return 0; }`

**Failure:** the generated C uses `%c`, which writes only one byte.

`compiler/internal/backend/emit.go` builds one combined `printf` call.
The char branch passes the Unicode scalar to C `%c`. It must instead encode
the scalar as UTF-8 and pass a short, terminated byte buffer to `%s`.
`std/string.peb` already contains the required encoding algorithm in
`push_char`, but the print path needs a C runtime helper.

Slices:

1. Add and directly test one C runtime UTF-8 encode helper. Touch only its
   header, implementation, and runtime smoke tests. Test ASCII and 2-, 3-,
   and 4-byte scalars in safe and release modes.
2. Teach only `buildPrint` to create the small buffer through its existing
   leading-statement mechanism. Add focused generated-C and compile-run tests
   for mixed ASCII and multi-byte output.
3. Run the full Go checks, both C runtime smoke modes, and the causation check.

### 2. `Allocator.{ ... }` record construction cannot compile

**Area:** symbol/checker, then backend generator

**Priority:** high; blocks `compiler/std/mem/arena.peb`

**Reproduction:** construct `Allocator.{ ptr = nil, alloc = my_alloc,
realloc = my_realloc, free = my_free }` with correctly typed functions.

**Current failure:** `N0001: type has no member` for all four fields.

`Allocator` is a `SymbolRuntimeType`. Its members come from the compiler
runtime prelude, not a parsed Pebble declaration. `resolveRecord` in
`compiler/internal/symbol/visit.go` uses the parsed-declaration member path.
The earlier attempted fix, commit `c4117b5`, proved that simple deferral is
not enough: typed IR needs stable field symbols. That fix resolved known
runtime members from the compiler-owned member table and passed symbol and
typed-IR tests. It was reverted because it exposed the separate backend gap
below, not because its checker result was disproved.

After typed IR succeeds, the backend rejects the runtime record with
`struct type has no TypeDeclaration`. `tir.RecordConstruct.Symbol` names a
runtime type, while aggregate collection assumes a parsed
`TypeDeclaration`. The C target already exists as `PebbleAllocator`, and
runtime field reads already map `ptr`, `alloc`, `realloc`, and `free`.

Slices:

1. Re-investigate `c4117b5` against current HEAD. Restore only the narrow
   runtime-member symbol resolution if it is still correct. Keep parsed type
   misspelling diagnostics unchanged. Add focused symbol and typed-IR tests.
   Stop when typed IR is valid; do not touch the backend.
2. Add only runtime `RecordConstruct` C emission for `PebbleAllocator`.
   Preserve the existing function-pointer calling convention. Add a focused
   backend compile-run test. Do not widen general record construction.
3. Compile and run a real `std:mem/arena` consumer, then run full checks and
   the causation checks for both slices.

### 3. Generic `Result[T, E]` methods do not narrow `self`

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

### 4. Qualified static methods do not exist

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

### 5. `main(argv []str)` cannot read process arguments

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

### 6. Inline slice construction fails in pure expression positions

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

### 7. `std/hmap.peb` and `std/set.peb` stop the CLI on `C0618`

**Area:** Pebble standard library or CLI diagnostic policy

**Priority:** low

Each file has a trailing return after a `while true` loop. The checker reports
an unreachable-statement warning. `pebc` exits with failure for every
diagnostic, including warnings. A similar dead return was safely removed from
`std/io.peb`, but each remaining function must be checked on its own.

Slices:

1. Verify and remove only the dead return in `std/hmap.peb` if its loop paths
   are exhaustive. Compile and run a real hmap consumer.
2. Verify and remove only the dead return in `std/set.peb` if its loop paths
   are exhaustive. Compile and run a real set consumer.
3. Separately decide whether CLI warnings should cause a nonzero exit. Do not
   change CLI policy as part of either standard-library slice.

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
