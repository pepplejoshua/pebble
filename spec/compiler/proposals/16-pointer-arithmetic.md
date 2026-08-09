# 16 — pointer arithmetic (`*T + uint`, `*T - uint`)

**Path considered and not taken.** After this investigation, the decision
was made to rewrite `compiler/std/mem/arena.peb` to avoid needing pointer
arithmetic at all (tracker item 3 of `13-v1-parity-gap-analysis.md`) rather
than reverse the reaffirmed language decision this document proposes
amending. Kept as a record of the investigation, not as an active plan — do
not implement anything below without a fresh decision.

Investigation note for tracker item 2 of `13-v1-parity-gap-analysis.md`
("pointer arithmetic (`*T + uint`, `*T - uint`) does not type-check"). This
document decides the semantics and records the exact checker/backend touch
points found. **Planning only — no production code change.**

This partially amends `open-language-decisions.md` §1.5 ("pointer ordering
and arithmetic are forbidden", reaffirmed unchanged by
`11-raw-pointers-and-unsafe-ops.md` §3): pointer `±` an integer is now
intended to be legal on the byte-cursor forms below. Everything else §1.5
covers stays unchanged — pointer `==`/`!=` (including against `nil`) remains
accepted; pointer **ordering** (`<`, `<=`, `>`, `>=`) remains forbidden.

## 1. Decided semantics

**Byte-stride on the byte-cursor forms only.**

- `ptr + n` means `ptr` advanced by `n` **bytes** — the semantics the arena
  already relies on by treating `*u8` as its byte-level cursor type. Not
  element-stride (`n * sizeof T`).
- Legal pointer forms: `*u8` and `*void` only. **Not** other `*T` (see
  §2 for why this is safe for the real consumer, and §6 for the alternative
  that was rejected).
- Offset operand: `uint` only.
- Result type: the same pointer type as the pointer operand
  (`*u8 + uint` → `*u8`, `*void + uint` → `*void`).
- `ptr - n` is the same rule with the same restricted pointer forms and the
  same `uint` offset. `*`, `/`, `%` on a pointer operand stay illegal.
- **Pointer-minus-pointer** (`p - q` producing a `uint`/`int` byte distance)
  is **explicitly deferred**: no site in the real consumer uses it, and the
  result type (`uint`/`int`) plus the pointee-equality rule need their own
  decision.
- Compound forms `ptr += n` / `ptr -= n` fall out of the same rule if cheap;
  no arena site uses them, so they may be deferred with pointer-minus-pointer
  if the `+`/`-` expression form is implemented first.

The integer-to-pointer conversion prohibition
(`open-language-decisions.md`, "Design decisions, not implementation defects")
is untouched: `*T + uint` is pointer arithmetic on an existing pointer, not a
conversion from an integer. Offset `n` is an addition on the pointer's byte
address, not a cast.

## 2. Confirmation against every arithmetic site in `compiler/std/mem/arena.peb`

Every arithmetic site, with the resolved operand types (`sizeof` returns
`uint`; `mem::align_up` returns `uint`; `header.size` / `arena.current.used`
are `uint` fields; `data` is `*void`, everything else is `*u8`):

| Site | Expression | Types | Offset meaning (bytes) |
| --- | --- | --- | --- |
| `arena.peb:104` | `current.ptr = current.ptr + total_aligned;` | `*u8 + uint` | advance past the allocated header+payload |
| `arena.peb:109` | `libc::memset(curr_ptr + sizeof MemHeader, 0, size);` | `*u8 + uint` | skip the header |
| `arena.peb:112` | `return curr_ptr + sizeof MemHeader;` | `*u8 + uint` → `*void` | payload start |
| `arena.peb:123` | `var ptr = arena.current.buffer + aligned_used;` | `*u8 + uint` | next aligned offset in the slab |
| `arena.peb:130` | `return ptr + sizeof MemHeader;` | `*u8 + uint` → `*void` | payload start |
| `arena.peb:159` | `return data + sizeof MemHeader;` | `*void + uint` → `*void` | payload start |
| `arena.peb:167` | `let header *MemHeader = (data - sizeof MemHeader);` | `*void - uint` | back up to the header |
| `arena.peb:168` | `(data + header.size) == (arena.current.buffer + arena.current.used)` | `*void + uint`, `*u8 + uint`, then `==` | end-of-allocation test (equality already allowed on pointers) |
| `arena.peb:177` | `slot.ptr = (data + header.size) as *u8;` | `*void + uint` | start of the reusable gap |

All nine sites are byte offsets. **None needs element-stride** (`n * sizeof
T` would be wrong at every one of them), and **none needs
pointer-minus-pointer**. All pointer operands are `*u8` or `*void`; typed
pointers (`*MemHeader`, `*FreeSlot`, `*Slab`, `*Arena`, `*Point`) are
produced by explicit `as` casts at the edges and are never arithmetic
operands themselves.

A grep of the whole `compiler/std` and `examples` tree found **zero other
uses** of pointer arithmetic anywhere, so the byte-cursor rule above is
sufficient for the entire real consumer base.

## 3. The exact rejection path (why `*T + uint` fails today)

Confirmed by reproduction (`go run ./cmd/pebc -run ../examples/arena_alloc.peb`
from `compiler/`): every `+` site fails with `error[T0505]: cannot unify
semantic type kind 2 with kind 1` (Kind 2 = `types.Pointer`, Kind 1 =
`types.Builtin` — see `internal/types/key.go`); the one `-` site fails with
both `error[T0507]: type does not satisfy the numeric capability` and T0505.
The typed-IR/validation phases are never reached.

The rejection is emitted by the **constraint layer**, in this order:

1. **`internal/check/operator_facts.go` `finishOperator`** (the walker that
   turns a binary expression into solver constraints):
   - `+` (lines 321-328): emits `Equal(left, right)` ("same operands"),
     `Equal(result, left)` ("operator result"), and — only when
     `plusNeedsNumeric` returns true — `Numeric(left)`. The unconditional
     `Equal(left, right)` is what unifies `*u8` against `uint`.
   - `-`, `*`, `/` (lines 314-320, all `operatorNumericSame`): emits
     `Numeric(left)`, `Numeric(right)`, `Equal(left, right)`,
     `Equal(result, left)`. Both the `Numeric` capability on the pointer and
     `Equal(left, right)` reject the pointer operand.
2. **`internal/infer/solve.go`** applies those constraints:
   - `constraintEqual` → `unify` (`solve.go:98`). A pointer and a builtin
     cannot unify; `internal/infer/unify.go:578-582` (`describeTypeConflict`)
     produces the T0505 text.
   - `constraintNumeric` → `applyCapability` → `checkCapabilities`
     (`internal/infer/capability.go:73-94`, the `capNumeric` branch at 87-89)
     produces the T0507 "does not satisfy the numeric capability".
3. After solving, `internal/check/operator_validation.go`
   `validateArithmeticOperators` (lines 79-187) would additionally reject a
   pointer operand with `C0603` ("operator operands or result have invalid
   types") — the `operatorAdd` branch (159-164) and `operatorNumericSame`
   branch (147-158) both only admit integer/float/`str` builtins. This phase
   is currently unreachable for these sites because solving fails first, but
   it is part of the acceptance path and must be updated.

**Result type the operation should produce:** the pointer type of the
pointer operand, unchanged. `*u8 + uint` → `*u8`, `*void + uint` → `*void`.
The checker must not propagate the `uint` operand's type to the result.

**Key architectural finding:** operand pointer-ness is not knowable at walk
time. `current.ptr` is a member access constrained via
`infer.StructuralField` (`internal/check/member_facts.go:129`) and its type
(`*u8`) is only resolved during solving; `var ptr = arena.current.buffer +
aligned_used;` (untyped local, line 123) has no known destination either. A
walker-side branch that checks `knownValues`/known types at
`finishOperator` time **cannot** recognize the arena's core sites — the
pointer-vs-numeric decision must be made where types are known, i.e. in the
solver (a dedicated constraint that dispatches on the resolved operand
types), or the walker must emit a constraint whose meaning is
"unify, unless one side is a pointer and the other is `uint`, in which case
apply the pointer rule". The existing shape-constraint machinery
(`infer.ConstrainShape`/`unifyShapes`) cannot be reused by emitting
`PointerShape` unconditionally for `+`: `matchKnownShape`
(`internal/infer/unify.go:256-273`) reports "expected pointer type" for a
numeric operand, which would break ordinary numeric `+`.

## 4. Checker touch points (what a later slice must change)

1. **`internal/check/operator_facts.go` `finishOperator`** (lines 311-356):
   the `+` and `-` cases must stop emitting `Equal(left, right)` (operands
   genuinely differ in the pointer case) and must not impose `Numeric` on a
   pointer operand. Introduce the pointer-arithmetic rule here, in whatever
   constraint shape §3's finding dictates.
2. **`internal/check/operator_validation.go`** `validateArithmeticOperators`
   (lines 79-187): accept the restricted pointer cases. Because
   `operatorNumericSame` is shared by `-`, `*`, and `/`, the pointer branch
   must be scoped by the retained record's `Token` (`syntax.Minus` only) so
   `*`/`/` keep rejecting pointers. `operatorAdd` is `+` only, so it is safe
   to extend wholesale. The offset operand must validate as `uint`.
3. **`internal/check/assignment_facts.go`** (lines 88-126): compound
   `+=`/`-=` reuse the same `operatorAdd`/`operatorNumericSame` families and
   emit the same `Numeric` + `Equal` constraint set; if `+=`/`-=` on
   pointers are in scope, the same branch is needed here.
4. **`internal/check/ir_builder_operators.go`**: `buildOperatorValue` (30-79)
   and `operatorHasIntegerOperand` (81-95) decide `CheckedArithmetic` vs
   `BinaryValue` by whether operand 0 is an integer builtin. A pointer-typed
   arithmetic result is neither shape today. Decide which `tir` node kind
   carries pointer arithmetic (a new kind is likely cleaner than widening
   `CheckedArithmetic`/`BinaryValue`), and set it here.
5. **`internal/tir/verify.go`**: node-kind verification (630-639 for
   `BinaryValue`, 810-820 for `CheckedArithmetic`/`CheckedShift`) needs a
   case for the pointer-arithmetic node if a new kind is added.
6. **`internal/infer`**: if a new dedicated constraint is chosen (see §3),
   add it to `constraint.go`, `solve.go`'s `apply` switch (96-129), and give
   it a unify/capability application in `unify.go`/`capability.go` that
   enforces "one operand is `*u8`/`*void`, the other is `uint`, result is
   the pointer type" when both sides are resolved.

## 5. Backend touch points

1. **`internal/backend/emit.go` `buildExpr` pointer dispatch** (10226-10323):
   pointer-typed nodes bypass the entry-width gate via a kind switch. Its
   `default` (10321-10322) reports "…which this backend does not lower" — a
   pointer-typed arithmetic result would land here today. Add the
   pointer-arithmetic case.
2. The lowering can reuse existing pointer↔integer machinery instead of new
   runtime primitives: `PointerToInteger` (`emit.go:7775`, `:10471`) already
   lowers `ptr as uint` to a C cast to `uint64_t`; `IntegerToPointer` /
   `PointerCast` (`:10308`, `:9441`) lower the reverse. Byte-stride
   arithmetic is then, e.g.,
   `(<pointer type>)((uint64_t)(<base>) +/- (<offset>))` — which avoids C
   `void*` arithmetic (a GNU extension) entirely.
3. The general-helper `uint` path (`buildUintExpr`, 7705+) is unaffected —
   pointer arithmetic results are pointer-typed, not `uint`-typed.

## 6. Alternatives considered and rejected

- **Element-stride on all `*T`** (C/Rust-style typed-pointer arithmetic):
  wrong for every arena site (they all need byte offsets) and gives `*void`
  no well-defined stride. Rejected.
- **Byte-stride on all `*T`**: uniform, but `*MemHeader + 1` meaning "one
  byte past a header" is a footgun with no consumer; restricting to the two
  byte-cursor forms keeps the language's promise that typed-pointer walking
  stays expressed via casts and indexing. The restricted rule also gives the
  validator a cheap, unambiguous check. Rejected in favor of the `*u8`/`*void`
  restriction.
- **Walker-side pointer detection**: shown impossible for the arena's core
  sites (§3) because operand types resolve only during solving. Rejected.

## 7. Out of scope / deferred

- Pointer-minus-pointer byte distance (`p - q`).
- Pointer ordering (`<` etc.) — stays forbidden (§1.5).
- Pointer arithmetic on non-`*u8`/`*void` types.
- `std/string.peb` or any other `std` module rewrite: not needed for this
  feature; the arena is the first sanctioned consumer and the rule is scoped
  to it.
