# Amendment: enum and integer conversion

**Status:** Accepted in principle by the language author; spec edits below are
Proposed. Owner: `06b` conversion matrix, with a one-line correction in `06a`
and an `OPEN-DECISIONS.md` update.

**Decision:** Pebble supports conversion between an enum and an integer, in
both directions, with deliberately asymmetric safety:

- `enum -> integer` is a total, explicit conversion.
- `integer -> enum` is a checked, explicit conversion that yields an optional.

## Why asymmetric

Every enum value has a valid integer, so `enum -> integer` cannot fail.

The reverse can. `06b` §"Structural control flow, targets, and defers"
requires enum switches to cover every variant and treats an exhaustive switch
as having no fallthrough. An unchecked `integer -> enum` could produce a value
matching no variant, so a switch the compiler proved exhaustive would fall
through a path it believes cannot exist. That is silent unsoundness, not a
crash, and it would quietly weaken a guarantee `06b` already depends on.

Go tolerates the unchecked form because its enums are named integers with no
exhaustiveness guarantee. Pebble's carry one, so it must check.

## Current state

The C prototype already implements the safe half and omits the unsafe half:
`src/checker.c:2479` permits `enum -> integral` in a cast, and no
`integral -> enum` rule exists. This amendment makes that asymmetry
deliberate and adds the checked reverse direction.

`06b-validation-and-typed-ir.md`'s composite matrix currently states
`enum | integer or reverse | forbidden`, which this amendment replaces.

## The integer value of a variant

A variant's integer is its **zero-based declaration-order ordinal**. Pebble
enums have no explicit variant values: `03a-grammar.md` §Types defines
`enum_type = "enum", "{", [ identifier, { ",", identifier }, [ "," ] ], "}"`,
identifiers only. The phase-6 constant evaluator already computes exactly this
ordinal for enum constants.

`integer -> enum` succeeds when the value is in `0 ..< variantCount` and
yields `none` otherwise. The operand is normally a runtime value, so this is
a runtime check emitted into the generated code.

When the operand is a compile-time constant accepted by the phase-6 constant
language, `06b` evaluates it during checking instead. An in-range constant
lowers to the variant with no runtime test; an out-of-range constant is a
compile-time diagnostic rather than a value that is always `none`. The
constant evaluator already computes enum ordinals, so this needs no new
machinery.

```pebble
let a ?Color = 1 as ?Color;      // constant, in range: folds to some green
let b ?Color = 99 as ?Color;     // constant, out of range: rejected at compile time
let c ?Color = fromC() as ?Color; // runtime value: runtime check, may be none
```

Both rules are written in terms of "the variant's value" so that they remain
correct unchanged if explicit variant values are ever added. See the open
follow-on question below, which materially affects C interop.

## Amendment to `06b-validation-and-typed-ir.md`

### 1. Composite conversion matrix

Replace the row

| enum | integer or reverse | forbidden |

with two rows:

| enum | any concrete integer | explicit; total; yields the variant's zero-based declaration ordinal |
| concrete integer | `?enum` | explicit; checked; yields `some` variant when the value names one and `none` otherwise |

`integer -> enum` is spelled as a cast to the **optional** enum type, so the
cast expression's type is still exactly its authored destination and `06a`'s
rule that a cast has the exact destination type is preserved. A cast directly
to the bare enum type stays forbidden, because it has no meaning for an
out-of-range value.

```pebble
type Color = enum { red, green, blue };

let n int = Color.green as int;      // 1, total
let c ?Color = 1 as ?Color;          // some green
let bad ?Color = 99 as ?Color;       // none, handled by the caller
```

### 2. Scope: payloadless enums only

These rules apply to `enum` declarations. They do **not** apply to
`union enum` tagged unions: a tagged variant carries a payload, so an integer
does not determine a value. Tagged-union to integer, and the reverse, remain
forbidden, as does reading a tag as an integer.

### 3. Typed IR consequence

`enum -> integer` lowers to a representation-preserving conversion with no
runtime work.

`integer -> ?enum` requires a checked conversion node in the closed IR
conversion set, in the same family as `CheckedIndex` and `CheckedSlice` but
producing an optional rather than faulting. It does not participate in
release-mode fault behavior, because it has no failure path — an out-of-range
input is an ordinary `none`.

## Amendment to `06a-semantic-fact-generation.md`

In §"Deferred to `06b` or later phases", remove `enum/integer conversion`
from the list of language decisions that "remain future". It is now decided.
The surrounding list is otherwise unchanged.

`06a` needs no generation change: both directions are ordinary casts, already
covered by the existing cast rule, which resolves the destination, visits the
source without destination equality, and retains a `castRecord` for `06b`.

## Amendment to `OPEN-DECISIONS.md`

Remove `Enum conversion rules` from §Semantics and add to §Resolved:

- **Enum conversion rules: decided.** `enum -> integer` is a total explicit
  conversion to the variant's declaration ordinal. `integer -> enum` is an
  explicit checked conversion spelled `n as ?Enum`, yielding `none` for a
  value that names no variant. Tagged unions are excluded. See
  `proposals/enum-integer-conversion.md`.

## Open follow-on question: explicit variant values

This amendment does not give Pebble explicit enum values, and that limits the
C interop it was motivated by.

Because a variant's integer is its declaration ordinal, a Pebble enum can only
ever produce `0, 1, 2, ...` in declaration order. Two consequences:

1. **Bit flags cannot be expressed.** A flags enum needs `1, 2, 4, 8`.
   Ordinals cannot produce that, so flag sets must be written as integer
   constants rather than as an enum.
2. **A C enum with non-sequential values cannot be mirrored.** Given
   `enum { READ = 1, WRITE = 2, EXEC = 4 }` in a C header, the corresponding
   Pebble enum converts to `0, 1, 2` and the interop is silently wrong.
3. **Reordering variants changes their integer.** Any value serialized to
   disk or sent over a wire breaks when a variant is inserted anywhere but
   the end. C has the same hazard but offers explicit values as the fix.

Adding `enum { read = 1, write = 2, exec = 4 }` would require a grammar change
in `03a`, a `04b`/`05a` metadata change to carry the value, and an extension
to the phase-6 constant evaluator, which already evaluates enum variants. The
conversion rules above would then read the declared value instead of the
ordinal, with no change to their wording.

This is a separate decision and does not block the conversion rules. It should
be settled before anyone relies on enum-to-integer values crossing a process
or language boundary.

## Non-goals

- Implicit conversion in either direction. Both require an authored cast; this
  does not weaken `06b`'s rule that no distinct concrete numeric pair converts
  implicitly.
- Arithmetic on enum values. `Color.red + 1` remains invalid; convert first.
- Ordering enums by their integer. `06b`'s existing rule stands: enum ordering
  is declaration order and requires exact nominal identity.
- Any change to tagged unions or to untagged-union safety.

## Acceptance

```pebble
type Color = enum { red, green, blue };

fn roundTrip() void {
    let n int = Color.blue as int;   // 2
    let back ?Color = n as ?Color;   // some blue
    let bad ?Color = 7 as ?Color;    // none
}
```

`Color.blue as ?int` and `7 as Color` must both be rejected, the first because
optional injection of a total conversion is not a checked conversion, the
second because a bare enum destination has no meaning for an invalid value.
