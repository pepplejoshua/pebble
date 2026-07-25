# Open language decisions — consolidated inventory

This is a reading aid, not a new spec. It gathers every open or deferred
**language** decision recorded across `spec/compiler/*.md` into one place, so
you don't hit them one at a time by surprise while writing Pebble programs.
Nothing here is invented: every entry is sourced from a specific file and
section, quoted or paraphrased faithfully. No syntax is proposed and no
winner is picked.

Status snapshot used for "what it blocks" below: 06a is nearly complete
(06a.1–06a.7 accepted, 06a.8 next); 06b has not started.

For each entry:
- **Undecided** — the plain-language gap.
- **Where** — file and section.
- **Blocks** — which phase/slice can't finish, or "nothing today" if it's a
  pure future feature.
- **Costs you** — what you can't write, or can't rely on, as a result. Said
  plainly, including when the cost is small.

---

## 1. Blocking now

_(decisions the tail of 06a or the whole of 06b actually needs)_

### 1.1 Tuple positional member access (`pair.0`) has no constraint to solve it

**Undecided:** the accepted `05b` constraint set has no relation for tuple
component access by ordinal (only `HasField`, matched by name). Right now
`pair.0` does not fail cleanly — it leaves an unresolved type variable that
leaks into every enclosing expression.

```pebble
let pair (i32, str) = (1, "a");
let first i32 = pair.0;   // fails today: T0510, "no unique semantic type"
```

**Where:** `spec/compiler/proposals/05b-tuple-component-constraint.md` (whole
document — status: **Proposed**, not yet accepted); the gap is also named in
`06a-semantic-fact-generation.md` §"Members, calls, and generic applications"
("Tuple numeric members retain the parsed index and base; structural
decomposition belongs to their generation shape" — the proposal calls this
sentence "the specification gap").

**Blocks:** reopens the already-accepted `05b.5` slice, gates a one-line `06a`
change to `member_facts.go`, and sits upstream of the `06a.8` handoff into
`06b`.

**Costs you:** as written today, positional tuple access does not work at
all, even when the tuple's type is fully known and annotated — not an edge
case, the plain case. Until this proposal is accepted you cannot rely on
`.0`/`.1`-style tuple field access anywhere in a program.

### 1.2 Whether calls/`sizeof`/floats can ever become constant expressions (narrow, real gap)

**Note first:** `OPEN-DECISIONS.md` lists "Constant-expression language and
evaluation rules" as a top-priority open item, but this is now **stale** —
`06a-semantic-fact-generation.md` §"Constant evaluator and
`ArrayLengthEvaluator`" fully specifies the accepted constant language, and
slice `06a.2` already ships it (see the accuracy section at the end of this
document). The language is closed and small:

```text
integer, Boolean, character, and string literals; grouping;
integer unary - and ~; Boolean unary !;
integer + - * / % << >> & | ^; Boolean && ||;
same-kind == != and ordered scalar comparisons;
references to module-level `let` declarations with constant initializers;
enum variant values.
```

**Undecided (the part that survives):** that same section states plainly
that "Calls, casts, `sizeof`, floats, optionals, pointers, aggregates,
indexing, interpolation, functions, parameters, locals, and `var` are not
constant expressions" — full stop, with no hedge about revisiting it. Nothing
in the phase docs flags whether this boundary (e.g. allowing `sizeof` or a
pure-function call in a constant context later) is itself an open language
question or a closed one; it is simply asserted. Treat it as decided until
you find text that says otherwise.

```pebble
let SIZE = sizeof(i32) * 4;   // rejected today: sizeof is not constant-evaluable
let buf [SIZE]i32;
```

**Where:** `06a-semantic-fact-generation.md` §"Constant evaluator and
`ArrayLengthEvaluator`" (lines ~249–266).

**Blocks:** nothing today — `06a.2` already shipped this and `06b`'s global/
array-length validation already consumes it (`06b-validation-and-typed-ir.md`
§"Constants, globals, and `sizeof`").

**Costs you:** array lengths and constant globals may only use literals,
grouping, the listed operators, references to other module-level constant
`let`s, and enum variants. `sizeof`, any function call, and float arithmetic
can never appear in `[N]T` or a global initializer today — no
`[sizeof(Header)]byte`-style array sizing, no computed float constants.

### 1.3 Untagged unions cannot be constructed, read, or written

**Undecided:** the untagged-union safety model (what makes reading/writing a
variant sound, if anything is enforced at all) has no accepted design.

```pebble
type Raw = union { i i32; f f32; };
let r Raw = Raw.{ i = 1 };   // rejected today: C0615
```

**Where:** `06b-validation-and-typed-ir.md` §"Members, calls, and generic
applications" ("untagged-union construction/read/write and extern opaque
construction are `C0615`"); listed as still future in
`06a-semantic-fact-generation.md` §"Deferred to 06b or later phases"
("untagged-union safety"); `OPEN-DECISIONS.md` §Semantics,
"Untagged-union safety model."

**Blocks:** `06b`'s record/aggregate validation slice, which is next in line.

**Costs you:** plain (non-`enum`) `union` types are currently declarable but
otherwise dead — you cannot build one, read a field from one, or write to
one. Only the tagged form (`union enum`) is usable today. If your design
needs a C-style raw union for interop or memory reuse, it does not work yet.

### 1.4 Enum-to-integer conversion — the two phase-6 documents disagree

**Undecided in one document, stated as decided in the other.**
`06a-semantic-fact-generation.md` §"Upstream status and deferred features"
lists "enum/integer conversion" among the "Language decisions already marked
future by phase 6" — i.e. still open. But `06b-validation-and-typed-ir.md`'s
own composite compatibility matrix already gives a concrete answer: the row
`enum | integer or reverse | forbidden`. Both documents are phase 6; they
disagree about whether this is decided.

```pebble
type Color = enum { red, green, blue };
let n int = Color.red as int;   // 06b's matrix says forbidden; 06a still calls this open
```

**Where:** `06a-semantic-fact-generation.md` line ~1247 vs.
`06b-validation-and-typed-ir.md` line ~404 (composite conversion matrix).
`OPEN-DECISIONS.md` §Semantics also lists "Enum conversion rules" as open,
agreeing with `06a`.

**Blocks:** `06b`'s conversion-matrix validation slice — it needs to know
whether the "forbidden" row is the accepted rule or a placeholder.

**Costs you:** right now — and possibly permanently, if `06b`'s matrix wins —
you cannot cast an enum to its ordinal integer or back. No `as int` escape
hatch for serialization, bit-flag tricks, or handing an enum across a C
boundary as a plain integer; you would need a hand-written mapping
function (e.g. a `switch`) instead.

### 1.5 Pointer arithmetic, ordering, and nil policy

**Undecided in one place, answered in another.** `05b-algebraic-inference.md`
§"Boundary with phase 6" assigns "pointer arithmetic, pointer comparison, and
nil policy beyond the structural fact above" to phase 6 as unowned. But
`06b-validation-and-typed-ir.md`'s operator table already gives concrete
rules: pointer `==`/`!=` is accepted (including against a context-shaped
`nil`), and "pointer ordering and arithmetic are forbidden."

**Where:** `05b-algebraic-inference.md` §"Boundary with phase 6" (line
~1445); `06b-validation-and-typed-ir.md` §"Exact operator validation" (lines
~433–439). `OPEN-DECISIONS.md` §Semantics: "Pointer arithmetic and pointer
comparison."

**Blocks:** `06b`'s operator-validation slice.

**Costs you:** you can compare a pointer for equality or against `nil`, but
`ptr + 1` and `ptr < other` are rejected outright — no manual pointer-walking
or C-style pointer-as-iterator code. Use slices/arrays and indexing instead.

### 1.6 Calling-convention compatibility and C variadics

**Undecided:** whether Pebble-convention and C-convention function types can
ever be adapted to each other, and what promotion rules a C-variadic call
uses.

**Where:** `05a-semantic-type-store.md` §"Composite identity" calls this
"the phase-6 open decision" explicitly. `06b-validation-and-typed-ir.md`
§"Calls and calling conventions" already states "no default promotions are
defined" and calling a C variadic function is `C0604`; §"Compatibility and
coercions" testing notes say "calling conventions never adapt."
`OPEN-DECISIONS.md` §Semantics: "Function compatibility across calling
conventions."

**Blocks:** `06b`'s call-validation slice.

**Costs you:** you cannot call a C variadic function (`printf` and friends)
from Pebble at all today, and a `Pebble`-convention function value is never
interchangeable with a `C`-convention one even when their fixed parameter
lists match exactly.

---

## 2. Blocking later

_(decisions tied to a phase that will need them, not yet reached)_

### 2.1 Generic monomorphization vs. erasure, and cross-module specialization

**Undecided:** whether every generic instantiation is monomorphized or some
use an erased/runtime form; whether inferred requirements are shown in
generated docs; and the rules for specialization visibility and cross-module
ownership.

**Where:** `07-generics.md` §"Open generic decisions" (the document's own
heading, matching the README's "Open" label).

**Blocks:** phase 7 (Generics), not yet started.

**Costs you:** you can already write a generic function whose body implies a
requirement (e.g. `fn max[T](a T, b T) T` needs `Ordered(T)`), but you don't
yet know whether instantiating it from two different modules produces one
shared specialization or a private one per call site — which affects binary
size and whether the compiler will ever surface "`T` must support `Ordered`"
in documentation rather than only at a failing call site.

### 2.2 Generic anonymous functions

**Undecided:** whether/how an anonymous function can ever take its own type
parameters — owner identity, inference, and lowering are unspecified.

```pebble
let identity = fn[T](x T) T { return x; };   // syntactically parses, always rejected (C0608)
```

**Where:** `06a-semantic-fact-generation.md` §"..." (lines ~625–638,
~1227–1234): syntactically valid, "unsupported in the initial checker
contract," visited only for closed-dispatch bookkeeping, rejected via
`unsupportedCallableRecord`/`C0608`. Same boundary in
`05b-algebraic-inference.md` §"Generic boundary" ("Generic anonymous functions
are outside the initial checker contract and never create a generic owner in
`05b`"). `06-checking-and-conversions.md` §"Resolved inputs and future
decisions" confirms the `C0608` rejection is the accepted interim behavior.

**Blocks:** nothing today (actively rejected, not silently wrong); blocks
whichever later phase wants to add generic lambdas.

**Costs you:** you can write a generic function or method, but never an
inline generic closure/lambda — all generic logic has to live in a named
top-level function or method today.

### 2.3 `_` as a generic type-argument placeholder

**Undecided:** the source/phase-7 contract for writing `_` to mean "infer
this type argument" is not accepted.

**Where:** `05b-algebraic-inference.md` §"Generic boundary" ("`_` remains
unsupported until its phase-7/source contract is accepted; it is not
interned as a type").

**Blocks:** phase 7.

**Costs you:** no partial generic-argument elision. You either supply every
type argument explicitly or omit all of them and rely on full inference —
you cannot write `Container[_, int]` to fill in only the arguments you care
about.

### 2.4 Slice and string lifetime, escape, and ownership

**Undecided:** whether a slice/string can safely outlive the storage it was
taken from, be returned from a function, or be stored in a struct — no
runtime representation or ownership model is fixed.

**Where:** `05a-semantic-type-store.md` §"Composite identity" ("Ownership,
lifetime, and runtime representation are intentionally not identity
components because those language rules remain open"); reaffirmed in
`06b-validation-and-typed-ir.md` §"Brackets, indexing, slicing, and checks"
("Slice lifetime, escape, ownership, and runtime representation are
explicitly future and nonblocking; `06b` neither rejects an otherwise
well-typed slice for escape nor invents a lifetime proof").
`OPEN-DECISIONS.md` highest-priority #4.

**Blocks:** no phase currently (typed IR just preserves enough information
for a later pass to lower it); will block whatever phase eventually defines
slice/string runtime representation, likely touching phases 9/10.

**Costs you:** slices and strings work today, but nothing is promised about
whether it's safe to return a slice of a local array, stash one in a
long-lived struct, or slice a string and use it after the original binding
goes away. Don't build APIs that depend on today's C-prototype escape
behavior; it may be restricted or redefined later.

### 2.5 Release-mode fault behavior (overflow, bounds, unwrap)

**Undecided:** whether arithmetic overflow, an out-of-bounds index/slice, or
unwrapping an empty optional panics, traps, or is undefined, and whether that
differs between "safe," "release," and "freestanding" configurations.

**Where:** `06b-validation-and-typed-ir.md` §"Remaining declaration and
statement legality" ("Direction and overflow are explicit lowering/runtime
behavior, not type inference") and §"Resolved upstream contracts and future
decisions" ("phase-10 release-mode behavior for numeric-cast,
arithmetic-fault, optional-unwrap, and bounds-check failures" is explicitly
future and nonblocking); `10-c-backend-and-runtime.md` §"Runtime ABI" lists
"assertion, bounds-failure, unwrap-failure, and panic behavior" and "safe,
release, and freestanding configurations" as still to be defined.
`OPEN-DECISIONS.md` highest-priority #3.

**Blocks:** phase 10 (C backend and runtime), not yet reached.

**Costs you:** typed IR already records *that* a check happened (checked
index, checked cast, etc.), but not what happens when it fails in each build
mode. Code that depends on a specific failure behavior — e.g. wraparound
arithmetic instead of a trap — is not portable across release modes yet, and
you can't rely on `release` mode being either faster-and-unchecked or
identical-but-checked until this is decided.

### 2.6 `char` semantic representation and C ABI (uncertain — may already be answered elsewhere)

**Undecided:** how Pebble's `char` (used for indexing `str`, in switches, in
comparisons) is represented at the C ABI boundary — byte, Unicode scalar
value, or something else.

**Where:** no phase document defines this; usage sites
(`06b-validation-and-typed-ir.md` string-indexing table, switch rules) all
say "Unicode-scalar index" for `str` indexing but never pin down `char`'s
storage width or its C-side representation. `OPEN-DECISIONS.md` §Semantics:
"Semantic representation and C ABI of `char`."

**Blocks:** phase 10 C-interop/ABI work.

**Costs you:** any C-facing function signature that needs to pass or receive
a Pebble `char` has no defined shape to target yet — treat `char` interop as
unstable until the ABI doc exists.

### 2.7 Backend/runtime shape: freestanding minimum, ABI stability, dialect/toolchain (marked uncertain — leans compiler-internal but has visible user impact)

**Undecided:** the minimum C dialect/toolchain supported, whether the
runtime ABI is versioned/stable across compiler releases, and the
freestanding runtime's minimum footprint. Partially resolved: `int`/`uint`
are already pinned to the target's native word representation (see below),
so "target data-model assumptions" is not fully open — `OPEN-DECISIONS.md`
lists it as though nothing is decided, which understates it.

**Where:** `10-c-backend-and-runtime.md` §"Runtime ABI" (the bulleted list:
"context and allocator layout... safe, release, and freestanding
configurations... runtime ABI version") and §"Current embedded behavior to
inventory" ("Each item must be accepted into the ABI, redesigned, or removed
before backend reimplementation"). The already-decided half is in the same
document's opening section: "Pebble `int` and `uint` use the target-native
signed and unsigned word representations, such as `intptr_t` and
`uintptr_t`; they do not inherit the implementation-defined width of C
`int`." `OPEN-DECISIONS.md` §Backend and runtime (all six bullets).

**Blocks:** phase 10.

**Costs you:** if you target freestanding/embedded, you don't yet know which
language features silently require a hosted allocator or libc, and because
the ABI isn't versioned yet, recompiling a program against a newer compiler
could change its runtime layout without warning. You *can* already rely on
`int`/`uint` being pointer-width on the target rather than a fixed 32 bits.
This item is more infrastructure than syntax, but it gates what platforms
you can ship to.

### 2.8 Driver/CLI modes: freestanding, no-main, custom entry point, release mode

**Undecided:** the Go driver has not decided which of the C prototype's CLI
modes it preserves. Several of these change what counts as a legal or
runnable Pebble program, not just how the compiler is invoked.

**Where:** `11-driver-and-cli.md` §"Modes to preserve or decide": "The
current prototype exposes parse-only, check-only, generate-C-only,
freestanding, no-main, custom entry point, release mode, warning mode,
include paths, library paths, libraries, local/system headers, custom C
compiler, C flags, and generated-C retention. Each option must state which
phase consumes it."

**Blocks:** phase 11 (Driver and CLI), not yet reached.

**Costs you:** you don't yet know whether you'll be able to compile a
library with no `fn main()`, designate a different function as the entry
point, or target a freestanding build — all of these are prototype behavior
only, not yet an accepted contract. Don't design a program around any of
them surviving unchanged.

---

## 3. Future features

_(explicitly deferred, blocking nothing today, listed so they aren't forgotten)_

### 3.1 Closures / closure capture semantics

**Undecided:** whether Pebble ever gains real closures (environment capture,
heap-allocated captured state) at all.

```pebble
fn makeCounter() fn() int {
    let count = 0;
    return fn() int { count = count + 1; return count; };  // capturing `count`: rejected
}
```

**Where:** `04b-name-resolution.md` §"Anonymous functions and captures"
("Whether closures are ever added is an explicit future language decision,
not an assumption of this resolver contract"); `05b-algebraic-inference.md`
§"05a discrepancy audit and unresolved decisions" ("whether the language ever
gains closures... `05b` defines no closure machinery"); `06a` (twice: §
"Function terms" and §"Deferred to 06b or later phases" — "Closure support is
an explicit unresolved language decision, not an assumed roadmap item");
`06-checking-and-conversions.md` §"Resolved inputs and future decisions."
`OPEN-DECISIONS.md` §Semantics: "Closure capture semantics."

**Blocks:** nothing today — the current noncapturing model is fully
specified and does not wait on this decision (see dependency note below).

**Costs you:** you cannot write a closure that captures a local, a
parameter, or a loop variable from its enclosing function — no
"make a counter/accumulator and hand it back" pattern, no capturing
callbacks. An anonymous function may only reference module-level globals and
other module members. If you write one that captures a local anyway, it is
rejected (checker diagnostic `C0617`) but the rest of the function is still
checked for other errors.

### 3.2 Distinct/newtype declarations

**Undecided:** `type X = int`-style declarations are currently pure aliases;
a future "distinct" or "newtype" form that creates a genuinely new nominal
type from an existing one is not designed.

**Where:** `05-types-and-inference.md` §"Type identity" ("Aliases resolve to
an existing type identity. A future distinct/newtype feature would create a
new nominal identity and must use separate syntax").

**Blocks:** nothing today.

**Costs you:** `type UserID = int` and `type OrderID = int` are both just
`int` — the compiler will not stop you from passing a `UserID` where an
`OrderID` is expected, or from mixing either up with a plain `int`. There is
no lightweight nominal-wrapper safety net for primitives until this lands.

### 3.3 Pointer mutability, nullability, and safety distinctions

**Undecided:** Pebble has exactly one pointer form, `*T`, identified only by
its pointee. No `const`-vs-mutable pointer, no address-space tag, no
non-null-by-construction pointer, and no safety/ownership bit exist yet.

**Where:** `05a-semantic-type-store.md` §"Composite identity" ("Pebble
currently has one raw pointer form; no mutability, ownership, address-space,
nullability, or safety bit is accepted. A future such distinction must be
added to the key before it can affect semantics").

**Blocks:** nothing today.

**Costs you:** the type system does not track or enforce pointer mutability
or non-nullability — `*T` says nothing about whether the pointee can be
mutated through it or whether it can be `nil`. Any such guarantee has to be
enforced by convention, not by the compiler.

### 3.4 Named constraint/trait syntax for generics

**Undecided:** generic requirements (like `Ordered(T)`) are inferred from a
generic body today; there is no source syntax to name or declare them
explicitly.

**Where:** `07-generics.md` §"Auto-constraints" ("A future named
constraint/trait system can expose these obligations in source syntax
without replacing the core mechanism").

**Blocks:** nothing today — inference already covers the mechanism.

**Costs you:** you cannot write `fn max[T: Ordered](a T, b T) T` or any
other explicit bound — you find out what a generic function requires only by
reading its body or hitting a failing instantiation, not from its signature.

### 3.5 Public structural traits for `Callable`/`Indexable`/`Sliceable`/`HasComponent`

**Undecided:** whether user code will ever be able to opt a type into being
callable, indexable, sliceable, or tuple-component-accessible via a public
trait, versus these staying built-in-only structural relations.

**Where:** `06a-semantic-fact-generation.md` §"Upstream status and deferred
features" ("Public generic callable/indexable/sliceable traits likewise
remain future language features; the closed structural constraints do not
create them"); reaffirmed in `06b-validation-and-typed-ir.md` §"Resolved
upstream contracts and future decisions" ("public callable/indexable/
sliceable traits... are not invented by this task").

**Blocks:** nothing today.

**Costs you:** `()`, `[]`, and slicing syntax work only on the compiler's
built-in categories (functions, arrays/slices/strings, tuples) — you cannot
make your own struct callable or indexable by implementing some trait; there
is no such extension point yet.

### 3.6 Long-term retention of compatibility syntax

**Undecided:** whether `print` as a statement, C-style `for`, range `loop`,
and the `some`/`none` optional spellings survive past the compatibility
parser, or are later replaced/removed.

**Where:** `OPEN-DECISIONS.md` §"Syntax and parser" is the only place this is
recorded; the phase docs describe these constructs' current parsing and
checking (e.g. `PrintStmt` in `06a`/`06b`, C-style `for` handling in `06a`
§"Statement and control facts") but do not revisit whether they are
permanent. This entry has not been picked up or resolved anywhere in the
phase documents (see accuracy note below).

**Blocks:** nothing today — all of these parse and check normally right now.

**Costs you:** nothing today; but code written using `print`, C-style `for`,
`loop`, `some`, or `none` may need mechanical migration later if any of them
are retired. Low risk to write with them now, but don't assume the spelling
is permanent.

### 3.7 Bound-method values

**Undecided:** whether `obj.method` can ever be taken as a standalone
callable value (a bound method), rather than only used in immediate call
position.

**Where:** `06a-semantic-fact-generation.md` §"Deferred to 06b or later
phases" (listed among still-future decisions: "bound-method values");
`06b-validation-and-typed-ir.md` diagnostic table, `C0608`: "invalid
callable/method declaration, generic anonymous function, or bound-method
value" — all three share one rejection code. Also confirmed earlier in this
document at §"Calls and calling conventions": "A method member outside
immediate-call position is `C0608`."

**Blocks:** nothing today.

**Costs you:** `let f = obj.method;` followed by calling `f(...)` later does
not work — you must call `obj.method(...)` directly at the call site every
time. No first-class bound methods to pass around as callbacks.

### 3.8 Unsafe pointer policy

**Undecided:** whether Pebble ever gets an "unsafe" pointer escape hatch
(unchecked casts, raw memory reinterpretation, etc.) beyond the single
restricted `*T` form described in §1.5/§3.3.

**Where:** listed as future in `06a-semantic-fact-generation.md` §"Deferred
to 06b or later phases" and `06b-validation-and-typed-ir.md` §"Resolved
upstream contracts and future decisions" ("unsafe pointer policy").

**Blocks:** nothing today.

**Costs you:** there is no sanctioned way to do the kind of raw,
unsafe-by-choice pointer manipulation some systems code relies on (e.g. an
`unsafe` block or an explicit opt-out of bounds/type checks) — everything
must go through the checked, restricted pointer/slice model.

### 3.9 Deliberate first-version lexer exclusions

**Undecided/not yet designed:** a specific, named list of surface-syntax
features the lexer does not support in this version, each one independently
addable later.

**Where:** `02-source-and-lexing.md` §"Deliberate first-version exclusions":
"Unicode identifiers; numeric type suffixes; raw strings; ordinary multiline
quoted strings; block comments; octal literals... Each exclusion can be
added independently later. None is needed to make the initial lexer or type
inference coherent." The same document also notes documentation comments and
nested block comments "may be added later" (§"Whitespace and comments").

**Blocks:** nothing today.

**Costs you, concretely:**
- identifiers must be ASCII — no identifiers in other scripts;
- no numeric literal suffixes (e.g. no `1u8`-style literal, only an
  annotation/context-driven type for the literal);
- no raw (unescaped) string literal syntax;
- no ordinary multiline quoted string literal — a literal string is one
  line;
- no block comments (`/* ... */` lexes as `/` then `*`, not a comment) and
  no documentation-comment syntax;
- no octal integer literals;
- no hexadecimal float literals and no `NaN`/infinity literal spelling
  (`02b-literals-and-interpolation.md` §"Floating-point literals": "no
  literal spelling initially" — unconstrained float literals default to
  `f64`);
- no compound bitwise-assignment operators `&=`, `|=`, `^=`, `<<=`, `>>=`,
  no `->` token, and no `**` exponentiation operator
  (`02a-token-inventory.md` §"Removed prototype token distinctions": "are
  not tokens in the initial language").

None of this is exotic to work around, but each is a small daily-writing
surprise if you assume a C-like lexer.

---

## Key dependencies between decisions

**The most significant one: closures do not gate the current anonymous-
function contract.** Four separate documents go out of their way to state
that whether Pebble ever adds real closures (§3.1) is completely independent
of the noncapturing, globally-hoisted anonymous-function model that already
ships:

- `04b-name-resolution.md` §"Anonymous functions and captures": "Whether
  closures are ever added is an explicit future language decision, not an
  assumption of this resolver contract."
- `05b-algebraic-inference.md` §"Generic boundary": generic anonymous
  functions' "future support may retain the same noncapturing, globally
  hoisted model and does not depend on closure support."
- `06a-semantic-fact-generation.md` (twice): the nongeneric contract's
  "future support may retain the same noncapturing, globally hoisted model
  and does not depend on closures," and separately, "Closure support is an
  explicit unresolved language decision, not an assumed roadmap item."

This means you can treat the current anonymous-function rules (§3.1's cost:
no capturing a local/parameter/loop variable) as **stable**, not provisional
— they will not be quietly rewritten if/when closures are eventually
designed. Practically: write callback-style code today assuming anonymous
functions stay global-only, and don't hold off on using them while waiting
for closures — the two are decoupled by explicit design statement, not by
omission.

**Second dependency: generic anonymous functions build on top of the same
noncapturing contract, and are separately gated.** §2.2's rejection (`C0608`)
is independent of both closures and of ordinary (nongeneric) anonymous
functions, which already work. If generic anonymous functions are ever
accepted, `05b-algebraic-inference.md` §"Generic boundary" says their design
"may retain the same noncapturing, globally hoisted model" — i.e. the same
decoupling from closures applies a second time, one level up.

**Third: the tuple-component proposal (§1.1) is a prerequisite the others
don't mention.** It reopens the already-accepted `05b.5` slice and touches
`06a`'s member-fact generation before `06a.8`'s handoff to `06b` — every
other "blocking now" item assumes `06a`'s output is already frozen and
correct, but this one is the one place a currently-*accepted* slice still
needs to change.

**Fourth: several "forbidden today" rules in §1.3–§1.6 are downstream of
still-open "will this ever be allowed" questions in §3.1, §3.7, and §3.8.**
Untagged-union access (§1.3), enum/integer conversion (§1.4), and
bound-method values (§3.7) are all rejected today via a fixed diagnostic
(`C0615`/matrix-forbidden/`C0608`), while the underlying "should this exist
at all" question is separately listed as future. Don't read "rejected today"
as "permanently impossible" for any of these — but don't expect it to change
soon either, since no document proposes a design for any of them.

## Is OPEN-DECISIONS.md still accurate?

`OPEN-DECISIONS.md` is 46 lines and has not kept pace with the phase docs.
Findings below are grouped by whether the phase docs have resolved,
partially resolved, or simply never mention a listed item — plus items live
in the phase docs that never made it into the file at all.

### Listed as open but actually resolved (stale)

The three entries in this subsection have since been moved to a `Resolved`
section in `OPEN-DECISIONS.md`. They are kept here with their reasoning so
the resolution can be re-checked.

- **"Which implicit conversions exist between concrete numeric types?"**
  (highest priority #1). Resolved: none. `06b-validation-and-typed-ir.md`'s
  primitive conversion matrix (§"Compatibility and coercions") states flatly
  that every distinct concrete numeric pair is `explicit`-only (an `as`
  cast) — "There is no implicit conversion between distinct concrete numeric
  types. `int`, `uint`, every exact-width integer, `f32`, and `f64` are
  distinct." The testing-contract section for this slice even states the
  completion bar as "no distinct numeric pair is implicit." This is a fully
  specified, closed matrix, not an open question. `OPEN-DECISIONS.md` should
  drop this item or mark it resolved.

- **"Constant-expression language and evaluation rules."** Resolved, and in
  detail: `06a-semantic-fact-generation.md` §"Constant evaluator and
  `ArrayLengthEvaluator`" gives the exact accepted grammar (literals,
  grouping, the listed operators, references to constant module-level
  `let`s, enum variants) and states exactly what is excluded (calls, casts,
  `sizeof`, floats, and more — see §1.2 above). Slice `06a.2` already ships
  it. What might still be open — whether that excluded set (e.g. `sizeof`)
  is ever widened — is not stated as an open question anywhere; it reads as
  a closed design decision, not a placeholder.

- **"Global initialization ordering."** Moot by construction, not open:
  `06b-validation-and-typed-ir.md` §"Constants, globals, and `sizeof`"
  requires every non-extern global `let`/`var` to have an initializer
  accepted by the (fully specified) `06a` constant language, and constant
  evaluation is memoized/cycle-checked at compile time, not sequenced at
  runtime. There is no runtime ordering left to specify. Worth a one-line
  replacement noting *why* it's moot, rather than deletion, since a reader
  might otherwise assume it's still unaddressed.

### Listed as open, and genuinely still open, but the phase docs already say more than the file does

- **"Pointer arithmetic and pointer comparison."** Not fully open:
  `06b-validation-and-typed-ir.md` §"Exact operator validation" already
  states pointer `==`/`!=` is accepted (exact `TypeID` plus a context-shaped
  `nil`), and ordering/arithmetic are forbidden. What remains genuinely
  undecided is only a *future* unsafe-pointer feature (§3.8), not the
  current rule. See §1.5 above.

- **"Enum conversion rules."** Same pattern: `06b`'s matrix already commits
  to "forbidden" for enum↔integer in both directions, while `06a` still logs
  the underlying decision as future/unresolved. See §1.4 above — this is the
  clearest case in the whole sweep of two phase-6 documents describing the
  same rule at different confidence levels.

- **"Semantic representation and C ABI of `char`."** Half-answered, not
  fully open: the type-system half is settled (`char` is an identity-only
  type, never part of numeric conversion or operator families, used for
  Unicode-scalar string indexing). The C ABI half — its size and encoding
  when crossing into generated C — is a genuine silent gap: no document
  states it, and none flags it as deferred either. See §2.6 above.

- **"Target data-model assumptions"** (Backend and runtime section). Not
  fully open: `10-c-backend-and-runtime.md` already fixes `int`/`uint` to
  the target's native pointer-width representation. See §2.7 above.

### Still fully open, matching the file as written

Ownership/lifetime of slices and strings (§2.4), overflow/release-mode
behavior (§2.5), untagged-union safety (§1.3), closure capture semantics
(§3.1), function compatibility across calling conventions (§1.6), and the
rest of the "Backend and runtime" list (ABI stability promise, runtime
linkage model, panic/safety-check behavior, freestanding runtime
requirements) all remain exactly as open as `OPEN-DECISIONS.md` says.

### Live in the phase docs but never listed in OPEN-DECISIONS.md

- Whether every generic is monomorphized or erased, doc display of
  inferred requirements, and specialization visibility/cross-module
  ownership — `07-generics.md`'s own "Open generic decisions" heading
  (§2.1). This is the file's own README-style "Open" label, used nowhere
  else in the whole spec tree, and it isn't referenced from
  `OPEN-DECISIONS.md` at all.
- Generic anonymous functions (§2.2) and the `_` generic-argument
  placeholder (§2.3) — both explicitly named as unresolved in `05b`/`06a`,
  neither mentioned in the open-decisions file.
- Bound-method values (§3.7) and unsafe pointer policy (§3.8) — both listed
  alongside closures in `06a`'s and `06b`'s "future" paragraphs, but only
  closures made it into `OPEN-DECISIONS.md`.
- Public structural traits for `Callable`/`Indexable`/`Sliceable` (§3.5) and
  distinct/newtype declarations (§3.2) — both named as future in their
  respective phase docs, neither tracked centrally.
- Driver/CLI mode retention — freestanding, no-main, custom entry point,
  release mode (§2.8) — `11-driver-and-cli.md`'s own "Modes to preserve or
  decide" heading, not reflected anywhere in `OPEN-DECISIONS.md`.
- The tuple-component-access gap (§1.1) is newer than the open-decisions
  file and lives only in the `proposals/` directory; it is arguably the
  single most urgent item in this whole document and isn't tracked
  centrally anywhere.

### Spec-hygiene note

`06`, `06a`, and `06b` — three of the largest documents in the tree — never
use the four README labels (**Required**/**Current**/**Proposed**/**Open**)
as inline tags anywhere; every rule is flat prose. `07-generics.md` and
`12-testing.md` are the only two documents that literally write "Open." This
makes grepping for the labels themselves an unreliable way to find open
decisions (this document was built by reading surrounding prose instead) —
worth knowing if you try to script a future sweep like this one.
