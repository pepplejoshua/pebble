// Package backend lowers typed IR to C source emitted against the versioned
// runtime ABI (runtime/include/pebble_rt.h). It is deliberately narrow: the
// current slice emits exactly two entry shapes — an empty-bodied Pebble-
// convention void entry function, and a zero-parameter integer entry whose
// width (int, i32, or i64) is decided once by the entry's own result type and never
// mixed within a body. The body matches a single recursive block grammar: a
// block is zero or more `let <name> <width> = <expression>;` /
// `var <name> <width> = <expression>;` local declarations, plus
// `x = <expression>;` reassignments of an already-declared local, a
// compound assignment or postfix increment/decrement (`x += y;`, `i++`, `i--`,
// a tir.CompoundStore built by buildCompoundStore, which combines through the
// same checked-arithmetic runtime helpers a plain `x = x + y` uses), and a
// `while <condition> { <loop body> }` loop statement, followed by a tail that
// is either one `return <expression>;`, a two-armed
// `if <condition> { <block> } else { <block> }`, or a
// `switch <subject> { case v1: <body> ... else: <body> }` switch statement;
// a condition is an integer
// comparison, a ==/!= equality between two bool values, a bare bool value, or
// a && / || combination of those, built by buildCondition, and the two arms
// are themselves blocks under the same rule, so an
// arm may contain its own locals, reassignments, nested if/else, and loops. A
// while loop's body is a block of local declarations, reassignments, if
// statements (a loop-body if is built by buildLoopIf and has an optional
// else), nested while loops (built by buildWhile), nested range loops (built
// by buildRangeLoop), and break/continue
// statements (built by buildLoopJump), with no required tail (see
// buildLoopBody); a while can only be a leading statement, never the block's
// tail, except for one terminal shape: a final `while true { ... }` whose
// loop body never breaks out of it (every exit is a return) is exhaustive
// and may be the last statement of a non-void body, lowered by the same
// buildWhile with no trailing return (see the tail switch's While case). A `loop start..end : name { <body> }` range loop (or `..=`, the
// inclusive form) is a leading statement exactly like a while, lowering to a
// C for loop whose loop counter IS the bound iterator (a tir.RangeLoop with
// the iterator's own symbol.SymbolID on its Symbol field and Children
// [start, end, body]; the unbound `loop start..end { ... }` form with no
// `: name` is rejected), its body built by the same buildLoopBody seeded with
// the iterator as an ordinary local of the entry's width, so a SymbolValue
// reference to the iterator inside the body resolves like any other local
// (see buildRangeLoop). A `for <init>; <cond>; <update> { <body> }` classic
// for loop (a tir.For) is a leading statement exactly like a while and range
// loop, lowering to a C for loop with the same three individually-optional
// clauses: each clause is built by the same machinery its block-level
// counterpart uses — the initializer, when present, is a single Initialize
// declaring a local of the entry's width or bool (see buildForInitClause,
// sharing buildScalarInitializeCore with an ordinary leading declaration), the
// condition, when present, is built by buildCondition, and the update, when
// present, is a single Store reassigning a local already in scope (see
// buildForUpdateClause, sharing buildStoreCore with an ordinary Store), with
// the init/update clauses emitted bare (no statement indent/newline, the
// for-header's own syntax supplying the semicolons). For.Children is
// variable-length: the checker appends only the clauses actually present in
// the fixed relative order initializer, condition, update, then the body a
// Block last, and buildFor disambiguates them by node category — the
// condition, when present, is the unique CategoryValue among the non-body
// children. The initializer's declared local seeds a cloned locals scope
// before the body is built (mirroring how a range loop seeds its iterator),
// so references to it inside the condition/update/body resolve through the
// existing machinery, and a (void) cast for it is emitted as the body's first
// statement (the -Wunused-variable defense). Locals declared in an enclosing block are visible in
// a nested block;
// locals declared inside an arm or loop body are visible only within that
// scope. Every expression in an accepted body must carry the entry's own
// width — a local of the other width (an i32 local inside an i64 entry, or
// vice versa) is a clean width-mismatch rejection, never a coercion, since
// this backend has no cast/coercion lowering yet. Everything else is rejected
// with a descriptive error instead of guessed.
//
// Since 10.17, an integer entry's body may also call other Pebble-convention
// functions declared in the same unit. Only functions actually
// reachable from the entry by a tir.DirectCall are emitted (discoverReachableHelpers
// does a post-order worklist walk starting from the entry's body, following
// every call), each as its own `static <width> pebble_fn_<symbolID>(PebbleContext
// *ctx, <params>...)` block emitted before pebble_user_main so a called
// function's C definition precedes its use. A called function's parameters
// (each of the entry's resolved width or bool) seed its own locals scope
// before its body is built and are declared in the C signature with the same
// pebble_local_<symbolID> naming locals use, so a reference to a parameter
// inside the body is read exactly like a reference to a declared local.
// Every called function must resolve to the
// entry's own integer width — there is no cast/coercion lowering — and since
// 10.33 a called function may also resolve to void, in exactly one position: a
// bare discarded-expression statement (`helper();` on its own line, a
// tir.ExpressionStatement whose single child is a tir.DirectCall to a
// void-returning function, emitted as a bare `pebble_fn_<symbolID>(ctx,
// <args>);` C statement by buildExpressionStatement — the leading-statement,
// loop-body, and deferred-statement positions all route through that one
// builder). Recursion
// (self- or mutual) is rejected cleanly at discovery time, since this backend
// has no forward-declaration mechanism yet. Each called function's body is
// built by the exact same buildBlock, with its own fresh locals scope seeded
// with the function's own parameters; a void helper's body ends in the
// ImplicitReturn the checker appends, which emits nothing.
// A call expression emits `pebble_fn_<calleeSymbolID>(ctx, <arg0>, <arg1>,
// ...)`: the typed IR's
// DirectCall records context forwarding via ContextAction (ContextForward for a
// Pebble-convention call) but carries no explicit context argument, so the
// backend prepends ctx itself, exactly as pebble_user_main receives it, and
// each argument is built by the grammar its callee parameter resolves to.
//
// Since 10.19, a local may also be declared as a tuple whose element types are
// exactly the entry's resolved width and/or bool, initialized from a tuple
// literal (a tir.TupleValue), with individual elements read back as ordinary
// values (a tir.Load of a tir.TuplePlace — the only shape real source produces
// for reading a tuple-typed local's element; see buildExpr's Load case). A
// tuple type is emitted as one C struct typedef, named pebble_tuple_<typeID>_t
// from the tuple type's own stable types.TypeID and written once per distinct
// tuple type before any function that references it (C requires definition
// before use); each typedef's fields are the positional `_0`, `_1`, ... in
// element order. Tuple element types are restricted to the entry's width and
// bool, reads route through buildExpr/buildBoolExpr by the element's own type,
// and every other tuple shape — a nested tuple element, a str element, a whole
// tuple copied from another value, assigning into or reassigning a tuple
// local, a tuple parameter or result, a TupleElementValue indexing a tuple
// literal, or a TupleCoerce — is a clean rejection, never a guessed lowering.
//
// Since 10.21, a local may also be declared as an optional whose payload type
// is exactly the entry's resolved width (i32 or i64) or bool, initialized from
// a `some <expr>` expression (a tir.SomeOptional), and force-unwrapped with
// the postfix `!` operator (a tir.CheckedOptionalUnwrap). An optional type is
// emitted as one C struct typedef, named pebble_optional_<typeID>_t from the
// optional type's own stable types.TypeID, with two fields: `bool has_value`
// and `value` (the payload's C type). Force-unwrap is bounds-checked via the
// runtime helper pebble_rt_checked_unwrap_i32/i64/bool. `none` literals
// (a tir.NoneOptional) construct an absent optional the same way; reassigning
// an optional local is rejected cleanly. A reachable helper may also return an
// optional (its C return type is the optional's own typedef name), consumed at
// the call site as the direct initializer of a matching optional-typed local
// (`var o ?int = f();`). Optional-typed
// function parameters, and optional payload types other than the entry's
// width, bool, a tuple/struct type, or an enum, are out of scope and rejected
// cleanly.
//
// Since 10.22, a local may also be declared as a struct whose every field's
// type is exactly the entry's resolved width or bool, initialized from a
// struct literal (a tir.RecordConstruct, e.g. Point.{ x = 1, y = 2 }), with
// individual fields read back as ordinary values (a tir.Load of a
// tir.FieldPlace — the only shape real source produces for reading a
// struct-typed local's field; see buildExpr's Load case). A struct type is
// emitted as one C struct typedef, named pebble_struct_<typeID>_t from the
// struct type's own stable types.TypeID and written once per distinct struct
// type before any function that references it; each typedef's fields are
// named pebble_field_<memberSymbolID> from each field's own stable
// symbol.SymbolID, in the struct's declared field order (from its
// TypeDeclaration's member list — a construction site may list fields in any
// order, so the declared order is resolved by the collection pass, not taken
// from any RecordConstruct). A struct local's initializer is a C99
// designated-initializer brace list ({ .pebble_field_25 = 1, ... }), which
// sidesteps the construction-vs-declared field ordering problem entirely.
// Field reads route through buildExpr/buildBoolExpr by the field's own type,
// and every other struct shape — a struct field of any type other than the
// entry's width or bool, a whole struct copied from another value, assigning
// into or reassigning a struct local, a struct parameter or result, a
// FieldValue reading a field off a struct literal directly, or nested field
// access — is a clean rejection, never a guessed lowering.
//
// Since 10.23, a local may also be declared as a str value, initialized from a
// string literal (a tir.StringLiteral) only. A str local is declared directly
// as the runtime ABI's PebbleStr (runtime/include/pebble_rt.h) — a fixed
// runtime type, not a program-specific shape — initialized from the literal's
// decoded bytes re-escaped into a safe C string literal (escapeCString emits a
// fixed-width octal escape for every non-printable byte, so C's maximal-munch
// escape rules can never swallow a following digit) and its compile-time
// decoded byte length, so no runtime strlen is involved. Two str values may be
// compared with ==, !=, <, <=, >, or >= — each operand either a str-typed local
// (a SymbolValue, built by buildStrOperand) or another string literal directly.
// Equality and inequality emit the runtime helper pebble_rt_str_eq(<a>, <b>)
// (==) or its negation (!=); ordering comparisons emit
// pebble_rt_str_cmp(<a>, <b>) <op> 0, where the runtime helper returns
// negative/zero/positive like C's memcmp/strcmp and <op> is the C translation
// of the source operator. A str comparison lowers to a plain tir.BinaryValue
// with two un-wrapped operand nodes (confirmed against a real fixture), handled
// in buildComparison alongside the integer and bool comparison paths. Since
// 10.42, a char value may also be produced by str indexing — s[i] (a
// tir.CheckedIndex, reachable from real source via e.g. `let c char = s[0];`).
// The checker lowers a str's bracket read to a bare CheckedIndex — not
// Load(CheckedIndexPlace), the node array/slice indexing uses — because a
// str's byte-level content is not addressable as a place, so the read is a
// pure decode-to-value operation, emitted as the runtime's UTF-8 decoder
// pebble_rt_str_char_at_i32/i64(<base>, <index>): the base is a str value (a
// str-typed local reference, a bare string literal, or a call to a
// str-returning helper) built by buildStrOperand, and the index is an
// integer expression built by buildExpr or by the int-literal/SymbolValue
// shortcut buildArrayPlaceRead uses. s[i] is a Unicode-scalar-value index
// (the i'th codepoint, not the i'th byte), and the runtime panics on a
// negative or out-of-range index or on malformed UTF-8 encountered along the
// way. Everything else str-shaped remains out of scope and a clean rejection:
// str elements inside a tuple, array, or optional, and concatenation and
// interpolation (InterpolatedString). A str field inside a struct is supported
// (see structFieldCType / buildStructBraceList).
//
// Since 10.36, a str-typed local may also be reassigned (a tir.Store whose
// place names a str local), and a helper function may declare str-typed
// parameters and results. A reassignment's new value must be a string literal
// — this slice is deliberately literal-to-literal only, matching the
// declaration's own scope — emitted as a whole-struct PebbleStr reassignment,
// `pebble_local_<sym> = (PebbleStr){ .data = ..., .len = <N> };`, whose inner
// construction text is byte-identical to the declaration's (both share
// buildStrLiteralValue). Reassigning a str local from anything else — a
// str-typed local, a call result, concatenation — is confirmed reachable from
// real source and a clean rejection naming what was found. A str-typed
// parameter (validateHelperSignature now admits str alongside width/bool/
// tuple/struct) seeds the callee's locals scope with localInfo{isStr: true}
// exactly as a str local's Initialize does and is declared in the C signature
// as the runtime's fixed PebbleStr, so a reference to it inside the body reads,
// compares, and returns exactly like a str local. A str-typed result is
// declared as PebbleStr and built with resultInfo{isStr: true}, so the helper's
// tail-position Return builds its value via buildStrOperand (a SymbolValue
// naming a str local, a string literal, or a call to another str-returning
// helper). A str-returning helper's result is supported in exactly three
// positions: a matching str-typed local's declaration initializer
// (`let s str = g();`), a ==/!= comparison operand (`g() == "hi"`), and another
// str-returning helper's return value (`return g();`) — all confirmed
// reachable from real source. Call-site arguments to a str parameter accept a
// str local, a string literal, or a str-returning call, all built by
// buildStrOperand.
//
// Since 10.24, a helper function may declare a parameter of tuple type (one
// of the shapes 10.19 supports) or struct type (one of the shapes 10.22
// supports) alongside the width/bool parameters 10.18 already allowed. Such a
// parameter seeds the callee's own locals scope with localInfo{tuple: ...} /
// localInfo{structType: ...} exactly as an Initialize of a tuple/struct local
// does, so element/field reads inside the body resolve through the same
// Load(TuplePlace)/Load(FieldPlace) machinery unchanged; the C parameter is
// declared with the aggregate's own typedef name
// (pebble_tuple_<typeID>_t / pebble_struct_<typeID>_t) and gets the same
// (void) cast every other parameter does.
//
// Since 10.25, a call site may pass for such a parameter either an already-
// declared tuple/struct-typed local in scope (a plain SymbolValue naming a
// local whose declared type matches the parameter's type, emitted as the
// local's own pebble_local_<symbol> C name — the typedef makes passing the
// whole aggregate by value trivially valid C) or a freshly-constructed
// aggregate built inline at the call site — a TupleValue (f((1, 2))) or a
// RecordConstruct (f(Point.{ x = 1, y = 2 })), both reachable from real
// source — emitted as a C99 compound-literal expression,
// (pebble_tuple_<typeID>_t){ <e0>, <e1>, ... } for a tuple and
// (pebble_struct_<typeID>_t){ .pebble_field_<m0> = <e0>, ... } for a struct
// (the designated-initializer form reuses the same field-resolution logic a
// struct local's declaration uses, so a construction site's field order still
// need not match the declared order). Both inline forms are built by
// buildTupleValueExpr / buildStructValueExpr, which share their brace-list
// construction (buildTupleBraceList / buildStructBraceList) with
// buildTupleLocalDeclaration / buildStructLocalDeclaration; the local
// declaration paths are unchanged and still emit a bare initializer brace
// list. An argument that is neither a local reference nor an inline aggregate
// construct — including a SourceAlias-wrapped argument from extra parens
// (f(((1, 2)))), rejected consistently with every other SourceAlias-wrapped
// argument in this backend — is a clean rejection naming what was found.
//
// Since 10.26, a helper function (not the entry — the entry's own C return
// type stays entryReturnType(width) regardless of what the language lets you
// write, since a process exit code must be an integer) may declare a tuple or
// struct result type, one of the shapes 10.19/10.22 already support. Such a
// helper is declared with its aggregate's own typedef name as its C return
// type (pebble_tuple_<typeID>_t / pebble_struct_<typeID>_t), and its body is
// built with a resultInfo recording that aggregate, so its tail-position
// `return` builds its value via buildAggregateReturnValue instead of buildExpr:
// the return value must be either a plain SymbolValue naming an
// aggregate-typed local already in scope of the matching type (forwarding an
// already-computed aggregate without re-construction) or a fresh inline
// construction — a TupleValue (return (20, 22)) or a RecordConstruct (return
// Point.{ x = 20, y = 22 }), emitted as the C99 compound-literal expression
// buildTupleValueExpr / buildStructValueExpr build (the same 10.25 expression
// builders an inline call argument uses). Calling a tuple/struct-returning
// helper is supported in exactly one position: as the direct initializer of a
// matching aggregate-typed local declaration — `let t (i32, i32) =
// helperReturningTuple();` — where buildTupleLocalDeclaration /
// buildStructLocalDeclaration now accept a DirectCall initializer whose
// callee's result type matches the local's declared type, built by the same
// buildDirectCall machinery buildExpr's DirectCall case uses (context and
// argument handling identical; only the result type differs from the scalar
// case). Calling such a helper in any other position — as a call argument
// (f(makeT())), as an operand (makeT().0), or as another helper's return value
// (return helperReturningTuple();) — is confirmed reachable from real source
// and rejected cleanly naming what was found, never guessed. The entry itself
// still cannot declare a tuple/struct result type. collectTupleTypes /
// collectStructTypes discover a tuple/struct type used only as a helper's
// result type from each reachable helper's ResultType, mirroring 10.24's
// Parameters scan, so such a typedef is still emitted even when no reachable
// body ever constructs one.
//
// Since 10.27, an array-typed local may also be initialized from an
// ArrayRepeat ([v; N] — a single value repeated N times, the other array
// literal form 10.20 deferred). Unlike 10.20's ArrayValue brace-list
// construction — which would evaluate v once per slot if naively duplicated
// into a C initializer list — an ArrayRepeat-initialized local is emitted as
// three C statements instead of one declaration line: the array's own bare
// declaration, a synthetic temp (pebble_repeat_<symbolID>) holding the
// repeat value evaluated exactly once via buildExpr/buildBoolExpr, and a
// `for (size_t pebble_i_<symbolID> = 0; ...)` loop that fills every slot
// from the temp. Both synthetic names derive from the local's own
// declaration symbol, which is collision-free by construction: ArrayRepeat
// only ever appears as that one local's own initializer, so no other
// statement in the same function can reuse the symbol ID. The count child of
// an ArrayRepeat node is a synthesized compile-time IntegerLiteral that
// always equals the array type's own TypeKey.Array() length (confirmed
// against a real fixture), so the loop bound comes from the array type, not
// from re-parsing the count child. Element types stay restricted to the
// entry's width or bool, matching 10.20; element reads afterward work
// completely unchanged, since nothing about how the array is read changes,
// only how it is initialized. ArrayRepeat in any other position — a call
// argument (array-typed parameters are unsupported, rejected by
// validateHelperSignature), nested inside another aggregate's construction
// (rejected by the element-type gates), or indexed directly ([v; N][i],
// which lowers to a bare CheckedIndex) — is confirmed reachable from real
// source but already rejected cleanly by the existing gates, never guessed.
//
// Since 10.34, a local may also be declared as a plain (payload-less) enum —
// type Color = enum { red, green, blue }; — initialized from a variant literal
// (an EnumVariantValue, Color.green, or a zero-payload VariantConstruct,
// Color.red(), both confirmed reachable from real source), reassigned (c =
// Color.red;), switched on (the switch subject is an enum-typed local or
// variant literal, each case a SwitchCase whose CaseValue is the variant
// symbol, emitted as `case pebble_variant_<caseValue>:`, multi-value cases and
// the else/default arm unchanged from 10.31), and compared (all six
// comparisons between two enum values, confirmed checker-reachable including
// the ordering operators, lowered to the plain C operator on the enum
// constants). A plain enum type is emitted as one C enum typedef,
// pebble_enum_<typeID>_t, with one named constant pebble_variant_<member> per
// variant in the enum's declared order (TypeDecl.Members) — the declared order
// IS the discriminant (Members[i] gets C value i), so case labels and stored
// values agree with the typedef by construction. An enum and a struct are both
// Nominal in the type snapshot, so isEnumType distinguishes them from the
// unit's own node graph (an enum's members carry no struct-field evidence);
// the enum check precedes the struct check everywhere the two could collide.
// A tagged union (union enum, a variant carrying a payload) is now supported
// since 10.35: a tagged union with at least one non-void variant whose
// construction reaches this backend is emitted as a tagged struct typedef
// (pebble_union_<typeID>_t) whose tag field is the union's own discriminant
// enum typedef (pebble_enum_<typeID>_t, the same one-constant-per-variant enum
// a plain enum emits) and whose payload union has one member per non-void
// variant actually constructed in the reachable program, each named
// pebble_field_<member> exactly like a struct field. Construction (Choice.value(5))
// lowers to a C99 compound literal, (pebble_union_<typeID>_t){ .tag =
// pebble_variant_<member>, .payload = { .pebble_field_<member> = <payload> } },
// a payload-less construction (Choice.empty / Choice.empty()) to the same
// literal with only the tag set. A tagged-union-typed local may be declared,
// reassigned, and switched on; the switch subject reads the local's .tag field
// (or a construction used directly as the subject builds the compound literal
// and reads its .tag), and the case labels are the same
// `case pebble_variant_<caseValue>:` a plain enum uses, since the discriminant
// ordinal scheme is identical. Payloads are restricted to exactly the entry's
// resolved width or bool — any other payload (a tuple/struct/array/optional/
// str/nested-enum, or the checker's unanchored int for a literal-arithmetic
// payload) is a clean rejection naming what is unsupported. No syntax exists in
// the language to read a payload back out of a matched case (a switch case
// value is a bare expression — there is no pattern-binding syntax), so this
// backend implements construction + storage + discriminant-only matching only.
// Since 10.45, a plain enum value may also be cast to an integer (an
// EnumToInteger, e.g. Color.green as i32) and used anywhere an integer of the
// destination width is valid. The cast lowers to a plain, unchecked C cast of
// the enum value's expression to the destination integer type — no runtime
// validity check is needed, because a well-typed enum value is always a valid
// member of its enum's declared variant set, so reading out its underlying
// integer representation (the variant's ordinal in declared order, which IS the
// C enum constant's value) is always well-defined; the destination width is
// resolved from the node's own Type exactly as IntegerCast resolves its own,
// the single operand (an enum-typed local reference, a variant literal, or a
// zero-payload variant construction) is built by buildEnumValue, and the
// emitted C is `(<destination C type>)(<enum value expression>)`. The reverse
// direction — integer cast to an enum, CheckedIntegerToEnum — is implemented
// since 10.46: `5 as Color` lowers through the single canonical-width runtime
// primitive pebble_rt_checked_int_to_enum (see the buildExpr case and
// buildCheckedIntegerToEnumExpr), which bounds-checks the integer against the
// destination enum's variant count — Pebble enums are ordinal, so an integer
// names a real variant exactly when 0 <= value < variant_count — with SAFE
// mode panicking out-of-range (PEBBLE_PANIC_ARITHMETIC_OVERFLOW) and RELEASE
// mode skipping the check entirely (a plain unchecked cast, trusting the
// input). The optional-destination form, OptionalIntegerToEnum (`5 as ?Color`),
// is implemented since this slice, but ONLY as a local variable declaration's
// initializer (see buildOptionalIntegerToEnumDeclaration): the cast must
// evaluate its source integer exactly once while producing both a has_value
// validity bool and an enum value, which needs a pre-declaration statement the
// backend can place only at the two local-declaration call sites; every other
// position is a clean rejection, not a double-evaluated emission.
// Enum-typed function parameters/results, and enum-typed tuple/struct/array
// elements and fields, remain clean rejections.
package backend

import (
	"fmt"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/tir"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
	"io"
	"sort"
	"strings"
)

// Emit writes C source for unit's designated entry function to w. The entry
// function (identified by entrySymbol) must be Pebble-convention and take zero
// parameters. Its result must be either void with a completely empty body (no
// statements — only ever an ImplicitReturn, i.e. exactly what `fn main() void
// {}` produces) or int/i32/i64 with a body matching the recursive block grammar: a
// block is zero or more `let <name> <width> = <expression>;` /
// `var <name> <width> = <expression>;` local declarations, plus
// `x = <expression>;` reassignments of an already-declared local (a tir.Store;
// see buildBlock) — and compound assignments and postfix increment/decrement
// (`x += y;`, `i++`, `i--`, a tir.CompoundStore; see buildCompoundStore, which
// lowers through the same checked-arithmetic runtime helpers a plain
// `x = x + y` uses) — and `while <comparison> { <loop body> }` loop statements (a
// tir.While; see buildWhile), `loop <start>..<end> : <name> { <loop body> }`
// range loop statements (a tir.RangeLoop; see buildRangeLoop), and
// `for <init>; <cond>; <update> { <loop body> }` classic for loop statements
// (a tir.For; see buildFor), followed by a
// tail that is either one
// `return <expression>;` or a two-armed `if <condition> { <block> } else {
// <block> }` whose condition is an integer comparison (<, <=, >, >=, ==, !=), a
// ==/!= equality between two bool values, a bare bool value, or a && / ||
// combination of those (see buildCondition);
// each arm is itself a block under the same grammar, so an arm may contain its
// own locals and nested if/else. The tail may also be a switch statement (see
// buildSwitch) whose subject is an integer or bool value and whose cases are
// integer or bool literals, each body ending in a return. Every expression — a local's initializer, a
// reassignment's new value, a return value, or an if/else arm's return value —
// may be a plain non-negative integer literal, a tree of checked negation and
// checked +, -, *, /, % arithmetic (see buildExpr), a reference to a local
// declared earlier in the same or an enclosing block, or a call to another
// Pebble-convention zero-parameter function whose result is the entry's own
// width (a tir.DirectCall, see buildExpr). A comparison's operands
// are additionally allowed to be int-typed integer literals (see
// buildComparisonOperand), or — for ==/!= — two bool values built under the
// bool grammar (see buildComparison). Checked operations emit
// pebble_rt_checked_*_i32 /
// pebble_rt_checked_*_i64 calls, chosen by the entry's resolved width (with int
// using the i32 helpers), so the
// language's overflow and divide-by-zero semantics survive into the emitted
// program; comparisons emit the plain C operator, which cannot overflow. The
// entry's width — int, i32, or i64, from its own result type — is resolved once here
// and threaded through every builder below; a body that mixes widths is
// rejected, never coerced. Any other shape returns a descriptive error and
// writes nothing to w; this package does not yet lower arbitrary expressions
// or statements.
//
// An integer entry may additionally call other functions. Every function
// actually reachable from the entry by a call (transitively — the reachability
// walk follows into each called function's own body) is validated and emitted
// as its own static helper function before pebble_user_main, with each called
// function's body built by the same buildBlock against a fresh locals scope
// seeded with that function's own parameters
// (see discoverReachableHelpers and buildHelperFunctions). A called function
// must be Pebble-convention, take parameters of only the entry's resolved
// width, bool, str, a tuple/struct type, or, since 10.38, a slice type whose
// element type is the entry's width or bool, and return exactly the
// entry's resolved width, str, a tuple/struct type, a slice type, an optional
// type, a pointer type, or void; a width mismatch at a call site or a parameter
// of any other
// type is a clean rejection. A void-result helper is supported since 10.33
// in exactly one position — a bare
// discarded-expression statement (`helper();`, see buildExpressionStatement)
// — while a call that is part of a
// cycle among helpers (a helper that can reach itself, directly or through
// others — the recursion boundary) is now supported: every reachable helper
// gets a C forward declaration before any definition, so recursive calls need
// no ordering. The one cycle shape still rejected is a cycle passing through
// the entry function itself (the entry is emitted under the fixed C name
// pebble_user_main, not as a pebble_fn_<symbolID> helper the forward-
// declaration pass covers).
// emitSymbols is the symbol result Emit resolves extern declarations against,
// scoped to one Emit invocation: it is set at the top of Emit and cleared by a
// deferred call when Emit returns, so a lookahead to the next Emit can never
// observe a stale table. It is package-level shared mutable state by deliberate
// tradeoff, NOT an oversight: this package assumes Emit is called
// single-threaded and non-reentrant (one Emit runs to completion before the
// next begins), so a package-level scoped slot is safe and avoids threading
// the table through the ~19-call-site buildDirectCall/externCName chain every
// builder in this file would otherwise have to carry. Emit itself guards this
// invariant: if a future caller ever does call Emit reentrantly or
// concurrently, Emit panics loudly rather than silently corrupting state. A
// future caller that needs concurrent compilation (a watch-mode compiler, a
// language server, parallel test execution) must thread this symbol table
// through the builders properly instead — that is a known, deliberate cost of
// the current design, documented here so it is not mistaken for an oversight.
var emitSymbols *symbol.Result

// emitAllocatorAdapters is the collection of file-scope C bridge functions a
// runtime Allocator record construction's callback fields need, scoped to one
// Emit invocation exactly like emitSymbols (set at the top of Emit, cleared by
// the same deferred call, guarded by the same reentrancy panic): keyed by the
// bridge's C name, deduplicated (two constructions referencing the same source
// function for the same slot share one bridge), and populated both by
// collectRuntimeAllocatorAdapters (which runs before any function body is
// emitted, so every bridge's prototype can be merged into the helper-prototype
// pass) and again — idempotently — by buildRuntimeAllocatorRecordDeclaration
// while it builds a construction's C text. Each entry carries the bridge's C
// prototype and definition, which Emit appends to the helper-prototype and
// helper-definition regions respectively. A bridge exists only for a callback
// field whose construction value is a reference to a top-level source function
// (a HoistedFunctionValue, the shape the checker produces for `alloc =
// my_alloc`); the bridge has the exact runtime callback ABI (hidden
// PebbleContext *ctx first parameter, size_t sizes) and forwards into the
// user's emitted helper, whose own C signature is necessarily different (its
// source-level ctx *void parameter is a separate C parameter after the implicit
// PebbleContext *ctx). The bridge is required because no incompatible
// function-pointer cast may appear in emitted C: the backend's cc build runs
// -Wall -Wextra -Werror, under which clang's -Wcast-function-type-mismatch
// rejects a direct cast from the user function's C type to the runtime ABI type.
var emitAllocatorAdapters map[string]runtimeAllocatorAdapter

func Emit(unit *tir.Unit, snapshot *types.Snapshot, entrySymbol symbol.SymbolID, fileSet *source.FileSet, symbols *symbol.Result, w io.Writer) error {
	if emitSymbols != nil {
		panic("backend: Emit called reentrantly/concurrently — this package's backend state is not safe for concurrent Emit calls")
	}
	if unit == nil {
		return fmt.Errorf("cannot emit C: nil typed-IR unit")
	}
	if snapshot == nil {
		return fmt.Errorf("cannot emit C: nil type snapshot")
	}
	if w == nil {
		return fmt.Errorf("cannot emit C: nil writer")
	}
	emitSymbols = symbols
	emitAllocatorAdapters = make(map[string]runtimeAllocatorAdapter)
	defer func() { emitSymbols = nil; emitAllocatorAdapters = nil }()

	decl, err := findEntryDeclaration(unit, entrySymbol)
	if err != nil {
		return err
	}
	result, err := validateEntrySignature(decl, snapshot)
	if err != nil {
		return err
	}
	block, blockID, err := findEntryBody(unit, decl)
	if err != nil {
		return err
	}
	if result == types.Void {
		if err := validateEmptyBody(unit, block); err != nil {
			return err
		}
		return emitEntryC(w, "", "", "", voidEntryUserMain, voidEntryMainBody, hasCExterns(unit))
	}
	helpers, err := discoverReachableHelpers(unit, snapshot, decl, blockID, result)
	if err != nil {
		return err
	}
	if err := collectRuntimeAllocatorAdapters(unit, snapshot, blockID, helpers); err != nil {
		return err
	}
	tupleTypes, err := collectTupleTypes(unit, snapshot, blockID, helpers)
	if err != nil {
		return err
	}
	optionalTypes, err := collectOptionalTypes(unit, snapshot, blockID, helpers)
	if err != nil {
		return err
	}
	structInfos, err := collectStructTypes(unit, snapshot, blockID, helpers, optionalTypes)
	if err != nil {
		return err
	}
	unionInfos, err := collectUnionTypes(unit, snapshot, result, blockID, helpers)
	if err != nil {
		return err
	}
	unions := make(map[types.TypeID]unionInfo, len(unionInfos))
	for _, info := range unionInfos {
		unions[info.typ] = info
	}
	enumInfos, err := collectEnumTypes(unit, snapshot, blockID, helpers, unions)
	if err != nil {
		return err
	}
	sliceInfos, err := collectSliceTypes(unit, snapshot, blockID, helpers)
	if err != nil {
		return err
	}
	for _, structInfo := range structInfos {
		for _, field := range structInfo.fields {
			if !isSlice(snapshot, field.typ) {
				continue
			}
			known := false
			for _, sliceInfo := range sliceInfos {
				known = known || sliceInfo.typ == field.typ
			}
			if !known {
				info, resolveErr := resolveSliceInfo(snapshot, field.typ)
				if resolveErr != nil {
					return resolveErr
				}
				sliceInfos = append(sliceInfos, info)
			}
		}
	}
	ordered, err := orderAggregateTypes(unit, snapshot, tupleTypes, optionalTypes, structInfos)
	if err != nil {
		return err
	}
	// Function-pointer typedefs (pebble_fnptr_<typeID>_t) are emitted BEFORE
	// the enum/aggregate typedef block, and merged with every function type
	// reachable ONLY through a struct field's own resolved type (function
	// -types slice 2: a struct field never referenced by name outside its
	// own construction still needs its field's function typedef collected,
	// mirroring how the slice-field backfill immediately above this comment
	// merges a struct field's slice type into sliceInfos) — since a struct
	// whose field type is a function type now names pebble_fnptr_<typeID>_t
	// as that field's C type (structFieldCType), C requires the function
	// typedef to be defined before the struct typedef that references it.
	// This reverses slice 1's original "function typedefs are self-contained,
	// append last" assumption, which held only because no aggregate typedef
	// could reference a function typedef yet.
	functionTypes, err := collectFunctionTypes(unit, snapshot, blockID, helpers)
	if err != nil {
		return err
	}
	functionTypesSeen := make(map[types.TypeID]bool, len(functionTypes))
	for _, id := range functionTypes {
		functionTypesSeen[id] = true
	}
	for _, structInfo := range structInfos {
		for _, field := range structInfo.fields {
			if !isFunctionType(snapshot, field.typ) || functionTypesSeen[field.typ] {
				continue
			}
			functionTypesSeen[field.typ] = true
			functionTypes = append(functionTypes, field.typ)
		}
	}
	functionTypedefs, err := buildFunctionTypedefs(snapshot, result, functionTypes)
	if err != nil {
		return err
	}
	// The enum typedef block is emitted BEFORE the aggregate typedef block:
	// since the OptionalIntegerToEnum slice an optional's value field may name
	// a plain enum typedef (pebble_enum_<typeID>_t, see optionalPayloadCType),
	// C requires the enum typedef to be defined before the optional struct
	// typedef that references it. Enum typedefs are self-contained (variant
	// constants only), so they have no forward dependencies and can safely lead
	// the block.
	enumTypedefs, err := buildEnumTypedefs(snapshot, enumInfos)
	if err != nil {
		return err
	}
	// A slice's .data field is a pointer to its element type, so a slice whose
	// element is a struct/tuple/optional names that aggregate's typedef in its
	// own typedef text. The aggregate typedefs are fully defined in the
	// aggregate block emitted AFTER the slices, so those typedef NAMES must be
	// declared (incompletely) before the slices — a C forward typedef
	// declaration — or the emitted slice typedef's `pebble_struct_<id>_t
	// *data;` field would fail cc with "unknown type name". The aggregate
	// definitions then carry the matching struct tag (see
	// sliceElementForwardDeclaredAggregates), so the forward declaration and
	// the definition complete the same C type.
	sliceElementAggregates := sliceElementForwardDeclaredAggregates(snapshot, sliceInfos)
	aggTypedefs, err := buildAggregateTypedefs(unit, snapshot, result, ordered.all, ordered.structs, sliceElementAggregates)
	if err != nil {
		return err
	}
	unionTypedefs, err := buildUnionTypedefs(unit, snapshot, result, unionInfos)
	if err != nil {
		return err
	}
	// The union typedef block is emitted BEFORE the aggregate typedef block: a
	// struct field or optional payload whose type is a tagged union names the
	// union's own typedef (pebble_union_<typeID>_t, see structFieldCType /
	// optionalPayloadCType), so C requires the union typedef to be defined
	// before the aggregate typedef that references it. Each union typedef pair
	// (the discriminant enum typedef followed by the tagged struct typedef, see
	// buildUnionTypedef) is self-contained — union payloads are restricted to
	// fixed-width integers, bool, and str, never another aggregate — so no
	// union typedef depends on an aggregate typedef, and the whole block can
	// safely lead the aggregate block (mirroring how the plain-enum block leads
	// it for enum-typed fields).
	typedefs := appendTypedefBlock(functionTypedefs, appendTypedefBlock(enumTypedefs, unionTypedefs))
	typedefs = appendTypedefBlock(typedefs, aggTypedefs)
	arrayTypes, err := collectArrayTypes(unit, snapshot, blockID, helpers)
	if err != nil {
		return err
	}
	arrayTypedefs, err := buildArrayTypedefs(unit, snapshot, result, arrayTypes)
	if err != nil {
		return err
	}
	typedefs = appendTypedefBlock(typedefs, arrayTypedefs)
	sliceTypedefs, err := buildSliceTypedefs(unit, snapshot, sliceInfos, result)
	if err != nil {
		return err
	}
	sliceForwardDecls := buildAggregateForwardDeclarations(snapshot, sliceElementAggregates)
	typedefs = appendTypedefBlock(sliceForwardDecls, appendTypedefBlock(sliceTypedefs, typedefs))
	helperPrototypes, err := buildHelperPrototypes(unit, snapshot, helpers, result)
	if err != nil {
		return err
	}
	helpersText, err := buildHelperFunctions(unit, snapshot, fileSet, helpers, result, unions)
	if err != nil {
		return err
	}
	statements, err := buildBlock(unit, snapshot, fileSet, blockID, nil, 0, result, resultInfo{kind: result}, unions)
	if err != nil {
		return err
	}
	// Every runtime Allocator callback bridge discovered by the
	// collectRuntimeAllocatorAdapters walk needs its C prototype declared before
	// the function body that references it (a helper body that constructs an
	// Allocator literal names its bridges in the construction's designated
	// initializers), and its C definition anywhere before the entry body that
	// also constructs one. Both are appended to the prototype and definition
	// regions Emit already emits, sorted by bridge name so the output is
	// deterministic despite the map's nondeterministic iteration order.
	if len(emitAllocatorAdapters) > 0 {
		names := make([]string, 0, len(emitAllocatorAdapters))
		for name := range emitAllocatorAdapters {
			names = append(names, name)
		}
		sort.Strings(names)
		adapterPrototypes := make([]string, 0, len(names))
		adapterDefinitions := make([]string, 0, len(names))
		for _, name := range names {
			adapterPrototypes = append(adapterPrototypes, emitAllocatorAdapters[name].prototype)
			adapterDefinitions = append(adapterDefinitions, emitAllocatorAdapters[name].definition)
		}
		helperPrototypes += "\n" + strings.Join(adapterPrototypes, "\n")
		helpersText += "\n" + strings.Join(adapterDefinitions, "\n")
	}
	return emitEntryC(w, typedefs, helperPrototypes, helpersText, fmt.Sprintf(integerEntryUserMain, entryReturnType(result), statements), integerEntryMainBody, hasCExterns(unit))
}

// hasCExterns reports whether the unit declares at least one C-convention
// extern function. When it does, the emitted C's preamble includes the common
// libc headers (stdlib/string/stdio/math) so a call to malloc, free, fopen, and
// friends is declared before use and survives the mandated -Wall -Wextra -Werror
// build. The headers are added wholesale whenever any C extern exists rather
// than tracked per function — precisely matching which libc header each extern
// needs is real complexity for zero real benefit right now.
func hasCExterns(unit *tir.Unit) bool {
	for _, node := range unit.Nodes() {
		if node.Kind == tir.ExternDeclaration && node.Convention == types.C {
			return true
		}
	}
	return false
}

func helperCName(decl tir.Node) string {
	if len(decl.TypeArgs) != 0 {
		return fmt.Sprintf("pebble_fn_%d_%d", decl.Symbol, decl.Function)
	}
	return fmt.Sprintf("pebble_fn_%d", decl.Symbol)
}

// externCName returns the real C name an extern function must be called by
// (malloc, free, fopen — never a pebble_fn_<symbolID> helper name). The name
// is resolved from the symbol table threaded into Emit (emitSymbols), mapping
// the extern declaration's stable symbol.SymbolID back to the authored
// identifier. A nil or missing symbol table is a clean error, never a guessed
// name: an extern call without its real C name would emit an undeclared
// identifier that fails the mandated -Werror build, so failing loudly here is
// strictly better.
func externCName(decl tir.Node) (string, error) {
	if emitSymbols == nil || emitSymbols.Symbols == nil {
		return "", fmt.Errorf("extern function symbol %d has no symbol-table lookup (Emit was called without a symbol result, so an extern call cannot be lowered to its real C name)", decl.Symbol)
	}
	s, ok := emitSymbols.Symbols.Symbol(decl.Symbol)
	if !ok {
		return "", fmt.Errorf("extern function symbol %d is not in the symbol table", decl.Symbol)
	}
	return s.Name, nil
}

// builtinFunctionCName reports the runtime C helper a compiler-owned builtin
// function call must lower to, resolving the call's symbol back to its
// BuiltinFunction identity via the symbol table threaded into Emit
// (emitSymbols, exactly like externCName). The wrapping u64 builtins lower to
// the runtime's pebble_rt_wrapping_<op>_u64 helpers, which implement plain
// modular-arithmetic wraparound in both SAFE and RELEASE modes and take no
// context. A symbol that is not a recognized builtin function reports false;
// a missing symbol table is a clean false (the caller then falls through to
// the ordinary declaration-resolution machinery, whose own error is the more
// precise one for a genuinely unresolvable callee).
func builtinFunctionCName(symbolID symbol.SymbolID) (string, bool) {
	if emitSymbols == nil || emitSymbols.Symbols == nil {
		return "", false
	}
	s, ok := emitSymbols.Symbols.Symbol(symbolID)
	if !ok || s.Error || s.Kind != symbol.SymbolBuiltinFunction {
		return "", false
	}
	switch s.BuiltinFunction {
	case symbol.BuiltinWrappingMulU64:
		return "pebble_rt_wrapping_mul_u64", true
	case symbol.BuiltinWrappingAddU64:
		return "pebble_rt_wrapping_add_u64", true
	default:
		return "", false
	}
}

// helperInfo is one reachable non-entry function discovered by
// discoverReachableHelpers: its FunctionDeclaration node (for validation and
// for the deterministic pebble_fn_<symbolID> C name) and the NodeID of its
// body Block (for buildBlock). The emission order of the returned slice is a
// post-order of the reachability walk — every callee precedes its caller — so
// a called function's C definition always precedes its use in the generated
// file (the one place that still matters: a non-recursive chain has no
// forward dependency anyway, and recursive calls are covered by the
// prototypes buildHelperPrototypes emits before every definition).
type helperInfo struct {
	decl  tir.Node
	block tir.NodeID
}

// reachabilityWalk carries the mutable state of the recursive reachability
// discovery in discoverReachableHelpers: the functions already fully walked
// (done), the functions on the current DFS path (stack — a callee found on
// the path is a cycle), and the post-order emission list (order).
type reachabilityWalk struct {
	unit     *tir.Unit
	snapshot *types.Snapshot
	width    types.BuiltinKind
	entry    symbol.SymbolID
	done     map[tir.FunctionID]bool
	stack    []tir.FunctionID
	order    []helperInfo
}

// visit walks one function's body for DirectCall nodes — and, since the
// function-values slice, HoistedFunctionValue nodes (a bare top-level function
// reference used as a value, resolved through the same findFunctionDeclaration
// machinery a called function uses) — recursing into every discovered callee's
// own body. The entry is the root of the walk; the entry
// itself is never added to the emission order (its C definition,
// pebble_user_main, is emitted separately after the helpers). A callee
// already fully walked (done) is a shared subgraph — a diamond, where two
// callers reach one callee — and is skipped, not re-walked, so each helper is
// emitted exactly once. A callee already on the current DFS path (stack) is a
// cycle — direct or mutual recursion — and is skipped the same way: it is
// already being walked on this path and will be marked done when its own walk
// completes, so the full reachable set is still discovered without infinite
// re-walking, and the recursive call is legal C because every helper has a
// forward-declared prototype. The one exception is a cycle passing THROUGH
// the entry function (a helper calling the entry back, or the entry calling
// itself): the entry is emitted under the fixed C name pebble_user_main, not
// as a pebble_fn_<symbolID> helper the forward-declaration pass covers, so a
// call to it has no valid C name and that cycle shape is rejected cleanly.
func (w *reachabilityWalk) visit(decl tir.Node, blockID tir.NodeID) error {
	if w.done[decl.Function] {
		return nil
	}
	if inStack := indexOfFunction(w.stack, decl.Function); inStack >= 0 {
		if decl.Symbol == w.entry {
			// The call edge just followed closes a cycle THROUGH THE ENTRY
			// FUNCTION (the DFS root, always on the stack): a helper calling
			// the entry back, or the entry calling itself. This one cycle
			// shape is still rejected: the entry is emitted under the fixed C
			// name pebble_user_main (after the helpers, with no prototype), so
			// a call to it cannot be lowered to a valid C name — it is not a
			// pebble_fn_<symbolID> helper the forward-declaration pass covers.
			cycle := append(append([]tir.FunctionID(nil), w.stack[inStack:]...), decl.Function)
			parts := make([]string, len(cycle))
			for i, id := range cycle {
				parts[i] = fmt.Sprintf("function %d", id)
			}
			return fmt.Errorf("recursive call through the entry function is not supported: the call chain %s is a cycle passing through the entry, which is emitted under the fixed C name pebble_user_main (not as a pebble_fn_<symbolID> helper the forward-declaration pass covers), so this backend cannot lower a call to it; recursion among helper functions (direct or mutual) is supported via forward declarations", strings.Join(parts, " -> "))
		}
		// The function is already on the current DFS path, so the call edge
		// just followed closes a cycle: decl can reach itself through
		// stack[inStack:] -> decl. This is the recursive-call shape
		// (direct or mutual) this backend now supports — since every
		// reachable helper gets a C forward declaration (prototype) before
		// any definition, a recursive call no longer needs any ordering
		// guarantee. The callee is skipped rather than re-walked: it is
		// already on this path, its own visit will mark it done when it
		// completes, and the walk has already discovered (and will emit)
		// every function it reaches. The full reachable set is therefore
		// still discovered — a cycle is a "don't re-walk" signal, not an
		// error.
		return nil
	}
	w.stack = append(w.stack, decl.Function)
	var calls []tir.Node
	if err := collectDirectCalls(w.unit, blockID, &calls); err != nil {
		return err
	}
	for _, call := range calls {
		// A call to a compiler-owned builtin function (wrapping_mul_u64 /
		// wrapping_add_u64) has no declaration in the unit to walk and no
		// pebble_fn_<symbolID> helper to emit: its call site lowers directly to
		// the runtime's pebble_rt_wrapping_<op>_u64 helper, which the runtime
		// library provides, so the callee is skipped exactly like an extern
		// declaration is skipped below — no body walk, no order entry.
		if _, builtin := builtinFunctionCName(call.Symbol); builtin {
			continue
		}
		var calleeDecl tir.Node
		var err error
		if len(call.TypeArgs) != 0 {
			calleeDecl, err = findCalledFunctionDeclaration(w.unit, call.Symbol, call.TypeArgs)
		} else {
			calleeDecl, err = findFunctionDeclaration(w.unit, call.Symbol, "called function")
			if err != nil {
				calleeDecl, err = findCalledFunctionByResult(w.unit, call.Symbol, call.Type)
			}
		}
		if err != nil {
			if len(call.TypeArgs) != 0 {
				return fmt.Errorf("called function symbol %d is a generic call with %d type argument(s), which this backend cannot lower without a built specialization", call.Symbol, len(call.TypeArgs))
			}
			return err
		}
		// An extern callee (a call to a C-convention extern fn declaration) is
		// not emitted as a pebble_fn_<symbolID> helper: it has no body
		// (HasBody is false) and no Pebble-style C definition to emit — the
		// libc header the preamble adds declares it, and the call site lowers
		// to the function's real C name. It is validated (C convention, and
		// every parameter and the result typed by a C spelling this backend
		// can emit) and then skipped: no body walk, no order entry, so no
		// helper prototype or definition is ever generated for it.
		if calleeDecl.Kind == tir.ExternDeclaration {
			if err := validateExternSignature(w.unit, calleeDecl, w.snapshot); err != nil {
				return err
			}
			continue
		}
		if err := validateHelperSignature(w.unit, calleeDecl, w.snapshot, w.width); err != nil {
			return err
		}
		_, calleeBlock, err := findFunctionBody(w.unit, calleeDecl, "called function")
		if err != nil {
			return err
		}
		if err := w.visit(calleeDecl, calleeBlock); err != nil {
			return err
		}
	}
	w.stack = w.stack[:len(w.stack)-1]
	w.done[decl.Function] = true
	if decl.Symbol != w.entry {
		w.order = append(w.order, helperInfo{decl: decl, block: blockID})
	}
	return nil
}

// sliceInfo is one distinct slice type the emitted program references,
// carrying the slice's own types.TypeID (the basis of the C typedef name
// pebble_slice_<typeID>_t) and its resolved element types.TypeID.
type sliceInfo struct {
	typ         types.TypeID
	elementType types.TypeID
}

// structFieldInfo is one field of a distinct struct type the emitted program
// references, resolved from the struct's own declaration: its member symbol
// (the field's stable symbol.SymbolID, the basis of the C field name
// pebble_field_<member>) and the field's resolved types.TypeID.
type structFieldInfo struct {
	member symbol.SymbolID
	typ    types.TypeID
}

// structInfo is one distinct struct type the emitted program references,
// carrying everything buildStructTypedef needs: the struct's own types.TypeID
// (the basis of the C typedef name pebble_struct_<typeID>_t), its declaration
// symbol, and its fields in the struct's *declared* order (from the struct's
// own TypeDecl.Members — a RecordConstruct's Fields may list the fields in
// any construction-site order, confirmed against a real fixture, so the
// declared order is resolved here once rather than at emit time).
type structInfo struct {
	typ    types.TypeID
	decl   symbol.SymbolID
	fields []structFieldInfo
}

type aggregateTypeOrder struct {
	all       []types.TypeID
	tuples    []types.TypeID
	optionals []types.TypeID
	structs   []structInfo
}

// enumInfo is one distinct plain enum type the emitted program references,
// carrying everything buildEnumTypedef needs: the enum's own types.TypeID (the
// basis of the C typedef name pebble_enum_<typeID>_t), its declaration symbol,
// and its variants in the enum's *declared* order (from the TypeDecl's
// Members list, resolved here once). The declared order is the natural, stable
// discriminant ordinal — variant Members[i] gets the C enum value i, so a
// switch's CaseValue labels and the value stored in an enum-typed local agree
// with the emitted typedef by construction.
type enumInfo struct {
	typ      types.TypeID
	decl     symbol.SymbolID
	variants []symbol.SymbolID
}

// unionMemberInfo is one variant of a distinct tagged-union type whose payload
// type is known from a construction site in the reachable program: its member
// symbol (the basis of the C union member name pebble_field_<member>) and the
// variant's resolved payload types.TypeID.
type unionMemberInfo struct {
	member      symbol.SymbolID
	payloadType types.TypeID
}

// unionInfo is one distinct tagged-union type the emitted program references,
// carrying everything buildUnionTypedef needs: the union's own types.TypeID
// (the basis of the C typedef name pebble_union_<typeID>_t), its declaration
// symbol, its variants in declared order (from the TypeDecl's Members list —
// the same discriminant-ordinal scheme a plain enum uses, so the tag enum
// typedef and the switch case labels agree by construction), and the union
// members whose payload types are known from construction sites (one union
// member per non-void variant actually constructed somewhere in the reachable
// program — a variant with no construction site never needs a union member,
// since no payload is ever read or written for it).
type unionInfo struct {
	typ      types.TypeID
	decl     symbol.SymbolID
	variants []symbol.SymbolID
	members  []unionMemberInfo
}

// printfSpecifier returns the <inttypes.h> PRI* macro name whose
// compile-time string expansion, string-concatenated into a printf format
// string as "%" <macro>, prints an argument of the given integer builtin's C
// type (cType). Using the fixed-width macros — PRId8/PRId16/PRId32/PRId64 for
// the signed types, PRIu8/PRIu16/PRIu32/PRIu64 for the unsigned — is the
// portable, warning-clean way to match printf specifiers to the exact-width
// C types this backend emits: the standard technically requires a variadic
// format specifier to match the promoted argument type, and a hand-picked
// %hhd/%hd would only happen to match int8_t/int16_t on common platforms.
// Int and Uint follow their cType mapping (int32_t and uint64_t
// respectively). Any non-integer kind returns "", matching cType's own
// ""-means-not-an-integer contract.
func printfSpecifier(width types.BuiltinKind) string {
	switch width {
	case types.Int:
		return "PRId32"
	case types.I8:
		return "PRId8"
	case types.I16:
		return "PRId16"
	case types.I32:
		return "PRId32"
	case types.I64:
		return "PRId64"
	case types.Uint:
		return "PRIu64"
	case types.U8:
		return "PRIu8"
	case types.U16:
		return "PRIu16"
	case types.U32:
		return "PRIu32"
	case types.U64:
		return "PRIu64"
	}
	return ""
}

// localInfo records what a declared local holds: an ordinary scalar — the
// entry's resolved integer width or bool, in kind — a str value, in isStr, a
// char value, in isChar, a
// tuple, in tuple (its
// tuple types.TypeID, stable within one Emit call), an array, in array, an
// optional, in optional, a struct, in structType, or a plain enum, in enumType.
// The fields are
// mutually exclusive: kind is zero
// for a compound or char local (a tuple/array/optional/struct/enum is not a
// types.BuiltinKind), isStr is true only for a str local (a str is a
// types.BuiltinKind but has no width or bool grammar this backend builds —
// it is initialized from a string literal or a call to a str-returning
// helper, reassigned from a string literal, and otherwise compared, passed,
// and returned via the str-value builders), isChar is true only for a char
// local (a char is a types.BuiltinKind but has a fixed int32_t representation
// this backend builds independently of the entry's width — it is initialized
// from a char literal, a char-typed local reference, or a call to a
// char-returning helper, reassigned the same ways, and otherwise compared,
// passed, and returned via the char-value builders), and tuple/array/optional/structType/enumType are
// zero for a
// scalar local. A struct value
// rather than a parallel map keeps the scope a single map threaded through
// every builder unchanged in shape — the existing
// `map[symbol.SymbolID]types.BuiltinKind` value type was widened to this struct
// so no call site needed a second argument, the option that changes the fewest
// existing call sites correctly.
type localInfo struct {
	kind         types.BuiltinKind
	isStr        bool
	isChar       bool
	tuple        types.TypeID
	array        types.TypeID
	optional     types.TypeID
	structType   types.TypeID
	enumType     types.TypeID
	arrayWrapped bool
	sliceType    types.TypeID
	pointerType  types.TypeID
	runtimeType  types.TypeID
	functionType types.TypeID
}

// runtimeAllocatorAdapter is one file-scope C bridge function generated for a
// runtime Allocator callback field whose construction value is a reference to a
// top-level source function (a HoistedFunctionValue, the shape the checker
// produces for `alloc = my_alloc`). The bridge has the exact runtime callback
// ABI — the hidden PebbleContext *ctx first parameter and the runtime's size_t
// sizes, i.e. PebbleAllocFn / PebbleReallocFn / PebbleFreeFn — and forwards
// into the user's emitted helper, whose own C signature is necessarily
// different: the user's source-level `ctx *void` parameter is a separate C
// parameter after the implicit PebbleContext *ctx, and the source-level `size
// uint` parameter is uint64_t, not size_t. The runtime ABI's ctx arrives at the
// bridge holding whatever the allocator call site passed as the first runtime
// argument (the allocator's state, per the existing allocator call lowering),
// and the bridge forwards it unchanged into the user's ctx parameter, cast to
// the user's void * — the "hidden context handling" the runtime callback ABI
// demands. A bridge must be a real file-scope function rather than a cast from
// the user function's C type to the runtime ABI type because the backend's cc
// build runs -Wall -Wextra -Werror, under which clang's
// -Wcast-function-type-mismatch rejects an incompatible function-pointer cast.
type runtimeAllocatorAdapter struct {
	name       string
	prototype  string
	definition string
}

// resultInfo records what the enclosing function's tail return must produce:
// an ordinary scalar — the entry's resolved integer width, in kind — a str
// value, in isStr, a char value, in isChar, a tuple, in tuple (its types.TypeID), a struct, in
// structType, a plain enum, in enumType, a tagged union, in unionType, a slice, in sliceType, an optional, in optionalType, or a
// function value, in functionType (its types.TypeID). The fields are
// mutually exclusive, mirroring localInfo: kind is zero for a compound or str
// or char result (a tuple/struct is not a types.BuiltinKind), isStr is true only for a
// str result, isChar is true only for a char result (whose C return type is
// the fixed int32_t, independent of the entry's width), and tuple/structType/enumType/unionType/sliceType/optionalType/functionType are zero
// for a scalar result. It is threaded alongside width through buildBlock and
// buildIf so a tuple/struct-returning helper's tail-position Return builds its
// value via buildAggregateReturnValue (a SymbolValue naming a matching
// aggregate-typed local, or a fresh TupleValue/RecordConstruct), a
// plain-enum-returning helper's tail-position Return builds its value via
// buildEnumValue (a variant literal, a reference to an enum-typed local, an
// integer-to-enum cast, or an enum-typed struct field read), a
// tagged-union-returning helper's tail-position Return builds its value via
// buildUnionValueExpr (a reference to a union-typed local, a variant
// construction, a union-typed struct field read, or a union-payload optional
// force-unwrap), a
// slice-returning helper's tail-position Return builds its value via
// buildSliceReturnValue (a SymbolValue naming a matching slice-typed local, or
// a fresh CheckedSlice construction emitted as the two-statement temp-then-
// construction shape), an optional-returning helper's tail-position Return
// builds its value via
// buildOptionalReturnValue (a SymbolValue naming a matching optional-typed
// local, a fresh SomeOptional/NoneOptional/OptionalInject construction, a call
// to another optional-returning helper, or a bare payload whose implicit
// injection is supplied there), a
// function-returning helper's tail-position Return builds its value via
// buildFunctionValue (a bare function reference, a function-typed local or
// parameter forward, a function-typed struct field forward, or a call to
// another function-returning helper), and a
// str-returning helper's tail-position Return builds its value via
// buildStrOperand (a SymbolValue naming a str local, a string literal, or a
// call to a str-returning helper) instead of
// buildExpr, which would reject an aggregate-, slice-, optional-, function-typed, or str-typed value. The entry's own body
// always threads resultInfo{kind: width} (a scalar, unchanged behavior), since
// the entry's C return type stays the integer entryReturnType regardless of
// what a helper may return.
type resultInfo struct {
	kind         types.BuiltinKind
	isStr        bool
	isChar       bool
	tuple        types.TypeID
	structType   types.TypeID
	enumType     types.TypeID
	unionType    types.TypeID
	sliceType    types.TypeID
	pointerType  types.TypeID
	optionalType types.TypeID
	functionType types.TypeID
	arrayType    types.TypeID
}

// cloneLocals returns a fresh copy of the given set of in-scope locals. Every
// recursive scope entry in buildBlock copies before extending, so a block's
// own declarations never leak into the map the caller or a sibling scope
// sees — a local declared inside one if arm is invisible to the sibling arm
// and to anything outside the arm.
func cloneLocals(locals map[symbol.SymbolID]localInfo) map[symbol.SymbolID]localInfo {
	cloned := make(map[symbol.SymbolID]localInfo, len(locals))
	for id, kind := range locals {
		cloned[id] = kind
	}
	return cloned
}

func mustNode(unit *tir.Unit, id tir.NodeID) tir.Node { n, _ := unit.Node(id); return n }

// buildSourceLoc resolves one typed-IR node's Span to the C text of the
// PebbleSourceLoc compound-literal argument every pebble_rt_checked_* call
// takes as its final argument: (PebbleSourceLoc){"<escaped file path>", <line>,
// <column>}. The file path is the source File's display path (File.Path),
// escaped with the same escapeCString scheme every other embedded string in
// this file uses, and the line/column are the File.Position resolution of the
// Span's Start byte offset (both one-based). When the location is genuinely
// unavailable — a nil fileSet, or a Span whose Source ID does not resolve to a
// file in the set (a synthetic/hand-built node with no authored span) — it
// falls back to the zero-valued (PebbleSourceLoc){0} rather than erroring: a
// missing location is not a compile failure, and the runtime's own panic
// report omits location entirely for a zero struct (pebble_rt.h documents line
// and column as 0 when genuinely unavailable).
func buildSourceLoc(fileSet *source.FileSet, span source.Span) string {
	if fileSet == nil {
		return "(PebbleSourceLoc){0}"
	}
	file, ok := fileSet.File(span.Source)
	if !ok {
		return "(PebbleSourceLoc){0}"
	}
	pos := file.Position(span.Start)
	return fmt.Sprintf("(PebbleSourceLoc){\"%s\", %d, %d}", escapeCString(file.Path()), pos.Line, pos.Column)
}

// escapeCString re-escapes a string literal's already-decoded byte content
// into the body of a C string literal, producing a C literal that is
// byte-for-byte the original decoded content. The decoded bytes are not
// assumed simple: a literal may contain a control character (\n, \t, \0,
// or any \xHH byte escape the lexer accepts), a quote, a backslash, or non-
// ASCII UTF-8. A double-quote and a backslash are escaped as the complete C
// escapes \" and \\ (complete escapes cannot absorb a following character).
// Every byte outside printable ASCII (0x20-0x7E) — control characters, NUL,
// and all non-ASCII bytes — is emitted as a fixed-width octal escape \NNN
// zero-padded to exactly three digits (e.g. \012 for newline, \007 for the
// bell byte). Fixed-width octal is the safe choice specifically because C's
// octal escape consumes at most three octal digits, so a \NNN escape can
// never accidentally absorb a following digit character the way C's \xHH
// hex escape can (\x09A is one out-of-range or wrong escape, whereas
// \011A is the byte 0x09 followed by 'A'). Everything in printable ASCII
// other than the two escaped characters is emitted verbatim. The result is a
// valid C string-literal body (never containing a raw double-quote or
// backslash), so the caller embeds it between two double-quote characters.
func escapeCString(text string) string {
	var b strings.Builder
	for i := 0; i < len(text); i++ {
		c := text[i]
		switch c {
		case '"':
			b.WriteString(`\"`)
		case '\\':
			b.WriteString(`\\`)
		default:
			if c >= 0x20 && c <= 0x7e {
				b.WriteByte(c)
			} else {
				fmt.Fprintf(&b, `\%03o`, c)
			}
		}
	}
	return b.String()
}

// withLeadingPre turns a statement body and an optional indent-free
// pre-statement into the full statement text: when pre is non-empty the pre is
// emitted as a preceding line at the same indent as the statement, otherwise
// the body is returned unchanged. It is the mechanical threading shape the
// leading-statement call sites use for an inline slice-construction call
// argument's temp declaration (the same shape buildLeadingStatement uses for
// buildScalarInitializeCore's pre), shared by every local-declaration
// initializer that calls buildDirectCallWithPre so the pre check lives in one
// place. body must carry its own leading indent; pre must not.
func withLeadingPre(pre, indent, body string) string {
	if pre != "" {
		return indent + pre + "\n" + body
	}
	return body
}

// checkedIndexBaseIsStr reports whether a bare CheckedIndex's base is a str
// value — the str-indexing case buildCharOperand's CheckedIndex case handles,
// whose element read is the stateless UTF-8 decoder callable on the base
// directly — versus a slice-typed value, the case buildSliceIndexValue handles
// and which needs a base materialized into a temp. The base is unwrapped past
// any SourceAlias (grouped-expression parens) transparently, exactly as
// buildPrint unwraps a print operand; the unwrapped base carries the same Type
// the SourceAlias did, so the check is exactly what the checker validated.
func checkedIndexBaseIsStr(unit *tir.Unit, snapshot *types.Snapshot, node tir.Node) (bool, error) {
	if len(node.Children) < 1 {
		return false, fmt.Errorf("entry function body expression contains a CheckedIndex with %d child(ren), want at least one (the value being indexed)", len(node.Children))
	}
	base, ok := unit.Node(node.Children[0])
	if !ok {
		return false, fmt.Errorf("entry function body expression contains a CheckedIndex referencing invalid base node %d", node.Children[0])
	}
	for base.Kind == tir.SourceAlias {
		if len(base.Children) != 1 {
			return false, fmt.Errorf("entry function body expression contains a CheckedIndex whose base SourceAlias has %d child(ren), want exactly one", len(base.Children))
		}
		base, ok = unit.Node(base.Children[0])
		if !ok {
			return false, fmt.Errorf("entry function body expression contains a CheckedIndex referencing invalid base node %d", base.Children[0])
		}
	}
	return isStr(snapshot, base.Type), nil
}

// wantName returns the human-readable name of the entry's resolved integer
// width ("i32" or "i64") for error messages that name the wanted type.
func wantName(width types.BuiltinKind) string {
	name, _ := builtinName(width)
	return name
}

// externCType returns the C spelling an extern declaration's parameter or
// result of the given type is declared with, so the emitted call site agrees
// with the libc header's own declaration. It accepts exactly the shapes an
// extern call site can build and consume: a fixed-width integer builtin (each
// resolved to its own C type, uint/u64 to uint64_t), bool, char (int32_t, the
// same convention a char value/local uses), str (const char *, the C spelling
// a real libc string parameter/result is declared with), f32/f64, a
// pointer to a supported pointee (via pointerTypeName), or void (result only).
// Any other type — a tuple, struct, slice, optional, function type, a bare
// opaque extern type (which has no known size/layout, so only its pointer
// form is spellable), or an
// opaque struct pointer whose pointee this backend cannot spell — is a clean
// rejection naming what was found, never a guessed C type.
func externCType(snapshot *types.Snapshot, id types.TypeID) (string, error) {
	if builtin, ok := resolvedBuiltin(snapshot, id); ok {
		if c := cType(builtin); c != "" {
			return c, nil
		}
		if c := floatCType(builtin); c != "" {
			return c, nil
		}
		switch builtin {
		case types.Void:
			return "void", nil
		case types.Bool:
			return "bool", nil
		case types.Char:
			return "int32_t", nil
		}
	}
	if isStr(snapshot, id) {
		return "const char *", nil
	}
	if isPointer(snapshot, id) {
		if pointee, ok := pointerPointeeType(snapshot, id); ok {
			if name := pointerTypeName(snapshot, pointee); name != "" {
				return name, nil
			}
		}
	}
	return "", fmt.Errorf("has type %s, which this backend cannot spell as a C extern parameter or result (want a fixed-width integer, uint, u64, bool, char, str, f32/f64, a pointer to a supported type, or void)", describeType(snapshot, id))
}

// integerLiteralText returns the C spelling of a decimal integer literal
// destined for a value position of the given builtin width: the plain decimal
// text, plus a "u" suffix when that width is an unsigned integer builtin
// (Uint, U8, U16, U32, or U64), so a large literal — e.g. the decimal form of
// UINT64_MAX — is parsed by the C compiler as an unsigned constant instead of
// triggering -Wimplicitly-unsigned-literal under the mandated -Wall -Wextra
// -Werror build. A plain "u" is sufficient for every unsigned width this
// backend emits: a suffixed decimal constant is promoted through unsigned int,
// unsigned long, and unsigned long long until one can represent it, so any
// value that fits in unsigned long long — every value Pebble can express at
// those widths, including UINT64_MAX for Uint/U64's uint64_t — is typed
// exactly. A literal destined for a signed width is returned unchanged.
func integerLiteralText(text string, width types.BuiltinKind) string {
	switch width {
	case types.Uint, types.U8, types.U16, types.U32, types.U64:
		return text + "u"
	}
	return text
}

// isNonNegativeDecimal reports whether s is a non-empty run of ASCII decimal
// digits with no sign prefix. IntegerNum is expected to be exactly this by
// construction; the check is defensive against malformed payloads.
func isNonNegativeDecimal(s string) bool {
	if s == "" {
		return false
	}
	for i := 0; i < len(s); i++ {
		if s[i] < '0' || s[i] > '9' {
			return false
		}
	}
	return true
}

// isValidFloatLiteralText reports whether s is a well-formed non-negative
// decimal floating-literal text of the shapes Pebble's lexer can produce:
// an integer part of one or more digits, optionally followed by a fractional
// part (a '.' plus one or more digits) and/or an exponent (a 'e'/'E' with an
// optional sign and one or more digits). Float is expected to be exactly one
// of these by construction — the checker's decodeFloatLiteral strips the
// '_' separators the lexer allowed and the infer layer has already parsed and
// bounded the literal — so the check is defensive against malformed payloads,
// exactly mirroring how buildExpr's IntegerLiteral case validates its own
// literal text with isNonNegativeDecimal before trusting it. Every accepted
// text is also a valid C floating constant verbatim (a decimal point and/or
// exponent are always present, and C accepts the same digit/dot/sign/exp
// spellings), so the text can be spliced into the emitted C unchanged.
func isValidFloatLiteralText(s string) bool {
	i := 0
	for i < len(s) && s[i] >= '0' && s[i] <= '9' {
		i++
	}
	// Integer part must be non-empty: the lexer starts a number with a digit.
	if i == 0 {
		return false
	}
	digits := func() bool {
		start := i
		for i < len(s) && s[i] >= '0' && s[i] <= '9' {
			i++
		}
		return i > start
	}
	if i < len(s) && s[i] == '.' {
		i++
		if !digits() {
			return false
		}
	}
	if i >= len(s) {
		return true
	}
	if s[i] != 'e' && s[i] != 'E' {
		return false
	}
	i++
	if i < len(s) && (s[i] == '+' || s[i] == '-') {
		i++
	}
	return digits() && i == len(s)
}

// The supported entry shapes share one adapter skeleton: the pebble_rt.h
// include, the Pebble-convention pebble_user_main taking the context, and a
// hosted C main that builds a default context and drives it. Only the
// pebble_user_main definition and the hosted main's final call differ between
// the void and i32 shapes, so the skeleton is written once and the two
// fragments below are the only shape-specific text.
const voidEntryUserMain = `static void pebble_user_main(PebbleContext *ctx) {
    (void)ctx;
}`

const voidEntryMainBody = `pebble_user_main(&ctx);
    return 0;`

// integerEntryUserMain is a format string; the first %s is the pebble_user_main
// return type for the entry's resolved width (entryReturnType) and the second
// %s is the statement sequence for
// pebble_user_main's body — the top-level block built by buildBlock: zero or
// more `<width> pebble_local_<id> = <built init expression>;` declarations and
// zero or more `pebble_local_<id> = <built value>;` reassignments, in
// declaration order, then the block's tail, which is either a
// `return <built return expression>;` or a two-armed if/else (whose arms may
// nest further blocks). The tail's value becomes pebble_user_main's return
// value and, through the hosted main's own return, the process exit code. With
// no locals the sequence is exactly the single return statement, so the
// zero-locals shape emits byte-identically to before for an i32 entry (whose
// return type is the legacy "int").
const integerEntryUserMain = `static %s pebble_user_main(PebbleContext *ctx) {
    (void)ctx;
%s
}`

const integerEntryMainBody = `return pebble_user_main(&ctx);`

// entryReturnType is the C return type pebble_user_main is declared with for
// a supported scalar entry result. An i32 entry keeps the legacy "int"
// spelling —
// byte-identical to the pre-i64 shape, and C int is the 32-bit type that entry
// already relied on. An i64 entry must be the exact-width int64_t, not int, so
// a 64-bit return value is not truncated to 32 bits before the hosted main
// narrows it to the process exit code. (The hosted int main's own
// return pebble_user_main(&ctx); then narrows int64_t to int — the POSIX exit
// code is only the low byte of what main returns — which a -Wall -Wextra
// -Werror build without -Wconversion does not warn about; verified by building
// an i64-entry program.) An f32/f64 entry (Float Stage A) is declared
// "float"/"double", and the hosted main's return pebble_user_main(&ctx);
// narrows that to int the same implicit way C defines float-to-integer
// conversion for in-range values (3.14 narrows to exit code 3) — verified to
// build warning-free under the same flags, so no explicit cast is needed at
// the call site.
func entryReturnType(width types.BuiltinKind) string {
	switch width {
	case types.I64:
		return "int64_t"
	case types.F32:
		return "float"
	case types.F64:
		return "double"
	}
	return "int"
}

// emitEntryC writes the shared adapter skeleton once the typed IR has been
// confirmed to describe one of the supported program shapes. typedefs is the C
// text of one struct typedef per distinct tuple/optional/struct type the
// program references, written before every function definition (helpers and
// pebble_user_main) since
// C requires a type to be defined before any function's body can use it; it is
// empty when the program has no tuples, optionals, or structs. prototypes is
// the C text of one forward declaration (C prototype) per reachable helper,
// written before the helper definitions so a recursive or mutually-recursive
// call anywhere in the file always has a preceding declaration in scope; it is
// empty when the program has no helpers. helpers is the
// C text of every reachable helper function (each a static
// pebble_fn_<symbolID> definition), written after the prototypes and before
// pebble_user_main; it is empty when the program
// has no helpers, in which case the emitted text is byte-identical to the
// pre-10.17 skeleton. <stdbool.h> is included unconditionally: it provides
// the C bool keyword and the true / false literals the moment any bool local
// or literal is emitted, and adding it for programs with no bool at all is
// harmless. <stdio.h> and <inttypes.h> are likewise included unconditionally:
// a print statement emits a printf call whose format string uses the
// <inttypes.h> PRI* macros for its fixed-width integer specifiers, so both
// headers are needed the moment any print is emitted, and adding them for
// programs with no print at all is harmless.
func emitEntryC(w io.Writer, typedefs, prototypes, helpers, userMain, mainBody string, hasExterns bool) error {
	if _, err := fmt.Fprint(w, `#include "pebble_rt.h"
#include <stdbool.h>
#include <stdio.h>
#include <inttypes.h>
`); err != nil {
		return err
	}
	if hasExterns {
		// A unit that calls any C-convention extern fn declares the common
		// libc headers so the real C names (malloc, free, fopen, ...) are
		// declared before use. None of these redeclare anything pebble_rt.h
		// already defines (that header only pulls in stdbool/stddef/stdint),
		// so there is no alias/redeclaration conflict to resolve.
		if _, err := fmt.Fprint(w, `#include <stdlib.h>
#include <string.h>
#include <math.h>
`); err != nil {
			return err
		}
	}
	if typedefs != "" {
		if _, err := fmt.Fprint(w, "\n"+typedefs+"\n"); err != nil {
			return err
		}
	}
	if prototypes != "" {
		if _, err := fmt.Fprint(w, "\n"+prototypes+"\n"); err != nil {
			return err
		}
	}
	if helpers != "" {
		if _, err := fmt.Fprint(w, "\n"+helpers+"\n"); err != nil {
			return err
		}
	}
	_, err := fmt.Fprintf(w, `
%s

int main(int argc, const char **argv) {
    (void)argc;
    (void)argv;
    PebbleContext ctx = pebble_rt_default_context();
    %s
}
`, userMain, mainBody)
	return err
}

func callingConventionName(c types.CallingConvention) string {
	switch c {
	case types.Pebble:
		return "Pebble"
	case types.C:
		return "C"
	default:
		return fmt.Sprintf("calling-convention(%d)", uint8(c))
	}
}

func builtinName(builtin types.BuiltinKind) (string, bool) {
	switch builtin {
	case types.Bool:
		return "bool", true
	case types.Char:
		return "char", true
	case types.Str:
		return "str", true
	case types.Void:
		return "void", true
	case types.Int:
		return "int", true
	case types.Uint:
		return "uint", true
	case types.I8:
		return "i8", true
	case types.I16:
		return "i16", true
	case types.I32:
		return "i32", true
	case types.I64:
		return "i64", true
	case types.U8:
		return "u8", true
	case types.U16:
		return "u16", true
	case types.U32:
		return "u32", true
	case types.U64:
		return "u64", true
	case types.F32:
		return "f32", true
	case types.F64:
		return "f64", true
	}
	return "", false
}
