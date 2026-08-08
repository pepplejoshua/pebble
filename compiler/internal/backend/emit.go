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
// tail. A `loop start..end : name { <body> }` range loop (or `..=`, the
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
// str fields/elements inside a tuple, array, optional, or struct, and
// concatenation and interpolation (InterpolatedString).
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
	"io"
	"sort"
	"strconv"
	"strings"

	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
	"github.com/pepplejoshua/pebble/compiler/internal/tir"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
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
	defer func() { emitSymbols = nil }()

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
	// The enum typedef block is emitted BEFORE the aggregate typedef block
	// (and, transitively, before the unions): since the OptionalIntegerToEnum
	// slice an optional's value field may name a plain enum typedef
	// (pebble_enum_<typeID>_t, see optionalPayloadCType), C requires the enum
	// typedef to be defined before the optional struct typedef that references
	// it. Enum typedefs are self-contained (variant constants only), so they
	// have no forward dependencies and can safely lead the block.
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
	typedefs := appendTypedefBlock(functionTypedefs, appendTypedefBlock(enumTypedefs, aggTypedefs))
	unionTypedefs, err := buildUnionTypedefs(unit, snapshot, result, unionInfos)
	if err != nil {
		return err
	}
	typedefs = appendTypedefBlock(typedefs, unionTypedefs)
	arrayTypes := collectHelperArrayTypes(snapshot, helpers)
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

// appendTypedefBlock appends a second typedef block onto a first, joining them
// with a blank line when both are non-empty. Either may be empty; the result is
// the non-empty one when only one is non-empty, and empty when both are.
func appendTypedefBlock(first, second string) string {
	if first == "" {
		return second
	}
	if second == "" {
		return first
	}
	return first + "\n" + second
}

// findEntryDeclaration locates the FunctionDeclaration node for entrySymbol.
// A specialization would carry non-empty TypeArgs; the entry cannot be
// generic, so those are deliberately excluded rather than assumed absent.
func findEntryDeclaration(unit *tir.Unit, entrySymbol symbol.SymbolID) (tir.Node, error) {
	return findFunctionDeclaration(unit, entrySymbol, "entry function")
}

// findFunctionDeclaration locates the non-generic FunctionDeclaration node
// for the given function symbol, generalizing findEntryDeclaration to any
// function the reachability walk resolves. Every typed-IR function this
// backend emits — the entry and every called helper — has exactly one such
// declaration; a generic instance would carry non-empty TypeArgs and is
// excluded, since generic calls are not lowered here.
func findFunctionDeclaration(unit *tir.Unit, symbolID symbol.SymbolID, what string) (tir.Node, error) {
	for _, node := range unit.Nodes() {
		if (node.Kind != tir.FunctionDeclaration && node.Kind != tir.ExternDeclaration) || node.Symbol != symbolID || len(node.TypeArgs) != 0 {
			continue
		}
		return node, nil
	}
	return tir.Node{}, fmt.Errorf("%s not found in unit: no non-generic FunctionDeclaration or ExternDeclaration for symbol %d", what, symbolID)
}

func findCalledFunctionDeclaration(unit *tir.Unit, symbolID symbol.SymbolID, typeArgs []types.TypeID) (tir.Node, error) {
	for _, node := range unit.Nodes() {
		if node.Kind != tir.FunctionDeclaration || node.Symbol != symbolID || len(node.TypeArgs) != len(typeArgs) {
			continue
		}
		match := true
		for i := range typeArgs {
			match = match && node.TypeArgs[i] == typeArgs[i]
		}
		if match {
			return node, nil
		}
	}
	return tir.Node{}, fmt.Errorf("called function symbol %d specialization not found", symbolID)
}

func findCalledFunctionByResult(unit *tir.Unit, symbolID symbol.SymbolID, result types.TypeID) (tir.Node, error) {
	for _, node := range unit.Nodes() {
		if node.Kind == tir.FunctionDeclaration && node.Symbol == symbolID && len(node.TypeArgs) != 0 && node.ResultType == result {
			return node, nil
		}
	}
	return tir.Node{}, fmt.Errorf("called function symbol %d concrete specialization not found", symbolID)
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

func findCallDeclaration(unit *tir.Unit, call tir.Node) (tir.Node, error) {
	if len(call.TypeArgs) != 0 {
		return findCalledFunctionDeclaration(unit, call.Symbol, call.TypeArgs)
	}
	decl, err := findFunctionDeclaration(unit, call.Symbol, "called function")
	if err == nil {
		return decl, nil
	}
	return findCalledFunctionByResult(unit, call.Symbol, call.Type)
}

// validateEntrySignature checks the entry's calling convention, parameter
// count, and result type against the supported shapes: a void result (empty
// body), an int/i32/i64 result, or, since Float Stage A, an f32/f64 result
// (body under the recursive block grammar). On
// success it returns the resolved result builtin (types.Void, types.Int,
// types.I32, types.I64, types.F32, or types.F64) — for an integer entry that returned builtin IS the width every
// builder downstream emits at, threaded through Emit rather than re-derived.
// Whether the body actually matches the result's shape is decided by the
// body-validation step the caller dispatches on.
func validateEntrySignature(decl tir.Node, snapshot *types.Snapshot) (types.BuiltinKind, error) {
	if decl.Convention != types.Pebble {
		return 0, fmt.Errorf("entry function uses %s calling convention, want Pebble", callingConventionName(decl.Convention))
	}
	if len(decl.Parameters) != 0 {
		return 0, fmt.Errorf("entry function has %d parameter(s), want 0 (main([]str) and main(i32, []str) are not supported yet)", len(decl.Parameters))
	}
	key, ok := snapshot.Key(decl.ResultType)
	if !ok {
		return 0, fmt.Errorf("entry function result type %d is not in the type snapshot", decl.ResultType)
	}
	builtin, ok := key.Builtin()
	if !ok || (builtin != types.Void && builtin != types.Int && builtin != types.I32 && builtin != types.I64 && builtin != types.F32 && builtin != types.F64) {
		return 0, fmt.Errorf("entry function result type is %s, want void, int, i32, i64, f32, or f64", describeType(snapshot, decl.ResultType))
	}
	return builtin, nil
}

// findEntryBody follows the entry declaration's FunctionID to its FunctionDecl
// and resolves that declaration's body node. The body node is a distinct
// Block entry in unit.Nodes(), separate from the FunctionDeclaration node
// found by findEntryDeclaration. It returns both the resolved Block node and
// its NodeID, so the caller can pass the ID into the recursive buildBlock.
func findEntryBody(unit *tir.Unit, decl tir.Node) (tir.Node, tir.NodeID, error) {
	return findFunctionBody(unit, decl, "entry function")
}

// findFunctionBody resolves the body Block for any function declaration,
// generalizing findEntryBody to a called helper: it follows the declaration's
// FunctionID to its FunctionDecl container and resolves that container's body
// node, returning both the Block node and its NodeID.
func findFunctionBody(unit *tir.Unit, decl tir.Node, what string) (tir.Node, tir.NodeID, error) {
	for _, fd := range unit.FunctionDeclarations() {
		if fd.FunctionID != decl.Function {
			continue
		}
		block, ok := unit.Node(fd.Node)
		if !ok {
			return tir.Node{}, 0, fmt.Errorf("%s body not found in unit: FunctionDecl %d has invalid body node %d", what, fd.FunctionID, fd.Node)
		}
		if block.Kind != tir.Block {
			return tir.Node{}, 0, fmt.Errorf("%s body is a %s, want a Block", what, block.Kind)
		}
		return block, fd.Node, nil
	}
	return tir.Node{}, 0, fmt.Errorf("%s body declaration not found in unit: no FunctionDecl for FunctionID %d", what, decl.Function)
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

func collectHelperArrayTypes(snapshot *types.Snapshot, helpers []helperInfo) []types.TypeID {
	seen := make(map[types.TypeID]bool)
	var ids []types.TypeID
	for _, helper := range helpers {
		for _, param := range helper.decl.Parameters {
			if isArray(snapshot, param.Type) && !seen[param.Type] {
				seen[param.Type] = true
				ids = append(ids, param.Type)
			}
		}
		if isArray(snapshot, helper.decl.ResultType) && !seen[helper.decl.ResultType] {
			seen[helper.decl.ResultType] = true
			ids = append(ids, helper.decl.ResultType)
		}
	}
	return ids
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

// discoverReachableHelpers finds exactly the set of non-entry functions the
// entry actually calls, transitively, by walking the entry's body for
// tir.DirectCall nodes and recursing into each newly-discovered callee's own
// body — a worklist/DFS over the direct-call edges starting from the entry,
// following into every function reached. Emitting exactly this reachable set
// (and nothing else) guarantees by construction that every emitted helper has
// at least one call site, so the mandated -Wall -Wextra -Werror build never
// warns about an unused static function. Each reached callee is validated
// (Pebble-convention, parameters each of the entry's width or bool, result
// exactly the entry's width —
// validateHelperSignature) and its body located (findFunctionBody) before
// recursing. The returned slice is a post-order of the walk — callees before
// callers — which is the emission order that keeps every call in the emitted
// C text forward (definition before use); since buildHelperPrototypes now
// emits a forward declaration for every reachable helper before any
// definition, that ordering is no longer a correctness requirement — a cycle
// (a function that can reach itself, directly or through others) is simply
// skipped, not rejected, so recursive and mutually-recursive functions are
// discovered and emitted like any other reachable helper. A cycle passing
// THROUGH the entry function is the one cycle shape still rejected (the entry
// has the fixed C name pebble_user_main, not a pebble_fn_<symbolID> helper
// name the forward-declaration pass covers).
func discoverReachableHelpers(unit *tir.Unit, snapshot *types.Snapshot, entryDecl tir.Node, entryBlockID tir.NodeID, width types.BuiltinKind) ([]helperInfo, error) {
	walk := &reachabilityWalk{
		unit:     unit,
		snapshot: snapshot,
		width:    width,
		entry:    entryDecl.Symbol,
		done:     make(map[tir.FunctionID]bool),
	}
	if err := walk.visit(entryDecl, entryBlockID); err != nil {
		return nil, err
	}
	return walk.order, nil
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

// collectDirectCalls appends every tir.DirectCall node in the tree rooted at
// nodeID, following Children and DeferChain, plus — since the function-values
// slice — every tir.HoistedFunctionValue node (a bare top-level function
// reference used as a value, e.g. the initializer of a function-typed local or
// the callee of an indirect call), whose referenced function must be emitted
// as a helper even though no DirectCall ever invokes it. The typed-IR node
// graph is
// single-parented, so this walk terminates and each node is visited at most
// once per path. A DeferRegister child is skipped here: the deferred statement
// inside it is only ever emitted at exit points whose DeferChain references
// the register, so a call inside it is reachable exactly when some exit's
// DeferChain walk (below, which DOES recurse into the register's children)
// reaches it. Walking the register's children at its registration position
// would instead treat a defer that never fires — registered in a region no
// exit of the program leaves through — as making its callee reachable,
// emitting a helper no emitted call site ever invokes and tripping
// -Wunused-function under the mandated -Wall -Wextra -Werror build. (This also
// keeps a deferred Store whose value is a helper call consistent: the callee
// is emitted only when that defer actually fires.)
func collectDirectCalls(unit *tir.Unit, nodeID tir.NodeID, out *[]tir.Node) error {
	node, ok := unit.Node(nodeID)
	if !ok {
		return fmt.Errorf("reachability walk references invalid node %d", nodeID)
	}
	if node.Kind == tir.DirectCall || node.Kind == tir.MethodCall || node.Kind == tir.HoistedFunctionValue || node.Kind == tir.GenericFunctionValue {
		*out = append(*out, node)
	}
	if node.Kind == tir.RecordConstruct {
		// A struct construction's field values (`Table.{ op = add }`,
		// function-types slice 2) are stored in node.Fields
		// ([]FieldInit{Field, Value}), NOT node.Children (confirmed by reading
		// collectStructTypesWalk's identical special-case for the same
		// reason) — so a HoistedFunctionValue used only as a field's
		// construction value (e.g. `add` in `Table.{ op = add }`) is walked
		// explicitly here; otherwise the referenced function is never
		// discovered as reachable and its pebble_fn_<symbolID> definition is
		// never emitted, leaving the struct's field initializer referencing
		// an undeclared identifier.
		for _, field := range node.Fields {
			if err := collectDirectCalls(unit, field.Value, out); err != nil {
				return err
			}
		}
	}
	for _, childID := range node.Children {
		if child, ok := unit.Node(childID); ok && child.Kind == tir.DeferRegister {
			continue
		}
		if err := collectDirectCalls(unit, childID, out); err != nil {
			return err
		}
	}
	for _, deferID := range node.DeferChain {
		if err := collectDirectCalls(unit, deferID, out); err != nil {
			return err
		}
	}
	return nil
}

// collectTupleTypes appends, in first-encountered order, the tuple TypeID of
// every tuple type the emitted program actually references: the entry body
// (root) followed by every reachable helper's body, each walked by the same
// Children + DeferChain traversal collectDirectCalls uses. A tuple type is
// referenced in exactly four places in the emitted C — a tuple-typed local's
// declaration (an Initialize whose initializer value carries the tuple type), a
// tuple construction (a TupleValue, whose Type is the tuple type), a
// tuple-typed parameter of a reachable helper (a FunctionDeclaration.Parameters
// entry's Type), and a tuple-typed result of a reachable helper (a
// FunctionDeclaration.ResultType, whose typedef its C signature names as its
// return type) — so collecting exactly those node shapes, each reachable
// helper's Parameters list, and each reachable helper's ResultType guarantees
// every typedef the program needs is discovered. The Parameters/ResultType
// coverage closes a real gap: a tuple type used only as a parameter type or
// only as a helper's result type (never constructed in any reachable body)
// still needs its typedef emitted, since the helper's C signature names
// pebble_tuple_<typeID>_t. The caller deduplicates (see Emit) so each distinct
// tuple type yields exactly one typedef, emitted before any function
// definition in the final output.
func collectTupleTypes(unit *tir.Unit, snapshot *types.Snapshot, entryBlockID tir.NodeID, helpers []helperInfo) ([]types.TypeID, error) {
	var collected []types.TypeID
	if err := collectTupleTypesWalk(unit, snapshot, entryBlockID, &collected); err != nil {
		return nil, err
	}
	for _, helper := range helpers {
		if err := collectTupleTypesWalk(unit, snapshot, helper.block, &collected); err != nil {
			return nil, err
		}
		// A reachable helper's own parameter list is a source of tuple types
		// the body walk cannot see: a tuple-typed parameter is referenced by
		// the helper's C signature even if no reachable body ever constructs a
		// tuple of that type, so its typedef must be discovered here too.
		for _, param := range helper.decl.Parameters {
			if isTuple(snapshot, param.Type) {
				collected = append(collected, param.Type)
			}
		}
		// A reachable helper's own result type is the same kind of source for
		// the typedef its C signature names as its return type (10.26): a
		// tuple-returning helper's C signature declares
		// pebble_tuple_<typeID>_t, so a tuple type that appears nowhere in any
		// reachable body still needs its typedef emitted. (For a reachable
		// tuple-returning helper the body walk usually finds the type anyway,
		// since the helper must produce a tuple to return; this scan closes the
		// same class of gap 10.24's Parameters scan closed, for the return side
		// — the type may be used only as the helper's result type.)
		if isTuple(snapshot, helper.decl.ResultType) {
			collected = append(collected, helper.decl.ResultType)
		}
	}
	seen := make(map[types.TypeID]bool, len(collected))
	var deduplicated []types.TypeID
	for _, id := range collected {
		if seen[id] {
			continue
		}
		seen[id] = true
		deduplicated = append(deduplicated, id)
	}
	return deduplicated, nil
}

// collectTupleTypesWalk appends every tuple type encountered in the tree rooted
// at nodeID to out, in first-encountered order, following Children and
// DeferChain exactly like collectDirectCalls so it visits the same reachable
// region of the node graph the body builders consume. Two node shapes carry a
// tuple type: a TupleValue node's own Type, and an Initialize whose initializer
// value carries a tuple type (a tuple-typed local declaration — the local's
// type is recorded on the initializer value node, not on the Initialize node
// itself, confirmed against a real fixture). A tuple initializer that is not a
// TupleValue (a whole-tuple copy of another local) is still a tuple-typed
// local and still needs its typedef; it is collected here by the Initialize
// rule even though buildLeadingStatement rejects that initializer shape.
func collectTupleTypesWalk(unit *tir.Unit, snapshot *types.Snapshot, nodeID tir.NodeID, out *[]types.TypeID) error {
	node, ok := unit.Node(nodeID)
	if !ok {
		return fmt.Errorf("tuple-type walk references invalid node %d", nodeID)
	}
	if node.Kind == tir.TupleValue {
		*out = append(*out, node.Type)
	}
	if node.Kind == tir.Initialize {
		for _, childID := range node.Children {
			if child, ok := unit.Node(childID); ok && isTuple(snapshot, child.Type) {
				*out = append(*out, child.Type)
			}
		}
	}
	for _, childID := range node.Children {
		if err := collectTupleTypesWalk(unit, snapshot, childID, out); err != nil {
			return err
		}
	}
	for _, deferID := range node.DeferChain {
		if err := collectTupleTypesWalk(unit, snapshot, deferID, out); err != nil {
			return err
		}
	}
	return nil
}

// collectOptionalTypes appends, in first-encountered order, the optional
// TypeID of every optional type the emitted program actually references: the
// entry body (root) followed by every reachable helper's body, each walked
// by the same Children + DeferChain traversal collectDirectCalls uses, plus
// each reachable helper's own declared parameter types and result type. An
// optional type is referenced in exactly two places in the emitted C — an
// optional-typed local's declaration (an Initialize whose initializer value
// carries the optional type) and a SomeOptional node (whose Type is the
// optional type) — so collecting exactly those two node shapes, each
// reachable helper's parameter list, and each reachable helper's result type
// guarantees every typedef the program needs is discovered (the parameter/
// result scans close the same gap the tuple/struct collection's own scans
// close: an optional type used only as a parameter type or only as a helper's
// result type, never constructed in any reachable body, still needs its
// typedef emitted since the helper's C signature names
// pebble_optional_<typeID>_t). The caller deduplicates so
// each distinct optional type yields exactly one typedef, emitted before any
// function definition in the final output.
func collectOptionalTypes(unit *tir.Unit, snapshot *types.Snapshot, entryBlockID tir.NodeID, helpers []helperInfo) ([]types.TypeID, error) {
	var collected []types.TypeID
	if err := collectOptionalTypesWalk(unit, snapshot, entryBlockID, &collected); err != nil {
		return nil, err
	}
	for _, helper := range helpers {
		if err := collectOptionalTypesWalk(unit, snapshot, helper.block, &collected); err != nil {
			return nil, err
		}
		// A reachable helper's own parameter list is a source of optional
		// types the body walk cannot see: an optional-typed parameter is
		// referenced by the helper's C signature (its C declaration names
		// pebble_optional_<typeID>_t) even if no reachable body ever
		// constructs or injects an optional of that type — the body walk only
		// collects SomeOptional nodes and Initialize children, so a bare
		// `g(5)` call-site argument (an OptionalInject whose type the walk
		// does not collect) or an unused `fn g(o ?int)` parameter would leave
		// the typedef unemitted. This is the same Parameters scan 10.24 added
		// for tuple/struct/slice parameters, applied to optional parameters.
		for _, param := range helper.decl.Parameters {
			if isOptional(snapshot, param.Type) {
				collected = append(collected, param.Type)
			}
		}
		// A reachable helper's own result type is a source of optional types
		// the body walk cannot see: an optional-returning helper's C signature
		// declares pebble_optional_<typeID>_t, so an optional type that appears
		// nowhere in any reachable body still needs its typedef emitted — the
		// same ResultType scan 10.26 added for tuple/struct result types. (For
		// a reachable optional-returning helper the caller must declare a
		// matching optional-typed local to consume the result, which the body
		// walk of that local's Initialize usually finds anyway; this scan
		// closes the same gap, for the return side.)
		if isOptional(snapshot, helper.decl.ResultType) {
			collected = append(collected, helper.decl.ResultType)
		}
	}
	seen := make(map[types.TypeID]bool, len(collected))
	var deduplicated []types.TypeID
	for _, id := range collected {
		if seen[id] {
			continue
		}
		seen[id] = true
		deduplicated = append(deduplicated, id)
	}
	return deduplicated, nil
}

// collectOptionalTypesWalk appends every optional type encountered in the tree
// rooted at nodeID to out, in first-encountered order, following Children and
// DeferChain exactly like collectDirectCalls so it visits the same reachable
// region of the node graph the body builders consume. Two node shapes carry an
// optional type: a SomeOptional node's own Type, and an Initialize whose
// initializer value carries an optional type (an optional-typed local
// declaration — confirmed against a real fixture: the local's type is recorded
// on the initializer value node, not on the Initialize node itself). The
// Initialize rule alone covers a `none`-initialized local too (a NoneOptional
// node carries its own optional Type exactly like SomeOptional does), so no
// separate NoneOptional case is needed here. A RecordConstruct's field values
// are the one more source the Children-only recursion would miss: an
// optional-typed struct field (`Box[int].{ value = some 5 }`) carries its
// SomeOptional field value in node.Fields, not node.Children, so the field
// value's optional Type is collected here — the same hole the struct walk's
// own RecordConstruct case closes for struct types.
func collectOptionalTypesWalk(unit *tir.Unit, snapshot *types.Snapshot, nodeID tir.NodeID, out *[]types.TypeID) error {
	node, ok := unit.Node(nodeID)
	if !ok {
		return fmt.Errorf("optional-type walk references invalid node %d", nodeID)
	}
	if node.Kind == tir.SomeOptional {
		if isOptional(snapshot, node.Type) {
			*out = append(*out, node.Type)
		}
	}
	if node.Kind == tir.Initialize {
		for _, childID := range node.Children {
			if child, ok := unit.Node(childID); ok && isOptional(snapshot, child.Type) {
				*out = append(*out, child.Type)
			}
		}
	}
	if node.Kind == tir.RecordConstruct {
		for _, field := range node.Fields {
			if value, ok := unit.Node(field.Value); ok && isOptional(snapshot, value.Type) {
				*out = append(*out, value.Type)
			}
		}
	}
	for _, childID := range node.Children {
		if err := collectOptionalTypesWalk(unit, snapshot, childID, out); err != nil {
			return err
		}
	}
	for _, deferID := range node.DeferChain {
		if err := collectOptionalTypesWalk(unit, snapshot, deferID, out); err != nil {
			return err
		}
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

// collectSliceTypes resolves, in first-encountered order, every slice type
// the emitted program actually references: the entry body (root) followed by
// every reachable helper's body, each walked by the same Children +
// DeferChain traversal collectDirectCalls uses. A slice type is referenced
// by exactly two node shapes — a CheckedSlice node (a slice expression
// whose Type is the slice type) and an Initialize whose initializer value
// carries a slice type (a slice-typed local declaration) — so collecting
// exactly those shapes guarantees every typedef the program needs is
// discovered. The returned sliceInfos are deduplicated by slice TypeID, so
// every distinct slice type yields exactly one typedef, emitted before any
// function definition in the final output.
func collectSliceTypes(unit *tir.Unit, snapshot *types.Snapshot, entryBlockID tir.NodeID, helpers []helperInfo) ([]sliceInfo, error) {
	var collected []types.TypeID
	if err := collectSliceTypesWalk(unit, snapshot, entryBlockID, &collected); err != nil {
		return nil, err
	}
	for _, helper := range helpers {
		if err := collectSliceTypesWalk(unit, snapshot, helper.block, &collected); err != nil {
			return nil, err
		}
		// A reachable helper's own parameter list is a source of slice types
		// the body walk cannot see: a slice-typed parameter is referenced by
		// the helper's C signature even if no reachable body ever constructs a
		// slice of that type (its slice values arrive as already-built
		// forwards through the call boundary), so its typedef must be
		// discovered here too — the same Parameters scan 10.24 added for
		// tuples/structs.
		for _, param := range helper.decl.Parameters {
			if isSlice(snapshot, param.Type) {
				collected = append(collected, param.Type)
			}
		}
		// A reachable helper's own result type is the same kind of source for
		// the typedef its C signature names as its return type (10.26): a
		// slice-returning helper's C signature declares
		// pebble_slice_<typeID>_t, so a slice type that appears nowhere in any
		// reachable body still needs its typedef emitted. (For a reachable
		// slice-returning helper the body walk usually finds the type anyway,
		// since the helper must produce a slice to return — a CheckedSlice
		// construction or a forward of a slice-typed local/parameter whose own
		// construction lives elsewhere in the reachable program; this scan
		// closes the same class of gap 10.24's Parameters scan closed, for the
		// return side.)
		if isSlice(snapshot, helper.decl.ResultType) {
			collected = append(collected, helper.decl.ResultType)
		}
	}
	seen := make(map[types.TypeID]bool, len(collected))
	var infos []sliceInfo
	for _, id := range collected {
		if seen[id] {
			continue
		}
		seen[id] = true
		info, err := resolveSliceInfo(snapshot, id)
		if err != nil {
			return nil, err
		}
		infos = append(infos, info)
	}
	return infos, nil
}

// collectSliceTypesWalk appends every slice type encountered in the tree
// rooted at nodeID to out, in first-encountered order, following Children
// and DeferChain exactly like collectDirectCalls so it visits the same
// reachable region of the node graph the body builders consume. Two node
// shapes carry a slice type: a CheckedSlice node's own Type (a slice
// expression), and an Initialize whose initializer value carries a slice
// type (a slice-typed local declaration — the local's type is recorded on
// the initializer value node, not on the Initialize node itself, the same
// pattern every other aggregate collection made).
func collectSliceTypesWalk(unit *tir.Unit, snapshot *types.Snapshot, nodeID tir.NodeID, out *[]types.TypeID) error {
	node, ok := unit.Node(nodeID)
	if !ok {
		return fmt.Errorf("slice-type walk references invalid node %d", nodeID)
	}
	if (node.Kind == tir.CheckedSlice || node.Kind == tir.SliceFromRaw) && isSlice(snapshot, node.Type) {
		*out = append(*out, node.Type)
	}
	if node.Kind == tir.FieldPlace && isSlice(snapshot, node.Type) {
		*out = append(*out, node.Type)
	}
	if node.Kind == tir.Initialize {
		for _, childID := range node.Children {
			if child, ok := unit.Node(childID); ok && isSlice(snapshot, child.Type) {
				*out = append(*out, child.Type)
			}
		}
	}
	for _, childID := range node.Children {
		if err := collectSliceTypesWalk(unit, snapshot, childID, out); err != nil {
			return err
		}
	}
	for _, deferID := range node.DeferChain {
		if err := collectSliceTypesWalk(unit, snapshot, deferID, out); err != nil {
			return err
		}
	}
	return nil
}

// resolveSliceInfo turns one collected slice TypeID into a sliceInfo with its
// element type resolved. The element type comes from the slice type's own
// Child() key, which for a Slice kind returns the element type.
func resolveSliceInfo(snapshot *types.Snapshot, id types.TypeID) (sliceInfo, error) {
	key, ok := snapshot.Key(id)
	if !ok {
		return sliceInfo{}, fmt.Errorf("slice type %d is not in the type snapshot", id)
	}
	if key.Kind() != types.Slice {
		return sliceInfo{}, fmt.Errorf("type %s is a %v, want a slice type", describeType(snapshot, id), key.Kind())
	}
	child, ok := key.Child()
	if !ok {
		return sliceInfo{}, fmt.Errorf("slice type %s has no element type", describeType(snapshot, id))
	}
	return sliceInfo{typ: id, elementType: child}, nil
}

// validateSliceElementType rejects a slice type whose element type is anything
// other than a fixed-width integer builtin (the entry's resolved width, uint,
// u8, u16, u32, u64, i8, i16, i32, or i64 — each resolved to its OWN width by
// resolvedBuiltin/cType, independent of the ambient `width` of the context the
// slice is being validated from), char, bool, or a tuple/optional/struct type
// — the same element gate 10.37
// enforces for a slice-typed local (see buildSliceLocalDeclaration and
// sliceElementCType), applied here to a slice-typed function parameter or
// result type so a helper signature naming a slice of str or any
// other unsupported element is a clean rejection before any body is built.
func validateSliceElementType(unit *tir.Unit, snapshot *types.Snapshot, width types.BuiltinKind, id types.TypeID) error {
	key, ok := snapshot.Key(id)
	if !ok {
		return fmt.Errorf("slice type %d is not in the type snapshot", id)
	}
	element, ok := key.Child()
	if !ok {
		return fmt.Errorf("slice type %s has no element type", describeType(snapshot, id))
	}
	if !isSupportedSliceElementType(unit, snapshot, element) {
		return fmt.Errorf("slice element type is %s, want a fixed-width integer, char, bool, tuple, optional, or struct", describeType(snapshot, element))
	}
	return nil
}

// isSupportedSliceElementType reports whether a slice element type is one this
// backend can emit: a fixed-width integer builtin (resolved to its own width by
// resolvedBuiltin/cType — the entry's width, uint, u8, u16, u32, u64, i8, i16,
// i32, or i64), char (the fixed int32_t), bool, or — matching the aggregate
// element types arrayElementCType/sliceElementCType already accept — a tuple,
// optional, or struct (an enum element is a Nominal type exactly like a struct
// element, and is deliberately excluded here, mirroring the enum rejection in
// both element-C-type builders). This is the single shared element gate
// sliceElementCType, buildSliceConstruction, and the index read/write value
// grammars all consult, mirroring how the function-types work admitted integer
// parameters/results by resolvedBuiltin/cType generically instead of a
// width-specific predicate list.
func isSupportedSliceElementType(unit *tir.Unit, snapshot *types.Snapshot, id types.TypeID) bool {
	if elementWidth, integerElement := resolvedBuiltin(snapshot, id); integerElement && cType(elementWidth) != "" {
		return true
	}
	if isChar(snapshot, id) || isBool(snapshot, id) {
		return true
	}
	if isEnumType(unit, snapshot, id) {
		return false
	}
	return isTuple(snapshot, id) || isOptional(snapshot, id) || isStruct(snapshot, id)
}

// collectFunctionTypes resolves, in first-encountered order, every function
// type the emitted program actually references as a first-class value: the
// entry body (root) followed by every reachable helper's body, each walked by
// the same Children + DeferChain traversal collectDirectCalls uses. A function
// type is referenced by exactly two node shapes — a value node whose own Type
// is the function type (a HoistedFunctionValue, the bare top-level function
// reference that seeds a function-typed local or an indirect call's callee,
// or a function-typed SymbolValue) and an IndirectCall's FunctionType field
// (the callee's own function type, which the general indirect call resolves
// its parameter list from) — so collecting exactly those shapes guarantees
// every fnptr typedef the program needs is discovered. The returned IDs are
// deduplicated by function TypeID, so every distinct function type yields
// exactly one typedef, emitted before any function definition in the final
// output.
func collectFunctionTypes(unit *tir.Unit, snapshot *types.Snapshot, entryBlockID tir.NodeID, helpers []helperInfo) ([]types.TypeID, error) {
	var collected []types.TypeID
	if err := collectFunctionTypesWalk(unit, snapshot, entryBlockID, &collected); err != nil {
		return nil, err
	}
	for _, helper := range helpers {
		if err := collectFunctionTypesWalk(unit, snapshot, helper.block, &collected); err != nil {
			return nil, err
		}
		// A reachable helper's own parameter list is a source of function
		// types the body walk cannot see: a function-typed parameter is
		// referenced by the helper's C signature even if no reachable body
		// ever mentions the parameter (a fn-typed parameter never read in the
		// body, or forwarded only through a call whose own FunctionType is a
		// DIFFERENT fn type), so its typedef must be discovered here too —
		// mirroring collectTupleTypes/collectStructTypes' identical
		// Parameters scan.
		for _, param := range helper.decl.Parameters {
			if isFunctionType(snapshot, param.Type) {
				collected = append(collected, param.Type)
			}
		}
		// A reachable helper's own result type is the same kind of source for
		// the typedef its C signature names as its return type: a
		// function-returning helper's C signature declares
		// pebble_fnptr_<typeID>_t, so a function type that appears nowhere in
		// any reachable body still needs its typedef emitted. (For a reachable
		// function-returning helper the body walk usually finds the type
		// anyway, since the helper must produce a function value to return;
		// this scan closes the same class of gap the Parameters scan above
		// closes, for the return side.)
		if isFunctionType(snapshot, helper.decl.ResultType) {
			collected = append(collected, helper.decl.ResultType)
		}
	}
	seen := make(map[types.TypeID]bool, len(collected))
	var deduplicated []types.TypeID
	for _, id := range collected {
		if seen[id] {
			continue
		}
		seen[id] = true
		deduplicated = append(deduplicated, id)
	}
	return deduplicated, nil
}

// collectFunctionTypesWalk appends every function type encountered in the tree
// rooted at nodeID to out, in first-encountered order, following Children and
// DeferChain exactly like collectDirectCalls so it visits the same reachable
// region of the node graph the body builders consume. Two node KINDS carry a
// first-class function type: a HoistedFunctionValue (a bare top-level
// function reference) and a function-typed SymbolValue (a reference to an
// in-scope function-typed local) — both confirmed the only two shapes
// buildFunctionValue handles. This is deliberately narrower than "any node
// whose own Type is a function type": the built-in Allocator's alloc/
// realloc/free fields are ALSO function-typed (a FieldValue/FieldPlace node
// accessing them has Type = fn(*void, uint) *void), but they are read
// through the allocator-specific indirect-call path (buildIndirectCall's
// allocatorCallee branch) using the runtime's own pre-existing
// PebbleAllocFn/PebbleReallocFn/PebbleFreeFn typedefs, never this general
// pebble_fnptr_<typeID>_t mechanism — collecting them here would wrongly
// demand a general function typedef for the runtime's own callback ABI
// (PebbleAllocFn/PebbleReallocFn/PebbleFreeFn), which is out of scope here,
// breaking every allocator call in the program. An IndirectCall's own FunctionType field is collected too, but
// only for the GENERAL case (an allocator IndirectCall never sets
// FunctionType, confirmed against a real fixture, so the != 0 guard already
// excludes it; the explicit node-kind restriction above is the primary,
// intentional guard).
func collectFunctionTypesWalk(unit *tir.Unit, snapshot *types.Snapshot, nodeID tir.NodeID, out *[]types.TypeID) error {
	node, ok := unit.Node(nodeID)
	if !ok {
		return fmt.Errorf("function-type walk references invalid node %d", nodeID)
	}
	if (node.Kind == tir.HoistedFunctionValue || node.Kind == tir.SymbolValue || node.Kind == tir.GenericFunctionValue) && isFunctionType(snapshot, node.Type) {
		*out = append(*out, node.Type)
	}
	if node.Kind == tir.RecordConstruct {
		// A struct construction's field values (`Table.{ op = add }`,
		// function-types slice 2) are NOT part of node.Children — they are
		// stored separately in node.Fields ([]FieldInit{Field, Value}),
		// confirmed by reading collectStructTypesWalk's identical need to
		// special-case RecordConstruct for the same reason — so a function
		// -typed field's value (a HoistedFunctionValue/SymbolValue the
		// Children-following recursion below would otherwise never reach) is
		// walked explicitly here.
		for _, field := range node.Fields {
			if err := collectFunctionTypesWalk(unit, snapshot, field.Value, out); err != nil {
				return err
			}
		}
	}
	if node.Kind == tir.IndirectCall && node.FunctionType != 0 && isFunctionType(snapshot, node.FunctionType) {
		// The checker sets IndirectCall.FunctionType on BOTH the allocator's
		// built-in indirect call and the general case (confirmed via a real
		// fixture — an earlier version of this check assumed it was set only
		// for the general case, which was wrong and caused every allocator
		// call in the program to be misidentified as needing a general
		// pebble_fnptr_<typeID>_t typedef for the allocator's own *void
		// -parameter runtime-callback ABI, which this backend does not emit
		// general function typedefs for, breaking every allocator call).
		// indirectCalleePlace is the
		// single shared signal (used by buildIndirectCall too) that actually
		// distinguishes the two: the allocator case's callee unwraps to a
		// FieldPlace/FieldValue, which is excluded here.
		if _, isAllocator, ok := indirectCalleePlace(unit, node); ok && !isAllocator {
			*out = append(*out, node.FunctionType)
		}
	}
	for _, childID := range node.Children {
		if err := collectFunctionTypesWalk(unit, snapshot, childID, out); err != nil {
			return err
		}
	}
	for _, deferID := range node.DeferChain {
		if err := collectFunctionTypesWalk(unit, snapshot, deferID, out); err != nil {
			return err
		}
	}
	return nil
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

// collectStructTypes resolves, in first-encountered order, every struct type
// the emitted program actually references: the entry body (root) followed by
// every reachable helper's body, each walked by the same Children + DeferChain
// traversal collectDirectCalls uses. A struct type is referenced in exactly
// four places in the emitted C — a struct-typed local's declaration (an
// Initialize whose initializer value carries the struct type), a struct
// construction (a RecordConstruct, whose Type is the struct type), a
// struct-typed parameter of a reachable helper (a FunctionDeclaration.Parameters
// entry's Type), and a struct-typed result of a reachable helper (a
// FunctionDeclaration.ResultType, whose typedef its C signature names as its
// return type) — so collecting exactly those node shapes, each reachable
// helper's Parameters list, and each reachable helper's ResultType guarantees
// every typedef the program needs is discovered. The Parameters/ResultType
// coverage closes a real gap: a struct type used only as a parameter type or
// only as a helper's result type (never constructed in any reachable body)
// still needs its typedef emitted, since the helper's C signature names
// pebble_struct_<typeID>_t. The walk also accumulates each field's resolved
// type from the same nodes (a RecordConstruct field value's own type, and a
// FieldPlace's Type), since the FieldDeclaration nodes in the unit carry only
// the field's symbol, never its type (confirmed against a real fixture — a
// further lookup is required, the same kind of confirmation 10.18 did for
// FunctionDeclaration.Parameters). The returned structInfos are deduplicated
// by struct TypeID and each resolved to its declared field order, so every
// distinct struct type yields exactly one typedef, emitted before any function
// definition in the final output.
func collectStructTypes(unit *tir.Unit, snapshot *types.Snapshot, entryBlockID tir.NodeID, helpers []helperInfo, optionalTypes []types.TypeID) ([]structInfo, error) {
	fieldTypes := make(map[symbol.SymbolID]types.TypeID)
	var collected []types.TypeID
	if err := collectStructTypesWalk(unit, snapshot, entryBlockID, &collected, fieldTypes); err != nil {
		return nil, err
	}
	// An optional type's payload is a source of struct types the body/helper
	// walks cannot see: `var o ?P = none;` never constructs a P anywhere, so
	// no FieldPlace/RecordConstruct evidence for it exists, but the
	// optional's own C struct typedef still names P's typedef as its .value
	// field type (optionalPayloadCType) and P's typedef must exist for that
	// to compile — mirroring the Parameters/ResultType scans below, for the
	// optional-payload source specifically. optionalTypes is the caller's
	// already-collected list of every optional type reachable in the
	// program (collectOptionalTypes runs first — see Emit).
	for _, optionalType := range optionalTypes {
		key, ok := snapshot.Key(optionalType)
		if !ok || key.Kind() != types.Optional {
			continue
		}
		payload, ok := key.Child()
		if !ok {
			continue
		}
		if isStruct(snapshot, payload) && !isEnumType(unit, snapshot, payload) {
			collected = append(collected, payload)
		}
	}
	for _, helper := range helpers {
		if err := collectStructTypesWalk(unit, snapshot, helper.block, &collected, fieldTypes); err != nil {
			return nil, err
		}
		// A reachable helper's own parameter list is a source of struct types
		// the body walk cannot see: a struct-typed parameter is referenced by
		// the helper's C signature even if no reachable body ever constructs a
		// struct of that type, so its typedef must be discovered here too.
		for _, param := range helper.decl.Parameters {
			if isStruct(snapshot, param.Type) && runtimeType(unit, snapshot, param.Type) == 0 && !isDefinitelyEnumType(unit, snapshot, param.Type) && !isOpaqueExternType(snapshot, param.Type) {
				collected = append(collected, param.Type)
			}
			// A pointer-typed parameter whose pointee is a struct (including
			// a pointer-receiver method's self parameter) references the
			// pointee's typedef in its own C signature, the same reason a
			// plain struct parameter does above.
			if isPointer(snapshot, param.Type) {
				if pointee, ok := pointerPointeeType(snapshot, param.Type); ok && isStruct(snapshot, pointee) && runtimeType(unit, snapshot, pointee) == 0 && !isDefinitelyEnumType(unit, snapshot, pointee) && !isOpaqueExternType(snapshot, pointee) {
					collected = append(collected, pointee)
				}
			}
		}
		// A reachable helper's own result type is the same kind of source for
		// the typedef its C signature names as its return type (10.26): a
		// struct-returning helper's C signature declares
		// pebble_struct_<typeID>_t, so a struct type that appears nowhere in
		// any reachable body still needs its typedef emitted, mirroring the
		// Parameters scan above. (For a reachable struct-returning helper the
		// body walk usually finds the type anyway, since the helper must
		// produce a struct to return — and resolveStructInfo still needs the
		// field types the body walk accumulates — so this closes the same class
		// of gap 10.24's Parameters scan closed, for the return side.)
		if isStruct(snapshot, helper.decl.ResultType) && runtimeType(unit, snapshot, helper.decl.ResultType) == 0 && !isDefinitelyEnumType(unit, snapshot, helper.decl.ResultType) && !isOpaqueExternType(snapshot, helper.decl.ResultType) {
			collected = append(collected, helper.decl.ResultType)
		}
		if isPointer(snapshot, helper.decl.ResultType) {
			if pointee, ok := pointerPointeeType(snapshot, helper.decl.ResultType); ok && isStruct(snapshot, pointee) && runtimeType(unit, snapshot, pointee) == 0 && !isDefinitelyEnumType(unit, snapshot, pointee) && !isOpaqueExternType(snapshot, pointee) {
				collected = append(collected, pointee)
			}
		}
	}
	seen := make(map[types.TypeID]bool, len(collected))
	var infos []structInfo
	for _, id := range collected {
		if seen[id] {
			continue
		}
		seen[id] = true
		info, err := resolveStructInfo(unit, snapshot, id, fieldTypes)
		if err != nil {
			return nil, err
		}
		infos = append(infos, info)
	}
	return infos, nil
}

// collectStructTypesWalk appends every struct type encountered in the tree
// rooted at nodeID to out, in first-encountered order, following Children and
// DeferChain exactly like collectDirectCalls so it visits the same reachable
// region of the node graph the body builders consume. Two node shapes carry a
// struct type: a RecordConstruct node's own Type, and an Initialize whose
// initializer value carries a struct type (a struct-typed local declaration —
// the local's type is recorded on the initializer value node, not on the
// Initialize node itself, confirmed against a real fixture, the same finding
// tuple/array/optional collection made). A RecordConstruct whose own type is a
// compiler-builtin runtime type (Allocator, Context) is excluded — it has no
// TypeDeclaration in the unit, so resolveStructInfo could never resolve it
// (see the guard on its append below). The same walk also records, in
// fieldTypes, every field symbol's resolved type from exactly the two nodes
// that carry it: a RecordConstruct field value node's own Type, and a
// FieldPlace node's Type — the only in-unit sources of a field's type, since
// the FieldDeclaration node carries no type.
func collectStructTypesWalk(unit *tir.Unit, snapshot *types.Snapshot, nodeID tir.NodeID, out *[]types.TypeID, fieldTypes map[symbol.SymbolID]types.TypeID) error {
	node, ok := unit.Node(nodeID)
	if !ok {
		return fmt.Errorf("struct-type walk references invalid node %d", nodeID)
	}
	if node.Kind == tir.RecordConstruct {
		// A compiler-builtin runtime type (Allocator, Context) is Nominal like
		// a struct but is never a parsed user struct: it has no TypeDeclaration
		// in the unit, so resolveStructInfo would fail on it, and its C typedef
		// (PebbleAllocator / PebbleContext) is hand-written in the emitted
		// header. A source-level Allocator literal lowers to a RecordConstruct
		// (e.g. std/alloc.peb's Allocator.{ ptr, alloc, realloc, free }), so a
		// runtime-typed construction must be excluded from the struct
		// collection here — the same runtimeType guard the Initialize,
		// Parameters, and ResultType collection paths already use. Its field
		// value types are still recorded and its field values still recurse
		// below, so nested ordinary structs inside a runtime construction keep
		// their typedefs.
		if runtimeType(unit, snapshot, node.Type) == 0 {
			*out = append(*out, node.Type)
		}
		for _, field := range node.Fields {
			if value, ok := unit.Node(field.Value); ok && value.Type != 0 {
				fieldTypes[field.Field] = value.Type
			}
			// A construction's field values are stored in node.Fields
			// ([]FieldInit), NOT node.Children, so the Children-following
			// recursion below never reaches a NESTED construction used only as
			// this struct's field value (e.g. `Outer[int].{ inner = Inner[int].{
			// val = 5 } }` — the Inner[int] RecordConstruct lives at
			// Fields[0].Value). Without this recursive walk the nested struct
			// type is never collected, so no C typedef is emitted for it and
			// the outer struct's field read resolves to an unresolved TypeID —
			// the same "Fields isn't in Children" gap collectFunctionTypesWalk
			// (0b6ed32) and collectOptionalTypesWalk (8c339d3) already close.
			if err := collectStructTypesWalk(unit, snapshot, field.Value, out, fieldTypes); err != nil {
				return err
			}
		}
	}
	if node.Kind == tir.FieldPlace {
		if node.Member != 0 && node.Type != 0 {
			fieldTypes[node.Member] = node.Type
		}
	}
	if node.Kind == tir.Initialize {
		for _, childID := range node.Children {
			// A struct-typed local's initializer carries the struct type; an
			// enum-typed local's initializer also carries a Nominal type (a
			// plain enum is Nominal exactly like a struct — see isEnumType),
			// so an enum child must be excluded here or it would be collected
			// as a struct and resolveStructInfo would fail trying to resolve
			// its members as fields. Enums are collected by
			// collectEnumTypes instead.
			if child, ok := unit.Node(childID); ok && isStruct(snapshot, child.Type) && runtimeType(unit, snapshot, child.Type) == 0 && !isEnumType(unit, snapshot, child.Type) {
				*out = append(*out, child.Type)
			}
			// A pointer-typed local whose pointee is a struct (`let p *Point =
			// ...;`) references the pointee's typedef in its own C
			// declaration (`pebble_struct_<id>_t *`), even though the local's
			// own Type is the pointer type, not the struct type — the body
			// walk above only ever inspects a node's own Type, so this case
			// is collected separately here.
			if child, ok := unit.Node(childID); ok && isPointer(snapshot, child.Type) {
				if pointee, ok := pointerPointeeType(snapshot, child.Type); ok && isStruct(snapshot, pointee) && runtimeType(unit, snapshot, pointee) == 0 && !isEnumType(unit, snapshot, pointee) {
					*out = append(*out, pointee)
				}
			}
		}
	}
	for _, childID := range node.Children {
		if err := collectStructTypesWalk(unit, snapshot, childID, out, fieldTypes); err != nil {
			return err
		}
	}
	for _, deferID := range node.DeferChain {
		if err := collectStructTypesWalk(unit, snapshot, deferID, out, fieldTypes); err != nil {
			return err
		}
	}
	return nil
}

type aggregateTypeOrder struct {
	all       []types.TypeID
	tuples    []types.TypeID
	optionals []types.TypeID
	structs   []structInfo
}

// orderAggregateTypes performs a stable dependency-first traversal. The input
// order is deliberately the historical tuple, optional, struct collection
// order, so unrelated programs retain their prior output. unit is threaded
// through so an enum-typed dependency (a plain enum is Nominal like a struct —
// see isEnumType) is never mistaken for a struct: enum types are not
// aggregates this pass orders (they are emitted by buildEnumTypedefs) and have
// no field dependencies to recurse into, so they are skipped entirely rather
// than appended to the postorder as a zero-valued structInfo.
func orderAggregateTypes(unit *tir.Unit, snapshot *types.Snapshot, tuples, optionals []types.TypeID, structs []structInfo) (aggregateTypeOrder, error) {
	structByType := make(map[types.TypeID]structInfo, len(structs))
	for _, info := range structs {
		structByType[info.typ] = info
	}
	var depth func(types.TypeID, map[types.TypeID]bool) int
	depth = func(id types.TypeID, active map[types.TypeID]bool) int {
		if active[id] {
			return 0
		}
		active[id] = true
		defer delete(active, id)
		key, ok := snapshot.Key(id)
		if !ok {
			return 0
		}
		var deps []types.TypeID
		switch key.Kind() {
		case types.Tuple:
			deps, _ = key.Elements()
		case types.Optional:
			if c, ok := key.Child(); ok {
				deps = []types.TypeID{c}
			}
		case types.Array:
			if _, c, ok := key.Array(); ok {
				deps = []types.TypeID{c}
			}
		case types.Nominal:
			if in, ok := structByType[id]; ok {
				for _, f := range in.fields {
					deps = append(deps, f.typ)
				}
			}
		}
		max := 0
		for _, d := range deps {
			// A compiler-builtin runtime type (Allocator, Context) is Nominal
			// like an ordinary struct but is NOT an aggregate this pass orders:
			// its C typedef (PebbleAllocator / PebbleContext) is hand-written in
			// pebble_rt.h, never emitted here, so it must not count as a nesting
			// level (a struct field whose type is Allocator is a leaf, not a
			// dependency — mirroring collectStructTypesWalk's runtimeType==0
			// guard). Without it a struct containing a runtime-typed field would
			// be miscounted as nested and rejected by the depth check below.
			if (isTuple(snapshot, d) || isOptional(snapshot, d) || isArray(snapshot, d) || (isStruct(snapshot, d) && runtimeType(unit, snapshot, d) == 0)) && !isEnumType(unit, snapshot, d) {
				if v := depth(d, active) + 1; v > max {
					max = v
				}
			}
		}
		return max
	}
	for _, id := range append(append(append([]types.TypeID{}, tuples...), optionals...), func() []types.TypeID {
		r := make([]types.TypeID, len(structs))
		for i := range structs {
			r[i] = structs[i].typ
		}
		return r
	}()...) {
		if depth(id, map[types.TypeID]bool{}) > 1 {
			return aggregateTypeOrder{}, fmt.Errorf("aggregate type %s has more than one level of nesting, which is unsupported", describeType(snapshot, id))
		}
	}
	result := aggregateTypeOrder{}
	// DFS postorder gives dependencies before users while preserving roots.
	seen := make(map[types.TypeID]bool)
	var post []types.TypeID
	var dfs func(types.TypeID) error
	dfs = func(id types.TypeID) error {
		if seen[id] {
			return nil
		}
		seen[id] = true
		key, _ := snapshot.Key(id)
		var deps []types.TypeID
		switch key.Kind() {
		case types.Tuple:
			deps, _ = key.Elements()
		case types.Optional:
			if c, ok := key.Child(); ok {
				deps = []types.TypeID{c}
			}
		case types.Nominal:
			if in, ok := structByType[id]; ok {
				for _, f := range in.fields {
					deps = append(deps, f.typ)
				}
			}
		}
		for _, dep := range deps {
			// A compiler-builtin runtime type (Allocator, Context) is Nominal
			// like an ordinary struct but is NOT an aggregate this pass orders:
			// its C typedef (PebbleAllocator / PebbleContext) is hand-written in
			// pebble_rt.h, never emitted here, and structByType has no entry for
			// it (it has no TypeDeclaration, so collectStructTypes never
			// collects it), so recursing would push a zero-valued structInfo
			// into the postorder and buildStructTypedef would reject it. Skip it
			// entirely — mirroring collectStructTypesWalk's runtimeType==0 guard
			// (a runtime type needs no typedef of its own).
			if (isTuple(snapshot, dep) || isOptional(snapshot, dep) || (isStruct(snapshot, dep) && runtimeType(unit, snapshot, dep) == 0)) && !isEnumType(unit, snapshot, dep) {
				if err := dfs(dep); err != nil {
					return err
				}
			}
		}
		post = append(post, id)
		return nil
	}
	all := append(append(append([]types.TypeID{}, tuples...), optionals...), func() []types.TypeID {
		r := make([]types.TypeID, len(structs))
		for i := range structs {
			r[i] = structs[i].typ
		}
		return r
	}()...)
	for _, id := range all {
		if err := dfs(id); err != nil {
			return aggregateTypeOrder{}, err
		}
	}
	for _, id := range post {
		if isTuple(snapshot, id) {
			result.tuples = append(result.tuples, id)
		} else if isOptional(snapshot, id) {
			result.optionals = append(result.optionals, id)
		} else if isStruct(snapshot, id) {
			result.structs = append(result.structs, structByType[id])
		}
	}
	result.all = post
	return result, nil
}

// resolveStructInfo turns one collected struct TypeID into a structInfo with
// its fields in declared order. The declaration symbol comes from the type's
// own Nominal key (TypeKey.Nominal); the declared field order comes from the
// corresponding TypeDecl's Members (unit.TypeDeclarations), which lists the
// field symbols in the struct's source declaration order — NOT the
// construction-site order a RecordConstruct's Fields carry, which is why the
// order is resolved here rather than from any construction node. Each field's
// type comes from TypeDecl.MemberTypes. For a generic instantiation the
// TypeKey.Nominal arguments are the instantiation's concrete type arguments,
// so a member whose recorded type is one of the struct's own type parameters
// (the checker records the parameter's TypeID for a directly parameter-typed
// field) is substituted against those arguments — resolving Pair[K, V]'s `key
// K` to Pair[int, int]'s int and Pair[int, bool]'s int independently, from the
// instantiation's own evidence rather than the per-symbol fallback below.
// Members whose template the checker left unresolved (unless the usage-derived
// fieldTypes map provides a type) are rejected rather than guessed.
func resolveStructInfo(unit *tir.Unit, snapshot *types.Snapshot, id types.TypeID, fieldTypes map[symbol.SymbolID]types.TypeID) (structInfo, error) {
	key, ok := snapshot.Key(id)
	if !ok {
		return structInfo{}, fmt.Errorf("struct type %d is not in the type snapshot", id)
	}
	if key.Kind() != types.Nominal {
		return structInfo{}, fmt.Errorf("type %s is a %v, want a struct type", structTypeName(id), key.Kind())
	}
	decl, arguments, ok := key.Nominal()
	if !ok {
		return structInfo{}, fmt.Errorf("type %s has no nominal declaration", structTypeName(id))
	}
	typeDecl, ok := findTypeDeclaration(unit, decl)
	if !ok {
		return structInfo{}, fmt.Errorf("struct type %s has no TypeDeclaration for symbol %d in the unit", structTypeName(id), decl)
	}
	var substitutions map[symbol.SymbolID]types.TypeID
	if len(arguments) > 0 {
		substitutions = structSubstitutions(unit, snapshot, decl, arguments)
	}
	fields := make([]structFieldInfo, len(typeDecl.Members))
	for i, member := range typeDecl.Members {
		fieldType := types.TypeID(0)
		if i < len(typeDecl.MemberTypes) {
			fieldType = typeDecl.MemberTypes[i]
		}
		if fieldType != 0 && substitutions != nil {
			substituted, err := snapshot.Substitute(fieldType, substitutions)
			if err != nil {
				return structInfo{}, fmt.Errorf("struct type %s field symbol %d type substitution: %v", structTypeName(id), member, err)
			}
			fieldType = substituted
		}
		if fieldType == 0 {
			// A member whose template wraps the struct's own parameter (e.g.
			// `?K` or `*K`) has no parameterized TypeID to substitute, so the
			// concrete field type is recovered per instantiation from the
			// struct's OWN construction evidence first: any RecordConstruct
			// whose type is exactly this instantiation (node.Type, not merely
			// the shared declaration) names the field's resolved type on its
			// value node. Two specializations share every field symbol, so the
			// per-symbol fieldTypes fallback below would let one
			// instantiation's type win over the other's; the scoped recovery
			// is what keeps them distinct.
			fieldType, ok = instantiatedFieldType(unit, id, member)
			if !ok {
				fieldType, ok = fieldTypes[member]
			}
			if !ok {
				return structInfo{}, fmt.Errorf("struct type %s field symbol %d has no resolvable type in the unit", structTypeName(id), member)
			}
		}
		fields[i] = structFieldInfo{member: member, typ: fieldType}
	}
	return structInfo{typ: id, decl: decl, fields: fields}, nil
}

// instantiatedFieldType recovers one field's type for a SPECIFIC struct
// instantiation from the unit's own construction evidence: any RecordConstruct
// whose type is exactly structType (not merely whose declaration matches) and
// whose Fields include member, returning that field value node's Type. Two
// specializations of one generic struct share every field symbol, so the
// recovery must be scoped to the instantiation's own type or the first
// constructed instantiation would supply the field type for the rest.
func instantiatedFieldType(unit *tir.Unit, structType types.TypeID, member symbol.SymbolID) (types.TypeID, bool) {
	for _, node := range unit.Nodes() {
		if node.Kind != tir.RecordConstruct || node.Type != structType {
			continue
		}
		for _, field := range node.Fields {
			if field.Field == member {
				if value, ok := unit.Node(field.Value); ok && value.Type != 0 {
					return value.Type, true
				}
			}
		}
	}
	return 0, false
}

// structSubstitutions builds the per-instantiation substitution map for a
// generic struct: it maps each of the struct's OWN type parameters (in their
// declared order, recovered from the struct's generic Nominal key in the
// snapshot — the Nominal whose arguments are that declaration's TypeParameter
// TypeIDs) to the concrete type arguments of the instantiation being resolved.
// The returned map is keyed by parameter declaration symbol, matching
// types.Snapshot.Substitute's key. A non-generic struct or a struct whose
// generic declaration key is absent from the snapshot yields nil (callers then
// fall back to MemberTypes/fieldTypes, which is exactly correct for the
// non-generic case).
func structSubstitutions(unit *tir.Unit, snapshot *types.Snapshot, decl symbol.SymbolID, arguments []types.TypeID) map[symbol.SymbolID]types.TypeID {
	parameters := structTypeParameters(unit, snapshot, decl)
	if len(parameters) == 0 || len(parameters) != len(arguments) {
		return nil
	}
	substitutions := make(map[symbol.SymbolID]types.TypeID, len(parameters))
	for index, parameter := range parameters {
		parameterKey, ok := snapshot.Key(parameter)
		if !ok {
			return nil
		}
		parameterSymbol, ok := parameterKey.TypeParameter()
		if !ok {
			return nil
		}
		substitutions[parameterSymbol] = arguments[index]
	}
	return substitutions
}

// structTypeParameters recovers the ordered list of a struct declaration's own
// type-parameter TypeIDs from the snapshot. The checker interns the generic
// declaration as a Nominal key whose arguments are that declaration's own
// TypeParameter TypeIDs in parameter order (e.g. Pair[K, V]'s declaration key
// carries [<K>, <V>]), which is the one Nominal in the snapshot whose arguments
// are all TypeParameter kinds. Its arguments are therefore the authoritative
// ordered parameter list needed to zip the instantiation's concrete arguments
// back onto the struct's parameters.
//
// That one-Nominal uniqueness assumption does NOT hold in a program where the
// generic declaration is referenced from other generic contexts: every
// generic function or method that names the declaration with its OWN type
// parameters interns a SEPARATE all-TypeParameter Nominal key (e.g.
// Entry[K, V] referenced from rehash[K, V]'s body interns a key whose
// arguments are rehash's own K/V TypeParameters, distinct from Entry's own),
// so a plain first-match scan returns whichever all-TypeParameter key was
// interned earliest — frequently a method/function context's, whose parameter
// symbols never match the field MemberTypes the substitution must rewrite.
// The declaration's OWN parameters are exactly the TypeParameters its own
// field MemberTypes reference (a directly parameter-typed field records the
// parameter's TypeID; each generic context's parameters are distinct symbols),
// so the scan prefers the candidate whose argument symbols include every
// field-referenced parameter symbol, falling back to the first all-TypeParameter
// key only when the declaration has no directly parameter-typed field (in which
// case no field's type depends on the substitution anyway).
func structTypeParameters(unit *tir.Unit, snapshot *types.Snapshot, decl symbol.SymbolID) []types.TypeID {
	referenced := map[symbol.SymbolID]bool{}
	if typeDecl, ok := findTypeDeclaration(unit, decl); ok {
		for _, memberType := range typeDecl.MemberTypes {
			if kind, ok := snapshot.Kind(memberType); ok && kind == types.TypeParameter {
				if key, ok := snapshot.Key(memberType); ok {
					if symbolID, ok := key.TypeParameter(); ok {
						referenced[symbolID] = true
					}
				}
			}
		}
	}
	var fallback []types.TypeID
	for id := range snapshot.IDs() {
		key, ok := snapshot.Key(id)
		if !ok || key.Kind() != types.Nominal {
			continue
		}
		candidate, arguments, ok := key.Nominal()
		if !ok || candidate != decl || len(arguments) == 0 {
			continue
		}
		allParameters := true
		for _, argument := range arguments {
			if kind, ok := snapshot.Kind(argument); !ok || kind != types.TypeParameter {
				allParameters = false
				break
			}
		}
		if !allParameters {
			continue
		}
		if fallback == nil {
			fallback = arguments
		}
		matches := true
		for symbolID := range referenced {
			found := false
			for _, argument := range arguments {
				if key, ok := snapshot.Key(argument); ok {
					if argumentSymbol, ok := key.TypeParameter(); ok && argumentSymbol == symbolID {
						found = true
						break
					}
				}
			}
			if !found {
				matches = false
				break
			}
		}
		if matches {
			return arguments
		}
	}
	return fallback
}

// findTypeDeclaration locates the TypeDecl container (its ordered Members list
// names the struct's declared fields) for a type declaration symbol. The
// TypeDeclaration *node* in the unit carries only the Symbol — its field list
// is on the TypeDecl container the builder published alongside it (the same
// division the unit makes between FunctionDeclaration nodes and FunctionDecl
// containers), so the container is the authoritative declared-field-order
// source.
func findTypeDeclaration(unit *tir.Unit, symbolID symbol.SymbolID) (tir.TypeDecl, bool) {
	for _, td := range unit.TypeDeclarations() {
		if td.Symbol == symbolID {
			return td, true
		}
	}
	return tir.TypeDecl{}, false
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

// collectEnumTypes resolves, in first-encountered order, every plain enum type
// the emitted program actually references: the entry body (root) followed by
// every reachable helper's body, each walked by the same Children + DeferChain
// traversal collectDirectCalls uses. A plain enum type is referenced in the
// emitted C in exactly two node shapes — an enum-typed local's declaration
// (an Initialize whose initializer value is an EnumVariantValue or a
// zero-payload VariantConstruct carrying the enum type) and a bare
// EnumVariantValue / VariantConstruct value node (whose own Type is the enum
// type) — so collecting exactly those shapes guarantees every typedef the
// program needs is discovered, exactly like collectTupleTypes /
// collectStructTypes discover their types from construction nodes. A
// VariantConstruct carrying one or more payload children is the only
// in-IR record of a variant with a non-void payload type — the tagged-union
// (union enum) form — and since 10.35 that is handled by collectUnionTypes
// instead: this pass excludes every such type (unions, threaded in as the
// caller's union map) so a tagged union is never emitted as a plain enum
// typedef (its discriminant enum typedef is emitted as the tag of its tagged
// struct instead, see buildUnionTypedef). The returned enumInfos are
// deduplicated by enum TypeID and each resolved to its declared variant order,
// so every distinct plain enum type yields exactly one typedef, emitted before
// any function definition in the final output. Enum-typed helper
// parameters/results are rejected earlier by validateHelperSignature (before a
// reachable helper is ever collected), so no Parameters/ResultType scan is
// needed here, mirroring how those two scans exist only to close struct/tuple
// param-result gaps.
func collectEnumTypes(unit *tir.Unit, snapshot *types.Snapshot, entryBlockID tir.NodeID, helpers []helperInfo, unions map[types.TypeID]unionInfo) ([]enumInfo, error) {
	var collected []types.TypeID
	if err := collectEnumTypesWalk(unit, snapshot, entryBlockID, &collected); err != nil {
		return nil, err
	}
	for _, helper := range helpers {
		if err := collectEnumTypesWalk(unit, snapshot, helper.block, &collected); err != nil {
			return nil, err
		}
	}
	seen := make(map[types.TypeID]bool, len(collected))
	var infos []enumInfo
	for _, id := range collected {
		if seen[id] {
			continue
		}
		seen[id] = true
		if _, isUnion := unions[id]; isUnion {
			// A tagged-union type is collected by this walk exactly like a
			// plain enum (its variants are enum-shaped — isEnumType returns
			// true for it), but it is not a plain enum: its discriminant enum
			// typedef is emitted as the tag field of its tagged struct (see
			// buildUnionTypedef), so it must be excluded from the plain-enum
			// typedef list or the same pebble_enum_<typeID>_t typedef would be
			// emitted twice.
			continue
		}
		info, err := resolveEnumInfo(unit, snapshot, id)
		if err != nil {
			return nil, err
		}
		infos = append(infos, info)
	}
	return infos, nil
}

// collectEnumTypesWalk appends every enum type encountered in the tree
// rooted at nodeID to out, in first-encountered order, following Children and
// DeferChain exactly like collectDirectCalls so it visits the same reachable
// region of the node graph the body builders consume. Four node shapes carry
// an enum type: an EnumVariantValue node's own Type (a variant literal,
// e.g. Color.green), a VariantConstruct node's own Type (a variant
// construction, e.g. Color.red() — the parenthesized-call form of a plain
// enum's payload-less variant, or e.g. Choice.value(5) — a tagged-union
// payload-carrying construction, which this walk collects exactly the same way
// and the caller filters out as a tagged union; see collectUnionTypes), a
// CheckedIntegerToEnum node's own Type (an integer cast to an enum, e.g.
// `5 as Color` — the node's Type is the destination enum type, so a cast that
// never participates in a local declaration still gets its typedef emitted),
// and an
// Initialize whose initializer value carries an enum type (an enum-typed local
// declaration — the local's type is recorded on the initializer value node,
// not on the Initialize node itself, confirmed against a real fixture, the
// same finding every aggregate collection made). The Initialize rule also
// collects an enum type used as a local's declared type with a rejected
// initializer shape (a whole-copy of another enum local), so its typedef is
// still emitted before the builder rejects the initializer — mirroring
// collectTupleTypesWalk's own rule. A payload-carrying VariantConstruct is no
// longer rejected here (10.35): it is the tagged-union construction, collected
// by collectUnionTypes, and the caller filters the type out of the plain-enum
// results.
func collectEnumTypesWalk(unit *tir.Unit, snapshot *types.Snapshot, nodeID tir.NodeID, out *[]types.TypeID) error {
	node, ok := unit.Node(nodeID)
	if !ok {
		return fmt.Errorf("enum-type walk references invalid node %d", nodeID)
	}
	if node.Kind == tir.EnumVariantValue {
		*out = append(*out, node.Type)
	}
	if node.Kind == tir.VariantConstruct {
		*out = append(*out, node.Type)
	}
	if node.Kind == tir.CheckedIntegerToEnum {
		// An integer cast to an enum (`5 as Color`): the node's own Type IS
		// the destination enum type, collected exactly like an
		// EnumVariantValue's — a cast that is never part of a local
		// declaration (e.g. a store's new value or a switch subject standing
		// alone) would otherwise leave the enum typedef unemitted. The
		// caller's tagged-union filter applies the same way it does to the
		// variant-shape rules above (a checked cast can only target a plain
		// enum in real source — the checker routes an integer to a
		// NominalEnum destination only — but the filter keeps the invariant
		// uniform).
		*out = append(*out, node.Type)
	}
	if node.Kind == tir.OptionalIntegerToEnum {
		// An integer cast to an optional enum (`5 as ?Color`): unlike the
		// checked cast, the node's own Type is the OPTIONAL type, not the
		// enum, so the destination enum is the optional's payload — resolved
		// here so the optional typedef's value field
		// (pebble_enum_<typeID>_t, see optionalPayloadCType) always has its
		// enum typedef emitted ahead of it.
		if key, ok := snapshot.Key(node.Type); ok && key.Kind() == types.Optional {
			if child, ok := key.Child(); ok && isEnumType(unit, snapshot, child) {
				*out = append(*out, child)
			}
		}
	}
	if node.Kind == tir.Initialize {
		for _, childID := range node.Children {
			if child, ok := unit.Node(childID); ok && isEnumType(unit, snapshot, child.Type) {
				*out = append(*out, child.Type)
			}
		}
	}
	for _, childID := range node.Children {
		if err := collectEnumTypesWalk(unit, snapshot, childID, out); err != nil {
			return err
		}
	}
	for _, deferID := range node.DeferChain {
		if err := collectEnumTypesWalk(unit, snapshot, deferID, out); err != nil {
			return err
		}
	}
	return nil
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

// collectUnionTypes resolves, in first-encountered order, every tagged-union
// type the emitted program actually references: the entry body (root) followed
// by every reachable helper's body, each walked by the same Children +
// DeferChain traversal collectDirectCalls uses. A tagged-union type is
// referenced by exactly one node shape — a payload-carrying VariantConstruct
// (e.g. Choice.value(5), a VariantConstruct whose Type is the union's own
// TypeID and whose Children are the payload expression(s); a plain enum's
// variants are payload-less, and the checker rejects calling one with an
// argument, C0604, so a payload-carrying VariantConstruct can only be a union
// enum). Each constructed variant's payload type is resolved from its own
// construction site's payload child Type (the checker anchors every
// construction of a variant to its one declared payload type, so all sites of
// a variant agree — confirmed against real fixtures at three payload shapes),
// and must be exactly the entry's resolved width, bool, or str — a tuple/
// struct/array/optional/nested-enum payload is a clean rejection naming what
// is unsupported, never guessed at, enforced here in the collection walk where
// each variant's payload type is first resolved. width is threaded so the
// payload gate can be enforced against the entry's own width. The returned
// unionInfos are deduplicated by union TypeID and each resolved to its
// declared variant order plus its constructed members, so every distinct union
// type yields exactly one tagged struct typedef (plus its tag enum typedef),
// emitted before any function definition in the final output.
func collectUnionTypes(unit *tir.Unit, snapshot *types.Snapshot, width types.BuiltinKind, entryBlockID tir.NodeID, helpers []helperInfo) ([]unionInfo, error) {
	payloads := make(map[types.TypeID]map[symbol.SymbolID]types.TypeID)
	var collected []types.TypeID
	if err := collectUnionTypesWalk(unit, snapshot, width, entryBlockID, &collected, payloads); err != nil {
		return nil, err
	}
	for _, helper := range helpers {
		if err := collectUnionTypesWalk(unit, snapshot, width, helper.block, &collected, payloads); err != nil {
			return nil, err
		}
	}
	seen := make(map[types.TypeID]bool, len(collected))
	var infos []unionInfo
	for _, id := range collected {
		if seen[id] {
			continue
		}
		seen[id] = true
		info, err := resolveUnionInfo(unit, snapshot, id, payloads[id])
		if err != nil {
			return nil, err
		}
		infos = append(infos, info)
	}
	return infos, nil
}

// collectUnionTypesWalk appends every tagged-union type encountered in the
// tree rooted at nodeID to out, in first-encountered order, following Children
// and DeferChain exactly like collectDirectCalls so it visits the same
// reachable region of the node graph the body builders consume. The one node
// shape that carries a tagged-union type is a VariantConstruct with one or
// more children: its Type is the union's own TypeID, its Member the variant
// symbol, and each child the payload expression whose own Type is the
// variant's declared payload type (confirmed against real fixtures). The walk
// records node.Type as a union type and, for each construction, the payload
// type under the variant's member symbol; a second construction of the same
// variant must carry the same payload type (the checker enforces one declared
// type per variant, so this is guaranteed for real source; a mismatch is a
// clean rejection for hand-built IR, never a guessed layout). The payload type
// is gated here — it must be exactly the entry's resolved width, bool, or str,
// since this backend emits exactly those three C types as union members; any
// other payload (a tuple/struct/array/optional/nested-enum) is a clean
// rejection naming what is unsupported.
func collectUnionTypesWalk(unit *tir.Unit, snapshot *types.Snapshot, width types.BuiltinKind, nodeID tir.NodeID, out *[]types.TypeID, payloads map[types.TypeID]map[symbol.SymbolID]types.TypeID) error {
	node, ok := unit.Node(nodeID)
	if !ok {
		return fmt.Errorf("union-type walk references invalid node %d", nodeID)
	}
	if node.Kind == tir.VariantConstruct && len(node.Children) >= 1 {
		// A payload-carrying variant construction. node.Type is the union's
		// own type (the 7feaf0c checker fix publishes the variant's term as the
		// union type — confirmed against a real fixture: the VariantConstruct's
		// Type is the union TypeID, not the payload's type). The payload child
		// node's own Type is the variant's declared payload type, anchored by
		// the checker at every construction site (confirmed against real
		// fixtures at three payload shapes: an i32 literal, a bool literal, and
		// an i32 expression referencing a local).
		if len(node.Children) != 1 {
			return fmt.Errorf("union variant symbol %d is constructed with %d payload(s); a tagged-union variant carries exactly one payload of %s, bool, or str", node.Member, len(node.Children), wantName(width))
		}
		payloadNode, ok := unit.Node(node.Children[0])
		if !ok {
			return fmt.Errorf("union variant symbol %d references invalid payload node %d", node.Member, node.Children[0])
		}
		if !isWidth(snapshot, width, payloadNode.Type) && !isBool(snapshot, payloadNode.Type) && !isStr(snapshot, payloadNode.Type) {
			return fmt.Errorf("union variant symbol %d carries a payload of type %s; only a payload of %s, bool, or str is supported", node.Member, describeType(snapshot, payloadNode.Type), wantName(width))
		}
		byMember, seen := payloads[node.Type]
		if !seen {
			byMember = make(map[symbol.SymbolID]types.TypeID)
			payloads[node.Type] = byMember
		}
		if existing, ok := byMember[node.Member]; ok && existing != payloadNode.Type {
			return fmt.Errorf("union variant symbol %d is constructed with inconsistent payload types %s and %s", node.Member, describeType(snapshot, existing), describeType(snapshot, payloadNode.Type))
		}
		byMember[node.Member] = payloadNode.Type
		*out = append(*out, node.Type)
	}
	for _, childID := range node.Children {
		if err := collectUnionTypesWalk(unit, snapshot, width, childID, out, payloads); err != nil {
			return err
		}
	}
	for _, deferID := range node.DeferChain {
		if err := collectUnionTypesWalk(unit, snapshot, width, deferID, out, payloads); err != nil {
			return err
		}
	}
	return nil
}

// resolveUnionInfo turns one collected union TypeID into a unionInfo with its
// variants in declared order and its constructed members resolved. The
// declaration symbol comes from the type's own Nominal key (TypeKey.Nominal);
// the declared variant order comes from the corresponding TypeDecl's Members
// (unit.TypeDeclarations), the same mechanism resolveEnumInfo uses for a plain
// enum (the TypeDeclaration *node* carries only the symbol, so the container is
// authoritative). The constructed members come from the payloads map the walk
// accumulated (member symbol -> resolved payload type), listed in declared
// variant order so the C union member order is deterministic regardless of
// construction-site order. The type must actually be enum-shaped, not a struct
// that shares the Nominal key shape — isEnumType distinguishes the two from the
// unit's own node graph, so a collected non-enum Nominal type is a clean
// rejection, not a guessed layout.
func resolveUnionInfo(unit *tir.Unit, snapshot *types.Snapshot, id types.TypeID, payloads map[symbol.SymbolID]types.TypeID) (unionInfo, error) {
	key, ok := snapshot.Key(id)
	if !ok {
		return unionInfo{}, fmt.Errorf("union type %d is not in the type snapshot", id)
	}
	if key.Kind() != types.Nominal {
		return unionInfo{}, fmt.Errorf("type %s is a %v, want a tagged-union type", unionTypeName(id), key.Kind())
	}
	decl, _, ok := key.Nominal()
	if !ok {
		return unionInfo{}, fmt.Errorf("type %s has no nominal declaration", unionTypeName(id))
	}
	if !isEnumType(unit, snapshot, id) {
		return unionInfo{}, fmt.Errorf("type %s is not a tagged-union type (its declaration symbol %d's members resolve to struct fields, not enum variants)", unionTypeName(id), decl)
	}
	typeDecl, ok := findTypeDeclaration(unit, decl)
	if !ok {
		return unionInfo{}, fmt.Errorf("union type %s has no TypeDeclaration for symbol %d in the unit", unionTypeName(id), decl)
	}
	if len(typeDecl.Members) == 0 {
		return unionInfo{}, fmt.Errorf("union type %s has no declared variants", unionTypeName(id))
	}
	members := make([]unionMemberInfo, 0, len(payloads))
	for _, variant := range typeDecl.Members {
		if payloadType, ok := payloads[variant]; ok {
			members = append(members, unionMemberInfo{member: variant, payloadType: payloadType})
		}
	}
	return unionInfo{typ: id, decl: decl, variants: append([]symbol.SymbolID(nil), typeDecl.Members...), members: members}, nil
}

// resolveEnumInfo turns one collected enum TypeID into an enumInfo with its
// variants in declared order. The declaration symbol comes from the type's own
// Nominal key (TypeKey.Nominal); the declared variant order comes from the
// corresponding TypeDecl's Members (unit.TypeDeclarations), which lists the
// variant symbols in the enum's source declaration order — the same mechanism
// resolveStructInfo uses for structs (the TypeDeclaration *node* carries only
// the symbol, so the container is authoritative). The type must actually be a
// plain enum, not a struct that shares the Nominal key shape — isEnumType
// distinguishes the two from the unit's own node graph, so a collected
// non-enum Nominal type is a clean rejection, not a guessed layout.
func resolveEnumInfo(unit *tir.Unit, snapshot *types.Snapshot, id types.TypeID) (enumInfo, error) {
	key, ok := snapshot.Key(id)
	if !ok {
		return enumInfo{}, fmt.Errorf("enum type %d is not in the type snapshot", id)
	}
	if key.Kind() != types.Nominal {
		return enumInfo{}, fmt.Errorf("type %s is a %v, want an enum type", enumTypeName(id), key.Kind())
	}
	decl, _, ok := key.Nominal()
	if !ok {
		return enumInfo{}, fmt.Errorf("type %s has no nominal declaration", enumTypeName(id))
	}
	if !isEnumType(unit, snapshot, id) {
		return enumInfo{}, fmt.Errorf("type %s is not a plain enum (its declaration symbol %d's members resolve to struct fields, not enum variants)", enumTypeName(id), decl)
	}
	typeDecl, ok := findTypeDeclaration(unit, decl)
	if !ok {
		return enumInfo{}, fmt.Errorf("enum type %s has no TypeDeclaration for symbol %d in the unit", enumTypeName(id), decl)
	}
	if len(typeDecl.Members) == 0 {
		return enumInfo{}, fmt.Errorf("enum type %s has no declared variants", enumTypeName(id))
	}
	return enumInfo{typ: id, decl: decl, variants: append([]symbol.SymbolID(nil), typeDecl.Members...)}, nil
}

// isEnumType reports whether id resolves to a plain enum type in the snapshot,
// as opposed to a struct — the two are indistinguishable in the type snapshot
// itself (both are Nominal keys carrying only the declaration symbol), so the
// distinction is resolved from the unit's own node graph directly: every
// declared member of a type carries its own member-declaration node — a
// struct field's is tir.FieldDeclaration, an enum variant's is
// tir.VariantDeclaration (confirmed unconditional: the checker's type-builder
// emits one of these for every member of every TypeDeclaration, independent of
// whether the member is ever actually used anywhere in the reachable program —
// see buildTypes). This is a direct, positive signal, not a heuristic guess
// from usage evidence: a type whose first member has a FieldDeclaration node is
// a struct, and one whose first member has a VariantDeclaration node is an
// enum, regardless of whether the type is ever constructed or field-accessed
// anywhere in the program. (An older version of this function guessed from
// FieldPlace/RecordConstruct usage evidence and defaulted to "enum" when no
// evidence was found either way — wrong for a struct that is declared but
// never constructed or field-accessed anywhere, e.g. only ever named as a
// `none`-optional's payload type, which produced an invalid reference to an
// enum typedef that was never emitted. The declaration-node signal has no such
// blind spot: it needs no usage evidence at all.)
func isEnumType(unit *tir.Unit, snapshot *types.Snapshot, id types.TypeID) bool {
	if snapshot == nil || unit == nil {
		return false
	}
	key, ok := snapshot.Key(id)
	if !ok || key.Kind() != types.Nominal {
		return false
	}
	decl, _, ok := key.Nominal()
	if !ok {
		return false
	}
	typeDecl, ok := findTypeDeclaration(unit, decl)
	if !ok {
		return false
	}
	members := make(map[symbol.SymbolID]bool, len(typeDecl.Members))
	for _, m := range typeDecl.Members {
		members[m] = true
	}
	for _, node := range unit.Nodes() {
		if !members[node.Symbol] {
			continue
		}
		switch node.Kind {
		case tir.FieldDeclaration:
			return false
		case tir.VariantDeclaration:
			return true
		}
	}
	// No member-declaration node was found at all (a type with zero declared
	// members, if that's even expressible) — fall back to true, matching this
	// function's prior default for the case genuinely no evidence exists.
	return true
}

// containsVariant reports whether id is one of the variant symbols in variants.
func containsVariant(variants []symbol.SymbolID, id symbol.SymbolID) bool {
	for _, variant := range variants {
		if variant == id {
			return true
		}
	}
	return false
}

// isDefinitelyEnumType reports whether id is a Nominal type whose declared
// members are enum variants by positive declaration-node evidence: at least
// one member carries a VariantDeclaration node and none carries a
// FieldDeclaration node. This is the precise form of isEnumType WITHOUT its
// no-evidence fallback (which reports true for a type whose members carry no
// declaration node at all — a method-only struct has no FieldDeclaration
// nodes — and would therefore wrongly exclude such a real struct from
// struct-type collection). It is used only where a false positive would
// wrongly drop a struct type from collection; isEnumType remains the shared
// "is this enum-shaped" test everywhere else.
func isDefinitelyEnumType(unit *tir.Unit, snapshot *types.Snapshot, id types.TypeID) bool {
	if unit == nil || snapshot == nil {
		return false
	}
	key, ok := snapshot.Key(id)
	if !ok || key.Kind() != types.Nominal {
		return false
	}
	decl, _, ok := key.Nominal()
	if !ok {
		return false
	}
	typeDecl, ok := findTypeDeclaration(unit, decl)
	if !ok {
		return false
	}
	members := make(map[symbol.SymbolID]bool, len(typeDecl.Members))
	for _, m := range typeDecl.Members {
		members[m] = true
	}
	sawVariant := false
	for _, node := range unit.Nodes() {
		if !members[node.Symbol] {
			continue
		}
		switch node.Kind {
		case tir.FieldDeclaration:
			return false
		case tir.VariantDeclaration:
			sawVariant = true
		}
	}
	return sawVariant
}

// isTaggedUnionType reports whether id is a tagged-union type in this program:
// an enum-shaped Nominal type (see isEnumType) carrying at least one
// payload-carrying construction somewhere in the unit. This is exactly the
// signal collectUnionTypes uses (a payload-carrying VariantConstruct is the
// one node shape that references a tagged-union type), so it agrees with which
// types get a pebble_union_<typeID>_t typedef pair: an enum-shaped type with
// no payload-carrying construction (a plain enum, or a union every variant of
// which is payload-less) is emitted as a plain pebble_enum_<typeID>_t typedef
// instead (see collectEnumTypes / buildUnionTypedef).
func isTaggedUnionType(unit *tir.Unit, snapshot *types.Snapshot, id types.TypeID) bool {
	if !isEnumType(unit, snapshot, id) {
		return false
	}
	for _, node := range unit.Nodes() {
		if node.Kind == tir.VariantConstruct && node.Type == id && len(node.Children) >= 1 {
			return true
		}
	}
	return false
}

// unionVariantPayloadMember reports whether (ownerType, member) is the payload
// member of a tagged-union variant in this program: ownerType is a tagged-union
// type, member is one of its declared variants, and a payload-carrying
// construction of that exact variant exists in the unit — the condition under
// which the union's typedef declares the C union member
// pebble_field_<member> under its .payload union (see resolveUnionInfo /
// buildUnionTypedef). This is the read-side test for a narrowed union-variant
// payload access (`self.Ok` inside `case .Ok:`, Slice A): a variant never
// constructed anywhere has no union member, so reading it would emit a
// projection no C union member satisfies — a clean rejection, never guessed C.
func unionVariantPayloadMember(unit *tir.Unit, snapshot *types.Snapshot, ownerType types.TypeID, member symbol.SymbolID) bool {
	if !isTaggedUnionType(unit, snapshot, ownerType) {
		return false
	}
	key, ok := snapshot.Key(ownerType)
	if !ok {
		return false
	}
	decl, _, ok := key.Nominal()
	if !ok {
		return false
	}
	typeDecl, ok := findTypeDeclaration(unit, decl)
	if !ok {
		return false
	}
	declared := false
	for _, m := range typeDecl.Members {
		if m == member {
			declared = true
			break
		}
	}
	if !declared {
		return false
	}
	for _, node := range unit.Nodes() {
		if node.Kind == tir.VariantConstruct && node.Type == ownerType && node.Member == member && len(node.Children) >= 1 {
			return true
		}
	}
	return false
}

// indexOfFunction returns the position of id in ids, or -1 if absent.
func indexOfFunction(ids []tir.FunctionID, id tir.FunctionID) int {
	for i, candidate := range ids {
		if candidate == id {
			return i
		}
	}
	return -1
}

// validateHelperSignature checks one called function against the constraints
// every reachable helper must satisfy: Pebble-convention, parameters whose
// types are exactly the entry's resolved width, bool, str, a tuple type, a
// struct type, an optional type, or a pointer type, and a result of exactly
// the entry's resolved width, bool, str, a tuple
// type, a struct type, an optional type, or void. The width
// rule is the same reasoning 10.13 established for locals — a called function
// of the other width (an i32 helper called from an i64 entry, or vice versa) is
// a clean width-mismatch rejection, never a coercion, since there is no
// cast/coercion lowering to fall back on. A parameter's own type has the same
// options a local has: the entry's width, bool, str (a str parameter is
// declared as the runtime's PebbleStr and read/compared/returned exactly like a
// str local — 10.36), a tuple type (one of the
// shapes 10.19 supports — element types the entry's width or bool), a
// struct type (one of the shapes 10.22 supports — field types the entry's
// width or bool), an optional type (whose payload is read back through the
// same has_value / force-unwrap machinery an optional local uses), or a
// pointer type; a tuple/struct/str/optional result type has the same options.
// The
// tuple/struct's own internal shape is validated wherever
// its typedef gets built (buildTupleTypedef / buildStructTypedef), not here,
// and an optional result's own payload shape is likewise validated wherever
// its typedef gets built (buildOptionalTypedef / optionalPayloadCType) — the
// same coverage an optional parameter's payload gets through
// collectOptionalTypes.
// Anything else (a pointer, an array, an enum, a helper of the
// other integer width) is a clean rejection naming the position. A void-result
// helper is accepted: 10.33 added the one position such a call is legal in —
// a bare discarded-expression statement (buildExpressionStatement), which the
// void call's only reachable shape from real source (helper(); as its own
// statement) produces. A void call in any value position is still rejected by
// the value builders themselves (buildExpr's width gate and
// buildAggregateCallInitializer's result-type match), never silently emitted.
// validateExternSignature checks a C-convention extern declaration the entry
// (or a helper) actually calls: it must be C-convention (the checker already
// enforces this, so a mismatch is hand-built IR), non-variadic (a C-convention
// variadic function type is rejected by the checker too), and every parameter
// and the result must be typed by a C spelling this backend can emit at a call
// site — a fixed-width integer builtin (including uint/u64, each resolved to
// its own C type), bool, char, str (const char *), a pointer to a supported
// pointee, f32/f64, or a void result. This mirrors validateHelperSignature's
// own gate but narrowed to the shapes an extern call site can actually
// produce: the parameter grammar is exactly buildCallArgument's and the result
// flows into whatever consumes the call. No pebble_fn_<symbolID> prototype,
// definition, or body lookup is ever attempted for an extern, and it is never
// added to the reachable-helper emission order (see reachabilityWalk.visit).
func validateExternSignature(unit *tir.Unit, decl tir.Node, snapshot *types.Snapshot) error {
	if decl.Kind != tir.ExternDeclaration {
		return fmt.Errorf("called function symbol %d is not an extern declaration", decl.Symbol)
	}
	if decl.Convention != types.C {
		return fmt.Errorf("called extern function symbol %d uses %s calling convention, want C", decl.Symbol, callingConventionName(decl.Convention))
	}
	if decl.HasBody {
		return fmt.Errorf("called extern function symbol %d has a body, which this backend does not emit for an extern declaration", decl.Symbol)
	}
	if decl.Variadic {
		return fmt.Errorf("called extern function symbol %d is variadic, which this backend does not support yet", decl.Symbol)
	}
	for i, param := range decl.Parameters {
		if _, err := externCType(snapshot, param.Type); err != nil {
			return fmt.Errorf("called extern function symbol %d parameter %d (symbol %d) %v", decl.Symbol, i, param.Symbol, err)
		}
	}
	if _, err := externCType(snapshot, decl.ResultType); err != nil {
		return fmt.Errorf("called extern function symbol %d result type %v", decl.Symbol, err)
	}
	return nil
}

func validateHelperSignature(unit *tir.Unit, decl tir.Node, snapshot *types.Snapshot, width types.BuiltinKind) error {
	if decl.Convention != types.Pebble {
		return fmt.Errorf("called function symbol %d uses %s calling convention, want Pebble", decl.Symbol, callingConventionName(decl.Convention))
	}
	for i, param := range decl.Parameters {
		// A parameter's type is resolved the same way a local's initializer's
		// is: the entry's resolved width (built by buildExpr), bool (built by
		// buildBoolExpr), a char value (built by buildCharOperand — since
		// 10.41 a char parameter is seeded like a char local and read/
		// compared/returned exactly as one), a str value (built by
		// buildStrOperand — since 10.36 a
		// str parameter is seeded like a str local and read/compared/returned
		// exactly as one), an f32/f64 value (built by buildFloatExpr at the
		// parameter's own float kind — since this slice a float parameter is
		// seeded like a float local and read/compared/returned/passed exactly
		// as one), a tuple/struct type (read back through the
		// Load(TuplePlace)/Load(FieldPlace) machinery), or, since 10.38, a
		// slice type (read back through the same Load(CheckedIndexPlace)
		// machinery a slice local uses), nothing else. This is
		// exactly the width-consistency rule 10.13 established for locals,
		// applied to parameters and extended to the aggregate and str local
		// grammars 10.19/10.22/10.23/10.37 already build. A slice-typed
		// parameter's element type must still be exactly the entry's resolved
		// width or bool — the same gate 10.37 enforces for a slice local — so
		// a parameter of a slice type whose element is unsupported (a slice of
		// tuples, str, and so on) is a clean rejection, not a guessed
		// lowering. A scalar parameter whose own resolved integer width shares
		// the entry's C representation (isCompatibleIntegerWidth — the shape a
		// generic specialization produces: clamp[i32] has i32-typed parameters
		// even when the entry is declared `int`, and int/i32 are distinct
		// builtins sharing int32_t) is admitted too; helperSignature declares
		// it at that shared C type and buildCallArgument builds its call-site
		// argument at the parameter's own width, so no cast is ever needed.
		if !isWidth(snapshot, width, param.Type) && !isCompatibleIntegerWidth(snapshot, width, param.Type) && !isUint(snapshot, param.Type) && !isU64(snapshot, param.Type) && !isBool(snapshot, param.Type) && !isChar(snapshot, param.Type) && !isStr(snapshot, param.Type) && !isFloat(snapshot, param.Type) && !isTuple(snapshot, param.Type) && !isStruct(snapshot, param.Type) && !isArray(snapshot, param.Type) && !isSlice(snapshot, param.Type) && !isPointer(snapshot, param.Type) && !isOptional(snapshot, param.Type) && !isFunctionType(snapshot, param.Type) && !isEnumType(unit, snapshot, param.Type) {
			return fmt.Errorf("called function symbol %d parameter %d (symbol %d) has type %s, want %s, bool, char, str, f32, f64, a tuple/struct type, a slice type, a pointer type, an optional type, a function type, or an enum/union type (a parameter may be the entry's integer width, uint, u64, bool, char, str, f32, f64, a tuple/struct type, a slice type, a pointer type, an optional type, a function type, or an enum/union type)", decl.Symbol, i, param.Symbol, describeType(snapshot, param.Type), wantName(width))
		}
		if isSlice(snapshot, param.Type) {
			if err := validateSliceElementType(unit, snapshot, width, param.Type); err != nil {
				return fmt.Errorf("called function symbol %d parameter %d (symbol %d) is a slice type with an unsupported element type: %v", decl.Symbol, i, param.Symbol, err)
			}
		}
		if isArray(snapshot, param.Type) {
			key, ok := snapshot.Key(param.Type)
			if !ok {
				return fmt.Errorf("called function symbol %d parameter %d (symbol %d) has array type missing from the type snapshot", decl.Symbol, i, param.Symbol)
			}
			_, element, ok := key.Array()
			if !ok || !isSupportedSliceElementType(unit, snapshot, element) {
				return fmt.Errorf("called function symbol %d parameter %d (symbol %d) has an unsupported array element type", decl.Symbol, i, param.Symbol)
			}
		}
		// A function-typed parameter's own signature needs no per-signature gate
		// here beyond the shared validateFunctionTypeSignature check: exactly
		// like an optional parameter's payload, it is validated wherever its
		// typedef gets built (buildFunctionTypedef via validateFunctionTypeSignature),
		// and collectFunctionTypes guarantees that typedef is emitted for every
		// reachable helper's parameter types, so an unsupported signature shape
		// (a float parameter, a tuple/struct/slice/optional/pointer parameter,
		// or an aggregate/str result) is a clean rejection at typedef build
		// time, never a guessed layout. The signature check is repeated here so
		// a reachable helper with a bad function-typed parameter fails during
		// helper discovery with the same message a function-typed local's
		// invalid type would name.
		if isFunctionType(snapshot, param.Type) {
			if err := validateFunctionTypeSignature(snapshot, width, param.Type); err != nil {
				return fmt.Errorf("called function symbol %d parameter %d (symbol %d) is a function type with an unsupported signature: %v", decl.Symbol, i, param.Symbol, err)
			}
		}
		// An optional parameter's own payload shape needs no per-payload gate
		// here: exactly like an optional result's, it is validated wherever its
		// typedef gets built (buildOptionalTypedef / optionalPayloadCType), and
		// collectOptionalTypes guarantees that typedef is emitted for every
		// reachable helper's parameter types, so an unsupported payload is a
		// clean rejection at typedef build time, never a guessed layout.
	}
	resultWidth, integerResult := resolvedBuiltin(snapshot, decl.ResultType)
	if (!integerResult || cType(resultWidth) == "") && !isBool(snapshot, decl.ResultType) && !isChar(snapshot, decl.ResultType) && !isStr(snapshot, decl.ResultType) && !isFloat(snapshot, decl.ResultType) && !isTuple(snapshot, decl.ResultType) && !isStruct(snapshot, decl.ResultType) && !isArray(snapshot, decl.ResultType) && !isSlice(snapshot, decl.ResultType) && !isVoid(snapshot, decl.ResultType) && !isPointer(snapshot, decl.ResultType) && !isOptional(snapshot, decl.ResultType) && !isFunctionType(snapshot, decl.ResultType) {
		return fmt.Errorf("called function symbol %d has result type %s, want its own integer width, bool, char, str, f32, f64, a tuple/struct result type, a slice result type, a pointer result type, an optional result type, a function result type, or void", decl.Symbol, describeType(snapshot, decl.ResultType))
	}
	if isSlice(snapshot, decl.ResultType) {
		if err := validateSliceElementType(unit, snapshot, width, decl.ResultType); err != nil {
			return fmt.Errorf("called function symbol %d has a slice result type with an unsupported element type: %v", decl.Symbol, err)
		}
	}
	if isArray(snapshot, decl.ResultType) {
		key, ok := snapshot.Key(decl.ResultType)
		if !ok {
			return fmt.Errorf("called function symbol %d has array result type missing from the type snapshot", decl.Symbol)
		}
		_, element, ok := key.Array()
		if !ok || !isSupportedSliceElementType(unit, snapshot, element) {
			return fmt.Errorf("called function symbol %d has an unsupported array result element type", decl.Symbol)
		}
	}
	if isFunctionType(snapshot, decl.ResultType) {
		if err := validateFunctionTypeSignature(snapshot, width, decl.ResultType); err != nil {
			return fmt.Errorf("called function symbol %d has a function result type with an unsupported signature: %v", decl.Symbol, err)
		}
	}
	return nil
}

// validateFunctionTypeSignature checks one function type used as a first-class
// value — a function-typed local's declared type, a function-typed value's own
// type, or an indirect call's callee type — against the constraints every such
// signature must satisfy, mirroring validateHelperSignature's own gate but
// narrowed to this slice's supported shapes. A function type must be
// Pebble-convention (a C-convention function type as a first-class local's
// type is not checker-reachable — the checker itself rejects assigning a
// C-convention function value to a fn(...) type and a `fn "C"(...)` type
// annotation fails typed-IR construction — so it is a clean rejection here,
// never supported), non-variadic, and every parameter must be one of the
// entry's resolved width, uint, bool, char, str, or a pointer type, and the
// result must be one of the entry's resolved width, bool, char, void, or a
// pointer type. This is deliberately the
// set of shapes this slice can both BUILD (the parameter grammar is exactly
// buildCallArgument's, so every fn-typed call argument is buildable; the
// result grammar is exactly the positions the backend can consume an indirect
// call's result in — the entry's return via buildExpr, a bool position via
// buildBoolExpr, a char position via buildCharOperand, a pointer result
// consumed by buildExpr's pointer-typed IndirectCall path, and a discarded
// statement via buildExpressionStatement) and whose C types are fully
// self-contained (the entry's cType, uint64_t, bool, int32_t, PebbleStr, a
// pointer's own `<pointee> *` spelling via pointerTypeName, or
// void — never a tuple/struct/slice/optional C type that would drag an
// aggregate typedef into the fnptr typedef and require the aggregate collectors
// to chase function-type signatures). Any other parameter/result shape — a
// float (which validateHelperSignature rejects for an ordinary helper anyway),
// a tuple/struct/slice/optional, or an aggregate/str result — is a
// clean rejection naming what is unsupported, the same gate buildFunctionTypedef
// re-checks before emitting a typedef.
func validateFunctionTypeSignature(snapshot *types.Snapshot, width types.BuiltinKind, id types.TypeID) error {
	key, ok := snapshot.Key(id)
	if !ok {
		return fmt.Errorf("function type %d is not in the type snapshot", id)
	}
	convention, parameters, result, variadic, ok := key.Function()
	if !ok {
		return fmt.Errorf("type %s is not a function type", describeType(snapshot, id))
	}
	if convention != types.Pebble {
		return fmt.Errorf("function type %s uses the %s calling convention, want Pebble (a C-convention function type as a first-class value is not supported yet)", describeType(snapshot, id), callingConventionName(convention))
	}
	if variadic {
		return fmt.Errorf("function type %s is variadic, which is not supported yet", describeType(snapshot, id))
	}
	// A function type's parameter is admitted when it is an integer builtin
	// this backend emits (resolved to its OWN width via resolvedBuiltin/cType,
	// independent of the ambient `width` of the context the function type is
	// being validated from — this is what lets a `fn(int) u64` type be
	// validated from a u64-width call context and a `fn(u64) int` type from an
	// entry-width context), or uint/u64 (both resolve to uint64_t), or
	// bool/char/str, or a pointer type (spelled via pointerTypeName, the same
	// way an ordinary helper's pointer parameter is). The signature's
	// parameter C types are decided by the same resolution in
	// functionTypeParamCType, and each call argument is built at its
	// parameter's own resolved width by buildCallArgument, so the kind of
	// each parameter determines how it is built rather than the ambient width.
	for i, parameter := range parameters {
		paramWidth, integerParam := resolvedBuiltin(snapshot, parameter)
		if !(integerParam && cType(paramWidth) != "") && !isBool(snapshot, parameter) && !isChar(snapshot, parameter) && !isStr(snapshot, parameter) && !isPointer(snapshot, parameter) {
			return fmt.Errorf("function type %s parameter %d has type %s, want %s, uint, u64, or another fixed-width integer, bool, char, str, or a pointer type (a function-typed value's signature may only mention parameter shapes this backend can build as a call argument)", describeType(snapshot, id), i, describeType(snapshot, parameter), wantName(width))
		}
	}
	if !isWidth(snapshot, width, result) && !isU64(snapshot, result) && !isBool(snapshot, result) && !isChar(snapshot, result) && !isVoid(snapshot, result) && !isPointer(snapshot, result) {
		return fmt.Errorf("function type %s has result type %s, want %s, u64, bool, char, void, or a pointer type (a function-typed value's signature may only mention result shapes this backend can lower as an indirect call's result)", describeType(snapshot, id), describeType(snapshot, result), wantName(width))
	}
	return nil
}

// buildHelperFunctions builds the C text for every reachable helper, in the
// post-order discovery gives (callees before callers), each as its own
// `static <width> pebble_fn_<symbolID>(PebbleContext *ctx, <params>...) { ... }`
// block with its body built by the exact same buildBlock the entry's body
// uses — no parallel body-builder. Before the body is built, the helper's own
// parameters seed its locals scope exactly as if each had been Initialize'd:
// every parameter maps to its resolved type — the entry's width, bool, char
// (localInfo{isChar}), str
// (localInfo{isStr}), a tuple
// type (localInfo{tuple}), a struct type (localInfo{structType}), a slice
// type (localInfo{sliceType}), a pointer type (localInfo{pointerType}), or an
// optional type (localInfo{optional}) — so a
// SymbolValue reference or a Store targeting a parameter inside the body
// resolves through the existing machinery unchanged, and a tuple/struct
// parameter's element/field reads resolve through the same
// Load(TuplePlace)/Load(FieldPlace) machinery a tuple/struct local uses. The
// C signature declares each parameter with the same pebble_local_<symbolID>
// naming every local uses, so a parameter and a local are textually identical
// inside the body (which is correct: they behave identically once inside the
// function), a tuple/struct parameter's C type being its aggregate's own
// typedef name (pebble_tuple_<typeID>_t / pebble_struct_<typeID>_t), a slice
// parameter's being its slice's own typedef name (pebble_slice_<typeID>_t),
// an optional parameter's being the optional's own typedef name
// (pebble_optional_<typeID>_t), and a str
// parameter's C type being the runtime's fixed PebbleStr. Each
// parameter also gets a `(void)pebble_local_<symbolID>;` immediately after
// the opening brace, the same -Wunused-parameter defense the `(void)ctx;`
// already provides for the context (confirmed: -Wunused-parameter genuinely
// fires under -Wall -Wextra -Werror for a declared-but-never-read parameter).
// Each helper gets its own fresh scope for anything its body declares (the
// seeded parameters plus whatever buildBlock adds), so a helper's locals are
// invisible to the entry and to sibling helpers, exactly as two blocks at the
// same nesting level are isolated.
func buildHelperFunctions(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, helpers []helperInfo, width types.BuiltinKind, unions map[types.TypeID]unionInfo) (string, error) {
	texts := make([]string, 0, len(helpers))
	for _, helper := range helpers {
		params, scope, bodyWidth, returnType, result, err := helperSignature(unit, snapshot, helper, width)
		if err != nil {
			return "", err
		}
		casts := make([]string, 0, len(helper.decl.Parameters))
		for _, param := range helper.decl.Parameters {
			casts = append(casts, fmt.Sprintf("    (void)pebble_local_%d;", param.Symbol))
		}
		statements, err := buildBlock(unit, snapshot, fileSet, helper.block, scope, 0, bodyWidth, result, unions)
		if err != nil {
			return "", err
		}
		paramList := ""
		if len(params) > 0 {
			paramList = ", " + strings.Join(params, ", ")
		}
		castText := ""
		if len(casts) > 0 {
			castText = strings.Join(casts, "\n") + "\n"
		}
		texts = append(texts, fmt.Sprintf(helperFunction, returnType, helperCName(helper.decl), paramList, castText, statements))
	}
	return strings.Join(texts, "\n"), nil
}

// helperSignature computes one reachable helper's C signature — the single
// source of truth shared by the forward-declaration pass
// (buildHelperPrototypes) and the definition pass (buildHelperFunctions), so
// the ~150-line parameter/result-type switch logic lives in exactly one place
// and a prototype and its definition can never disagree on a parameter's C
// type or the C return type. It returns:
//   - params: the C declaration of each parameter, in order (each
//     "<cType> pebble_local_<id>"; the prototype and the definition both use
//     the identical parameter names, so the two can never mismatch).
//   - scope: the locals-scope seeding the parameters produce — one
//     localInfo per parameter symbol, exactly the entry a definition's body
//     (buildBlock) needs to resolve references to its own parameters; the
//     prototype pass discards it, the definition pass seeds its scope with it.
//   - bodyWidth: the width the helper's body is built at — the entry's width
//     unless the helper's own result type is another supported integer
//     builtin (an i64 helper inside an i32 entry builds at i64).
//   - returnType: the C return type (cType(bodyWidth) for a scalar result,
//     the aggregate typedef name for a tuple/struct/slice/optional/function-
//     typed result, the fixed int32_t / bool / PebbleStr / void / pointer C
//     type names for the other result shapes).
//   - result: the resultInfo describing the result shape, so a definition's
//     tail-position Return is built by the right builder (buildExpr,
//     buildAggregateReturnValue, buildCharOperand, buildOptionalValue, etc.).
//
// A helper whose ResultType is a tuple/struct is declared with its
// aggregate's own typedef name as the C return type instead of the entry's
// scalar cType(width), and its body is built with a resultInfo recording that
// aggregate so the tail-position Return is built by buildAggregateReturnValue
// rather than buildExpr. A scalar-result helper is unchanged: cType(width) and
// resultInfo{kind: width}, so its emitted text is byte-identical to before
// this slice. A void-result helper (10.33) is declared with the C return type
// "void" and resultInfo{kind: types.Void}. The tuple/struct shape is
// validated wherever its typedef is built (buildTupleTypedef /
// buildStructTypedef), exactly like a tuple/struct parameter's, and an
// optional result's payload shape is likewise validated wherever its typedef
// is built (buildOptionalTypedef / optionalPayloadCType).
func helperSignature(unit *tir.Unit, snapshot *types.Snapshot, helper helperInfo, width types.BuiltinKind) (params []string, scope map[symbol.SymbolID]localInfo, bodyWidth types.BuiltinKind, returnType string, result resultInfo, err error) {
	scope = make(map[symbol.SymbolID]localInfo, len(helper.decl.Parameters))
	params = make([]string, 0, len(helper.decl.Parameters))
	for _, param := range helper.decl.Parameters {
		switch {
		case isWidth(snapshot, width, param.Type):
			params = append(params, cType(width)+fmt.Sprintf(" pebble_local_%d", param.Symbol))
			scope[param.Symbol] = localInfo{kind: width}
		case isCompatibleIntegerWidth(snapshot, width, param.Type):
			// A parameter whose own resolved integer width shares the entry's
			// C representation (a generic specialization substituted its type
			// parameter with a concrete fixed-width integer — clamp[i32] from
			// an `int`-declared entry: a distinct builtin from the entry's own
			// width that is textually the same C type). The C parameter is
			// declared with cType(width), which is byte-identical to the
			// parameter's own cType (the compatibility check guarantees the
			// two match), and the parameter seeds the callee's locals scope at
			// its OWN width (localInfo{kind: paramWidth}, exactly as a local
			// declared at that width is seeded by buildScalarInitializeCore),
			// so a reference to the parameter inside the body resolves through
			// the existing buildExpr machinery at the parameter's own width
			// unchanged, and a call site's argument — built by
			// buildCallArgument at that same parameter width — passes an
			// identically-typed C value.
			paramWidth, _ := resolvedBuiltin(snapshot, param.Type)
			params = append(params, cType(paramWidth)+fmt.Sprintf(" pebble_local_%d", param.Symbol))
			scope[param.Symbol] = localInfo{kind: paramWidth}
		case isUint(snapshot, param.Type):
			params = append(params, "uint64_t"+fmt.Sprintf(" pebble_local_%d", param.Symbol))
			scope[param.Symbol] = localInfo{kind: types.Uint}
		case isU64(snapshot, param.Type):
			// A u64-typed parameter seeds the callee's locals scope as a
			// u64 local (localInfo{kind: types.U64}), exactly as a u64
			// local's Initialize does, so a reference to the parameter
			// inside the body resolves through the existing buildExpr
			// machinery at the u64 width unchanged. The C parameter is
			// declared as the fixed uint64_t — the same C type a u64
			// local is declared with — so passing a u64 by value is
			// trivially valid C.
			params = append(params, "uint64_t"+fmt.Sprintf(" pebble_local_%d", param.Symbol))
			scope[param.Symbol] = localInfo{kind: types.U64}
		case isBool(snapshot, param.Type):
			params = append(params, fmt.Sprintf("bool pebble_local_%d", param.Symbol))
			scope[param.Symbol] = localInfo{kind: types.Bool}
		case isChar(snapshot, param.Type):
			// A char-typed parameter seeds the callee's locals scope as a
			// char local (localInfo.isChar), exactly as a char local's
			// Initialize does, so a reference to the parameter inside the
			// body resolves through the existing buildCharOperand
			// machinery unchanged (read in any of the six comparisons,
			// forwarded by a char-returning helper's return, or passed to
			// another char parameter). The C parameter is declared as the
			// fixed int32_t — the same C type a char local is declared
			// with, no typedef involved — so passing a char by value is
			// trivially valid C.
			params = append(params, "int32_t"+fmt.Sprintf(" pebble_local_%d", param.Symbol))
			scope[param.Symbol] = localInfo{isChar: true}
		case isFloat(snapshot, param.Type):
			// An f32/f64-typed parameter seeds the callee's locals scope as
			// a float local (localInfo{kind: kind}, where kind is the
			// parameter's own f32/f64 kind — the exact localInfo shape
			// buildScalarInitializeCore records for a float local of that
			// kind), so a reference to the parameter inside the body
			// resolves through the existing buildFloatExpr machinery at that
			// same float kind unchanged (read in any of the six comparisons,
			// used in float arithmetic, forwarded by a float-returning
			// helper's return, or passed to another float parameter). The C
			// parameter is declared at the parameter's own float C type
			// (floatCType — float for f32, double for f64), the same C type
			// a float local is declared with, no typedef involved — so
			// passing a float by value is trivially valid C.
			floatKind := resolvedFloatKind(snapshot, param.Type)
			params = append(params, floatCType(floatKind)+fmt.Sprintf(" pebble_local_%d", param.Symbol))
			scope[param.Symbol] = localInfo{kind: floatKind}
		case isTuple(snapshot, param.Type):
			// A tuple-typed parameter seeds the callee's locals scope as a
			// tuple local (localInfo.tuple), exactly as a tuple local's
			// Initialize does, so element reads inside the body resolve
			// through the existing Load(TuplePlace) machinery unchanged.
			// The C parameter is declared with the tuple's own struct
			// typedef name, so passing the whole tuple by value is trivially
			// valid C (a call site passes a tuple-typed local's own name).
			params = append(params, tupleTypeName(param.Type)+fmt.Sprintf(" pebble_local_%d", param.Symbol))
			scope[param.Symbol] = localInfo{tuple: param.Type}
		case isEnumType(unit, snapshot, param.Type):
			// An enum/union-typed parameter seeds the callee's locals scope
			// as an enum local (localInfo.enumType), exactly as an
			// enum/union local's Initialize does, so a switch subject
			// referencing the parameter and a narrowed union-variant payload
			// read (`r.Ok` inside `case .Ok:` — the Load(FieldPlace) shape
			// buildStructFieldRead resolves) inside the body resolve through
			// the existing enum/union machinery unchanged. The C parameter is
			// declared with the type's own typedef name — pebble_union_
			// <typeID>_t for a tagged union (a payload-carrying construction
			// exists somewhere in the reachable program, so the union typedef
			// pair is emitted; see isTaggedUnionType), pebble_enum_<typeID>_t
			// for a plain enum — so passing the whole value by value is
			// trivially valid C (a call site passes an enum/union-typed
			// value of the same C type).
			ctypeName := enumTypeName(param.Type)
			if isTaggedUnionType(unit, snapshot, param.Type) {
				ctypeName = unionTypeName(param.Type)
			}
			params = append(params, ctypeName+fmt.Sprintf(" pebble_local_%d", param.Symbol))
			scope[param.Symbol] = localInfo{enumType: param.Type}
		case isStruct(snapshot, param.Type):
			// A struct-typed parameter seeds the callee's locals scope as a
			// struct local (localInfo.structType), exactly as a struct
			// local's Initialize does, so field reads inside the body
			// resolve through the existing Load(FieldPlace) machinery
			// unchanged, declared with the struct's own struct typedef name.
			params = append(params, runtimeTypeName(unit, snapshot, param.Type)+fmt.Sprintf(" pebble_local_%d", param.Symbol))
			scope[param.Symbol] = localInfo{structType: param.Type, runtimeType: param.Type}
		case isArray(snapshot, param.Type):
			params = append(params, arrayTypeName(param.Type)+fmt.Sprintf(" pebble_local_%d", param.Symbol))
			scope[param.Symbol] = localInfo{array: param.Type, arrayWrapped: true}
		case isSlice(snapshot, param.Type):
			// A slice-typed parameter (10.38) seeds the callee's locals
			// scope as a slice local (localInfo.sliceType), exactly as a
			// slice local's Initialize does, so an index of the parameter
			// inside the body (`s[0]`) resolves through the existing
			// Load(CheckedIndexPlace) machinery a slice local uses
			// unchanged, declared with the slice type's own struct typedef
			// name (pebble_slice_<typeID>_t — the same typedef 10.37
			// builds for a slice local, no new typedef shape needed). The
			// element type is validated to be the entry's width or bool by
			// validateHelperSignature, so the typedef always builds.
			params = append(params, sliceTypeName(param.Type)+fmt.Sprintf(" pebble_local_%d", param.Symbol))
			scope[param.Symbol] = localInfo{sliceType: param.Type}
		case isOptional(snapshot, param.Type):
			// An optional-typed parameter seeds the callee's locals scope
			// as an optional local (localInfo.optional), exactly as an
			// optional local's Initialize does, so a reference to the
			// parameter inside the body — a `.has_value` read, a `!`
			// force-unwrap, forwarding it in a return or as another
			// call's argument — resolves through the existing optional-
			// local machinery (buildOptionalHasValue, the
			// CheckedOptionalUnwrap path, buildOptionalReturnValue)
			// completely unchanged, no new read-path code. The C
			// parameter is declared with the optional type's own struct
			// typedef name (pebble_optional_<typeID>_t — the same
			// typedef 10.21 builds for an optional local, no new typedef
			// shape needed), so passing the whole optional by value is
			// trivially valid C (a call site passes an optional-typed
			// value of the same C type). The payload type is validated
			// wherever its typedef is built (buildOptionalTypedef via
			// optionalPayloadCType) — the same coverage an optional
			// local's payload gets.
			params = append(params, optionalTypeName(param.Type)+fmt.Sprintf(" pebble_local_%d", param.Symbol))
			scope[param.Symbol] = localInfo{optional: param.Type}
		case isStr(snapshot, param.Type):
			// A str-typed parameter seeds the callee's locals scope as a
			// str local (localInfo.isStr), exactly as a str local's
			// Initialize does, so a reference to the parameter inside the
			// body resolves through the existing buildStrOperand machinery
			// unchanged (read in a ==/!= comparison, forwarded by a
			// str-returning helper's return, or passed to another str
			// parameter). The C parameter is declared as the runtime ABI's
			// fixed PebbleStr type — the same C type a str local is declared
			// with, no typedef involved — so passing a str by value is
			// trivially valid C.
			params = append(params, "PebbleStr"+fmt.Sprintf(" pebble_local_%d", param.Symbol))
			scope[param.Symbol] = localInfo{isStr: true}
		case isPointer(snapshot, param.Type):
			// A pointer-typed parameter seeds the callee's locals scope
			// as a pointer local (localInfo.pointerType). The C parameter
			// is declared with the pointer type's own C type name
			// (pointee_c_type *), so passing a pointer by value is
			// trivially valid C. pointerTypeName takes the pointee, not
			// the pointer type itself, so the pointee must be extracted
			// first.
			paramPointeeTypeID, paramPointeeOK := pointerPointeeType(snapshot, param.Type)
			ctypeName := ""
			if paramPointeeOK {
				ctypeName = pointerTypeName(snapshot, paramPointeeTypeID)
			}
			if ctypeName == "" {
				return nil, nil, 0, "", resultInfo{}, fmt.Errorf("called function symbol %d parameter (symbol %d) has unsupported pointer type %s", helper.decl.Symbol, param.Symbol, describeType(snapshot, param.Type))
			}
			params = append(params, ctypeName+fmt.Sprintf(" pebble_local_%d", param.Symbol))
			scope[param.Symbol] = localInfo{pointerType: param.Type}
		case isFunctionType(snapshot, param.Type):
			// A function-typed parameter (function-types slice 3) seeds
			// the callee's locals scope as a function-typed local
			// (localInfo.functionType — the SAME field a function-typed
			// local's Initialize uses), so a reference to the parameter
			// inside the body — as an indirect call's callee (`f(x, y)`),
			// forwarded as another function-typed parameter's argument, or
			// returned by a function-result helper — resolves through the
			// existing buildFunctionValue machinery unchanged (its
			// SymbolValue case reads localInfo.functionType exactly like
			// buildFunctionLocalDeclaration's scope entry does). The C
			// parameter is declared with the function type's own pointer
			// typedef (pebble_fnptr_<typeID>_t, slice 1's functionTypeName
			// — the same C type a function-typed local is declared with),
			// so passing a function value by value is trivially valid C
			// (a call site passes a function value of the same C type).
			// The function type's own signature is validated by
			// validateHelperSignature above, so the typedef always builds.
			params = append(params, functionTypeName(param.Type)+fmt.Sprintf(" pebble_local_%d", param.Symbol))
			scope[param.Symbol] = localInfo{functionType: param.Type}
		default:
			// validateHelperSignature rules any unsupported parameter out
			// before a reachable helper is ever built, so this branch is
			// defense for hand-built IR only.
			return nil, nil, 0, "", resultInfo{}, fmt.Errorf("called function symbol %d parameter (symbol %d) has type %s, want %s, bool, char, str, f32, f64, a tuple/struct type, a slice type, a pointer type, an optional type, or a function type", helper.decl.Symbol, param.Symbol, describeType(snapshot, param.Type), wantName(width))
		}
	}
	bodyWidth = width
	if resultWidth, integerResult := resolvedBuiltin(snapshot, helper.decl.ResultType); integerResult && cType(resultWidth) != "" {
		bodyWidth = resultWidth
	}
	returnType = cType(bodyWidth)
	result = resultInfo{kind: bodyWidth}
	switch {
	case isVoid(snapshot, helper.decl.ResultType):
		// A void-result helper (10.33) is declared with the C return type
		// "void" — a void call has no value to return, so its body's tail
		// is an ImplicitReturn that emits nothing (buildBlock's ImplicitReturn
		// case). resultInfo records types.Void so buildBlock knows the tail
		// is a legal fall-through rather than a missing return, and the
		// helper is only ever reached by a bare discarded-expression
		// statement call (buildExpressionStatement), never as a value.
		returnType = "void"
		result = resultInfo{kind: types.Void}
	case isBool(snapshot, helper.decl.ResultType):
		// A bool-result helper (added for the function-types slice, whose
		// required bool-parameter/bool-result function-type test needs a
		// bool-returning function to be emittable as a helper) is declared
		// with the C return type "bool" and resultInfo{kind: types.Bool} so
		// buildBlock's tail-position Return builds its value via
		// buildBoolExpr rather than buildExpr, which would reject a
		// bool-typed value.
		returnType = "bool"
		result = resultInfo{kind: types.Bool}
	case isChar(snapshot, helper.decl.ResultType):
		// A char-result helper (10.41) is declared with the fixed C
		// int32_t as its C return type — the same C type a char local is
		// declared with, independent of the entry's resolved width, no
		// typedef involved — and resultInfo records the char shape so
		// buildBlock's tail-position Return builds its value via
		// buildCharOperand (a char literal, a SymbolValue naming a
		// char-typed local, or a call to another char-returning helper)
		// rather than buildExpr, which would reject a char-typed value.
		returnType = "int32_t"
		result = resultInfo{isChar: true}
	case isFloat(snapshot, helper.decl.ResultType):
		// An f32/f64-result helper (this slice) is declared with the C
		// return type of the result's own float kind (floatCType — float
		// for f32, double for f64, the same C type a float local is
		// declared with, no typedef involved) and resultInfo records the
		// float kind (resultInfo{kind: kind}) — the exact resultInfo shape
		// the entry's own f32/f64-returning main threads — so buildBlock's
		// tail-position Return builds its value via buildFloatExpr (a float
		// literal, a SymbolValue naming a float-typed local of the same
		// kind, float arithmetic, or a call to another float-returning
		// helper) rather than buildExpr, which would reject a float-typed
		// value.
		floatKind := resolvedFloatKind(snapshot, helper.decl.ResultType)
		returnType = floatCType(floatKind)
		result = resultInfo{kind: floatKind}
	case isTuple(snapshot, helper.decl.ResultType):
		returnType = tupleTypeName(helper.decl.ResultType)
		result = resultInfo{tuple: helper.decl.ResultType}
	case isStruct(snapshot, helper.decl.ResultType):
		returnType = runtimeTypeName(unit, snapshot, helper.decl.ResultType)
		result = resultInfo{structType: helper.decl.ResultType}
	case isArray(snapshot, helper.decl.ResultType):
		returnType = arrayTypeName(helper.decl.ResultType)
		result = resultInfo{arrayType: helper.decl.ResultType}
	case isStr(snapshot, helper.decl.ResultType):
		// A str-result helper (10.36) is declared with the runtime ABI's
		// fixed PebbleStr as its C return type — the same C type a str
		// local is declared with, no typedef involved — and resultInfo
		// records the str shape so buildBlock's tail-position Return builds
		// its value via buildStrOperand (a SymbolValue naming a str local, a
		// string literal, or a call to another str-returning helper) rather
		// than buildExpr, which would reject a str-typed value.
		returnType = "PebbleStr"
		result = resultInfo{isStr: true}
	case isSlice(snapshot, helper.decl.ResultType):
		// A slice-result helper (10.38) is declared with the slice type's
		// own struct typedef name (pebble_slice_<typeID>_t) as its C return
		// type — the same typedef 10.37 builds for a slice local, no new
		// typedef shape needed — and resultInfo records the slice shape so
		// buildBlock's tail-position Return builds its value via
		// buildSliceReturnValue (a SymbolValue naming a slice-typed local,
		// or a fresh CheckedSlice construction) rather than buildExpr,
		// which would reject a slice-typed value. The element type is
		// validated to be the entry's width or bool by
		// validateHelperSignature, so the typedef always builds.
		returnType = sliceTypeName(helper.decl.ResultType)
		result = resultInfo{sliceType: helper.decl.ResultType}
	case isPointer(snapshot, helper.decl.ResultType):
		// A pointer-result helper is declared with the pointer type's own
		// C type name as its return type. pointerTypeName takes the
		// pointee, not the pointer type itself (it appends " *" to the
		// pointee's own C type), so the pointee must be extracted first.
		// The body's tail-position Return builds its value via buildExpr
		// (which now handles pointer-typed nodes: AddressOf, SymbolValue,
		// NilPointer, DirectCall). resultInfo records the pointer shape
		// so buildBlock's tail-position Return can build the value
		// correctly.
		pointeeTypeID, ok := pointerPointeeType(snapshot, helper.decl.ResultType)
		if !ok {
			return nil, nil, 0, "", resultInfo{}, fmt.Errorf("called function symbol %d has unsupported pointer result type %s", helper.decl.Symbol, describeType(snapshot, helper.decl.ResultType))
		}
		returnType = pointerTypeName(snapshot, pointeeTypeID)
		result = resultInfo{pointerType: helper.decl.ResultType}
	case isOptional(snapshot, helper.decl.ResultType):
		// An optional-result helper is declared with the optional type's
		// own struct typedef name (pebble_optional_<typeID>_t) as its C
		// return type — the same typedef 10.21 builds for an optional
		// local, no new typedef shape needed — and resultInfo records the
		// optional shape so buildBlock's tail-position Return builds its
		// value via buildOptionalValue (a SymbolValue naming an
		// optional-typed local, a fresh SomeOptional/NoneOptional/
		// OptionalInject construction, or a call to another
		// optional-returning helper) rather than buildExpr, which would
		// reject an optional-typed value. The payload type is validated
		// wherever its typedef is built (buildOptionalTypedef via
		// optionalPayloadCType), exactly like a tuple/struct result's
		// internal shape.
		returnType = optionalTypeName(helper.decl.ResultType)
		result = resultInfo{optionalType: helper.decl.ResultType}
	case isFunctionType(snapshot, helper.decl.ResultType):
		// A function-result helper (function-types slice 3) is declared
		// with the function type's own pointer typedef
		// (pebble_fnptr_<typeID>_t) as its C return type — the same
		// typedef slice 1 builds for a function-typed local, no new typedef
		// shape needed — and resultInfo records the function-result shape
		// (resultInfo.functionType, mirroring how resultInfo.optionalType
		// was added for optional results) so buildBlock's tail-position
		// Return builds its value via buildFunctionValue (a bare function
		// reference, a function-typed local or parameter forward, a
		// function-typed struct field forward, or a call to another
		// function-result helper) rather than buildExpr, which would
		// reject a function-typed value. The function type's own signature
		// is validated by validateHelperSignature above, so the typedef
		// always builds.
		returnType = functionTypeName(helper.decl.ResultType)
		result = resultInfo{functionType: helper.decl.ResultType}
	}
	return params, scope, bodyWidth, returnType, result, nil
}

// buildHelperPrototypes builds one C forward declaration (prototype) per
// reachable helper, in the same order as the definitions buildHelperFunctions
// emits, using the exact same helperSignature source of truth — a prototype
// and its definition always agree on every parameter's C type and the C
// return type, and reuse the identical pebble_local_<id> parameter names, so
// the mandated -Wall -Wextra -Werror build cannot warn about a prototype
// disagreeing with its definition. The prototypes are emitted before ANY
// definition, which is what makes recursive/mutually-recursive calls legal C:
// a function's own body (or another function's body) may call a function
// whose definition comes later in the file, because the earlier prototype
// already declares it. The parameter names are the same ones the definition
// uses, so no -Wunused-parameter or other pedantry can fire on the prototype
// text itself (parameter names in a declaration are inert, but keeping them
// identical is the conservative choice). Every helper is reachable (the
// reachability walk emits exactly the reachable set), so a prototype never
// precedes a static function with no call site, and no -Wunused-function
// warning appears.
func buildHelperPrototypes(unit *tir.Unit, snapshot *types.Snapshot, helpers []helperInfo, width types.BuiltinKind) (string, error) {
	prototypes := make([]string, 0, len(helpers))
	for _, helper := range helpers {
		params, _, _, returnType, _, err := helperSignature(unit, snapshot, helper, width)
		if err != nil {
			return "", err
		}
		paramList := ""
		if len(params) > 0 {
			paramList = ", " + strings.Join(params, ", ")
		}
		prototypes = append(prototypes, fmt.Sprintf(helperPrototype, returnType, helperCName(helper.decl), paramList))
	}
	return strings.Join(prototypes, "\n"), nil
}

// validateEmptyBody accepts only a block with no statements, or the single
// synthesized ImplicitReturn that a void entry's empty body ends with. Any
// other statement content is rejected, not best-effort lowered.
func validateEmptyBody(unit *tir.Unit, block tir.Node) error {
	if len(block.Children) == 0 {
		return nil
	}
	if len(block.Children) == 1 {
		if child, ok := unit.Node(block.Children[0]); ok && child.Kind == tir.ImplicitReturn {
			return nil
		}
	}
	if child, ok := unit.Node(block.Children[0]); ok {
		return fmt.Errorf("entry function body is not empty: unsupported statement %s found; this backend only emits an empty-bodied void entry", child.Kind)
	}
	return fmt.Errorf("entry function body is not empty: %d statement(s) found; this backend only emits an empty-bodied void entry", len(block.Children))
}

// buildBlock validates one block under the entry body's recursive grammar and
// builds its C statement sequence. A block is zero or more `<cType> <width>
// pebble_local_<id>` declarations (one per Initialize, in declaration order),
// zero or more `pebble_local_<id> = <built value>;` reassignments (one per
// Store, targeting a local already in scope), and zero or more `while (...)
// { <loop body> }` loop statements (one per While, built by buildWhile), zero
// or more `for (<init>; <cond>; <update>) { <loop body> }` classic for loop
// statements (one per For, built by buildFor), and zero or more range loop
// statements (one per RangeLoop, built by buildRangeLoop) — a
// loop is only ever a leading statement here, never the block's tail —
// followed by a tail that is either the single `return <expression>;`, a
// two-armed if/else built by buildIf, or a switch statement built by
// buildSwitch; each if arm and each case body is itself a block under the
// same grammar, so buildBlock recurses into both arms and case bodies.
// width is the entry's
// resolved integer width (types.Int, types.I32, or types.I64), threaded through to every
// expression and declaration built here so the emitted C type names and
// runtime helper names follow the width. locals is the set of
// symbols visible at the block's entry (the enclosing scopes' declarations)
// and is copied at entry: every addition this block makes — its own
// declarations, and anything an arm's or a loop body's subtree declares —
// stays in that copy and never mutates the map the caller or a sibling scope
// sees. That copy-per-scope discipline is what makes a local declared inside
// one arm (or inside a loop body) invisible to its siblings and to any scope
// outside it, while locals declared in an enclosing block remain visible
// inside. depth is the nesting level of this block below the function body (0
// for the entry body itself); statements and the if/else braces are indented
// one level per depth so nested output stays well-formed C. Any other shape is
// rejected with a descriptive error, not best-effort lowered.
func buildBlock(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, blockID tir.NodeID, locals map[symbol.SymbolID]localInfo, depth int, width types.BuiltinKind, result resultInfo, unions map[types.TypeID]unionInfo) (string, error) {
	block, ok := unit.Node(blockID)
	if !ok {
		return "", fmt.Errorf("entry function body references invalid block node %d", blockID)
	}
	if block.Kind != tir.Block {
		return "", fmt.Errorf("entry function body block is a %s, want a Block", block.Kind)
	}
	if len(block.Children) == 0 {
		return "", fmt.Errorf("entry function body block is empty, want zero or more local declarations or reassignments followed by exactly one return or a two-armed if/else")
	}
	scope := cloneLocals(locals)
	indent := strings.Repeat("    ", depth+1)
	var statements []string
	for i := 0; i < len(block.Children)-1; i++ {
		statement, ok := unit.Node(block.Children[i])
		if !ok {
			return "", fmt.Errorf("entry function body block references invalid statement node %d", block.Children[i])
		}
		if statement.Kind == tir.While {
			// A while loop is a leading statement in the block grammar, never
			// the tail: it runs its body (which may itself declare locals and
			// reassign enclosing ones) as many times as its condition holds,
			// then control falls through to the statements after it. The loop
			// body is its own scope (buildWhile clones, exactly as buildIf's
			// arms do), so nothing the loop declares leaks into this block's
			// scope map.
			whileText, err := buildWhile(unit, snapshot, fileSet, statement, scope, depth, width, result, unions)
			if err != nil {
				return "", err
			}
			statements = append(statements, whileText)
			continue
		}
		if statement.Kind == tir.RangeLoop {
			// A range loop is a leading statement in the block grammar exactly
			// like a while — never the tail — lowering to a C for loop over the
			// bound iterator. Its body is its own scope (buildRangeLoop seeds
			// the iterator and buildLoopBody clones), so nothing the loop
			// declares leaks into this block's scope map.
			rangeText, err := buildRangeLoop(unit, snapshot, fileSet, statement, scope, depth, width, result, unions)
			if err != nil {
				return "", err
			}
			statements = append(statements, rangeText)
			continue
		}
		if statement.Kind == tir.For {
			// A classic for loop is a leading statement in the block grammar
			// exactly like a while — never the tail — lowering to a C for loop
			// with the same three individually-optional clauses. Its body is
			// its own scope (buildFor seeds the initializer's local and
			// buildLoopBody clones), so nothing the loop declares leaks into
			// this block's scope map.
			forText, err := buildFor(unit, snapshot, fileSet, statement, scope, depth, width, result, unions)
			if err != nil {
				return "", err
			}
			statements = append(statements, forText)
			continue
		}
		if statement.Kind == tir.DeferRegister {
			// A DeferRegister in a block's leading-statement sequence is a
			// registration marker the checker's analysis already consumed; the
			// backend must emit nothing at this position. The deferred statement
			// is only ever emitted at exit points whose DeferChain references it.
			continue
		}
		text, err := buildLeadingStatement(unit, snapshot, fileSet, block.Children[i], scope, indent, depth, "entry function body block", width, result, unions)
		if err != nil {
			return "", err
		}
		statements = append(statements, text)
	}
	last, ok := unit.Node(block.Children[len(block.Children)-1])
	if !ok {
		return "", fmt.Errorf("entry function body block references invalid statement node %d", block.Children[len(block.Children)-1])
	}
	switch last.Kind {
	case tir.Return:
		// A return in the block's tail position — the enclosing function's
		// final statement. The return value's grammar, the deferred statements
		// that must run first, and the slice-construction temp-then-construction
		// shape are all built by the shared buildReturnStatement (also used by
		// buildFallthroughBody for a return inside a fall-through statement
		// sequence), so the emission logic lives in exactly one place.
		text, err := buildReturnStatement(unit, snapshot, fileSet, last, scope, indent, "entry function body block", width, result, unions)
		if err != nil {
			return "", err
		}
		statements = append(statements, text)
	case tir.ImplicitReturn:
		// A void-result function (a reachable void helper — the entry's own
		// void shape is handled separately by validateEmptyBody and never
		// reaches buildBlock) ends its body with a synthesized ImplicitReturn,
		// confirmed against real fixtures. It emits nothing: the C function
		// simply falls off the end, which is legal for a `static void`
		// function. Any DeferChain on it is emitted first (a defer registered
		// inside a void helper must still fire at the helper's exit). A
		// non-void function can never end in an ImplicitReturn from real
		// source (the checker rejects fall-through with C0607), so this case
		// only ever fires for result.kind == types.Void; a hand-built non-void
		// block ending in ImplicitReturn is a clean rejection.
		if result.kind != types.Void {
			return "", fmt.Errorf("entry function body block statement is an ImplicitReturn, want a Return of an integer expression, a two-armed if/else, or a switch (an implicit fall-through tail only appears in a void function, but the enclosing function resolves to a non-void result)")
		}
		deferText, err := buildDeferredStatements(unit, snapshot, fileSet, last.DeferChain, scope, indent, "entry function body block", width, unions)
		if err != nil {
			return "", err
		}
		if deferText != "" {
			statements = append(statements, deferText)
		}
	case tir.If:
		ifText, err := buildIf(unit, snapshot, fileSet, last, scope, depth, width, result, unions)
		if err != nil {
			return "", err
		}
		statements = append(statements, ifText)
	case tir.Switch:
		switchText, err := buildSwitch(unit, snapshot, fileSet, last, scope, depth, width, result, unions)
		if err != nil {
			return "", err
		}
		statements = append(statements, switchText)
	default:
		return "", fmt.Errorf("entry function body block statement is a %s, want a Return of an integer expression, a two-armed if/else, or a switch", last.Kind)
	}
	return strings.Join(statements, "\n"), nil
}

// buildReturnStatement validates and builds the C text for one return
// statement (tir.Return) in any position: the tail of a block (buildBlock),
// or a return inside a fall-through statement sequence — an if arm, a switch
// case body, or a loop body (see buildFallthroughBody). The return value is
// built under the grammar the enclosing function's result selects:
// buildCharOperand for a char result, buildStrOperand for a str result,
// buildAggregateReturnValue for a tuple/struct result, buildSliceReturnValue
// for a slice result (whose fresh-construction shape needs the two-statement
// temp-then-construction form, threaded in as a pre-return statement),
// buildFloatExpr for a float result, and buildExpr for a scalar integer
// result. The DeferChain's deferred statements are emitted first (in the
// chain's own LIFO order), then the return line itself:
//
//	<indent>return <value>;
//
// with any pre-return statements and deferred statements joined ahead of it.
// Any other shape — a return with a child count other than exactly one — is a
// clean rejection naming what was found. context names the enclosing
// construct in error messages.
func buildReturnStatement(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, returnNode tir.Node, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind, result resultInfo, unions map[types.TypeID]unionInfo) (string, error) {
	if len(returnNode.Children) != 1 {
		if len(returnNode.Children) == 0 && result.kind == types.Void {
			// A bare `return;` inside a void-returning helper's body — the
			// std/hmap.peb maybe_grow shape (`if self.cap == 0 { self.rehash(8);
			// return; }`): a return with no value is only legal in a void
			// function, and lowers to a plain C `return;` after any deferred
			// statements fire, exactly as the void helper's ImplicitReturn tail
			// emits nothing but its deferred statements.
			deferText, err := buildDeferredStatements(unit, snapshot, fileSet, returnNode.DeferChain, scope, indent, context, width, unions)
			if err != nil {
				return "", err
			}
			if deferText != "" {
				return deferText + "\n" + indent + "return;", nil
			}
			return indent + "return;", nil
		}
		return "", fmt.Errorf("%s return statement has %d argument(s), want exactly one expression", context, len(returnNode.Children))
	}
	var returnValue string
	var err error
	var preReturn string
	if result.kind == types.Bool {
		// The enclosing function returns bool (a reachable helper whose
		// ResultType is bool, added for the function-types slice), so the
		// return value is built under the bool grammar by buildBoolExpr rather
		// than buildExpr, which rejects a bool-typed value. Supported return
		// shapes are a SymbolValue naming a bool-typed local in scope, a bool
		// literal, a comparison, a ! negation, or an && / || combination.
		returnValue, err = buildBoolExpr(unit, snapshot, fileSet, returnNode.Children[0], scope, width)
	} else if result.isChar {
		// The enclosing function returns char (a reachable helper whose
		// ResultType is char — the entry always threads a scalar resultInfo),
		// so the return value is built under the char grammar by
		// buildCharOperand rather than buildExpr, which rejects a
		// char-typed value. Supported return shapes are a SymbolValue
		// naming a char-typed local in scope, a char literal, or a call to
		// another char-returning helper.
		returnValue, err = buildCharOperand(unit, snapshot, fileSet, returnNode.Children[0], scope, width)
	} else if result.isStr {
		// The enclosing function returns str (a reachable helper whose
		// ResultType is str — the entry always threads a scalar resultInfo),
		// so the return value is built under the str grammar by
		// buildStrOperand rather than buildExpr, which rejects a str-typed
		// value. Supported return shapes are a SymbolValue naming a
		// str-typed local in scope, a string literal, or a call to another
		// str-returning helper.
		returnValue, err = buildStrOperand(unit, snapshot, fileSet, returnNode.Children[0], scope, width)
	} else if result.tuple != 0 || result.structType != 0 {
		// The enclosing function returns a tuple/struct (a reachable helper
		// whose ResultType is an aggregate — the entry always threads a
		// scalar resultInfo), so the return value is built under the
		// aggregate grammar by buildAggregateReturnValue rather than
		// buildExpr, which rejects an aggregate-typed value. Supported
		// return shapes are a SymbolValue naming an aggregate-typed local
		// in scope of the matching type, a fresh inline TupleValue /
		// RecordConstruct of the matching type (both built via 10.25's
		// expression builders), or a DirectCall to a struct-returning helper
		// (a return forward); anything else is a clean rejection. The
		// builder returns a (pre, expr) pair — the DirectCall shape's
		// construction pre is threaded into the statement sequence ahead of
		// the final return line, exactly as the slice path below does.
		preReturn, returnValue, err = buildAggregateReturnValue(unit, snapshot, fileSet, returnNode.Children[0], scope, result, indent, width)
	} else if result.arrayType != 0 {
		returnValue, err = buildArrayReturnValue(unit, snapshot, fileSet, returnNode.Children[0], scope, result.arrayType, width)
	} else if result.sliceType != 0 {
		// The enclosing function returns a slice (a reachable helper whose
		// ResultType is a slice type), so the return value is built under
		// the slice grammar by buildSliceReturnValue rather than buildExpr,
		// which rejects a slice-typed value. Supported return shapes are a
		// SymbolValue naming a slice-typed local in scope (a single-
		// statement forward) or a fresh CheckedSlice construction, which
		// needs the same two-statement temp-then-construction shape a slice
		// local's declaration uses. The temp-declaration statement is
		// threaded into the statement sequence as an extra pre-return
		// statement, the same mechanical shape the deferred statements
		// below demonstrate — just for construction complexity rather than
		// deferred cleanup.
		preReturn, returnValue, err = buildSliceReturnValue(unit, snapshot, fileSet, returnNode.Children[0], scope, result, indent, width)
	} else if result.optionalType != 0 {
		// The enclosing function returns an optional (a reachable helper
		// whose ResultType is an optional type), so the return value is
		// built under the optional grammar by buildOptionalValue
		// rather than buildExpr, which rejects an optional-typed value.
		// Supported return shapes are a SymbolValue naming an
		// optional-typed local in scope of the matching type, a fresh
		// SomeOptional / NoneOptional / OptionalInject construction of the
		// matching type (built by the shared buildOptionalValueExpr), a
		// DirectCall to another optional-returning helper (a return
		// forward), or a bare payload value whose implicit injection into
		// the optional is supplied here; anything else is a clean
		// rejection.
		returnValue, err = buildOptionalValue(unit, snapshot, fileSet, returnNode.Children[0], scope, result.optionalType, "entry function body return statement", width)
	} else if result.functionType != 0 {
		// The enclosing function returns a function type (a reachable helper
		// whose ResultType is a function type — the entry always threads a
		// scalar resultInfo), so the return value is built under the function
		// grammar by buildFunctionValue rather than buildExpr, which rejects
		// a function-typed value. Supported return shapes are a bare function
		// reference (a HoistedFunctionValue), a reference to an in-scope
		// function-typed local or parameter (a SymbolValue), a function-typed
		// struct field read (a FieldValue or Load(FieldPlace)), a generic
		// function referenced as a value (a GenericFunctionValue), or a call
		// to another function-returning helper (a DirectCall whose result type
		// is the function type — a return forward); anything else is a clean
		// rejection.
		returnValue, err = buildFunctionValue(unit, snapshot, fileSet, mustNode(unit, returnNode.Children[0]), scope, "entry function body return statement", width)
	} else if result.kind == types.F32 || result.kind == types.F64 {
		// A float-returning entry (a main declared to return f32/f64 — the
		// one float-returning position Float Stage A supports; float helper
		// results are rejected upstream by validateHelperSignature, so only
		// the entry's resultInfo can carry a float kind), so the return
		// value is built under the float grammar by buildFloatExpr rather
		// than buildExpr, which rejects a float-typed value. Supported
		// return shapes are a float literal or a SymbolValue naming a
		// float-typed local in scope of the same float kind.
		returnValue, err = buildFloatExpr(unit, snapshot, fileSet, returnNode.Children[0], scope, result.kind)
	} else if result.kind == types.Uint {
		// A uint-returning helper (a reachable helper whose ResultType is
		// uint — helperSignature records resultInfo{kind: types.Uint} and
		// declares the C return type uint64_t), so the return value is built
		// under the uint grammar by buildUintExpr rather than buildExpr,
		// which rejects a uint-typed checked-arithmetic tree over a
		// SizeofType operand and has no uint arithmetic. Supported return
		// shapes are a SymbolValue naming a uint-typed local in scope, a
		// uint-typed checked-arithmetic tree, a sizeof result, or an
		// integer literal.
		returnValue, err = buildUintExpr(unit, snapshot, fileSet, returnNode.Children[0], scope, width)
	} else {
		returnValue, err = buildExpr(unit, snapshot, fileSet, returnNode.Children[0], scope, width, width)
	}
	if err != nil {
		return "", err
	}
	deferText, err := buildDeferredStatements(unit, snapshot, fileSet, returnNode.DeferChain, scope, indent, context, width, unions)
	if err != nil {
		return "", err
	}
	parts := []string{}
	if preReturn != "" {
		parts = append(parts, preReturn)
	}
	if deferText != "" {
		parts = append(parts, deferText)
	}
	parts = append(parts, indent+"return "+returnValue+";")
	return strings.Join(parts, "\n"), nil
}

// buildSwitch validates and builds the C text for a switch statement used as a
// block's tail: a tir.Switch whose Children[0] is the subject expression
// (built by buildExpr for an integer subject, buildBoolExpr for a bool
// subject, buildEnumValue for a plain enum subject — the enum subject
// grammar added by 10.34 — or, since 10.35, the .tag field read of a
// tagged-union subject, whose value is a local reference or an inline variant
// construction built as the union's compound literal) and Children[1:] are
// SwitchCase nodes. Each case
// value becomes its
// own C case label; multiple SwitchCase nodes sharing the same body node ID (a
// multi-value `case v1, v2:` clause) become stacked C case labels sharing one
// body. An else arm (HasElse) maps to C's `default:`. The emitted text is:
//
//	<indent>switch (<subject>) {
//	<indent>    case <v1>:
//	<indent>    case <v2>: {
//	<indent>        <body>
//	<indent>    }
//	<indent>    default: {
//	<indent>        <body>
//	<indent>    }
//	<indent>}
//
// A SwitchCase's body child may be a Block (a multi-statement case body
// requiring braces in the source) or a bare statement (a single-statement case
// body with no braces). A Block body is built via buildBlock at the next
// nesting depth; a bare statement is built directly. Every case body must end
// in a return or a two-armed if/else whose arms each end in return — the same
// tail-statement grammar buildBlock enforces for every other block in this
// backend. A CaseValue-based case (an enum variant) is supported since 10.34
// when the subject is a plain enum type or, since 10.35, a tagged-union type:
// such a case's label is
// `case pebble_variant_<caseValue>:` (see buildCaseLabel), the case value's C
// enum constant, and its value (the variant's ordinal in the enum's declared
// order) matches the subject's own typedef by construction. Any other shape is
// a clean rejection naming what was found.
//
// buildSwitch is the thin tail-position entry point: it builds every case
// body under buildSwitchCaseBody's "must end in return" grammar. The
// fall-through twin, buildLoopSwitch, reuses this same core via
// buildSwitchStatement with fallthrough set, so the subject-building and
// case-grouping logic lives in exactly one place.
func buildSwitch(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, switchNode tir.Node, locals map[symbol.SymbolID]localInfo, depth int, width types.BuiltinKind, result resultInfo, unions map[types.TypeID]unionInfo) (string, error) {
	return buildSwitchStatement(unit, snapshot, fileSet, switchNode, locals, depth, width, result, unions, false)
}

// buildLoopSwitch validates and builds the C text for a switch statement used
// as a non-tail, fall-through statement — a leading statement in a top-level
// function body, a statement inside a loop body, an if arm, or another switch
// case body. It is the fall-through twin of buildSwitch: everything about
// subject-building and case grouping is shared through buildSwitchStatement,
// and only the per-case body requirement differs — a case body is an ordinary
// statement sequence that MAY end in a return but may also simply fall
// through (see buildLoopSwitchCaseBody), instead of buildSwitchCaseBody's
// every-arm-must-return grammar. It serves both the top-level leading
// position (buildLeadingStatement's Switch case) and the loop-body/arm
// position (buildFallthroughStatement's Switch case): the two have no
// substantive difference, since a break/continue inside a case body targets
// the nearest enclosing loop or switch by Pebble's own control-flow rules,
// and the emitted C break/continue resolves to the same construct.
func buildLoopSwitch(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, switchNode tir.Node, locals map[symbol.SymbolID]localInfo, depth int, width types.BuiltinKind, result resultInfo, unions map[types.TypeID]unionInfo) (string, error) {
	return buildSwitchStatement(unit, snapshot, fileSet, switchNode, locals, depth, width, result, unions, true)
}

// buildSwitchStatement is the shared core behind buildSwitch and
// buildLoopSwitch: it validates and builds the C text for a switch statement
// with exactly the same subject-building, case-grouping, and label emission in
// both positions. The only difference is the case-body builder selected by
// fallthrough: false selects buildSwitchCaseBody (each body must end in a
// return), true selects buildLoopSwitchCaseBody (each body is an ordinary
// fall-through statement sequence that may or may not return).
func buildSwitchStatement(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, switchNode tir.Node, locals map[symbol.SymbolID]localInfo, depth int, width types.BuiltinKind, result resultInfo, unions map[types.TypeID]unionInfo, fallThrough bool) (string, error) {
	if len(switchNode.Children) < 2 {
		return "", fmt.Errorf("switch statement has %d child(ren), want at least 2 (the subject and one case)", len(switchNode.Children))
	}
	// Build the subject expression. The subject's resolved type decides the
	// grammar: an integer subject (the entry's width) is built by buildExpr,
	// a bool subject by buildBoolExpr, a tagged-union subject by
	// buildUnionConstruction (reading its .tag field), a plain enum subject
	// by buildEnumValue.
	// The subject type is on the subject node itself (Children[0]).
	subjectNode, ok := unit.Node(switchNode.Children[0])
	if !ok {
		return "", fmt.Errorf("switch statement references invalid subject node %d", switchNode.Children[0])
	}
	// enumSubject is nonzero exactly when the subject is an enum-typed type
	// (a plain enum or a tagged union); it governs the case labels (CaseValue
	// cases become `case pebble_variant_<caseValue>:`).
	var enumSubject types.TypeID
	var enumVariants []symbol.SymbolID
	if isEnumType(unit, snapshot, subjectNode.Type) {
		enumSubject = subjectNode.Type
		info, err := resolveEnumInfo(unit, snapshot, enumSubject)
		if err != nil {
			return "", err
		}
		enumVariants = info.variants
	}
	var subjectExpr string
	var err error
	if enumSubject != 0 {
		if _, isUnion := unions[enumSubject]; isUnion {
			// A tagged-union-typed subject: its value is the union's tagged
			// struct, so the switch compares the stored discriminant — the
			// tag field — against the case labels. A reference to a
			// union-typed local (a SymbolValue) is read as
			// `pebble_local_<sym>.tag`; a variant construction used directly as
			// the subject (switch Choice.value(5), confirmed checker-reachable)
			// is built as the union's compound literal and its .tag field read
			// the same way. The case labels are unchanged: `case
			// pebble_variant_<caseValue>:` names the same enum constant the
			// stored tag holds (see buildCaseLabel).
			switch subjectNode.Kind {
			case tir.SymbolValue:
				info, declared := locals[subjectNode.Symbol]
				if !declared || info.enumType == 0 {
					return "", fmt.Errorf("switch subject references symbol %d, which is not an enum-typed local declared earlier in the body", subjectNode.Symbol)
				}
				if info.enumType != enumSubject {
					return "", fmt.Errorf("switch subject references symbol %d, a local of type %s, not the subject's union type %s", subjectNode.Symbol, describeType(snapshot, info.enumType), unionTypeName(enumSubject))
				}
				subjectExpr = fmt.Sprintf("pebble_local_%d.tag", subjectNode.Symbol)
			case tir.VariantConstruct, tir.EnumVariantValue:
				construction, buildErr := buildUnionConstruction(unit, snapshot, fileSet, subjectNode, locals, "switch subject", unions, width)
				if buildErr != nil {
					return "", buildErr
				}
				subjectExpr = construction + ".tag"
			default:
				return "", fmt.Errorf("switch subject is a %s of tagged-union type %s, want a reference to a union-typed local in scope or a union variant construction", subjectNode.Kind, unionTypeName(enumSubject))
			}
		} else {
			// A plain-enum-typed subject: a reference to an enum-typed local
			// (a SymbolValue) or a variant literal (an EnumVariantValue /
			// zero-payload VariantConstruct) — buildEnumValue handles all three.
			subjectExpr, err = buildEnumValue(unit, snapshot, fileSet, switchNode.Children[0], locals, width)
		}
	} else if isWidth(snapshot, width, subjectNode.Type) {
		subjectExpr, err = buildExpr(unit, snapshot, fileSet, switchNode.Children[0], locals, width, width)
	} else if isBool(snapshot, subjectNode.Type) {
		subjectExpr, err = buildBoolExpr(unit, snapshot, fileSet, switchNode.Children[0], locals, width)
	} else if subjectNode.Kind == tir.IntegerLiteral && subjectNode.Type == snapshot.Builtins().Int {
		// An int-typed integer literal as the subject: the checker leaves
		// it as the unanchored int builtin when no width-anchoring position
		// is available. Lowered directly as its decimal text, the same
		// precedent buildComparisonOperand and buildRangeBound use.
		text := subjectNode.Literal.IntegerNum
		if !isNonNegativeDecimal(text) {
			return "", fmt.Errorf("switch subject contains an integer literal with malformed text %q", text)
		}
		subjectExpr = text
	} else if subjectNode.Kind == tir.SymbolValue && subjectNode.Type == snapshot.Builtins().Int {
		// An int-typed SymbolValue: only reachable from hand-built IR in
		// this backend's grammar (no real source produces an int-typed
		// local), but accepted for completeness.
		if _, declared := locals[subjectNode.Symbol]; !declared {
			return "", fmt.Errorf("switch subject references symbol %d, which is not a local in scope", subjectNode.Symbol)
		}
		subjectExpr = fmt.Sprintf("pebble_local_%d", subjectNode.Symbol)
	} else {
		return "", fmt.Errorf("switch subject has type %s, want %s or bool, or an enum/tagged-union type", describeType(snapshot, subjectNode.Type), wantName(width))
	}
	if err != nil {
		return "", err
	}
	// Group case nodes by shared body node ID to detect multi-value case
	// labels (a `case 1, 2:` clause produces two SwitchCase nodes sharing
	// one body node ID). Preserve encounter order within each group and
	// across groups.
	type caseGroup struct {
		bodyID  tir.NodeID
		caseIDs []tir.NodeID
		elseID  tir.NodeID // non-zero if this group is the else/default arm
	}
	// Use a slice to preserve encounter order; a map for O(1) body-to-group
	// lookup.
	groupByBody := make(map[tir.NodeID]int)
	var groups []caseGroup
	for _, caseID := range switchNode.Children[1:] {
		caseNode, ok := unit.Node(caseID)
		if !ok {
			return "", fmt.Errorf("switch statement references invalid case node %d", caseID)
		}
		if caseNode.Kind != tir.SwitchCase {
			return "", fmt.Errorf("switch statement child is a %s, want a SwitchCase", caseNode.Kind)
		}
		if caseNode.HasElse {
			// The else/default arm has no Literal and no CaseValue.
			groups = append(groups, caseGroup{bodyID: caseNode.Children[0], elseID: caseID})
			continue
		}
		if caseNode.CaseValue != 0 {
			// An enum-variant case: its CaseValue is the variant symbol, which
			// becomes the C enum constant label. It requires an enum-typed
			// subject (the checker only produces CaseValue cases for an
			// enum/tagged-union subject — confirmed against a real fixture),
			// and the variant must be one of the subject enum's declared
			// variants.
			if enumSubject == 0 {
				return "", fmt.Errorf("switch case references enum variant symbol %d, but the subject is not an enum or tagged-union type", caseNode.CaseValue)
			}
			if !containsVariant(enumVariants, caseNode.CaseValue) {
				return "", fmt.Errorf("switch case references variant symbol %d, which is not one of the subject enum %s's declared variants", caseNode.CaseValue, enumTypeName(enumSubject))
			}
		}
		// Case body node: a SwitchCase with 1 child has the body directly as
		// Children[0]; with 2 children the body is still Children[0] (the
		// second is unused defense — the body block arrives as the direct
		// child, confirmed against real fixtures).
		var bodyID tir.NodeID
		if len(caseNode.Children) == 1 {
			bodyID = caseNode.Children[0]
		} else if len(caseNode.Children) == 2 {
			bodyID = caseNode.Children[0]
		} else {
			return "", fmt.Errorf("switch case has %d child(ren), want 1 or 2 (the body block)", len(caseNode.Children))
		}
		if idx, exists := groupByBody[bodyID]; exists {
			groups[idx].caseIDs = append(groups[idx].caseIDs, caseID)
		} else {
			idx := len(groups)
			groupByBody[bodyID] = idx
			groups = append(groups, caseGroup{bodyID: bodyID, caseIDs: []tir.NodeID{caseID}})
		}
	}
	indent := strings.Repeat("    ", depth+1)
	caseIndent := strings.Repeat("    ", depth+2)
	// A fall-through switch's case body is emitted with a trailing `break;`
	// inside its braces: in C a case body falls through into the next case
	// unless it ends in a jump, and a Pebble case body may simply fall through
	// off its last statement. The break is emitted unconditionally — after a
	// body that already ends in a return/break/continue it is unreachable C,
	// which is valid and warning-free under the mandated -Wall -Wextra -Werror
	// (no -Wunreachable-code), and it is what makes a body that does NOT end in
	// a jump terminate its case instead of leaking into the next case's
	// statements. The tail-position switch never needs it, because every case
	// body ends in a return (see buildSwitchCaseBody).
	bodyWrap := func(bodyText string) string {
		if fallThrough {
			return fmt.Sprintf("{\n%s\n%sbreak;\n%s}", bodyText, caseIndent+"    ", caseIndent)
		}
		return fmt.Sprintf("{\n%s\n%s}", bodyText, caseIndent)
	}
	var parts []string
	for _, g := range groups {
		if g.elseID != 0 {
			// The else/default arm.
			bodyText, err := buildSwitchCaseBodyOrFallthrough(unit, snapshot, fileSet, g.bodyID, locals, depth+2, width, result, unions, fallThrough)
			if err != nil {
				return "", err
			}
			parts = append(parts, fmt.Sprintf("%sdefault: %s", caseIndent, bodyWrap(bodyText)))
			continue
		}
		// Emit stacked case labels for each SwitchCase in the group.
		for _, caseID := range g.caseIDs {
			caseNode, _ := unit.Node(caseID)
			label, err := buildCaseLabel(snapshot, caseNode, width)
			if err != nil {
				return "", err
			}
			parts = append(parts, fmt.Sprintf("%s%s", caseIndent, label))
		}
		// The body is shared across all cases in the group.
		bodyText, err := buildSwitchCaseBodyOrFallthrough(unit, snapshot, fileSet, g.bodyID, locals, depth+2, width, result, unions, fallThrough)
		if err != nil {
			return "", err
		}
		parts = append(parts, fmt.Sprintf("%s%s", caseIndent, bodyWrap(bodyText)))
	}
	return fmt.Sprintf("%sswitch (%s) {\n%s\n%s}", indent, subjectExpr, strings.Join(parts, "\n"), indent), nil
}

// buildSwitchCaseBodyOrFallthrough selects the case-body builder for one
// switch case by the switch's position: a tail-position switch (fallthrough
// false) builds the body under buildSwitchCaseBody's every-arm-must-return
// grammar; a fall-through switch (fallthrough true) builds it under
// buildLoopSwitchCaseBody's may-fall-through grammar.
func buildSwitchCaseBodyOrFallthrough(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, bodyID tir.NodeID, locals map[symbol.SymbolID]localInfo, depth int, width types.BuiltinKind, result resultInfo, unions map[types.TypeID]unionInfo, fallThrough bool) (string, error) {
	if fallThrough {
		return buildLoopSwitchCaseBody(unit, snapshot, fileSet, bodyID, locals, depth, width, result, unions)
	}
	return buildSwitchCaseBody(unit, snapshot, fileSet, bodyID, locals, depth, width, result, unions)
}

// buildCaseLabel emits one C `case <value>:` label from a SwitchCase node.
// An enum-variant case (CaseValue set — a CaseValue-based case, produced by
// the checker for an enum subject) is emitted as
// `case pebble_variant_<caseValue>:`, the variant's C enum constant, whose
// value (the variant's ordinal in the enum's declared order) matches the
// subject's own typedef by construction. An integer literal is emitted as its
// decimal text; a bool literal is emitted as `0` (false) or `1` (true), since
// C treats bool as an integer type and switch cases require integral constant
// expressions. Any other case shape is a clean rejection.
func buildCaseLabel(snapshot *types.Snapshot, caseNode tir.Node, width types.BuiltinKind) (string, error) {
	if caseNode.CaseValue != 0 {
		// An enum-variant case label, emitted as the variant's C enum constant
		// name. buildSwitch has already verified the subject is a plain enum
		// and the variant belongs to it; this function only spells the label.
		return "case " + enumVariantName(caseNode.CaseValue) + ":", nil
	}
	switch caseNode.Literal.Kind {
	case tir.LiteralInteger:
		text := caseNode.Literal.IntegerNum
		if !isNonNegativeDecimal(text) {
			return "", fmt.Errorf("switch case contains an integer literal with malformed text %q", text)
		}
		litWidth, _ := resolvedBuiltin(snapshot, caseNode.Type)
		return "case " + integerLiteralText(text, litWidth) + ":", nil
	case tir.LiteralBool:
		if caseNode.Literal.Bool {
			return "case 1:", nil
		}
		return "case 0:", nil
	default:
		return "", fmt.Errorf("switch case has literal kind %s, want an integer or bool constant", caseNode.Literal.Kind)
	}
}

// buildSwitchCaseBody builds the C text for a switch case's body. The body may
// be a Block node (a multi-statement case body) or a bare statement node (a
// single-statement case body). A Block body is built via buildBlock at the next
// nesting depth — the same recursive block grammar every other block in this
// backend uses — so an arm may contain its own locals, reassignments, nested
// if/else, and loops. A bare statement body is built directly: the only
// supported bare statement is a Return (the case body must end in a return),
// built at the next nesting depth with the same expression grammar buildBlock's
// tail return uses.
func buildSwitchCaseBody(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, bodyID tir.NodeID, locals map[symbol.SymbolID]localInfo, depth int, width types.BuiltinKind, result resultInfo, unions map[types.TypeID]unionInfo) (string, error) {
	bodyNode, ok := unit.Node(bodyID)
	if !ok {
		return "", fmt.Errorf("switch case body references invalid node %d", bodyID)
	}
	if bodyNode.Kind == tir.Block {
		return buildBlock(unit, snapshot, fileSet, bodyID, locals, depth, width, result, unions)
	}
	// Bare single-statement case body: must be a Return.
	if bodyNode.Kind == tir.Return {
		if len(bodyNode.Children) != 1 {
			return "", fmt.Errorf("switch case bare return statement has %d argument(s), want exactly one expression", len(bodyNode.Children))
		}
		indent := strings.Repeat("    ", depth+1)
		var returnValue string
		var err error
		var preReturn string
		if result.isChar {
			// A char-returning function's bare single-statement case body
			// returning a char value: built under the char grammar by
			// buildCharOperand, exactly like buildBlock's tail-position Return
			// case.
			returnValue, err = buildCharOperand(unit, snapshot, fileSet, bodyNode.Children[0], locals, width)
		} else if result.isStr {
			returnValue, err = buildStrOperand(unit, snapshot, fileSet, bodyNode.Children[0], locals, width)
		} else if result.tuple != 0 || result.structType != 0 {
			preReturn, returnValue, err = buildAggregateReturnValue(unit, snapshot, fileSet, bodyNode.Children[0], locals, result, indent, width)
		} else if result.sliceType != 0 {
			// A slice-returning function's bare single-statement case body
			// returning a fresh slice construction: the construction needs the
			// two-statement temp-then-construction shape (see
			// buildSliceReturnValue), so the temp declaration is returned
			// separately and joined into the case body ahead of the final
			// return line, the same mechanical shape the deferred statements
			// below demonstrate.
			preReturn, returnValue, err = buildSliceReturnValue(unit, snapshot, fileSet, bodyNode.Children[0], locals, result, indent, width)
		} else if result.kind == types.F32 || result.kind == types.F64 {
			// A float-returning entry's bare single-statement case body
			// returning a float value: built under the float grammar by
			// buildFloatExpr, exactly like buildBlock's tail-position Return
			// case.
			returnValue, err = buildFloatExpr(unit, snapshot, fileSet, bodyNode.Children[0], locals, result.kind)
		} else {
			returnValue, err = buildExpr(unit, snapshot, fileSet, bodyNode.Children[0], locals, width, width)
		}
		if err != nil {
			return "", err
		}
		deferText, err := buildDeferredStatements(unit, snapshot, fileSet, bodyNode.DeferChain, locals, indent, "switch case body", width, unions)
		if err != nil {
			return "", err
		}
		parts := []string{}
		if preReturn != "" {
			parts = append(parts, preReturn)
		}
		if deferText != "" {
			parts = append(parts, deferText)
		}
		parts = append(parts, indent+"return "+returnValue+";")
		return strings.Join(parts, "\n"), nil
	}
	return "", fmt.Errorf("switch case body is a %s, want a Block or a Return", bodyNode.Kind)
}

// buildLoopSwitchCaseBody builds the C text for one case body of a
// fall-through switch (see buildLoopSwitch): the case-body twin of
// buildSwitchCaseBody whose only difference is that the body is an ordinary
// fall-through statement sequence rather than one that must end in a return.
// The body may be a Block node (a multi-statement case body) or a bare
// statement node (a single-statement case body with no braces). A Block body
// is built via buildFallthroughBody — the same shared "arbitrary statement
// sequence, no forced tail" builder a fall-through if's arms use, so a case
// body may declare locals, reassign enclosing ones, print, call void helpers,
// nest ifs/switches/loops, return early, or fall through off the end — and a
// bare statement body is built directly via buildFallthroughStatement. Both
// are the exact same dispatch a loop body's statements go through, so a break
// or continue inside a case body resolves (by Pebble's own control-flow
// rules) to the nearest enclosing loop or switch and is emitted as the
// equivalent C jump. The caller (buildSwitchStatement) wraps the returned
// text with a trailing `break;` inside the case's braces, since a C case body
// that does not itself end in a jump must break to avoid leaking into the
// next case (see bodyWrap there).
func buildLoopSwitchCaseBody(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, bodyID tir.NodeID, locals map[symbol.SymbolID]localInfo, depth int, width types.BuiltinKind, result resultInfo, unions map[types.TypeID]unionInfo) (string, error) {
	bodyNode, ok := unit.Node(bodyID)
	if !ok {
		return "", fmt.Errorf("switch case body references invalid node %d", bodyID)
	}
	if bodyNode.Kind == tir.Block {
		return buildFallthroughBody(unit, snapshot, fileSet, bodyID, locals, depth, width, result, unions, "switch case body")
	}
	// Bare single-statement case body: built as one fall-through statement by
	// the same dispatch a statement inside a Block case body (or a loop body,
	// or an if arm) goes through, so it may be a Store, a call, a print, a
	// return, a nested if/switch, and so on — not just a Return.
	indent := strings.Repeat("    ", depth+1)
	return buildFallthroughStatement(unit, snapshot, fileSet, bodyID, locals, indent, depth, width, result, unions, "switch case body")
}

// buildIf validates and builds the C text for a two-armed if/else block: a
// tir.If with HasElse set, whose condition is a direct integer comparison
// (buildComparison) and whose two arms are Blocks built by recursing into
// buildBlock at the next nesting depth, each receiving the same locals set the
// enclosing block threads in (buildBlock copies it per arm, so the two arms
// never see each other's declarations). The emitted text is indented at this
// block's depth:
//
//	<indent>if (<condition>) {
//	<arm body, one level deeper>
//	<indent>} else {
//	<arm body, one level deeper>
//	<indent>}
//
// Any other shape — an If without an else, an arm that is not a Block, or a
// block with the wrong child count — is a clean rejection naming what was
// found.
func buildIf(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, ifNode tir.Node, locals map[symbol.SymbolID]localInfo, depth int, width types.BuiltinKind, result resultInfo, unions map[types.TypeID]unionInfo) (string, error) {
	if !ifNode.HasElse {
		return "", fmt.Errorf("entry function body ends with an if without an else; this backend only supports the two-armed if/else whose arms each end in one return, found an if with no else")
	}
	if len(ifNode.Children) != 3 {
		return "", fmt.Errorf("entry function body ends with an if with %d child(ren), want exactly 3 (condition, then-arm, else-arm)", len(ifNode.Children))
	}
	condition, err := buildCondition(unit, snapshot, fileSet, ifNode.Children[0], locals, width)
	if err != nil {
		return "", err
	}
	thenText, err := buildBlock(unit, snapshot, fileSet, ifNode.Children[1], locals, depth+1, width, result, unions)
	if err != nil {
		return "", err
	}
	elseText, err := buildBlock(unit, snapshot, fileSet, ifNode.Children[2], locals, depth+1, width, result, unions)
	if err != nil {
		return "", err
	}
	indent := strings.Repeat("    ", depth+1)
	return fmt.Sprintf("%sif (%s) {\n%s\n%s} else {\n%s\n%s}", indent, condition, thenText, indent, elseText, indent), nil
}

// buildWhile validates and builds the C text for a while loop statement: a
// tir.While with exactly two children — Children[0] the condition, a direct
// integer comparison built by buildComparison (the same six operators and the
// same int-literal-in-condition handling an if condition uses), and Children[1]
// the loop body, which must be a Block built by buildLoopBody at the next
// nesting depth. The loop body is its own scope: buildLoopBody clones the
// incoming locals before extending them, so a local declared inside the loop is
// invisible outside it and re-initializes each C iteration (correct C
// block-scope behavior). The emitted text is indented at this block's depth,
// mirroring buildIf exactly:
//
//	<indent>while (<condition>) {
//	<loop body statements, one level deeper>
//	<indent>}
//
// Any other shape — a wrong child count, or a body that is not a Block — is a
// clean rejection naming what was found.
func buildWhile(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, whileNode tir.Node, locals map[symbol.SymbolID]localInfo, depth int, width types.BuiltinKind, result resultInfo, unions map[types.TypeID]unionInfo) (string, error) {
	if len(whileNode.Children) != 2 {
		return "", fmt.Errorf("entry function body block while loop has %d child(ren), want exactly 2 (the condition, then the loop body)", len(whileNode.Children))
	}
	condition, err := buildCondition(unit, snapshot, fileSet, whileNode.Children[0], locals, width)
	if err != nil {
		return "", err
	}
	bodyText, err := buildLoopBody(unit, snapshot, fileSet, whileNode.Children[1], locals, depth+1, width, result, unions)
	if err != nil {
		return "", err
	}
	indent := strings.Repeat("    ", depth+1)
	return fmt.Sprintf("%swhile (%s) {\n%s\n%s}", indent, condition, bodyText, indent), nil
}

// buildRangeLoop validates and builds the C text for a range loop statement: a
// tir.RangeLoop with exactly three children — Children[0] the start value,
// Children[1] the end value, and Children[2] the loop body, which must be a
// Block built by buildLoopBody at the next nesting depth — and a bound
// iterator (`loop start..end : name { ... }`) whose own symbol.SymbolID is
// recorded on the node's Symbol field. The loop lowers directly to a C for
// loop whose loop counter IS the iterator, the representation this backend's
// design decides:
//
//	<indent>for (int32_t pebble_local_<iterSym> = <start>; pebble_local_<iterSym> < <end>; pebble_local_<iterSym>++) {
//	<loop body statements, one level deeper>
//	<indent>}
//
// `<` for the exclusive form (`..`), `<=` for the inclusive form (`..=`),
// from the node's RangeInclusive field. The iterator's own C type is the
// entry's resolved width (cType(width)); the start/end are ordinary integer
// expressions built by buildRangeBound (an int-typed integer literal lowered
// as its decimal text, anything else via buildExpr at the entry's width — the
// checker leaves the bounds as the unanchored int builtin whenever the
// iterator is never used in a width-anchoring position, confirmed against a
// real fixture). Before the body is built, the loop's own locals scope is
// seeded with the iterator (scope[iteratorSymbol] = localInfo{kind: width}),
// exactly the seeding pattern a helper's parameters already use, so a
// SymbolValue reference to the iterator inside the body resolves through the
// existing machinery unchanged. The loop body is built by the exact same
// buildLoopBody buildWhile uses, one level deeper; the body is its own scope
// (buildLoopBody clones), so nothing the body declares leaks outside, while
// the seeded iterator remains visible inside. break/continue inside the body
// are handled by buildLoopBody's own Break/Continue cases — plain C
// break/continue already target the nearest enclosing loop, which the emitted
// for loop is (confirmed against a real fixture that a Break/Continue inside
// a range loop body names the range loop's own Region as its Target). Any
// other shape — a wrong child count, an unbound loop (`loop start..end {
// ... }` with no `: name`, which has no way to be observed from inside and is
// low-value), or a body that is not a Block — is a clean rejection naming
// what was found.
func buildRangeLoop(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, rangeNode tir.Node, locals map[symbol.SymbolID]localInfo, depth int, width types.BuiltinKind, result resultInfo, unions map[types.TypeID]unionInfo) (string, error) {
	if len(rangeNode.Children) != 3 {
		return "", fmt.Errorf("entry function body block range loop has %d child(ren), want exactly 3 (the start value, the end value, then the loop body)", len(rangeNode.Children))
	}
	if rangeNode.Symbol == 0 {
		// The unbound form (`loop start..end { ... }`, no `: name`) carries no
		// iterator symbol on the node (Symbol stays zero — confirmed against a
		// real fixture) and nothing for the body to read the loop's current
		// value from, so it is low-value and rejected cleanly rather than
		// lowered with a synthetic counter the source never names.
		return "", fmt.Errorf("entry function body block contains an unbound range loop (loop start..end { ... } with no `: name` iterator); only the bound `loop start..end : name { ... }` form is supported")
	}
	startText, err := buildRangeBound(unit, snapshot, fileSet, rangeNode.Children[0], locals, width)
	if err != nil {
		return "", err
	}
	endText, err := buildRangeBound(unit, snapshot, fileSet, rangeNode.Children[1], locals, width)
	if err != nil {
		return "", err
	}
	// The loop's own scope is a clone of the enclosing set seeded with the
	// iterator as an ordinary local of the loop's bound type — the same
	// seeding pattern a helper's parameters use — so a SymbolValue reference
	// to the iterator inside the body (and a Store reassigning it, were the
	// checker to permit one) resolves through the existing machinery with zero
	// changes to buildExpr. The clone discipline keeps the iterator and
	// anything the body declares out of this block's own scope map. The
	// iterator's type is the bounds' own type, not necessarily the entry's
	// width: a range loop's two bounds always share one concrete integer type
	// (the checker anchors the start bound to the end bound's type), and when
	// that type is uint (std/hmap.peb's `loop 0..new_cap : i { ... }` over a
	// uint capacity) the iterator must be a uint64_t declared at uint's own C
	// type — declaring it at the entry width and comparing it against a
	// uint64_t bound would trip -Wsign-compare under the mandated -Wall
	// -Wextra -Werror. The body's references to the iterator then resolve at
	// that same type (uint routes through buildUintExpr, any other
	// non-entry-width integer through buildExpr at its own width).
	boundType := width
	if startNode, startOK := unit.Node(rangeNode.Children[0]); startOK {
		if resolved, integer := resolvedBuiltin(snapshot, startNode.Type); integer && cType(resolved) != "" {
			boundType = resolved
		}
	}
	loopScope := cloneLocals(locals)
	loopScope[rangeNode.Symbol] = localInfo{kind: boundType}
	bodyText, err := buildLoopBody(unit, snapshot, fileSet, rangeNode.Children[2], loopScope, depth+1, width, result, unions)
	if err != nil {
		return "", err
	}
	rangeOp := "<"
	if rangeNode.RangeInclusive {
		rangeOp = "<="
	}
	indent := strings.Repeat("    ", depth+1)
	return fmt.Sprintf("%sfor (%s pebble_local_%d = %s; pebble_local_%d %s %s; pebble_local_%d++) {\n%s\n%s}", indent, cType(boundType), rangeNode.Symbol, startText, rangeNode.Symbol, rangeOp, endText, rangeNode.Symbol, bodyText, indent), nil
}

// buildRangeBound builds one bound (the start or the end) of a range loop. A
// bound is an ordinary integer expression built by buildExpr at the entry's
// resolved width — a literal, a local reference, arithmetic, a helper call,
// anything buildExpr already builds. The one exception mirrors
// buildComparisonOperand: when the range loop's iterator is never used in a
// width-anchoring position, the checker leaves the bounds as the snapshot's
// unanchored int builtin (confirmed against real fixtures — `loop 0..3 : i {
// if i == 2 { ... } }` has int-typed bounds while `loop 0..3 : i { sum = sum
// + i; }` anchors both to i32), so an int-typed integer literal is lowered
// directly as its decimal text and an int-typed SymbolValue — only ever a
// range-loop iterator in this backend's grammar, e.g. a nested loop whose
// bound reads the outer loop's iterator — as its pebble_local_<symbol> name
// (the iterator is always declared in C at the entry's width, so the name is
// the correct C lvalue; assigning `0` into the iterator's int32_t and
// comparing it against `3` is trivially valid C). An int-typed non-literal
// bound — int-typed arithmetic from a loop whose iterator is never used —
// reaches buildExpr and is rejected there by its width gate, exactly as the
// rest of this backend treats int-typed arithmetic.
func buildRangeBound(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, id tir.NodeID, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind) (string, error) {
	node, ok := unit.Node(id)
	if !ok {
		return "", fmt.Errorf("entry function body block range loop references invalid bound node %d", id)
	}
	if node.Kind == tir.IntegerLiteral && node.Type == snapshot.Builtins().Int {
		text := node.Literal.IntegerNum
		if !isNonNegativeDecimal(text) {
			return "", fmt.Errorf("entry function body block range loop bound contains an integer literal with malformed text %q", text)
		}
		return text, nil
	}
	if node.Kind == tir.SymbolValue && node.Type == snapshot.Builtins().Int {
		if _, declared := locals[node.Symbol]; !declared {
			return "", fmt.Errorf("entry function body block range loop bound references symbol %d, which is not a local in scope", node.Symbol)
		}
		return fmt.Sprintf("pebble_local_%d", node.Symbol), nil
	}
	// Any other bound is built at its OWN resolved integer width rather than
	// the ambient entry width, exactly as buildComparisonOperand and
	// buildCallArgument resolve their operands: a range loop's two bounds
	// always share one concrete integer type (the checker anchors the start
	// bound to the end bound's type), and that type need not be the entry's
	// width — std/hmap.peb's rehash/with_capacity loop over a uint-typed
	// capacity (`loop 0..new_cap : i { ... }`), where the checker anchors
	// the `0` start bound to uint, so the literal reaches this path as a
	// uint-typed IntegerLiteral that buildExpr's entry-width gate would
	// reject. uint is deliberately routed through buildUintExpr (the
	// dedicated uint grammar), excluded from the general buildExpr path the
	// same way buildCallArgument/buildComparisonOperand exclude it; any
	// other non-entry-width integer bound flows through buildExpr at the
	// bound's own resolved width.
	boundWidth, integerBound := resolvedBuiltin(snapshot, node.Type)
	if integerBound && cType(boundWidth) != "" && !isUint(snapshot, node.Type) {
		return buildExpr(unit, snapshot, fileSet, id, locals, boundWidth, width)
	}
	if isUint(snapshot, node.Type) {
		return buildUintExpr(unit, snapshot, fileSet, id, locals, width)
	}
	return buildExpr(unit, snapshot, fileSet, id, locals, width, width)
}

// buildFor validates and builds the C text for a classic for loop statement
// (a tir.For): `for <init>; <cond>; <update> { <body> }` with each of the
// three clauses individually optional, lowering directly to a C for loop with
// the same three clauses:
//
//	<indent>for (<init>; <cond>; <update>) {
//	<loop body statements, one level deeper>
//	<indent>}
//
// For.Children is a variable-length list the checker builds by appending, in
// this fixed relative order, only the clauses actually present — the
// initializer, then the condition, then the update — with the body a Block
// always last (confirmed against real fixtures for every clause-presence
// combination). The clauses are disambiguated by their node categories, not
// by position assumptions: the condition, when present, is the unique
// CategoryValue child among the non-body children, and every other clause
// child is a CategoryNonvalue. The initializer, when present, must be a
// single Initialize declaring a local of the entry's width or bool (built by
// buildForInitClause) — a Store/CompoundStore/ExpressionStatement initializer
// is reachable from real source (`for step = 0; ...`, `for x += 1; ...`,
// `for x + 1; ...`) and is a clean rejection, matching the backend's rule
// that only an Initialize declares a local. The update, when present, must be
// a single Store reassigning a local already in scope or a single CompoundStore
// (a compound assignment such as `step += 1` or a postfix `step++`, built by
// buildForUpdateClause through buildStoreCore / buildCompoundStore) — a
// discarded-expression update (`for x + 1; ...`) is reachable from real source
// and is a clean rejection. With no condition present the checker's fixed relative
// order leaves at most `[initializer?] [update?]`, so an Initialize child is
// the initializer and a Store/CompoundStore child is the update; a lone
// no-condition Store is treated as the update (the in-scope update-only shape
// `for ; ; update {
// ... }`) — note this makes a no-condition Store *initializer* (`for step =
// 0;; { ... }`, out of scope but reachable) structurally indistinguishable
// from update-only and silently lowered as the update, a real ambiguity with
// no IR-level way to tell them apart (the For node carries only Region and
// Children). The condition is built by the exact same buildCondition an
// if/while condition uses. The body is built by the exact same buildLoopBody
// a while/range loop uses, one level deeper, against a cloned scope seeded
// with the initializer's local if it declares one — so a SymbolValue
// reference to the initializer's local inside the condition, update, or body
// resolves through the existing machinery unchanged, mirroring how a range
// loop seeds its iterator. If the initializer declares a local, a `(void)
// pebble_local_<symbol>;` cast is emitted as the body's first statement, the
// same -Wunused-variable defense every declared local gets (confirmed: cc
// fires -Wunused-variable under -Wall -Wextra -Werror for a for-init local
// never referenced anywhere, and the cast is a no-op when it is). The body is
// its own scope (buildLoopBody clones), so nothing the body declares leaks
// outside, while the seeded initializer local remains visible inside.
// break/continue inside the body are handled by buildLoopBody's own
// Break/Continue cases — plain C break/continue already target the nearest
// enclosing loop, which the emitted for loop is (the same confirmation a
// range loop made). Any other shape — an ambiguous clause list, an
// out-of-scope initializer or update, a missing or non-Block body — is a
// clean rejection naming what was found.
func buildFor(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, forNode tir.Node, locals map[symbol.SymbolID]localInfo, depth int, width types.BuiltinKind, result resultInfo, unions map[types.TypeID]unionInfo) (string, error) {
	if len(forNode.Children) < 1 || len(forNode.Children) > 4 {
		return "", fmt.Errorf("entry function body block for loop has %d child(ren), want 1 to 4 (the optional initializer, condition, and update clauses, then the loop body)", len(forNode.Children))
	}
	bodyID := forNode.Children[len(forNode.Children)-1]
	bodyNode, ok := unit.Node(bodyID)
	if !ok {
		return "", fmt.Errorf("entry function body block for loop references invalid body node %d", bodyID)
	}
	if bodyNode.Kind != tir.Block {
		return "", fmt.Errorf("entry function body block for loop body is a %s, want a Block", bodyNode.Kind)
	}
	clauses := forNode.Children[:len(forNode.Children)-1]
	// The condition, when present, is the unique CategoryValue child among the
	// non-body clauses (confirmed against real fixtures for every
	// clause-presence combination). Every other clause child is a
	// CategoryNonvalue (an Initialize/Store/CompoundStore/ExpressionStatement),
	// so a unique CategoryValue can only be the condition, and it appears in
	// the position matching its role: the checker appends clauses in the fixed
	// relative order initializer, condition, update.
	condIndex := -1
	for i, clauseID := range clauses {
		clause, clauseOK := unit.Node(clauseID)
		if !clauseOK {
			return "", fmt.Errorf("entry function body block for loop references invalid clause node %d", clauseID)
		}
		category, categoryOK := tir.CategoryOf(clause.Kind)
		if !categoryOK {
			return "", fmt.Errorf("entry function body block for loop clause %d has unknown node kind %s", i, clause.Kind)
		}
		if category == tir.CategoryValue {
			if condIndex >= 0 {
				return "", fmt.Errorf("entry function body block for loop has %d value clause(s), want at most one (the condition)", len(clauses))
			}
			condIndex = i
		}
	}
	loopScope := cloneLocals(locals)
	var initText, initPre, condText, updateText, updatePre string
	var initSymbol symbol.SymbolID
	var updateID tir.NodeID
	if condIndex >= 0 {
		// The condition is present. The initializer slot is the at-most-one
		// nonvalue clause before it, and the update slot the at-most-one
		// nonvalue clause after it. A nonvalue clause before the condition is
		// the initializer and must be an Initialize; a nonvalue clause after
		// it is the update and must be a Store.
		if condIndex > 1 {
			return "", fmt.Errorf("entry function body block for loop has %d clause(s) before its condition, want at most one (the initializer)", condIndex)
		}
		if len(clauses)-condIndex-1 > 1 {
			return "", fmt.Errorf("entry function body block for loop has %d clause(s) after its condition, want at most one (the update)", len(clauses)-condIndex-1)
		}
		if condIndex == 1 {
			pre, text, symbol, err := buildForInitClause(unit, snapshot, fileSet, clauses[0], loopScope, width)
			if err != nil {
				return "", err
			}
			initText = text
			initPre = pre
			initSymbol = symbol
		}
		cond, err := buildCondition(unit, snapshot, fileSet, clauses[condIndex], loopScope, width)
		if err != nil {
			return "", err
		}
		condText = cond
		if len(clauses)-condIndex-1 == 1 {
			updateID = clauses[len(clauses)-1]
			pre, text, err := buildForUpdateClause(unit, snapshot, fileSet, clauses[len(clauses)-1], loopScope, width, unions)
			if err != nil {
				return "", err
			}
			updateText = text
			updatePre = pre
		}
	} else {
		// No condition: the checker's fixed relative order leaves at most
		// `[initializer?] [update?]`, so an Initialize child is the
		// initializer and a Store child is the update.
		switch len(clauses) {
		case 0:
			// All three clauses absent — an infinite loop from the header's
			// perspective (`for (; ; )`).
		case 1:
			clause, _ := unit.Node(clauses[0])
			switch clause.Kind {
			case tir.Initialize:
				pre, text, symbol, err := buildForInitClause(unit, snapshot, fileSet, clauses[0], loopScope, width)
				if err != nil {
					return "", err
				}
				initText = text
				initPre = pre
				initSymbol = symbol
			case tir.Store:
				// A lone no-condition Store is the update-only shape
				// (`for ; ; update { ... }`), the in-scope interpretation.
				// A no-condition Store *initializer* (`for step = 0;; { ...
				// }`) is structurally indistinguishable from it (confirmed
				// against real fixtures) and is documented as a real
				// ambiguity: it lowers as the update, never guessed as
				// something else. A lone no-condition CompoundStore is the
				// same update-only shape for a compound assignment or postfix
				// increment (`for ; ; i++ { ... }`).
				updateID = clauses[0]
				pre, text, err := buildForUpdateClause(unit, snapshot, fileSet, clauses[0], loopScope, width, unions)
				if err != nil {
					return "", err
				}
				updateText = text
				updatePre = pre
			case tir.CompoundStore:
				updateID = clauses[0]
				pre, text, err := buildForUpdateClause(unit, snapshot, fileSet, clauses[0], loopScope, width, unions)
				if err != nil {
					return "", err
				}
				updateText = text
				updatePre = pre
			default:
				return "", fmt.Errorf("entry function body block for loop with no condition has a %s clause, want an Initialize (a local declaration), a Store (an update), or a CompoundStore (an update)", clause.Kind)
			}
		case 2:
			initClause, _ := unit.Node(clauses[0])
			updateClause, _ := unit.Node(clauses[1])
			if initClause.Kind != tir.Initialize {
				return "", fmt.Errorf("entry function body block for loop with no condition leads with a %s clause, want an Initialize (the initializer declares a local)", initClause.Kind)
			}
			if updateClause.Kind != tir.Store && updateClause.Kind != tir.CompoundStore {
				return "", fmt.Errorf("entry function body block for loop with no condition follows the initializer with a %s clause, want a Store or CompoundStore (the update)", updateClause.Kind)
			}
			pre, text, symbol, err := buildForInitClause(unit, snapshot, fileSet, clauses[0], loopScope, width)
			if err != nil {
				return "", err
			}
			initText = text
			initPre = pre
			initSymbol = symbol
			updateID = clauses[1]
			pre, text, err = buildForUpdateClause(unit, snapshot, fileSet, clauses[1], loopScope, width, unions)
			if err != nil {
				return "", err
			}
			updateText = text
			updatePre = pre
		default:
			return "", fmt.Errorf("entry function body block for loop with no condition has %d clause(s), want at most two (an initializer and an update)", len(clauses))
		}
	}
	bodyText, err := buildLoopBody(unit, snapshot, fileSet, bodyID, loopScope, depth+1, width, result, unions)
	if err != nil {
		return "", err
	}
	if initSymbol != 0 {
		// The initializer declared a local whose C declaration lives in the
		// for-header, so the (void) cast against -Wunused-variable must go
		// inside the loop body — the for-init local's C scope is the loop
		// itself, and cc fires the warning for a for-init local never
		// referenced anywhere (confirmed with a real compile). The cast is the
		// body's first statement, at the body's own statement indent.
		bodyIndent := strings.Repeat("    ", depth+2)
		bodyText = bodyIndent + fmt.Sprintf("(void)pebble_local_%d;", initSymbol) + "\n" + bodyText
	}
	indent := strings.Repeat("    ", depth+1)
	forText := fmt.Sprintf("%sfor (%s; %s; %s) {\n%s\n%s}", indent, initText, condText, updateText, bodyText, indent)
	var preLines string
	if initPre != "" {
		// The initializer's source-integer temp lives in the enclosing block,
		// not the for-header (a header clause is a single C declaration, and
		// the int64_t temp and the optional-typed local have different C
		// types), so it is emitted as a statement before the for — the same
		// leading position updatePre uses.
		preLines += indent + initPre + "\n"
	}
	if updatePre != "" {
		updateNode, ok := unit.Node(updateID)
		if !ok || len(updateNode.Children) == 0 {
			return "", fmt.Errorf("entry function body block for loop update references invalid compound place")
		}
		lvalue, _, err := buildPlaceLValue(unit, snapshot, fileSet, updateNode.Children[0], loopScope, width)
		if err != nil {
			return "", err
		}
		tempName := fmt.Sprintf("pebble_compound_ptr_%d", updateID)
		updatePre = fmt.Sprintf("%s *%s;", cType(width), tempName)
		updateText = fmt.Sprintf("%s = &(%s), %s", tempName, lvalue, updateText)
		forText = fmt.Sprintf("%sfor (%s; %s; %s) {\n%s\n%s}", indent, initText, condText, updateText, bodyText, indent)
		preLines += indent + updatePre + "\n"
	}
	if preLines != "" {
		return preLines + forText, nil
	}
	return forText, nil
}

// buildForInitClause validates and builds the C init-clause text for a classic
// for loop's initializer: `<cType> pebble_local_<symbol> = <expr>` — a C
// declaration with no leading indent, no statement-terminating newline, and no
// trailing `;` of its own (the for-header `for (<init>; <cond>; <update>)`'s
// first `;` is what terminates the init clause). The initializer must be a
// single Initialize (a local declaration) — a Store (an assignment), a
// CompoundStore, or a discarded ExpressionStatement initializer are all
// reachable from real source but out of scope and cleanly rejected, matching
// the backend's rule that only an Initialize declares a local. The declared
// local may be of any integer width (not just the entry's resolved width),
// bool, or char — the same scalar grammars a bare Initialize supports —
// validated and emitted by buildScalarInitializeCore, which also records the
// local in the caller's loop scope so the condition, update, and body can
// reference it. Returns the clause text and the declared symbol (so buildFor
// can emit the (void) cast as the body's first statement), plus, for an
// OptionalIntegerToEnum initializer, a pre statement text that must be emitted
// BEFORE the for statement — the source integer's one-time-evaluation temp
// declaration, which a single for-header declaration cannot hold alongside the
// optional-typed local (the two have different C types, and a for-header
// declaration is a single C declaration) — mirroring the updatePre mechanism
// buildCompoundStore uses.
func buildForInitClause(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, id tir.NodeID, scope map[symbol.SymbolID]localInfo, width types.BuiltinKind) (string, string, symbol.SymbolID, error) {
	statement, ok := unit.Node(id)
	if !ok {
		return "", "", 0, fmt.Errorf("entry function body block for loop initializer references invalid node %d", id)
	}
	if statement.Kind != tir.Initialize {
		return "", "", 0, fmt.Errorf("entry function body block for loop initializer is a %s, want a local declaration (an Initialize); a for-loop initializer must declare a local of %s or bool", statement.Kind, wantName(width))
	}
	if len(statement.Children) != 1 {
		return "", "", 0, fmt.Errorf("entry function body block for loop initializer initializes %d value(s), want exactly one expression", len(statement.Children))
	}
	if _, declared := scope[statement.Symbol]; declared {
		return "", "", 0, fmt.Errorf("entry function body block for loop initializer declares local %d more than once", statement.Symbol)
	}
	initValue, ok := unit.Node(statement.Children[0])
	if !ok {
		return "", "", 0, fmt.Errorf("entry function body block for loop initializer references invalid value node %d", statement.Children[0])
	}
	if initValue.Kind == tir.OptionalIntegerToEnum {
		// An integer-to-optional-enum cast as the for-loop initializer
		// (`for var c ?Color = 5 as ?Color; ...`): the cast is supported only
		// in a local-declaration initializer, and the for-loop initializer IS
		// a local declaration — but the header clause must be a single C
		// declaration, and the cast needs the source hoisted into an int64_t
		// temp of a different C type (see
		// buildOptionalIntegerToEnumDeclaration), so the temp's declaration is
		// returned as a pre statement buildFor emits before the for statement,
		// and the header clause is the optional-typed local's own declaration
		// reading that temp.
		pre, core, err := buildOptionalIntegerToEnumDeclaration(unit, snapshot, fileSet, statement, initValue, scope, "entry function body block for loop initializer", id, width)
		if err != nil {
			return "", "", 0, err
		}
		scope[statement.Symbol] = localInfo{optional: initValue.Type}
		return pre, core, statement.Symbol, nil
	}
	pre, core, err := buildScalarInitializeCore(unit, snapshot, fileSet, statement, initValue, scope, "entry function body block for loop initializer", width)
	if err != nil {
		return "", "", 0, err
	}
	return pre, core, statement.Symbol, nil
}

// buildForUpdateClause validates and builds the C update-clause text for a
// classic for loop's update: `pebble_local_<symbol> = <expr>` with no leading
// indent and no trailing `;` — the for statement's own header syntax supplies
// the semicolons, so the update clause is a bare assignment expression, not a
// full C statement (the equivalent block-level Store's trailing `;` is
// deliberately omitted). The update must be a single Store reassigning a
// local already in scope, validated and emitted by buildStoreCore against the
// local's own declared type (the entry's width or bool), or a single
// CompoundStore (a compound assignment such as `step += 1` or a postfix
// `step++`), validated and emitted by buildCompoundStore; a discarded-
// expression update (`for x + 1; ...`) is reachable from real source but out
// of scope and cleanly rejected.
func buildForUpdateClause(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, id tir.NodeID, scope map[symbol.SymbolID]localInfo, width types.BuiltinKind, unions map[types.TypeID]unionInfo) (string, string, error) {
	statement, ok := unit.Node(id)
	if !ok {
		return "", "", fmt.Errorf("entry function body block for loop update references invalid node %d", id)
	}
	if statement.Kind != tir.Store && statement.Kind != tir.CompoundStore {
		return "", "", fmt.Errorf("entry function body block for loop update is a %s, want a Store (a reassignment of a local already in scope) or a CompoundStore (a compound assignment or postfix increment/decrement); a for-loop update must be a single assignment", statement.Kind)
	}
	if statement.Kind == tir.CompoundStore {
		return buildCompoundStore(unit, snapshot, fileSet, id, statement, scope, "entry function body block for loop update", width)
	}
	core, err := buildStoreCore(unit, snapshot, fileSet, statement, scope, "entry function body block for loop update", width, unions)
	return "", core, err
}

// buildScalarInitializeCore builds the declaration text for a scalar local at
// its own declared builtin type — any integer width (not just the entry's
// resolved width), bool, char, or, since Float Stage A, a float — WITHOUT the
// leading indent and WITHOUT the
// trailing `;` (and without the trailing (void) cast) a full block-level
// declaration statement gets: `<cType> pebble_local_<symbol> = <expr>`. It is
// the scalar tail of the Initialize dispatch, shared by buildLeadingStatement
// (which prepends the indent and appends `;` plus the (void) cast to form the
// full statement) and buildForInitClause (which uses the core as the
// for-header init clause, where the for statement's own header syntax supplies
// the terminating `;`), so the integer-width/bool/char/float validation, the
// buildExpr/buildBoolExpr/buildCharOperand/buildFloatExpr dispatch, and the
// scope recording
// live in exactly one place. An integer local is emitted at its own declared
// width (cType(kind)) and its initializer is built by buildExpr at that same
// width — so e.g. an i64 local inside an i32 function is an int64_t whose
// initializer is built at i64, not i32; a bool local is emitted as a C bool
// (built by buildBoolExpr); a char local is emitted as the fixed C int32_t
// (built by buildCharOperand); a float local is emitted at its own declared
// float type (floatCType(kind) — float for f32, double for f64) and its
// initializer is built by buildFloatExpr at that same kind. Anything else — a
// tuple/array/optional/struct/
// str local — is a clean rejection naming the type, matching
// buildLeadingStatement's own rule. On success the local is recorded in scope
// (localInfo{kind: kind} for an integer or float, localInfo{kind: types.Bool} for a
// bool, or localInfo{isChar: true} for a char) so a later reference or
// reassignment resolves against the same type.
func buildScalarInitializeCore(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, statement, initValue tir.Node, scope map[symbol.SymbolID]localInfo, context string, width types.BuiltinKind) (string, string, error) {
	// Check for a CheckedOptionalUnwrap whose child is a DirectCall or
	// MethodCall — a force-unwrap of a call result (`let v = m.get(5)!;`).
	// The call result must be materialized into a temp C variable so the
	// unwrap helper can reference its .has_value and .value fields without
	// evaluating the call twice. The temp is declared as the optional's own
	// C struct type and lives in the enclosing block scope, threaded as a
	// pre-statement before the local's own declaration.
	if initValue.Kind == tir.CheckedOptionalUnwrap && len(initValue.Children) == 1 {
		if child, ok := unit.Node(initValue.Children[0]); ok && (child.Kind == tir.DirectCall || child.Kind == tir.MethodCall) {
			kind, ok := resolvedBuiltin(snapshot, initValue.Type)
			if !ok || cType(kind) == "" {
				return "", "", fmt.Errorf("%s declares a local of type %s, want an integer type, bool, char, or float", context, describeType(snapshot, initValue.Type))
			}
			callPre, callExpr, err := buildDirectCallWithPre(unit, snapshot, fileSet, child, scope, width)
			if err != nil {
				return "", "", err
			}
			optionalType := child.Type
			tempName := fmt.Sprintf("pebble_optional_temp_%d", statement.Symbol)
			pre := fmt.Sprintf("%s %s = %s;", optionalTypeName(optionalType), tempName, callExpr)
			if callPre != "" {
				pre = callPre + "\n" + pre
			}
			unwrapSuffix := optionalUnwrapSuffix(snapshot, initValue.Type)
			if unwrapSuffix == "" {
				return "", "", fmt.Errorf("%s declares a local of type %s initialized from an optional unwrap with a payload type %s, which has no runtime unwrap helper", context, describeType(snapshot, initValue.Type), describeType(snapshot, initValue.Type))
			}
			core := fmt.Sprintf("%s pebble_local_%d = pebble_rt_checked_unwrap_%s(%s.has_value, %s.value, %s)", cType(kind), statement.Symbol, unwrapSuffix, tempName, tempName, buildSourceLoc(fileSet, initValue.Span))
			scope[statement.Symbol] = localInfo{kind: kind}
			return pre, core, nil
		}
	}
	kind, ok := resolvedBuiltin(snapshot, initValue.Type)
	if !ok {
		return "", "", fmt.Errorf("%s local declaration declares a local of type %s, want an integer type, bool, char, or float", context, describeType(snapshot, initValue.Type))
	}
	switch kind {
	case types.Bool:
		// A bool local: emitted as a C bool, its value built by buildBoolExpr
		// (the bool grammar is genuinely different from the integer one).
		initExpr, err := buildBoolExpr(unit, snapshot, fileSet, statement.Children[0], scope, width)
		if err != nil {
			return "", "", err
		}
		scope[statement.Symbol] = localInfo{kind: types.Bool}
		return "", fmt.Sprintf("bool pebble_local_%d = %s", statement.Symbol, initExpr), nil
	case types.Char:
		// A char local: emitted as the fixed C int32_t (the language's char
		// is a full Unicode scalar value, always int32_t regardless of the
		// entry's resolved width), its value built by buildCharOperand (a char
		// literal, a reference to an in-scope char-typed local, or a call to a
		// char-returning helper). The scope entry records isChar so a later
		// reference or reassignment is validated and emitted as a char.
		initExpr, err := buildCharOperand(unit, snapshot, fileSet, statement.Children[0], scope, width)
		if err != nil {
			return "", "", err
		}
		scope[statement.Symbol] = localInfo{isChar: true}
		return "", fmt.Sprintf("int32_t pebble_local_%d = %s", statement.Symbol, initExpr), nil
	case types.F32, types.F64:
		// A float local (f32 or f64, Stage A): emitted at the local's own
		// declared float C type (floatCType — float for f32, double for f64),
		// its value built by buildFloatExpr (a float literal or a reference to
		// an in-scope float-typed local of the same kind). The scope entry
		// records the local's own float kind (localInfo{kind: kind}, exactly
		// as an integer local records its own width) so a later reference or
		// reassignment is validated and emitted as that kind's float.
		initExpr, err := buildFloatExpr(unit, snapshot, fileSet, statement.Children[0], scope, kind)
		if err != nil {
			return "", "", err
		}
		scope[statement.Symbol] = localInfo{kind: kind}
		return "", fmt.Sprintf("%s pebble_local_%d = %s", floatCType(kind), statement.Symbol, initExpr), nil
	case types.Uint:
		// A uint local: emitted at uint's own C type (cType — uint64_t, the
		// platform-native pointer-width type), its value built by buildUintExpr
		// (the dedicated uint grammar: sizeof results, checked uint
		// arithmetic, a reference to an in-scope uint-typed local, an
		// integer literal, or — the std/hmap.peb rehash/with_capacity shape —
		// a uint-typed checked-arithmetic tree over a SizeofType operand,
		// which buildExpr's general grammar rejects). This mirrors how every
		// other uint value position routes (call arguments, comparison
		// operands, struct field construction, optional payloads); before
		// this fix a uint local's initializer fell through to buildExpr,
		// which has no SizeofType case. The scope entry records the uint kind
		// (localInfo{kind: types.Uint}, exactly as helperSignature seeds a
		// uint parameter) so a later reference or reassignment is validated
		// and emitted as a uint.
		initExpr, err := buildUintExpr(unit, snapshot, fileSet, statement.Children[0], scope, width)
		if err != nil {
			return "", "", err
		}
		scope[statement.Symbol] = localInfo{kind: types.Uint}
		return "", fmt.Sprintf("%s pebble_local_%d = %s", cType(types.Uint), statement.Symbol, initExpr), nil
	}
	if cType(kind) == "" {
		// Anything that is not bool/char and not an integer builtin the
		// backend emits (str, void) is a clean rejection naming the type,
		// matching buildLeadingStatement's own rule.
		return "", "", fmt.Errorf("%s local declaration declares a local of type %s, want an integer type, bool, char, or float", context, describeType(snapshot, initValue.Type))
	}
	// An integer local of any builtin width, not just the entry's own:
	// emitted at the local's own declared width (cType(kind)), so e.g. an
	// i64 local inside an i32 function is an int64_t, and its initializer is
	// built by buildExpr at that same width (buildExpr re-checks every node
	// in the initializer is the local's own width). The scope entry records
	// the local's own width so a later reference or reassignment is
	// validated and emitted as that width's integer.
	if initValue.Kind == tir.DirectCall || initValue.Kind == tir.MethodCall {
		// An integer local declared directly from a call — `let x =
		// f(a[1:3]);` — routed through buildDirectCallWithPre so an inline
		// slice-construction call argument's temp declaration is threaded as
		// the pre (a local declaration is a leading-statement position,
		// mirroring how the CheckedOptionalUnwrap case above threads a
		// force-unwrap temp). The call's result type is initValue.Type,
		// which resolvedBuiltin above already confirmed resolves to this
		// local's own kind, so the declaration is valid without buildExpr's
		// width re-check (buildExpr's DirectCall case emits the identical
		// call text for a non-slice call).
		callPre, callExpr, err := buildDirectCallWithPre(unit, snapshot, fileSet, initValue, scope, width)
		if err != nil {
			return "", "", err
		}
		scope[statement.Symbol] = localInfo{kind: kind}
		return callPre, fmt.Sprintf("%s pebble_local_%d = %s", cType(kind), statement.Symbol, callExpr), nil
	}
	initExpr, err := buildExpr(unit, snapshot, fileSet, statement.Children[0], scope, kind, width)
	if err != nil {
		return "", "", err
	}
	scope[statement.Symbol] = localInfo{kind: kind}
	return "", fmt.Sprintf("%s pebble_local_%d = %s", cType(kind), statement.Symbol, initExpr), nil
}

// buildStoreCore builds the value text for a reassignment of a local already
// in scope, WITHOUT the leading indent and WITHOUT the trailing `;` a full
// block-level Store statement gets: `pebble_local_<symbol> = <expr>`. It is
// the Store dispatch, shared by buildLeadingStatement (which prepends the
// indent and appends the `;` to form the full statement) and
// buildForUpdateClause (which uses the core as the for-header update clause,
// where the for statement's own syntax supplies the `;`), so the
// place-validation and the buildExpr/buildBoolExpr/buildFloatExpr dispatch
// live in exactly
// one place. The place must be a plain StoragePlace naming a local in scope,
// or, since 10.39, a CheckedIndexPlace naming an element of an array or
// slice local (`arr[i] = v;` / `s[i] = v;`), and the new value is validated
// and emitted against the resolved place type — the local's own declared type
// for a StoragePlace (the local's own integer width via buildExpr — an i64
// local reassigned inside an i32 function builds its new value at i64 — a
// float local via buildFloatExpr at its own recorded float kind, so an f64
// local reassigned inside an f32 function builds its new value at f64 — bool
// via buildBoolExpr, or, since 10.36, str — a new value that must be a string
// literal, emitted as a whole-struct PebbleStr reassignment; see the isStr
// branch below), or the resolved element type for a CheckedIndexPlace (the
// entry's width via buildExpr or bool via buildBoolExpr, exactly as a scalar
// value position dispatches), mirroring buildLeadingStatement's Store case
// exactly, including its rejections of a Store targeting a
// tuple/array/optional/struct local.
func buildStoreCore(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, statement tir.Node, scope map[symbol.SymbolID]localInfo, context string, width types.BuiltinKind, unions map[types.TypeID]unionInfo) (string, error) {
	if len(statement.Children) != 2 {
		return "", fmt.Errorf("%s reassignment has %d child(ren), want exactly two: the place being reassigned and the new value", context, len(statement.Children))
	}
	place, ok := unit.Node(statement.Children[0])
	if !ok {
		return "", fmt.Errorf("%s reassignment references invalid place node %d", context, statement.Children[0])
	}
	if place.Kind != tir.StoragePlace && place.Kind != tir.CheckedIndexPlace && place.Kind != tir.DereferencePlace && place.Kind != tir.FieldPlace {
		return "", fmt.Errorf("%s reassignment targets a %s, want a plain StoragePlace naming a local in scope, a CheckedIndexPlace naming an element of an array or slice local, a FieldPlace, or a DereferencePlace for a write through a pointer", context, place.Kind)
	}
	if place.Kind == tir.CheckedIndexPlace || place.Kind == tir.DereferencePlace || place.Kind == tir.FieldPlace {
		// An indexed element write (`arr[i] = v;` / `s[i] = v;`) or a
		// write-through-pointer (`*p = v;`). The left-hand lvalue text is
		// built entirely by buildPlaceLValue, which handles both CheckedIndex
		// (bounds-checked array/slice element) and DereferencePlace (null-
		// checked pointer dereference). The new value is built against the
		// resolved target type: buildExpr for the entry's width, buildBoolExpr
		// for bool.
		lvalue, elementType, err := buildPlaceLValue(unit, snapshot, fileSet, statement.Children[0], scope, width)
		if err != nil {
			return "", err
		}
		if isBool(snapshot, elementType) {
			storeValue, err := buildBoolExpr(unit, snapshot, fileSet, statement.Children[1], scope, width)
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("%s = %s", lvalue, storeValue), nil
		}
		if elementWidth, integerElement := resolvedBuiltin(snapshot, elementType); integerElement && cType(elementWidth) != "" {
			// An integer element of any fixed-width builtin, not just the
			// entry's own: the new value is built at the ELEMENT's own resolved
			// width (a u8 slice element inside an i32 function builds its new
			// value at u8), mirroring how a scalar local's reassignment builds
			// at the local's own declared width.
			storeValue, err := buildExpr(unit, snapshot, fileSet, statement.Children[1], scope, elementWidth, width)
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("%s = %s", lvalue, storeValue), nil
		}
		if isChar(snapshot, elementType) {
			storeValue, err := buildCharOperand(unit, snapshot, fileSet, statement.Children[1], scope, width)
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("%s = %s", lvalue, storeValue), nil
		}
		if isPointer(snapshot, elementType) {
			storeValue, err := buildExpr(unit, snapshot, fileSet, statement.Children[1], scope, width, width)
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("%s = %s", lvalue, storeValue), nil
		}
		if isEnumType(unit, snapshot, elementType) {
			// An enum-typed field (or indexed element) write — `entry.state =
			// .Occupied;`, the std/hmap.peb insert shape: the C field is declared
			// at its own pebble_enum_<typeID>_t (see structFieldCType), and the
			// new value is a variant literal built by the same buildEnumValue an
			// enum-typed local's reassignment uses, so `lvalue = pebble_variant_<m>;`
			// is the direct, uncoerced C store.
			storeValue, err := buildEnumValue(unit, snapshot, fileSet, statement.Children[1], scope, width)
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("%s = %s", lvalue, storeValue), nil
		}
		if isSlice(snapshot, elementType) {
			// A slice-typed field (or indexed element) write — `self.entries =
			// new_entries;`, the std/hmap.peb rehash shape: the target is a
			// struct field declared at its own pebble_slice_<typeID>_t (see
			// structFieldCType), and the new value is a reference to an
			// already-declared slice-typed local of the matching type, emitted
			// as the local's own pebble_local_<symbol> C name — the same
			// whole-struct-copy value shape buildSliceArgument accepts for a
			// slice call argument, so `lvalue = pebble_local_<symbol>;` is the
			// direct, uncoerced C store.
			valueNode, ok := unit.Node(statement.Children[1])
			if !ok {
				return "", fmt.Errorf("%s reassignment references invalid value node %d", context, statement.Children[1])
			}
			if valueNode.Kind == tir.SliceFromRaw {
				// A bare SliceFromRaw directly as the slice-field reassignment
				// value — `self.data = slice ptr, new_cap;`, the std/string.peb
				// grow shape: the Store's value is a SliceFromRaw node (not a
				// reference to a slice-typed local), whose construction is a
				// single compound-literal expression (buildRawSliceConstruction,
				// the same construction a slice-typed local declaration's
				// SliceFromRaw initializer uses), so
				// `lvalue = <construction>;` is the direct, uncoerced C store.
				// The construction is for valueNode.Type, which must be exactly
				// the place's resolved slice type (defense for hand-built IR).
				if valueNode.Type != elementType {
					return "", fmt.Errorf("%s reassigns a slice-typed place of type %s from a SliceFromRaw of type %s", context, sliceTypeName(elementType), describeType(snapshot, valueNode.Type))
				}
				construction, err := buildRawSliceConstruction(unit, snapshot, fileSet, valueNode, scope, width, context)
				if err != nil {
					return "", err
				}
				return fmt.Sprintf("%s = %s", lvalue, construction), nil
			}
			if valueNode.Kind != tir.SymbolValue {
				return "", fmt.Errorf("%s reassigns a slice-typed place from a %s, want a reference to a slice-typed local in scope", context, valueNode.Kind)
			}
			valueInfo, declared := scope[valueNode.Symbol]
			if !declared || valueInfo.sliceType != elementType {
				return "", fmt.Errorf("%s reassigns a slice-typed place of type %s from symbol %d, which is not a slice-typed local in scope of that type", context, sliceTypeName(elementType), valueNode.Symbol)
			}
			return fmt.Sprintf("%s = %s", lvalue, fmt.Sprintf("pebble_local_%d", valueNode.Symbol)), nil
		}
		return "", fmt.Errorf("%s reassigns an element of type %s, want a fixed-width integer, char, bool, pointer, or enum", context, describeType(snapshot, elementType))
	}
	targetInfo, declared := scope[place.Symbol]
	if !declared {
		return "", fmt.Errorf("%s reassigns symbol %d, which is not a local in scope", context, place.Symbol)
	}
	// The new value is validated and emitted against the local's own declared
	// type: the local's own integer width for an integer local (buildExpr at
	// that width — an i64 local reassigned inside an i32 function builds its
	// new value at i64, not i32), the float grammar for a float local
	// (buildFloatExpr at the local's own recorded float kind — an f64 local
	// reassigned inside an f32 function builds its new value at f64, not f32),
	// the bool grammar for a bool local
	// (buildBoolExpr). A value of the wrong type — a bool assigned to an
	// integer local, or an integer assigned to a bool local — is rejected by
	// the appropriate builder.
	switch targetInfo.kind {
	case types.Int, types.I8, types.I16, types.I32, types.I64, types.U8, types.U16, types.U32, types.U64:
		storeValue, err := buildExpr(unit, snapshot, fileSet, statement.Children[1], scope, targetInfo.kind, width)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("pebble_local_%d = %s", place.Symbol, storeValue), nil
	case types.Uint:
		// A Store whose place names a uint-typed local is a uint
		// reassignment: the new value is built by buildUintExpr at uint's
		// own grammar (sizeof results, checked uint arithmetic, a reference
		// to an in-scope uint-typed local), mirroring how a uint local's
		// declaration initializer routes (buildScalarInitializeCore) and
		// how every other uint value position routes — the general
		// buildExpr path has no SizeofType case and no uint-typed checked
		// arithmetic. std/hmap.peb's insert reassigns its uint local index
		// (`index = (index + 1) % self.cap;`), the exact shape that
		// motivates this case.
		storeValue, err := buildUintExpr(unit, snapshot, fileSet, statement.Children[1], scope, width)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("pebble_local_%d = %s", place.Symbol, storeValue), nil
	case types.F32, types.F64:
		// A Store whose place names a float-typed local (f32 or f64, Stage A)
		// is a float reassignment: the new value is built by buildFloatExpr at
		// the local's own recorded float kind (a float literal or a reference
		// to an in-scope float-typed local of that same kind), so `x = 2.5;`
		// emits `pebble_local_<sym> = 2.5;` at the local's own C type. A value
		// of any other shape or type is a clean rejection by buildFloatExpr.
		storeValue, err := buildFloatExpr(unit, snapshot, fileSet, statement.Children[1], scope, targetInfo.kind)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("pebble_local_%d = %s", place.Symbol, storeValue), nil
	case types.Bool:
		storeValue, err := buildBoolExpr(unit, snapshot, fileSet, statement.Children[1], scope, width)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("pebble_local_%d = %s", place.Symbol, storeValue), nil
	default:
		if targetInfo.enumType != 0 {
			if _, isUnion := unions[targetInfo.enumType]; isUnion {
				// A Store whose place names a tagged-union-typed local is a
				// whole-value reassignment — c = Choice.value(5); — whose new
				// value is a variant construction built by
				// buildUnionConstruction (a C99 compound literal of the
				// union's struct typedef), emitted as
				// `pebble_local_<sym> = (pebble_union_<id>_t){ .tag = ... };`.
				storeValue, err := buildUnionConstruction(unit, snapshot, fileSet, mustNode(unit, statement.Children[1]), scope, context, unions, width)
				if err != nil {
					return "", err
				}
				return fmt.Sprintf("pebble_local_%d = %s", place.Symbol, storeValue), nil
			}
			// A Store whose place names an enum-typed local is a whole-value
			// reassignment of a plain enum local — c = Color.red; — whose new
			// value is a variant literal (an EnumVariantValue, or a
			// zero-payload VariantConstruct) built by the enum value builder,
			// emitted as `pebble_local_<sym> = pebble_variant_<member>;`.
			storeValue, err := buildEnumValue(unit, snapshot, fileSet, statement.Children[1], scope, width)
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("pebble_local_%d = %s", place.Symbol, storeValue), nil
		}
		if targetInfo.isStr {
			// A Store whose place names a str-typed local is a whole-str
			// reassignment. The only supported new-value shape is a string
			// literal (a StringLiteral), the same single shape a str local's
			// declaration accepts — this slice is deliberately literal-to-
			// literal only, so `s = "hi";` works while reassigning from any
			// other value does not. The emitted C is a whole-struct
			// reassignment, `pebble_local_<sym> = (PebbleStr){ .data = ...,
			// .len = <N> };`, whose inner PebbleStr construction text is the
			// exact same byte-for-byte text buildStrLocalDeclaration embeds in
			// a str local's declaration from the same literal (via
			// buildStrLiteralValue — the (PebbleStr) compound-literal cast is
			// what makes the brace text a valid C assignment expression).
			// Reassigning a str local from anything else — a str-typed local
			// (s = t;), a call result (s = g();), string concatenation (s =
			// "h" + "i";), all confirmed reachable from real source against
			// real fixtures — is a clean rejection naming what was found,
			// never a guessed lowering.
			storeValue, ok := unit.Node(statement.Children[1])
			if !ok {
				return "", fmt.Errorf("%s reassignment references invalid value node %d", context, statement.Children[1])
			}
			if storeValue.Kind != tir.StringLiteral {
				return "", fmt.Errorf("%s reassigns symbol %d, a str-typed local, from a %s; reassigning a str local from anything other than a string literal is not supported yet", context, place.Symbol, storeValue.Kind)
			}
			valueText, err := buildStrLiteralValue(storeValue)
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("pebble_local_%d = (PebbleStr)%s", place.Symbol, valueText), nil
		}
		if targetInfo.isChar {
			// A Store whose place names a char-typed local is a char
			// reassignment. The new value is built by buildCharOperand under
			// the char grammar — the same three shapes a char local's
			// declaration accepts (a char literal, a reference to an in-scope
			// char-typed local, or a call to a char-returning helper), each
			// emitted as an int32_t value — so `c = 'b';`, `c = d;`, and
			// `c = g();` (all confirmed checker-reachable against real
			// fixtures) reassign the fixed-width int32_t local correctly. A
			// value of any other shape or type is a clean rejection naming
			// what was found.
			storeValue, err := buildCharOperand(unit, snapshot, fileSet, statement.Children[1], scope, width)
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("pebble_local_%d = %s", place.Symbol, storeValue), nil
		}
		if targetInfo.functionType != 0 {
			// A Store whose place names a function-typed local is a
			// whole-value reassignment — `f = g;` — whose new value is
			// another function-typed value built by buildFunctionValue (a
			// reference to an in-scope function-typed local or a bare
			// function value), emitted as
			// `pebble_local_<sym> = <value>;`. The checker has already
			// coerced the new value to the local's own declared function
			// type (the two signatures match), so the assigned C function
			// pointer is always the local's own pebble_fnptr_<typeID>_t.
			storeValue, err := buildFunctionValue(unit, snapshot, fileSet, mustNode(unit, statement.Children[1]), scope, context, width)
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("pebble_local_%d = %s", place.Symbol, storeValue), nil
		}
		if targetInfo.tuple != 0 {
			// A Store whose place names a tuple-typed local is a
			// whole-tuple reassignment, which is out of scope this slice
			// (only element reads of a tuple local are supported, never
			// assignment into or reassignment of one).
			return "", fmt.Errorf("%s reassigns symbol %d, a tuple-typed local of type %s; reassigning a whole tuple is not supported yet", context, place.Symbol, describeType(snapshot, targetInfo.tuple))
		}
		if targetInfo.array != 0 {
			return "", fmt.Errorf("%s reassigns symbol %d, an array-typed local of type %s; reassigning a whole array is not supported yet", context, place.Symbol, describeType(snapshot, targetInfo.array))
		}
		if targetInfo.optional != 0 {
			// A Store whose place names an optional-typed local is a
			// whole-value reassignment — `tombstone_index = some index;`, the
			// std/hmap.peb insert shape — whose new value is an optional value
			// built by the same buildOptionalValue machinery an optional
			// local's declaration initializer uses (a SomeOptional / none, a
			// forward of an in-scope optional-typed local, or a call to an
			// optional-returning helper), emitted as
			// `pebble_local_<sym> = <optional value>;` where the value is the
			// optional's own C type (pebble_optional_<typeID>_t), so the whole
			// reassignment is a plain C struct assignment.
			storeValue, err := buildOptionalValue(unit, snapshot, fileSet, statement.Children[1], scope, targetInfo.optional, context, width)
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("pebble_local_%d = %s", place.Symbol, storeValue), nil
		}
		if targetInfo.structType != 0 {
			return "", fmt.Errorf("%s reassigns symbol %d, a struct-typed local of type %s; reassigning a whole struct is not supported yet", context, place.Symbol, describeType(snapshot, targetInfo.structType))
		}
		if targetInfo.pointerType != 0 {
			// A Store whose place names a pointer-typed local is a pointer
			// reassignment — `p = q;` or `p = nil;`. The new value is a
			// pointer expression built by buildExpr which now handles
			// pointer-typed nodes (AddressOf, SymbolValue, NilPointer).
			storeValue, err := buildExpr(unit, snapshot, fileSet, statement.Children[1], scope, width, width)
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("pebble_local_%d = %s", place.Symbol, storeValue), nil
		}
		return "", fmt.Errorf("%s reassigns symbol %d, which is a local of type %s, want %s or bool", context, place.Symbol, describeType(snapshot, place.Type), wantName(width))
	}
}

// buildCompoundStore builds the value text for a compound assignment — a
// tir.CompoundStore, covering the +=, -=, *=, /=, %= family AND a postfix
// ++/-- (which the checker builds as a CompoundStore with + or - and a
// literal-one value child; see buildPostfixUpdate) — WITHOUT the leading
// indent and WITHOUT the trailing `;` a full block-level CompoundStore
// statement gets: `<lvalue> = <combined value>`.
//
// It is the direct compound twin of buildStoreCore, sharing its place grammar
// exactly: a plain StoragePlace naming a local in scope, a CheckedIndexPlace
// naming an element of an array or slice local (`arr[i] += 1;`), a FieldPlace
// (`self.field -= 1;`), or a DereferencePlace (`*p *= 2;`), the lvalue text
// built by buildPlaceLValue the same way buildStoreCore builds a plain
// reassignment's left side.
//
// The combination goes through the SAME checked-arithmetic runtime helpers
// buildExpr's CheckedArithmetic case uses (pebble_rt_checked_add_i32/...,
// picked by the same checkedArithmeticHelper operator mapping) — a compound
// assignment's whole point is that `x += y` carries the identical overflow and
// divide-by-zero semantics as `x = x + y`, so the emitted C is
// `<lvalue> = pebble_rt_checked_<op>_<suffix>(<lvalue>, <value>, <loc>)`, NOT
// a raw C `+=` (which would silently skip the checked semantics). The lvalue
// text appears twice — once read into the helper, once as the write target —
// which is side-effect-free for a plain local and bounds-checks per evaluation
// for an indexed lvalue whose index expression is itself side-effect-free.
//
// The combined value is validated and emitted against the resolved place type,
// exactly as buildStoreCore dispatches a plain reassignment's new value: an
// integer place builds its value by buildExpr at the place's own resolved
// width (an i64 local inside an i32 function combines at i64, via the _i64
// helper) and combines through the checked helper at that same width; a float
// place (a float compound assignment is checker-reachable — the -=, *=, /=
// families are NumericSame and += is Add, both accepting floats) builds its
// value by buildFloatExpr at the place's own float kind and combines with the
// plain C operator buildFloatExpr's BinaryValue case uses, since floats have
// no checked arithmetic anywhere in this backend (IEEE floats have no defined
// overflow/divide-by-zero fault). Any other place type (bool, str, char, enum,
// union, tuple, array, optional, struct, slice, pointer, runtime) is a clean
// rejection naming what was found, never a guessed lowering.
//
// The operator must be one of the checked-arithmetic set +, -, *, /, % — the
// only operators compoundOperator in the checker can attach to a CompoundStore
// (+= -> +, -= -> -, *= -> *, /= -> /, %= -> %, and a postfix ++/-- -> + or
// -). A CompoundStore carrying any other operator is hand-built IR and a clean
// rejection.
func buildCompoundStore(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, id tir.NodeID, statement tir.Node, scope map[symbol.SymbolID]localInfo, context string, width types.BuiltinKind) (string, string, error) {
	if len(statement.Children) != 2 {
		return "", "", fmt.Errorf("%s compound assignment has %d child(ren), want exactly two: the place being combined into and the value to combine into it", context, len(statement.Children))
	}
	place, ok := unit.Node(statement.Children[0])
	if !ok {
		return "", "", fmt.Errorf("%s compound assignment references invalid place node %d", context, statement.Children[0])
	}
	if place.Kind != tir.StoragePlace && place.Kind != tir.CheckedIndexPlace && place.Kind != tir.DereferencePlace && place.Kind != tir.FieldPlace {
		return "", "", fmt.Errorf("%s compound assignment targets a %s, want a plain StoragePlace naming a local in scope, a CheckedIndexPlace naming an element of an array or slice local, a FieldPlace, or a DereferencePlace for a write through a pointer", context, place.Kind)
	}
	// The operator must be one of the five checked-arithmetic operators — the
	// full set compoundOperator in the checker can attach to a CompoundStore
	// (see the doc comment). Anything else is hand-built IR and a clean
	// rejection, never a guessed lowering.
	switch statement.Operator {
	case syntax.Plus, syntax.Minus, syntax.Star, syntax.Slash, syntax.Percent:
	default:
		return "", "", fmt.Errorf("%s compound assignment uses operator %s, want +, -, *, /, or %%", context, statement.Operator)
	}
	if place.Kind == tir.StoragePlace {
		targetInfo, declared := scope[place.Symbol]
		if !declared {
			return "", "", fmt.Errorf("%s compound assignment combines into symbol %d, which is not a local in scope", context, place.Symbol)
		}
		// The lvalue is the local's own C name; the combined value is built
		// against the local's own declared type, mirroring buildStoreCore's
		// targetInfo.kind switch: an integer local combines at its own declared
		// width (an i64 local inside an i32 function combines at i64), a float
		// local at its own float kind, and everything else — bool, str, char,
		// enum, union, tuple, array, optional, struct, slice, pointer, runtime —
		// is a clean rejection naming the local's type.
		lvalue := fmt.Sprintf("pebble_local_%d", place.Symbol)
		switch targetInfo.kind {
		case types.Int, types.I8, types.I16, types.I32, types.I64, types.U8, types.U16, types.U32, types.U64:
			core, err := buildCompoundIntegerCore(unit, snapshot, fileSet, statement, lvalue, targetInfo.kind, scope, context, width)
			return "", core, err
		case types.Uint:
			core, err := buildCompoundUintCore(unit, snapshot, fileSet, statement, lvalue, scope, context, width)
			return "", core, err
		case types.F32, types.F64:
			core, err := buildCompoundFloatCore(unit, snapshot, fileSet, statement, lvalue, targetInfo.kind, scope, context)
			return "", core, err
		default:
			return "", "", fmt.Errorf("%s compound assignment combines into symbol %d, a %s local; compound assignment is supported only for integer and float locals", context, place.Symbol, describeType(snapshot, place.Type))
		}
	}
	// A non-plain place (indexed/field/dereference): the lvalue text and the
	// resolved element type come from buildPlaceLValue, exactly as buildStoreCore
	// builds a plain indexed/field/deref reassignment's left side. The element
	// must be the entry's own resolved width — the one scalar grammar a
	// non-plain element can take — so the checked helper is chosen at that
	// width; anything else (a bool, a pointer, or a non-entry-width integer
	// element) is a clean rejection.
	lvalue, elementType, err := buildPlaceLValue(unit, snapshot, fileSet, statement.Children[0], scope, width)
	if err != nil {
		return "", "", err
	}
	if isUint(snapshot, elementType) {
		// A uint-typed field (or indexed element) compound assignment —
		// `self.len += 1;`, the std/hmap.peb insert shape: the value is built
		// by buildUintExpr and combined with the plain C operator, exactly as
		// a uint-typed local's compound assignment does (buildCompoundUintCore).
		core, err := buildCompoundUintCore(unit, snapshot, fileSet, statement, lvalue, scope, context, width)
		return "", core, err
	}
	if !isWidth(snapshot, width, elementType) {
		return "", "", fmt.Errorf("%s compound assignment combines into an element of type %s, want %s", context, describeType(snapshot, elementType), wantName(width))
	}
	tempName := fmt.Sprintf("pebble_compound_ptr_%d", id)
	core, err := buildCompoundIntegerCore(unit, snapshot, fileSet, statement, "(*"+tempName+")", width, scope, context, width)
	if err != nil {
		return "", "", err
	}
	pre := fmt.Sprintf("%s *%s = &(%s);", cType(width), tempName, lvalue)
	return pre, core, nil
}

// buildCompoundIntegerCore builds the combined-value text for a compound
// assignment whose place resolves to an integer type: the new value is built by
// buildExpr at the place's own resolved width (placeWidth — the local's own
// declared width for a StoragePlace, the entry's resolved width for a
// non-plain place) and combined through the checked-arithmetic runtime helper
// checkedArithmeticHelper picks for the operator at that width, so `i += 1`
// emits `pebble_local_<i> = pebble_rt_checked_add_i32(pebble_local_<i>, 1,
// <loc>)` with the identical overflow and divide-by-zero semantics as a plain
// `i = i + 1`. A place width with no checked helper (any integer builtin other
// than int/i32/i64/u64 — the backend has no checked runtime primitive at those
// widths) is a clean rejection rather than a malformed helper name. u64 is
// admitted for the +, -, * family this slice added (pebble_rt_checked_add/
// sub/mul_u64), but a u64 /= or %= is cleanly rejected: there is no
// checked_div_u64/mod_u64, so emitting one would be a call to a nonexistent
// helper.
func buildCompoundIntegerCore(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, statement tir.Node, lvalue string, placeWidth types.BuiltinKind, scope map[symbol.SymbolID]localInfo, context string, width types.BuiltinKind) (string, error) {
	if checkedSuffix(placeWidth) == "" {
		return "", fmt.Errorf("%s compound assignment combines at %s, which has no checked-arithmetic runtime helper; compound assignment is supported only at int, i32, or i64", context, wantName(placeWidth))
	}
	if placeWidth == types.U64 && (statement.Operator == syntax.Slash || statement.Operator == syntax.Percent) {
		return "", fmt.Errorf("%s compound assignment %ss at u64, which has no checked division/modulo runtime helper; u64 compound assignment is supported only for +, -, or *", context, statement.Operator)
	}
	helper, _ := checkedArithmeticHelper(statement.Operator, placeWidth)
	value, err := buildExpr(unit, snapshot, fileSet, statement.Children[1], scope, placeWidth, width)
	if err != nil {
		return "", err
	}
	return fmt.Sprintf("%s = %s(%s, %s, %s)", lvalue, helper, lvalue, value, buildSourceLoc(fileSet, statement.Span)), nil
}

// buildCompoundFloatCore builds the combined-value text for a compound
// assignment whose place resolves to a float type: the new value is built by
// buildFloatExpr at the place's own float kind (placeWidth — f32 or f64) and
// combined with the plain C operator buildFloatExpr's BinaryValue case uses,
// `x = (x + y)` — the same unchecked lowering every float arithmetic in this
// backend uses, since IEEE floats have no defined overflow or divide-by-zero
// fault and no checked float runtime primitives exist. %= on a float is
// rejected (the checker's operatorIntegralSame family never admits a float to
// %=, so a real fixture cannot produce it).
func buildCompoundFloatCore(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, statement tir.Node, lvalue string, placeWidth types.BuiltinKind, scope map[symbol.SymbolID]localInfo, context string) (string, error) {
	if statement.Operator == syntax.Percent {
		return "", fmt.Errorf("%s compound assignment uses %%%% on a float local, want +, -, *, or / (%% is integral-only)", context)
	}
	op, _ := arithmeticOperator(statement.Operator)
	value, err := buildFloatExpr(unit, snapshot, fileSet, statement.Children[1], scope, placeWidth)
	if err != nil {
		return "", err
	}
	return fmt.Sprintf("%s = (%s %s %s)", lvalue, lvalue, op, value), nil
}

// buildCompoundUintCore builds the combined-value text for a compound
// assignment whose place resolves to uint: the new value is built by
// buildUintExpr (the dedicated uint grammar — sizeof results, checked uint
// arithmetic, references to in-scope uint locals, integer casts) and combined
// with the plain C operator arithmeticOperator picks for the operator, `x = (x
// + y)` — the same unchecked lowering buildUintExpr uses for every uint
// arithmetic in this backend, exactly as buildCompoundFloatCore is the plain
// unchecked lowering for floats. This covers both the uint-typed struct field
// the std/hmap.peb insert shape uses (`self.len += 1;` — a FieldPlace whose
// resolved element type is uint) and a uint-typed local (`sum += 1`). A uint
// place has no checked runtime helper (checkedSuffix maps no width to uint), so
// the plain C operator is the whole lowering, never a malformed helper name.
func buildCompoundUintCore(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, statement tir.Node, lvalue string, scope map[symbol.SymbolID]localInfo, context string, width types.BuiltinKind) (string, error) {
	if statement.Operator == syntax.Percent {
		return "", fmt.Errorf("%s compound assignment uses %%%% on a uint place, want +, -, *, or / (%% is integral-only)", context)
	}
	op, _ := arithmeticOperator(statement.Operator)
	value, err := buildUintExpr(unit, snapshot, fileSet, statement.Children[1], scope, width)
	if err != nil {
		return "", err
	}
	return fmt.Sprintf("%s = (%s %s %s)", lvalue, lvalue, op, value), nil
}

// buildLoopBody validates and builds the C statement sequence for a loop body
// (a while's, a range loop's, or a classic for loop's): a Block built by the
// shared buildFallthroughBody fall-through statement-sequence builder, with
// the loop-body context naming. This is the loop-specific entry point
// (buildWhile/buildRangeLoop/buildFor/buildLoopIf recurse here); the same
// builder serves a fall-through if's arms and a fall-through switch's case
// bodies uniformly, since they all need the identical "arbitrary statement
// sequence, no forced tail" capability. An empty loop body (zero children) is
// legal — `while cond {}` is a real, if useless, program — and emits no
// statements at all.
func buildLoopBody(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, bodyID tir.NodeID, locals map[symbol.SymbolID]localInfo, depth int, width types.BuiltinKind, result resultInfo, unions map[types.TypeID]unionInfo) (string, error) {
	return buildFallthroughBody(unit, snapshot, fileSet, bodyID, locals, depth, width, result, unions, "entry function body block while loop body")
}

// buildFallthroughBody validates and builds the C statement sequence for an
// ordinary, fall-through block: a Block whose children are local declarations
// (Initialize), reassignments (Store), print statements (Print), bare
// discarded-expression statements (ExpressionStatement), conditional if
// statements (a tir.If built by buildLoopIf — the else is optional), switch
// statements (a tir.Switch built by buildLoopSwitch — case bodies may fall
// through or return), nested while/range/classic-for loops (built by
// buildWhile/buildRangeLoop/buildFor), return statements (built by
// buildReturnStatement), and break/continue statements (a tir.Break /
// tir.Continue built by buildLoopJump), built one level deeper than the
// enclosing block. A fall-through block has no required tail — it just runs
// statements and does not need to end in a return or if — so buildBlock is
// deliberately not reused here; the grammar is genuinely different. This is
// the shared builder for every sequence with that shape: a loop body
// (buildLoopBody), a fall-through if's arm (buildLeadingIf and buildLoopIf),
// and a fall-through switch's case body (buildLoopSwitchCaseBody), all of
// which need the identical "arbitrary statement sequence, no forced tail"
// capability. The block is its own scope: locals are cloned from the
// enclosing set (the same cloneLocals discipline buildIf's arms use) before
// any declaration is added, so a local declared inside it is invisible
// outside it. A nested loop's body and each if arm/switch case body are their
// own scopes in turn (buildWhile/buildRangeLoop/buildFor/buildLoopIf and
// buildLoopSwitch all recurse into this same builder, which clones per
// entry), so a local declared inside one of them is invisible to its siblings
// and to anything outside it. A break or continue inside the sequence targets
// the nearest enclosing loop or switch by Pebble's own control-flow rules;
// the emitted C break/continue resolves to the same construct, so the
// translation is direct and correct (see buildLoopJump). Any other statement
// kind is a clean rejection naming what was found. An empty block (zero
// children) emits no statements at all. context names the enclosing construct
// in error messages.
func buildFallthroughBody(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, bodyID tir.NodeID, locals map[symbol.SymbolID]localInfo, depth int, width types.BuiltinKind, result resultInfo, unions map[types.TypeID]unionInfo, context string) (string, error) {
	body, ok := unit.Node(bodyID)
	if !ok {
		return "", fmt.Errorf("%s references invalid node %d", context, bodyID)
	}
	if body.Kind != tir.Block {
		return "", fmt.Errorf("%s is a %s, want a Block", context, body.Kind)
	}
	if len(body.Children) == 0 {
		return "", nil
	}
	scope := cloneLocals(locals)
	indent := strings.Repeat("    ", depth+1)
	var statements []string
	for _, childID := range body.Children {
		text, err := buildFallthroughStatement(unit, snapshot, fileSet, childID, scope, indent, depth, width, result, unions, context)
		if err != nil {
			return "", err
		}
		if text != "" {
			statements = append(statements, text)
		}
	}
	return strings.Join(statements, "\n"), nil
}

// buildFallthroughStatement validates and builds the C text for one statement
// in a fall-through statement sequence (see buildFallthroughBody), dispatching
// on the statement kind exactly as buildBlock's leading-statement loop and the
// loop-body statement switch do, but with the loop body's own "no required
// tail" grammar extended to the whole sequence: a nested while/range loop/for
// loop recurses into its own loop builder (which recurses back into
// buildFallthroughBody for its body), a conditional if is built by buildLoopIf
// (arms are themselves fall-through sequences, optional else), a switch by
// buildLoopSwitch (case bodies are themselves fall-through sequences), a
// return by buildReturnStatement, a break/continue by buildLoopJump, a
// DeferRegister emits nothing, and everything else — an Initialize, a Store,
// an ExpressionStatement, a Print — flows through the shared
// buildLeadingStatement. indent is the statement's C indentation (the
// sequence's own indent, depth+1 levels); depth is the sequence's nesting
// depth, passed to the nested control-flow builders unchanged so they open
// their own bodies one level deeper. context names the enclosing construct in
// error messages.
func buildFallthroughStatement(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, childID tir.NodeID, scope map[symbol.SymbolID]localInfo, indent string, depth int, width types.BuiltinKind, result resultInfo, unions map[types.TypeID]unionInfo, context string) (string, error) {
	statement, ok := unit.Node(childID)
	if !ok {
		return "", fmt.Errorf("%s references invalid statement node %d", context, childID)
	}
	var text string
	var err error
	switch statement.Kind {
	case tir.While:
		// A nested while inside a fall-through sequence reuses buildWhile
		// unchanged: it already recurses into buildLoopBody (via
		// buildFallthroughBody) for its own body, so nested loops compose
		// without any change to buildWhile itself.
		text, err = buildWhile(unit, snapshot, fileSet, statement, scope, depth, width, result, unions)
	case tir.RangeLoop:
		// A nested range loop inside a fall-through sequence (a loop body, an
		// if arm, or a switch case body) reuses buildRangeLoop unchanged: it
		// recurses into this same builder for its own body, so nested range
		// loops compose exactly like nested whiles do.
		text, err = buildRangeLoop(unit, snapshot, fileSet, statement, scope, depth, width, result, unions)
	case tir.For:
		// A nested classic for loop inside a fall-through sequence reuses
		// buildFor unchanged: it recurses into this same builder for its own
		// body, so nested classic for loops compose exactly like nested whiles
		// and range loops do.
		text, err = buildFor(unit, snapshot, fileSet, statement, scope, depth, width, result, unions)
	case tir.If:
		// A conditional statement inside a fall-through sequence is built by
		// buildLoopIf: its arms are themselves fall-through sequences (no
		// required tail, optional else), genuinely different from the
		// tail-requiring buildIf. Because buildLoopIf recurses into
		// buildLoopBody for each arm, a break or continue inside an arm is
		// handled by this same switch, unchanged.
		text, err = buildLoopIf(unit, snapshot, fileSet, statement, scope, depth, width, result, unions)
	case tir.Switch:
		// A switch statement inside a fall-through sequence is built by
		// buildLoopSwitch: its case bodies are themselves fall-through
		// sequences (may return or fall through), unlike the tail-requiring
		// buildSwitch. Because buildLoopSwitch recurses into this same
		// dispatch for each case body, a break or continue inside a case body
		// is handled here, unchanged — C's own break/continue scoping resolves
		// it to the nearest enclosing loop or switch, which matches Pebble's
		// break-target rules (see buildLoopSwitch).
		text, err = buildLoopSwitch(unit, snapshot, fileSet, statement, scope, depth, width, result, unions)
	case tir.Return:
		// A return inside a fall-through sequence — an if arm, a switch case
		// body, or a loop body — exits the enclosing function immediately,
		// built by the same shared buildReturnStatement the block-tail Return
		// case uses.
		text, err = buildReturnStatement(unit, snapshot, fileSet, statement, scope, indent, context, width, result, unions)
	case tir.Break:
		text, err = buildLoopJump(unit, snapshot, fileSet, statement, "break", indent, context, scope, width, unions)
	case tir.Continue:
		text, err = buildLoopJump(unit, snapshot, fileSet, statement, "continue", indent, context, scope, width, unions)
	case tir.DeferRegister:
		// A DeferRegister in a fall-through statement sequence is a
		// registration marker the checker's analysis already consumed; the
		// backend must emit nothing at this position. The deferred statement
		// is only ever emitted at exit points whose DeferChain references it.
		return "", nil
	case tir.ExpressionStatement:
		// A bare discarded-expression statement — `helper();` on its own line
		// — flows through the same shared leading-statement builder buildBlock
		// uses, so the emission logic lives in exactly one place. (The default
		// case below would reach buildLeadingStatement too; the case is
		// spelled out so this switch documents the supported kinds the way it
		// does for While/RangeLoop/For/If/Switch/Return/Break/Continue/
		// DeferRegister.)
		text, err = buildLeadingStatement(unit, snapshot, fileSet, childID, scope, indent, depth, context, width, result, unions)
	case tir.Print:
		// A print statement — `print a, b;` on its own line — flows through
		// the same shared leading-statement builder buildBlock uses, so the
		// emission logic lives in exactly one place.
		text, err = buildLeadingStatement(unit, snapshot, fileSet, childID, scope, indent, depth, context, width, result, unions)
	default:
		text, err = buildLeadingStatement(unit, snapshot, fileSet, childID, scope, indent, depth, context, width, result, unions)
	}
	if err != nil {
		return "", err
	}
	return text, nil
}

// buildLoopIf validates and builds the C text for a conditional statement
// (tir.If) inside a while loop body. Unlike buildIf — which handles the
// two-armed, both-arms-return if/else a block must end with — a loop-body if
// is just a conditional statement: its arms are loop bodies (see buildLoopBody)
// with no required tail, and the else is optional. The child count is derived
// from HasElse (confirmed against a real fixture dump): a no-else If has
// exactly two children — the condition and the then-arm — and a HasElse If has
// exactly three — the condition, then-arm, and else-arm. The condition is a
// direct integer comparison built by buildComparison, exactly as buildIf and
// buildWhile use. Each arm is built by buildLoopBody at the next nesting depth,
// which clones the incoming locals per arm, so a local declared inside one arm
// is invisible to the sibling arm and to anything outside the if, while locals
// declared in the enclosing loop body remain visible inside both arms. The
// emitted text is indented at this statement's depth, mirroring buildIf:
//
//	<indent>if (<condition>) {
//	<then statements, one level deeper>
//	<indent>}
//
// or, with an else:
//
//	<indent>if (<condition>) {
//	<then statements, one level deeper>
//	<indent>} else {
//	<else statements, one level deeper>
//	<indent>}
//
// Any other shape — a child count inconsistent with HasElse, or an arm that is
// not a Block — is a clean rejection naming what was found.
func buildLoopIf(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, ifNode tir.Node, locals map[symbol.SymbolID]localInfo, depth int, width types.BuiltinKind, result resultInfo, unions map[types.TypeID]unionInfo) (string, error) {
	if ifNode.HasElse && len(ifNode.Children) != 3 {
		return "", fmt.Errorf("entry function body block while loop body if has an else arm but %d child(ren), want exactly 3 (condition, then-arm, else-arm)", len(ifNode.Children))
	}
	if !ifNode.HasElse && len(ifNode.Children) != 2 {
		return "", fmt.Errorf("entry function body block while loop body if has no else arm but %d child(ren), want exactly 2 (condition, then-arm)", len(ifNode.Children))
	}
	condition, err := buildCondition(unit, snapshot, fileSet, ifNode.Children[0], locals, width)
	if err != nil {
		return "", err
	}
	thenText, err := buildLoopBody(unit, snapshot, fileSet, ifNode.Children[1], locals, depth+1, width, result, unions)
	if err != nil {
		return "", err
	}
	indent := strings.Repeat("    ", depth+1)
	if !ifNode.HasElse {
		return fmt.Sprintf("%sif (%s) {\n%s\n%s}", indent, condition, thenText, indent), nil
	}
	elseText, err := buildLoopBody(unit, snapshot, fileSet, ifNode.Children[2], locals, depth+1, width, result, unions)
	if err != nil {
		return "", err
	}
	return fmt.Sprintf("%sif (%s) {\n%s\n%s} else {\n%s\n%s}", indent, condition, thenText, indent, elseText, indent), nil
}

// buildLeadingIf validates and builds the C text for a conditional statement
// (tir.If) as an ordinary leading statement in a top-level function body —
// the non-tail, non-loop position, e.g. a guard clause `if x > 0 { return 1;
// }` followed by more statements. It is the top-level twin of buildLoopIf
// (which buildIf, the tail-requiring two-armed form, is NOT: a leading if
// need not be the block's last statement and its arms need not end in
// return). Exactly like buildLoopIf, the child count is derived from HasElse
// (a no-else If has exactly two children — the condition and the then-arm —
// and a HasElse If has exactly three), the condition is a direct integer
// comparison built by buildComparison, and the else is optional. The arms are
// built by buildFallthroughBody — the same shared "arbitrary statement
// sequence, no forced tail" builder a switch case body and a loop body use —
// at the next nesting depth, which clones the incoming locals per arm, so a
// local declared inside one arm is invisible to the sibling arm and to
// anything outside the if, while locals declared in the enclosing body remain
// visible inside both arms. The emitted text is indented at this statement's
// depth, mirroring buildIf and buildLoopIf:
//
//	<indent>if (<condition>) {
//	<then statements, one level deeper>
//	<indent>}
//
// or, with an else:
//
//	<indent>if (<condition>) {
//	<then statements, one level deeper>
//	<indent>} else {
//	<else statements, one level deeper>
//	<indent>}
//
// Any other shape — a child count inconsistent with HasElse, or an arm that is
// not a Block — is a clean rejection naming what was found. context names the
// enclosing construct in error messages and is prefixed with " arm" for each
// arm's own error messages.
func buildLeadingIf(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, ifNode tir.Node, locals map[symbol.SymbolID]localInfo, depth int, width types.BuiltinKind, result resultInfo, unions map[types.TypeID]unionInfo, context string) (string, error) {
	if ifNode.HasElse && len(ifNode.Children) != 3 {
		return "", fmt.Errorf("%s if has an else arm but %d child(ren), want exactly 3 (condition, then-arm, else-arm)", context, len(ifNode.Children))
	}
	if !ifNode.HasElse && len(ifNode.Children) != 2 {
		return "", fmt.Errorf("%s if has no else arm but %d child(ren), want exactly 2 (condition, then-arm)", context, len(ifNode.Children))
	}
	condition, err := buildCondition(unit, snapshot, fileSet, ifNode.Children[0], locals, width)
	if err != nil {
		return "", err
	}
	thenText, err := buildFallthroughBody(unit, snapshot, fileSet, ifNode.Children[1], locals, depth+1, width, result, unions, context+" arm")
	if err != nil {
		return "", err
	}
	indent := strings.Repeat("    ", depth+1)
	if !ifNode.HasElse {
		return fmt.Sprintf("%sif (%s) {\n%s\n%s}", indent, condition, thenText, indent), nil
	}
	elseText, err := buildFallthroughBody(unit, snapshot, fileSet, ifNode.Children[2], locals, depth+1, width, result, unions, context+" arm")
	if err != nil {
		return "", err
	}
	return fmt.Sprintf("%sif (%s) {\n%s\n%s} else {\n%s\n%s}", indent, condition, thenText, indent, elseText, indent), nil
}

// buildDeferredStatements emits the C statements for each DeferRegister node in
// a DeferChain, in the chain's own order (already LIFO — last-registered-first
// — computed by the checker's deferChainFor in ir_builder_control.go). Each
// DeferRegister's single child is the deferred statement itself, built by the
// appropriate statement builder. The emitted text is a sequence of C statements
// at the given indent, joined by newlines, to be placed immediately before the
// actual exit statement (return/break/continue). The checker already rejects
// deferred return/break/continue/nested defer (C0613), so the only reachable
// deferred statement kinds from real source are Store (reassignment),
// CompoundStore (a compound assignment or postfix increment/decrement), Print
// (the built-in), and — since 10.33 — a bare discarded-expression statement
// that is a call to a void-returning function (defer helper();, built by the
// same buildExpressionStatement the leading-statement case uses). A
// DeferRegister whose child is an unsupported
// statement kind is a clean rejection naming what was found.
func buildDeferredStatements(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, chain []tir.NodeID, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind, unions map[types.TypeID]unionInfo) (string, error) {
	if len(chain) == 0 {
		return "", nil
	}
	var parts []string
	for _, deferRegID := range chain {
		deferReg, ok := unit.Node(deferRegID)
		if !ok {
			return "", fmt.Errorf("%s references invalid DeferRegister node %d", context, deferRegID)
		}
		if deferReg.Kind != tir.DeferRegister {
			return "", fmt.Errorf("%s DeferChain entry %d is a %s, want a DeferRegister", context, deferRegID, deferReg.Kind)
		}
		if len(deferReg.Children) != 1 {
			return "", fmt.Errorf("%s DeferRegister %d has %d child(ren), want exactly one (the deferred statement)", context, deferRegID, len(deferReg.Children))
		}
		stmt, ok := unit.Node(deferReg.Children[0])
		if !ok {
			return "", fmt.Errorf("%s DeferRegister %d references invalid statement child %d", context, deferRegID, deferReg.Children[0])
		}
		switch stmt.Kind {
		case tir.Store:
			core, err := buildStoreCore(unit, snapshot, fileSet, stmt, scope, context, width, unions)
			if err != nil {
				return "", err
			}
			parts = append(parts, indent+core+";")
		case tir.CompoundStore:
			// A deferred compound assignment or postfix increment/decrement —
			// `defer i += 1;` — built by the same shared buildCompoundStore a
			// non-deferred compound assignment uses, so the emission logic
			// lives in exactly one place.
			pre, core, err := buildCompoundStore(unit, snapshot, fileSet, deferReg.Children[0], stmt, scope, context, width)
			if err != nil {
				return "", err
			}
			if pre != "" {
				parts = append(parts, indent+pre)
			}
			parts = append(parts, indent+core+";")
		case tir.ExpressionStatement:
			// A deferred call to a void-returning function — `defer
			// helper();` — built by the same shared statement builder a
			// non-deferred discarded-expression statement uses, so the emission
			// logic lives in exactly one place. The builder rejects a deferred
			// call to a non-void-returning function (and any non-call discarded
			// expression) cleanly.
			text, err := buildExpressionStatement(unit, snapshot, fileSet, stmt, scope, indent, context, width)
			if err != nil {
				return "", err
			}
			parts = append(parts, text)
		case tir.Print:
			// A deferred print statement — `defer print a, b;` — built by the
			// same shared buildPrint a leading print statement uses, so the
			// emission logic lives in exactly one place. A slice-index
			// operand's temp declaration is returned as a leading pre-statement
			// at the same indent, the same mechanical shape the deferred
			// CompoundStore pre uses.
			pre, text, err := buildPrint(unit, snapshot, fileSet, stmt, scope, indent, context, width)
			if err != nil {
				return "", err
			}
			if pre != "" {
				parts = append(parts, pre)
			}
			parts = append(parts, text)
		case tir.Initialize:
			// A deferred local declaration is reachable from real source but
			// not yet supported as a deferred statement in this backend's
			// current scope. Reject cleanly rather than guess.
			return "", fmt.Errorf("%s deferred statement is an Initialize (local declaration), which is not supported as a deferred statement yet", context)
		default:
			return "", fmt.Errorf("%s deferred statement is a %s, which is not a supported deferred statement kind (only Store reassignment, a CompoundStore compound assignment or postfix increment/decrement, and a void-returning function call used as a statement are supported)", context, stmt.Kind)
		}
	}
	return strings.Join(parts, "\n"), nil
}

// buildLoopJump validates and builds the C text for one break/continue
// statement in a loop body. A tir.Break or tir.Continue is a leaf node (no
// children, confirmed against real fixtures) whose Target names the region of
// the loop the jump leaves, and whose DeferChain carries the DeferRegister
// nodes whose deferred statements must run before the jump. The chain is
// emitted as ordinary C statements (via buildDeferredStatements) immediately
// before the break/continue, in the chain's own LIFO order. The emitted C is
// the deferred statements followed by `break;` / `continue;` at the current
// indent: the language has no labeled break/continue, so a jump's Target
// always names the nearest enclosing loop and plain C break/continue — which
// already target the nearest enclosing loop by C's own scoping rules — is a
// direct, correct translation. No runtime helper is involved, and Target's
// value never needs to be consulted or compared; it is confirmed (against a
// nested-loop fixture) to name the loop that actually contains the jump, and
// the checker (C0611) already guarantees that loop is an enclosing one.
func buildLoopJump(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, statement tir.Node, keyword string, indent, context string, scope map[symbol.SymbolID]localInfo, width types.BuiltinKind, unions map[types.TypeID]unionInfo) (string, error) {
	deferText, err := buildDeferredStatements(unit, snapshot, fileSet, statement.DeferChain, scope, indent, context, width, unions)
	if err != nil {
		return "", err
	}
	jump := fmt.Sprintf("%s%s;", indent, keyword)
	if deferText == "" {
		return jump, nil
	}
	return deferText + "\n" + jump, nil
}

// buildLeadingStatement validates and builds one leading statement in the
// block grammar shared by buildBlock and buildFallthroughBody: an Initialize (a
// local declaration), a Store (a reassignment of a local already in scope), a
// Print, a bare discarded-expression statement (an ExpressionStatement), and —
// since the non-tail if/switch work — a conditional if statement (a tir.If
// built by buildLeadingIf: arms are fall-through sequences, optional else)
// and a switch statement (a tir.Switch built by buildLoopSwitch: case bodies
// may fall through or return). The If and Switch cases are how a leading
// function-body statement sequence admits ordinary, non-tail control flow: an
// `if` or `switch` followed by more statements.
// context names the enclosing construct in error messages; indent is the
// statement's C indentation; depth is the enclosing block's nesting depth,
// passed to the If/Switch builders so they open their arms/case bodies one
// level deeper. scope is the set of in-scope locals, each mapped
// to a localInfo recording the resolved type it was declared with: the entry's
// integer width or bool for a scalar local, the tuple type's TypeID for a
// tuple local, the array type's TypeID for an array local, the optional
// type's TypeID for an optional local, or the struct type's TypeID for a
// struct local. An Initialize adds its symbol to it once validated, a Store
// reads it. width is
// the entry's resolved integer width; an integer local's C type name
// follows it (int32_t for an i32 entry, int64_t for an i64 entry), and a
// local whose value carries the other width is rejected by buildExpr, so an
// i32 local inside an i64 entry (or vice versa) is a clean width-mismatch
// error, not an attempted coercion. A local whose value carries the bool
// builtin is a bool local, declared as C `bool` and built by buildBoolExpr;
// its scope entry records types.Bool so a later reference or reassignment is
// emitted and validated against the same type. A local whose value carries a
// struct type is a struct local: it is declared as C `pebble_struct_<typeID>_t`
// and initialized from a struct literal (see buildStructLocalDeclaration), and
// its scope entry records the struct type so a later field read resolves the
// struct type being projected (see buildExpr's Load case). A local whose value
// carries a
// tuple type is a tuple local: it is declared as C `pebble_tuple_<typeID>_t`,
// initialized from a tuple literal whose element expressions are each built by
// the grammar their own element type selects, and its scope entry records the
// tuple type so a later element read resolves the element being indexed (see
// buildExpr's Load case). The
// caller is responsible for having already cloned scope if the statements must
// not leak into a sibling or enclosing scope (buildBlock and buildLoopBody both
// do). Any other statement kind is a clean rejection naming what was found.
func buildLeadingStatement(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, id tir.NodeID, scope map[symbol.SymbolID]localInfo, indent string, depth int, context string, width types.BuiltinKind, result resultInfo, unions map[types.TypeID]unionInfo) (string, error) {
	statement, ok := unit.Node(id)
	if !ok {
		return "", fmt.Errorf("%s references invalid statement node %d", context, id)
	}
	switch statement.Kind {
	case tir.Initialize:
		if len(statement.Children) != 1 {
			return "", fmt.Errorf("%s local declaration initializes %d value(s), want exactly one expression", context, len(statement.Children))
		}
		if _, declared := scope[statement.Symbol]; declared {
			return "", fmt.Errorf("%s declares local %d more than once", context, statement.Symbol)
		}
		initValue, ok := unit.Node(statement.Children[0])
		if !ok {
			return "", fmt.Errorf("%s local declaration references invalid value node %d", context, statement.Children[0])
		}
		if isTuple(snapshot, initValue.Type) {
			// A tuple-typed local: its type is the initializer value's Type
			// (the Initialize node carries no Type itself — confirmed against
			// a real fixture). The supported initializer is a tuple literal
			// (TupleValue); every other tuple initializer shape is a clean
			// rejection.
			return buildTupleLocalDeclaration(unit, snapshot, fileSet, statement, initValue, scope, indent, context, width)
		}
		if isArray(snapshot, initValue.Type) {
			if initValue.Kind == tir.DirectCall {
				return buildArrayCallInitializer(unit, snapshot, fileSet, statement, initValue, scope, indent, context, width)
			}
			return buildArrayLocalDeclaration(unit, snapshot, fileSet, statement, initValue, scope, indent, context, width)
		}
		if isOptional(snapshot, initValue.Type) {
			// An optional-typed local: its type is the initializer value's
			// Type (the Initialize node carries no Type itself, confirmed
			// against a real fixture — same as tuple/array locals). The
			// supported initializers are SomeOptional (some <expr>), none, and
			// since the OptionalIntegerToEnum slice an integer-to-optional-
			// enum cast (`5 as ?Color`); every other optional initializer
			// shape is a clean rejection.
			return buildOptionalLocalDeclaration(unit, snapshot, fileSet, statement, initValue, scope, indent, context, width, id)
		}
		if isEnumType(unit, snapshot, initValue.Type) {
			// An enum-typed local: its type is the initializer value's Type
			// (the Initialize node carries no Type itself, confirmed against
			// a real fixture — same as the compound locals above), and the
			// type is Nominal exactly like a struct's (see isEnumType), so
			// this check must precede the struct check below. The type is a
			// tagged union (10.35) exactly when the caller's union map, built
			// from reachable payload-carrying constructions, contains it: such
			// a local is declared as the union's tagged struct and initialized
			// from a variant construction (see buildUnionLocalDeclaration). A
			// plain-enum-typed local is declared as the enum typedef and
			// initialized from a variant literal (an EnumVariantValue, e.g.
			// Color.green, or a zero-payload VariantConstruct, e.g.
			// Color.red()); every other enum initializer shape is a clean
			// rejection.
			if _, isUnion := unions[initValue.Type]; isUnion {
				return buildUnionLocalDeclaration(unit, snapshot, fileSet, statement, initValue, scope, indent, context, unions, width)
			}
			return buildEnumLocalDeclaration(unit, snapshot, fileSet, statement, initValue, scope, indent, context, width)
		}
		if isStruct(snapshot, initValue.Type) {
			if runtimeType(unit, snapshot, initValue.Type) != 0 {
				return buildRuntimeLocalDeclaration(unit, snapshot, fileSet, statement, initValue, scope, indent, context, width)
			}
			// A struct-typed local: its type is the initializer value's Type
			// (the Initialize node carries no Type itself, confirmed against
			// a real fixture — same as tuple/array/optional locals). The
			// supported initializer is a RecordConstruct (a struct literal);
			// every other struct initializer shape is a clean rejection.
			return buildStructLocalDeclaration(unit, snapshot, fileSet, statement, initValue, scope, indent, context, width)
		}
		if isStr(snapshot, initValue.Type) {
			// A str-typed local: its type is the initializer value's Type
			// (the Initialize node carries no Type itself, confirmed against
			// a real fixture — same as the compound locals above). The
			// supported initializer is a StringLiteral (a string literal), or
			// since 10.36 a call to a str-returning helper (a DirectCall whose
			// result type is str, the one supported call position for
			// declaring a str local); every other str initializer shape is a
			// clean rejection.
			return buildStrLocalDeclaration(unit, snapshot, fileSet, statement, initValue, scope, indent, context, width)
		}
		if isSlice(snapshot, initValue.Type) {
			// A slice-typed local: its type is the initializer value's Type
			// (the Initialize node carries no Type itself, same as every other
			// compound local). The supported initializer is a CheckedSlice
			// (a slice expression like a[1:3]), or since 10.38 a call to a
			// slice-returning helper (a DirectCall whose result type is the
			// slice type, the one supported call position for declaring a
			// slice local); every other slice initializer
			// shape is a clean rejection.
			return buildSliceLocalDeclaration(unit, snapshot, fileSet, statement, initValue, scope, indent, context, width)
		}
		if isPointer(snapshot, initValue.Type) {
			// A pointer-typed local: its type is the initializer value's Type
			// (the Initialize node carries no Type itself, same as every other
			// compound local). The supported initializers are an AddressOf
			// expression (`let p *i32 = &y;`), another pointer-typed local
			// (pointer-to-pointer copy), a nil literal, a pointer-returning
			// call, or an explicit pointer-to-pointer cast; every other
			// pointer initializer shape is a clean rejection.
			return buildPointerLocalDeclaration(unit, snapshot, fileSet, statement, initValue, scope, indent, context, width)
		}
		if isFunctionType(snapshot, initValue.Type) {
			// A function-typed local: its type is the initializer value's Type
			// (the Initialize node carries no Type itself, same as every other
			// compound local). The supported initializer is a function value —
			// a bare top-level function reference (a HoistedFunctionValue) or
			// another function-typed local (a SymbolValue); every other
			// function initializer shape is a clean rejection (see
			// buildFunctionLocalDeclaration).
			return buildFunctionLocalDeclaration(unit, snapshot, fileSet, statement, initValue, scope, indent, context, width)
		}
		pre, core, err := buildScalarInitializeCore(unit, snapshot, fileSet, statement, initValue, scope, context, width)
		if err != nil {
			return "", err
		}
		// A local that a later statement never reads would otherwise trigger
		// -Wunused-variable under the mandated -Wall -Wextra -Werror; a
		// redundant (void) cast is a no-op when the local IS read later, so it
		// is emitted unconditionally rather than tracking whether a use
		// actually follows. buildScalarInitializeCore produced the declaration
		// core (`<cType> pebble_local_<id> = <expr>`); the indent, the
		// statement-terminating `;`, and the (void) cast turn it into this
		// full statement, byte-identical to before this helper was extracted.
		// A non-empty pre (a temp declaration for a force-unwrap of a call
		// result, e.g. `let v = m.get(5)!;`) is threaded as an extra leading
		// statement, the same mechanical shape buildForInitClause returns its
		// OptionalIntegerToEnum pre.
		if pre != "" {
			return indent + pre + "\n" + indent + core + ";\n" + indent + fmt.Sprintf("(void)pebble_local_%d;", statement.Symbol), nil
		}
		return indent + core + ";\n" + indent + fmt.Sprintf("(void)pebble_local_%d;", statement.Symbol), nil
	case tir.Store:
		// A Store reassigns a local declared earlier in this block or an
		// enclosing one; it does not declare a new symbol, so it never
		// touches scope. The checker refuses to emit a Store targeting a
		// `let` (C0606: the assignment place is not writable), so any
		// Store this backend sees, from real source, necessarily targets
		// a `var`. The value text is built by buildStoreCore (shared with the
		// for-loop update clause); the indent and the trailing `;` turn it
		// into this full statement, byte-identical to before the helper was
		// extracted.
		core, err := buildStoreCore(unit, snapshot, fileSet, statement, scope, context, width, unions)
		if err != nil {
			return "", err
		}
		return indent + core + ";", nil
	case tir.CompoundStore:
		// A CompoundStore is a compound assignment — `i += 1;`, `arr[j] -= 1;`,
		// `self.field *= 2;`, `*p /= 3;`, `x %= 4;` — or a postfix `i++` /
		// `i--` (the checker builds a postfix update as a CompoundStore with +
		// or - and a literal-one value child, so one emission covers both). It
		// does not declare a new symbol, so it never touches scope, exactly
		// like a Store. The value text is built by buildCompoundStore (shared
		// with the for-loop update clause); the indent and the trailing `;`
		// turn it into this full statement, mirroring the Store case.
		pre, core, err := buildCompoundStore(unit, snapshot, fileSet, id, statement, scope, context, width)
		if err != nil {
			return "", err
		}
		if pre != "" {
			return indent + pre + "\n" + indent + core + ";", nil
		}
		return indent + core + ";", nil
	case tir.ExpressionStatement:
		// A bare discarded-expression statement — `helper();` written as its
		// own statement, its result unused (a tir.ExpressionStatement wrapping
		// exactly one value, built by the checker's controlExpression case).
		// The only supported shape is a call to a void-returning function,
		// emitted as a bare C statement by the shared buildExpressionStatement
		// (also used by buildDeferredStatements for a deferred call); any other
		// discarded expression — a non-call value, or a call to a non-void-
		// returning function — is a clean rejection naming what was found.
		return buildExpressionStatement(unit, snapshot, fileSet, statement, scope, indent, context, width)
	case tir.Print:
		// A print statement — `print a, b, c;` — emitted as one combined
		// printf call by the shared buildPrint (also used by buildLoopBody's
		// explicit Print case and buildDeferredStatements for a deferred
		// print), so the emission logic lives in exactly one place. A
		// slice-index operand's temp declaration is returned as a leading
		// pre-statement and threaded into this statement sequence before the
		// printf line, exactly as a return threads its pre-return temp.
		pre, line, err := buildPrint(unit, snapshot, fileSet, statement, scope, indent, context, width)
		if err != nil {
			return "", err
		}
		if pre != "" {
			return pre + "\n" + line, nil
		}
		return line, nil
	case tir.If:
		// A conditional if statement as an ordinary leading statement — the
		// non-tail shape, e.g. a guard clause `if x > 0 { return 1; }`
		// followed by more code. Its arms are fall-through statement sequences
		// (no required tail, optional else), built by buildLeadingIf, the
		// top-level twin of buildLoopIf.
		return buildLeadingIf(unit, snapshot, fileSet, statement, scope, depth, width, result, unions, context)
	case tir.Switch:
		// A switch statement as an ordinary leading statement — a non-tail
		// switch whose case bodies may fall through or return, built by
		// buildLoopSwitch (the same fall-through switch the loop-body/arm
		// position uses). This is the only place a top-level function body can
		// contain a non-tail switch; see buildLoopSwitch.
		return buildLoopSwitch(unit, snapshot, fileSet, statement, scope, depth, width, result, unions)
	default:
		return "", fmt.Errorf("%s statement is a %s, want a local declaration (Initialize), a reassignment (Store), a compound assignment or postfix increment/decrement (CompoundStore), a call to a void-returning function used as a statement (ExpressionStatement), a print statement (Print), a conditional if statement (If), or a switch statement (Switch)", context, statement.Kind)
	}
}

// buildExpressionStatement builds the C statement text for one
// tir.ExpressionStatement — a bare discarded-expression statement such as
// `helper();` written as its own statement, produced by the checker's
// controlExpression case with no StatementForm set and a single value child
// (confirmed against real fixtures). It is the statement-context twin of a
// value-context call: a tir.DirectCall to a function of ANY result type — a
// void-returning helper called purely for its side effect, or a call to a
// non-void-returning function whose result is deliberately discarded (legal
// Pebble, and ordinary C: discarding a function's return value at a bare call
// statement is always allowed, never warning even under -Wall -Wextra -Werror)
// — emitted as `pebble_fn_<calleeSymbolID>(ctx, <args>);` at the given indent,
// mirroring buildDirectCall's two return shapes exactly
// (`pebble_fn_<sym>(ctx)` with no arguments, `pebble_fn_<sym>(ctx, <args>)`
// with some) but as a statement instead of a value expression. The callee is
// resolved through findFunctionDeclaration, and its result type places no
// restriction on the shape: the call is emitted identically whether it returns
// void or a value, and whatever it returns is discarded by C. The call text
// itself is built by buildDirectCall unchanged, so argument building, context
// threading, and the convention/context-action checks are identical to a
// value-context call. Any other ExpressionStatement child — a discarded
// non-call expression (anything that is not a DirectCall, MethodCall, or
// IndirectCall — the latter handled separately above) — is a clean rejection
// naming what was found. The function is shared by buildLeadingStatement's
// ExpressionStatement case (which covers both buildBlock's and buildLoopBody's
// leading-statement sequences) and buildDeferredStatements' deferred-statement
// case, so the emission logic lives in exactly one place.
func buildExpressionStatement(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, statement tir.Node, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind) (string, error) {
	if len(statement.Children) != 1 {
		return "", fmt.Errorf("%s discarded-expression statement has %d child(ren), want exactly one (the expression being discarded)", context, len(statement.Children))
	}
	expr, ok := unit.Node(statement.Children[0])
	if !ok {
		return "", fmt.Errorf("%s discarded-expression statement references invalid value node %d", context, statement.Children[0])
	}
	if expr.Kind == tir.IndirectCall {
		callExpr, err := buildIndirectCall(unit, snapshot, fileSet, expr, scope, width)
		if err != nil {
			return "", err
		}
		return indent + callExpr + ";", nil
	}
	if expr.Kind != tir.DirectCall && expr.Kind != tir.MethodCall {
		return "", fmt.Errorf("%s discarded-expression statement discards a %s, which is not supported as a bare statement yet (only a direct, method, or indirect call is)", context, expr.Kind)
	}
	callPre, callExpr, err := buildDirectCallWithPre(unit, snapshot, fileSet, expr, scope, width)
	if err != nil {
		return "", err
	}
	// A bare call statement is a leading-statement position, so an inline
	// slice-construction argument's temp declaration is emitted as a preceding
	// statement line at the same indent (the buildScalarInitializeCore pre
	// threading shape).
	if callPre != "" {
		return indent + callPre + "\n" + indent + callExpr + ";", nil
	}
	return indent + callExpr + ";", nil
}

// buildPrint builds the C text for one print statement — a tir.Print whose
// Children are the printed operands in source order, one node per operand
// (built by the checker's controlPrint case from `print a, b, c;`, each
// operand independently type-checked). The emission matches v1's print
// codegen shape exactly: ONE combined printf call per print statement, not
// one per operand — every operand's format specifier is concatenated into a
// single format string (ending in the literal `\n`, so every print statement
// produces exactly one line of output) and every operand's value is a single
// argument, in the same order. The checker already restricts print operands to
// exactly bool, char, str, any integer builtin, or any float builtin (C0612 —
// a nominal enum operand like `print Color.red;` is rejected upstream), so
// this dispatch is exactly the set of values this backend already knows how
// to build, each through its OWN existing builder, never a new value-building
// path:
//
//   - integer (Int/Uint/I8/I16/I32/I64/U8/U16/U32/U64) — buildExpr at the
//     operand's own resolved integer kind, with the format specifier chosen
//     from the <inttypes.h> PRId8/PRId16/PRId32/PRId64/PRIu8/PRIu16/PRIu32/
//     PRIu64 macros (string-literal-concatenated into the format string at
//     compile time) so the specifier's width exactly matches the fixed-width
//     C type cType produces for the operand — never a hand-picked %hhd/%hd
//     whose width only happens to match — keeping the mandated
//     -Wall -Wextra -Werror build -Wformat-clean for every integer width.
//   - bool — buildBoolExpr, wrapped in a C ternary (`<expr> ? "true" :
//     "false"`) so the printed text is the word true/false; the format
//     specifier is %s, exactly v1's approach of making the bool argument a
//     const char * before the format string is built.
//   - char — buildCharOperand (a char's C type is the fixed int32_t scalar
//     value, which this backend never hands to printf directly): the scalar is
//     encoded to UTF-8 by the runtime helper pebble_rt_char_to_utf8 into a
//     fresh per-operand uint8_t[5] buffer declared in the print's leading
//     pre-statements, and the combined printf gets the buffer cast to
//     const char * under a %s specifier. A bare %c would write only one byte,
//     corrupting every char beyond U+007F, so every char operand — ASCII or
//     not — goes through the helper.
//   - str — buildStrOperand (a str local, a string literal, or a call to a
//     str-returning helper), with the argument being the value's .data field
//     cast to const char * (the runtime's PebbleStr is
//     { const uint8_t *data; size_t len; }, and %s wants a char *; the
//     explicit cast is what keeps the emitted call -Wformat-clean), specifier
//     %s.
//   - float (F32/F64) — buildFloatExpr at the operand's own float kind;
//     f32/f64 promote to double in a variadic call either way, so %f for both
//     is correct and matches v1.
//
// The emitted statement is:
//
//	<indent>printf("<spec0><spec1>...\n", <arg0>, <arg1>);
//
// with the \n spelled as the C two-character escape, and no arguments when the
// print has no operands (only reachable from hand-built IR — the parser
// requires at least one expression — emitting printf("\n") to print a blank
// line, matching v1's zero-expression print). Each integer specifier is
// emitted as the macro name OUTSIDE the surrounding quotes (`"%"PRId32`, not
// `"%PRId32"`), so the preprocessor expands the macro to the exact-width
// specifier text and adjacent-literal concatenation folds it into the format
// string; the bool/char/str/float specifiers are the plain `%s`/`%s`/`%s`/`%f`
// literals (a char's %s is backed by the pre-statement UTF-8 buffer above).
// Every operand value is built
// under the grammar its own resolved type selects; a print operand of any type
// the checker does not allow as printable is a clean rejection naming what was
// found, never guessed. The function is shared by buildLeadingStatement's
// Print case (which covers both buildBlock's and buildLoopBody's
// leading-statement sequences), buildLoopBody's explicit Print case, and
// buildDeferredStatements' deferred-statement case, so the emission logic
// lives in exactly one place.
func buildPrint(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, statement tir.Node, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind) (string, string, error) {
	var formatParts []string
	var args []string
	var preParts []string
	for _, childID := range statement.Children {
		child, ok := unit.Node(childID)
		if !ok {
			return "", "", fmt.Errorf("%s print statement references invalid operand node %d", context, childID)
		}
		// A parenthesized operand — `print ("hi")` — arrives wrapped in
		// tir.SourceAlias nodes (one per grouping level, confirmed against a
		// real fixture dump), which record grouped-expression parens and
		// nothing else, so the operand is unwrapped to its innermost node
		// before its type is dispatched on. Unwrapping here keeps the
		// per-type value builders untouched (buildExpr/buildBoolExpr/
		// buildFloatExpr unwrap a SourceAlias themselves, but buildCharOperand
		// and buildStrOperand have no SourceAlias case); the unwrapped node
		// carries the same Type the SourceAlias did, so the dispatch below is
		// exactly what the checker validated.
		operandID := childID
		for child.Kind == tir.SourceAlias {
			if len(child.Children) != 1 {
				return "", "", fmt.Errorf("%s print operand is a SourceAlias with %d child(ren), want exactly one", context, len(child.Children))
			}
			operandID = child.Children[0]
			child, ok = unit.Node(operandID)
			if !ok {
				return "", "", fmt.Errorf("%s print statement references invalid operand node %d", context, operandID)
			}
		}
		kind, ok := resolvedBuiltin(snapshot, child.Type)
		if !ok {
			return "", "", fmt.Errorf("%s print operand is a %s of type %s, want bool, char, str, an integer, or a float", context, child.Kind, describeType(snapshot, child.Type))
		}
		// A bare CheckedIndex whose base is a SLICE-typed value (not a str) —
		// `print view()[0];`, indexing a call's slice result directly, the
		// confirmed real-world gap this slice closes. The indexed element read
		// needs the base materialized into a temp local (see
		// buildSliceIndexValue), whose temp-declaration statement this print
		// position — a statement sequence — hosts as a leading pre-statement,
		// exactly as buildReturnStatement threads a pre-return temp. A
		// str-index base (char result) stays on buildCharOperand's own
		// CheckedIndex case below.
		isSliceIndex := false
		if child.Kind == tir.CheckedIndex {
			strBase, err := checkedIndexBaseIsStr(unit, snapshot, child)
			if err != nil {
				return "", "", err
			}
			isSliceIndex = !strBase
		}
		var arg string
		var err error
		var sliceIndexPre string
		var charPreParts []string
		switch {
		case cType(kind) != "":
			// An integer operand of any builtin width, not just the entry's
			// own: its value is built by buildExpr at its own resolved kind
			// (re-checking every node in the expression carries that width,
			// exactly as a scalar local declaration does), and its specifier
			// comes from the <inttypes.h> PRI* macros whose expansion matches
			// the operand's fixed-width C type. The macro name is emitted
			// OUTSIDE the string quotes (`"%"PRId32`), so the preprocessor
			// expands it to the specifier text and the adjacent literals
			// concatenate into the format string — never spelled
			// `"%PRId32"`, which would put a literal invalid `%P` specifier
			// in the format. A slice-index operand's read (element C type)
			// carries the same width as its element, so the same specifier
			// applies.
			formatParts = append(formatParts, `"%"`+printfSpecifier(kind))
			if isSliceIndex {
				sliceIndexPre, arg, err = buildSliceIndexValue(unit, snapshot, fileSet, operandID, child, scope, width, false)
			} else {
				arg, err = buildExpr(unit, snapshot, fileSet, operandID, scope, kind, width)
			}
		case kind == types.Bool:
			// A bool operand prints as the words true/false: build the bool
			// expression under the bool grammar, then wrap it in the C ternary
			// that selects the const char * literal, so the %s specifier's
			// argument is already the pointer the format string wants — v1's
			// own approach for bool in print. A slice-index operand's read is
			// a C bool, so the same ternary wraps it.
			formatParts = append(formatParts, `"%s"`)
			if isSliceIndex {
				var read string
				sliceIndexPre, read, err = buildSliceIndexValue(unit, snapshot, fileSet, operandID, child, scope, width, true)
				if err == nil {
					arg = "(" + read + " ? \"true\" : \"false\")"
				}
			} else {
				arg, err = buildBoolExpr(unit, snapshot, fileSet, operandID, scope, width)
				if err == nil {
					arg = "(" + arg + " ? \"true\" : \"false\")"
				}
			}
		case kind == types.Char:
			// A char operand prints as the UTF-8 encoding of the single
			// character its int32_t scalar value encodes. The scalar is built
			// under the char grammar (buildCharOperand, or the slice-index
			// read for a slice-element operand — both yield the same int32_t
			// char value), then the runtime helper pebble_rt_char_to_utf8
			// encodes it — 1-4 UTF-8 bytes plus the trailing NUL — into a
			// fresh per-operand uint8_t[5] buffer declared in this print's
			// pre-statements, and the combined printf's %s consumes that
			// buffer. The scalar is never passed to printf directly: a %c
			// writes only a single byte, so any char beyond U+007F would print
			// corrupt bytes instead of its full UTF-8 sequence (the exact bug
			// this encoding closes); routing every char operand — ASCII
			// included — through the helper keeps the emitted C uniform. The
			// buffer name derives from the operand's own unwrapped node ID —
			// the stable identity of this operand, unique across the unit — so
			// a print with several char operands gets one distinct buffer each
			// with no collision, the same pebble_slice_index_<nodeID>
			// convention buildSliceIndexValue uses for its temp.
			formatParts = append(formatParts, `"%s"`)
			var scalar string
			if isSliceIndex {
				sliceIndexPre, scalar, err = buildSliceIndexValue(unit, snapshot, fileSet, operandID, child, scope, width, false)
			} else {
				scalar, err = buildCharOperand(unit, snapshot, fileSet, operandID, scope, width)
			}
			if err == nil {
				bufferName := fmt.Sprintf("pebble_char_utf8_%d", operandID)
				charPreParts = append(charPreParts,
					fmt.Sprintf("uint8_t %s[5];", bufferName),
					fmt.Sprintf("pebble_rt_char_to_utf8(%s, %s);", scalar, bufferName))
				arg = "(const char *)" + bufferName
			}
		case kind == types.Str:
			// A str operand prints its bytes: the value is built under the
			// str grammar, and the %s argument is the value's .data field cast
			// to const char * (the reachable str values this backend builds
			// all originate from NUL-terminated C string literals, so %s
			// reads exactly the intended bytes).
			formatParts = append(formatParts, `"%s"`)
			arg, err = buildStrOperand(unit, snapshot, fileSet, operandID, scope, width)
			if err == nil {
				arg = "(const char *)" + arg + ".data"
			}
		case kind == types.F32 || kind == types.F64:
			// A float operand prints with %f; f32/f64 promote to double in a
			// variadic call either way, so the one specifier covers both,
			// matching v1. The value is built under the float grammar at its
			// own float kind.
			formatParts = append(formatParts, `"%f"`)
			arg, err = buildFloatExpr(unit, snapshot, fileSet, operandID, scope, kind)
		default:
			return "", "", fmt.Errorf("%s print operand is a %s of type %s, want bool, char, str, an integer, or a float", context, child.Kind, describeType(snapshot, child.Type))
		}
		if err != nil {
			return "", "", err
		}
		if sliceIndexPre != "" {
			preParts = append(preParts, indent+sliceIndexPre)
		}
		for _, pre := range charPreParts {
			preParts = append(preParts, indent+pre)
		}
		args = append(args, arg)
	}
	line := indent + "printf(" + strings.Join(formatParts, "") + `"\n"`
	if len(args) != 0 {
		line += ", " + strings.Join(args, ", ")
	}
	return strings.Join(preParts, "\n"), line + ");", nil
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

func buildRuntimeLocalDeclaration(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, statement, initValue tir.Node, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind) (string, error) {
	if initValue.Kind != tir.FieldValue && initValue.Kind != tir.Load && initValue.Kind != tir.SymbolValue {
		return "", fmt.Errorf("%s declares a runtime-typed local initialized from a %s", context, initValue.Kind)
	}
	expr, err := buildRuntimeValue(unit, snapshot, fileSet, initValue, scope, width)
	if err != nil {
		return "", err
	}
	scope[statement.Symbol] = localInfo{runtimeType: initValue.Type}
	return fmt.Sprintf("%s%s pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, runtimeTypeName(unit, snapshot, initValue.Type), statement.Symbol, expr, indent, statement.Symbol), nil
}

func buildRuntimeValue(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, node tir.Node, scope map[symbol.SymbolID]localInfo, width types.BuiltinKind) (string, error) {
	if node.Kind == tir.SymbolValue {
		if node.Symbol == unit.Runtime().Context {
			return "(*ctx)", nil
		}
		if info, ok := scope[node.Symbol]; ok && info.runtimeType != 0 {
			return fmt.Sprintf("pebble_local_%d", node.Symbol), nil
		}
	}
	if node.Kind == tir.FieldValue && len(node.Children) == 1 {
		baseNode, ok := unit.Node(node.Children[0])
		if !ok {
			return "", fmt.Errorf("invalid runtime receiver")
		}
		base, err := buildRuntimeValueNode(unit, snapshot, fileSet, node.Children[0], scope, width)
		if err != nil {
			return "", err
		}
		owner := baseNode.Type
		if field, ok := runtimeFieldName(unit, owner, node.Member); ok {
			return base + "." + field, nil
		}
	}
	if node.Kind == tir.Load && len(node.Children) == 1 {
		place, ok := unit.Node(node.Children[0])
		if !ok {
			return "", fmt.Errorf("invalid runtime field place")
		}
		if place.Kind == tir.FieldPlace {
			return buildStructFieldRead(unit, snapshot, fileSet, place, scope, width, false)
		}
	}
	return "", fmt.Errorf("runtime value %s is not supported", node.Kind)
}

func buildRuntimeValueNode(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, id tir.NodeID, scope map[symbol.SymbolID]localInfo, width types.BuiltinKind) (string, error) {
	node, ok := unit.Node(id)
	if !ok {
		return "", fmt.Errorf("invalid runtime value node %d", id)
	}
	if node.Kind == tir.ContextValue {
		return "(*ctx)", nil
	}
	return buildRuntimeValue(unit, snapshot, fileSet, node, scope, width)
}

// resultInfo records what the enclosing function's tail return must produce:
// an ordinary scalar — the entry's resolved integer width, in kind — a str
// value, in isStr, a char value, in isChar, a tuple, in tuple (its types.TypeID), a struct, in
// structType, a slice, in sliceType, an optional, in optionalType, or a
// function value, in functionType (its types.TypeID). The fields are
// mutually exclusive, mirroring localInfo: kind is zero for a compound or str
// or char result (a tuple/struct is not a types.BuiltinKind), isStr is true only for a
// str result, isChar is true only for a char result (whose C return type is
// the fixed int32_t, independent of the entry's width), and tuple/structType/sliceType/optionalType/functionType are zero
// for a scalar result. It is threaded alongside width through buildBlock and
// buildIf so a tuple/struct-returning helper's tail-position Return builds its
// value via buildAggregateReturnValue (a SymbolValue naming a matching
// aggregate-typed local, or a fresh TupleValue/RecordConstruct), a
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

// buildTupleLocalDeclaration builds one tuple-typed local's declaration: a
// `pebble_tuple_<typeID>_t pebble_local_<symbol> = { <element>, ... };` whose
// element expressions are the TupleValue initializer's children in order, each
// built by the grammar its own element type selects — buildExpr for an element
// of the entry's width, buildBoolExpr for a bool element. Every element type
// must be exactly the entry's width or bool; anything else (a str element, a
// nested tuple element) is a clean rejection naming the element position, since
// this backend emits exactly those two C field types. The local's scope entry
// records its tuple type (a localInfo with tuple set), so a later element read
// resolves the tuple type being indexed. Two initializer shapes are supported
// (10.26): a TupleValue (a tuple literal), emitted as a bare brace list, or a
// DirectCall to a tuple-returning helper whose result type matches the local's
// declared type, emitted by the same call-building machinery buildExpr's
// DirectCall case uses (see buildAggregateCallInitializer). Initializing a
// tuple local from any other value — a whole-tuple copy of another local,
// anything else — is a clean rejection. Like every scalar local, the
// declaration is followed by a (void) cast against -Wunused-variable.
func buildTupleLocalDeclaration(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, statement, initValue tir.Node, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind) (string, error) {
	if initValue.Kind == tir.DirectCall || initValue.Kind == tir.MethodCall {
		// A call to a tuple-returning helper used as the direct initializer of
		// a matching tuple-typed local — `let t (i32, i32) =
		// helperReturningTuple();` — the one position (10.26) in which calling
		// a tuple-returning helper is supported.
		return buildAggregateCallInitializer(unit, snapshot, fileSet, statement, initValue, scope, indent, context, width, true)
	}
	if initValue.Kind != tir.TupleValue {
		return "", fmt.Errorf("%s declares a tuple-typed local of type %s initialized from a %s, want a TupleValue (a tuple literal) or a call to a tuple-returning helper; initializing a tuple local from another value is not supported yet", context, tupleTypeName(initValue.Type), initValue.Kind)
	}
	braceList, err := buildTupleBraceList(unit, snapshot, fileSet, initValue, scope, context, width)
	if err != nil {
		return "", err
	}
	scope[statement.Symbol] = localInfo{tuple: initValue.Type}
	return fmt.Sprintf("%s%s pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, tupleTypeName(initValue.Type), statement.Symbol, braceList, indent, statement.Symbol), nil
}

// buildAggregateCallInitializer builds a tuple/struct-typed local's declaration
// whose initializer is a DirectCall to a helper returning the same aggregate
// type (10.26): `let t (i32, i32) = helperReturningTuple();`. This is the one
// position in which calling a tuple/struct-returning helper is supported — the
// direct initializer of a matching aggregate-typed local declaration. The
// call's result type is the DirectCall node's own Type, which is the callee's
// resolved result type (confirmed against a real fixture), and it must be
// exactly the local's declared type — double-checked against the callee's
// declared ResultType (defense for hand-built IR), so the emitted C never
// initializes a local of one aggregate type from a call returning another. The
// call itself is built by buildDirectCall, the same call-building machinery
// buildExpr's DirectCall case uses, so context and argument handling are
// identical to a scalar call — only the result type differs. wantTuple selects
// the tuple grammar (the local is declared pebble_tuple_<typeID>_t and its
// scope entry records localInfo{tuple}) over the struct grammar
// (pebble_struct_<typeID>_t and localInfo{structType}). Like every local, the
// declaration is followed by a (void) cast against -Wunused-variable.
func buildAggregateCallInitializer(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, statement, initValue tir.Node, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind, wantTuple bool) (string, error) {
	calleeDecl, err := findCallDeclaration(unit, initValue)
	if err != nil {
		return "", err
	}
	if calleeDecl.ResultType != initValue.Type {
		what := "tuple"
		if !wantTuple {
			what = "struct"
		}
		return "", fmt.Errorf("%s declares a %s-typed local of type %s initialized from a call to symbol %d whose declared result type %s does not match", context, what, describeType(snapshot, initValue.Type), initValue.Symbol, describeType(snapshot, calleeDecl.ResultType))
	}
	callPre, callExpr, err := buildDirectCallWithPre(unit, snapshot, fileSet, initValue, scope, width)
	if err != nil {
		return "", err
	}
	if wantTuple {
		scope[statement.Symbol] = localInfo{tuple: initValue.Type}
		return withLeadingPre(callPre, indent, fmt.Sprintf("%s%s pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, tupleTypeName(initValue.Type), statement.Symbol, callExpr, indent, statement.Symbol)), nil
	}
	scope[statement.Symbol] = localInfo{structType: initValue.Type}
	return withLeadingPre(callPre, indent, fmt.Sprintf("%s%s pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, structTypeName(initValue.Type), statement.Symbol, callExpr, indent, statement.Symbol)), nil
}

func buildArrayCallInitializer(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, statement, initValue tir.Node, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind) (string, error) {
	calleeDecl, err := findCallDeclaration(unit, initValue)
	if err != nil {
		return "", err
	}
	if calleeDecl.ResultType != initValue.Type {
		return "", fmt.Errorf("%s declares an array-typed local of type %s initialized from a call whose result type is %s", context, describeType(snapshot, initValue.Type), describeType(snapshot, calleeDecl.ResultType))
	}
	callPre, callExpr, err := buildDirectCallWithPre(unit, snapshot, fileSet, initValue, scope, width)
	if err != nil {
		return "", err
	}
	scope[statement.Symbol] = localInfo{array: initValue.Type, arrayWrapped: true}
	return withLeadingPre(callPre, indent, fmt.Sprintf("%s%s pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, arrayTypeName(initValue.Type), statement.Symbol, callExpr, indent, statement.Symbol)), nil
}

// buildOptionalCallInitializer builds an optional-typed local's declaration
// whose initializer is a DirectCall to a helper returning the same optional
// type: `var o ?int = f();`. This mirrors the tuple/struct
// buildAggregateCallInitializer shape exactly: an optional result, like a
// tuple/struct result, is a C struct type read back whole at the call site, so
// the call expression (built by buildDirectCall, the same call-building
// machinery buildExpr's DirectCall case uses) is the local's whole initializer
// — a `pebble_optional_<typeID>_t pebble_local_<symbol> = f(ctx, ...);`
// assignment. The call's result type is the DirectCall node's own Type, which
// is the callee's resolved result type, and it must be exactly the local's
// declared type — double-checked against the callee's declared ResultType
// (defense for hand-built IR), so the emitted C never initializes a local of
// one optional type from a call returning another. The local's scope entry
// records its optional type (localInfo with optional set), so a later
// has_value read or force-unwrap resolves the optional type being unwrapped.
// Like every local, the declaration is followed by a (void) cast against
// -Wunused-variable.
func buildOptionalCallInitializer(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, statement, initValue tir.Node, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind) (string, error) {
	calleeDecl, err := findCallDeclaration(unit, initValue)
	if err != nil {
		return "", err
	}
	if calleeDecl.ResultType != initValue.Type {
		return "", fmt.Errorf("%s declares an optional-typed local of type %s initialized from a call to symbol %d whose declared result type %s does not match", context, describeType(snapshot, initValue.Type), initValue.Symbol, describeType(snapshot, calleeDecl.ResultType))
	}
	callPre, callExpr, err := buildDirectCallWithPre(unit, snapshot, fileSet, initValue, scope, width)
	if err != nil {
		return "", err
	}
	scope[statement.Symbol] = localInfo{optional: initValue.Type}
	return withLeadingPre(callPre, indent, fmt.Sprintf("%s%s pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, optionalTypeName(initValue.Type), statement.Symbol, callExpr, indent, statement.Symbol)), nil
}

// buildTupleBraceList validates one TupleValue node's element list and builds
// its brace-list content, `{ <e0>, <e1>, ... }`, with each element expression
// built by the grammar its own element type selects — buildExpr for an element
// of the entry's width, buildBoolExpr for a bool element. Every element type
// must be exactly the entry's width or bool; anything else (a str element, a
// nested tuple element) is a clean rejection naming the element position,
// since this backend emits exactly those two C field types. context names the
// enclosing construct in error messages. The function is shared by the two
// places a TupleValue's elements are built (10.25): a tuple-typed local's
// declaration initializer (buildTupleLocalDeclaration embeds the returned
// brace list in the declaration statement) and a freshly-constructed tuple
// built inline as a call argument (buildTupleValueExpr wraps the same brace
// list in a compound-literal cast), so element-type validation and the
// buildExpr/buildBoolExpr dispatch live in exactly one place.
func buildTupleBraceList(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, node tir.Node, scope map[symbol.SymbolID]localInfo, context string, width types.BuiltinKind) (string, error) {
	key, ok := snapshot.Key(node.Type)
	if !ok {
		return "", fmt.Errorf("%s contains a tuple value whose type %d is not in the type snapshot", context, node.Type)
	}
	elements, ok := key.Elements()
	if !ok {
		return "", fmt.Errorf("%s contains a tuple value of type %s, which has no element list", context, tupleTypeName(node.Type))
	}
	if len(node.Children) != len(elements) {
		return "", fmt.Errorf("%s contains a tuple value of type %s with %d element expression(s), want %d (one per declared element)", context, tupleTypeName(node.Type), len(node.Children), len(elements))
	}
	exprs := make([]string, len(elements))
	for i, elementType := range elements {
		switch {
		case isWidth(snapshot, width, elementType):
			elementExpr, err := buildExpr(unit, snapshot, fileSet, node.Children[i], scope, width, width)
			if err != nil {
				return "", err
			}
			exprs[i] = elementExpr
		case isBool(snapshot, elementType):
			elementExpr, err := buildBoolExpr(unit, snapshot, fileSet, node.Children[i], scope, width)
			if err != nil {
				return "", err
			}
			exprs[i] = elementExpr
		case isTuple(snapshot, elementType):
			elementExpr, err := buildNestedAggregateValue(unit, snapshot, fileSet, node.Children[i], scope, elementType, context, width)
			if err != nil {
				return "", err
			}
			exprs[i] = elementExpr
		case isStruct(snapshot, elementType):
			elementExpr, err := buildNestedAggregateValue(unit, snapshot, fileSet, node.Children[i], scope, elementType, context, width)
			if err != nil {
				return "", err
			}
			exprs[i] = elementExpr
		case isOptional(snapshot, elementType):
			elementExpr, err := buildNestedAggregateValue(unit, snapshot, fileSet, node.Children[i], scope, elementType, context, width)
			if err != nil {
				return "", err
			}
			exprs[i] = elementExpr
		default:
			return "", fmt.Errorf("%s contains a tuple value of type %s whose element %d is %s, want %s or bool", context, tupleTypeName(node.Type), i, describeType(snapshot, elementType), wantName(width))
		}
	}
	return "{ " + strings.Join(exprs, ", ") + " }", nil
}

// buildTupleValueExpr builds a freshly-constructed tuple value as an ordinary
// C expression (10.25): a TupleValue node lowered to a positional C99 compound
// literal, `(pebble_tuple_<typeID>_t){ <e0>, <e1>, ... }`, whose element
// expressions are the TupleValue's children in order — the tuple typedef's
// field order is already the construction order, so a positional compound
// literal is a direct, correct lowering. The element list is built and
// validated by buildTupleBraceList (the same logic a tuple-typed local's
// declaration initializer uses), so an element of any type other than the
// entry's width or bool is rejected exactly the same way it would be in a
// declaration. The cast makes the compound literal a value usable anywhere a
// tuple-typed value is needed — in this slice, only as a call argument for a
// tuple-typed parameter (buildAggregateArgument). The node must be a
// TupleValue; the caller already guarantees this, so the kind check is defense
// for hand-built IR.
func buildTupleValueExpr(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, node tir.Node, scope map[symbol.SymbolID]localInfo, context string, width types.BuiltinKind) (string, error) {
	if node.Kind != tir.TupleValue {
		return "", fmt.Errorf("%s contains a %s, want a TupleValue (a tuple literal)", context, node.Kind)
	}
	braceList, err := buildTupleBraceList(unit, snapshot, fileSet, node, scope, context, width)
	if err != nil {
		return "", err
	}
	return fmt.Sprintf("(%s)%s", tupleTypeName(node.Type), braceList), nil
}

func buildNestedAggregateValue(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, id tir.NodeID, scope map[symbol.SymbolID]localInfo, typ types.TypeID, context string, width types.BuiltinKind) (string, error) {
	node, ok := unit.Node(id)
	if !ok {
		return "", fmt.Errorf("%s references invalid aggregate value", context)
	}
	if node.Kind == tir.SymbolValue {
		info, ok := scope[node.Symbol]
		if !ok {
			return "", fmt.Errorf("%s references unknown aggregate symbol", context)
		}
		if info.tuple != typ && info.array != typ && info.optional != typ && info.structType != typ {
			return "", fmt.Errorf("%s aggregate symbol has the wrong type", context)
		}
		return fmt.Sprintf("pebble_local_%d", node.Symbol), nil
	}
	switch {
	case isTuple(snapshot, typ):
		return buildTupleValueExpr(unit, snapshot, fileSet, node, scope, context, width)
	case isStruct(snapshot, typ):
		return buildStructValueExpr(unit, snapshot, fileSet, node, scope, context, width)
	case isOptional(snapshot, typ):
		return buildOptionalValueExpr(unit, snapshot, fileSet, node, scope, context, width)
	}
	return "", fmt.Errorf("%s aggregate type is unsupported", context)
}

// buildArrayLocalDeclaration builds a fixed-length C array from an ArrayValue
// literal or an ArrayRepeat ([v; N]) initializer. Array elements use the same
// integer/bool builders as scalar locals; nested arrays and all other element
// types remain out of scope. An ArrayValue initializer emits the array's
// declaration directly with a C brace-list initializer (10.20); an ArrayRepeat
// initializer is emitted by buildArrayRepeatLocalDeclaration as a three-
// statement sequence (bare declaration, one-time-evaluated repeat temp, fill
// loop) so the repeat value is evaluated exactly once, not once per slot
// (10.27).
func buildArrayLocalDeclaration(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, statement, initValue tir.Node, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind) (string, error) {
	if initValue.Kind != tir.ArrayValue && initValue.Kind != tir.ArrayRepeat {
		return "", fmt.Errorf("%s declares an array-typed local of type %s initialized from a %s, want an ArrayValue (an array literal) or an ArrayRepeat (a [v; N] repeat initializer); initializing an array local from another value is not supported yet", context, describeType(snapshot, initValue.Type), initValue.Kind)
	}
	key, ok := snapshot.Key(initValue.Type)
	if !ok {
		return "", fmt.Errorf("%s declares an array-typed local whose type %d is not in the type snapshot", context, initValue.Type)
	}
	length, elementType, ok := key.Array()
	if !ok {
		return "", fmt.Errorf("%s declares an array-typed local of type %s, which has no array length and element type", context, describeType(snapshot, initValue.Type))
	}
	if _, err := arrayLengthLiteral(length, width); err != nil {
		return "", fmt.Errorf("%s: %v", context, err)
	}
	// Every scalar element type must be a fixed-width integer builtin (resolved
	// to its OWN width by resolvedBuiltin/cType — the entry's width, uint, u8,
	// u16, u32, u64, i8, i16, i32, or i64), char, or bool — the same
	// isSupportedSliceElementType gate the slice-side element builders use,
	// widened here from the entry-width-only check so a slice constructed from
	// an array of u8/char/etc. elements (a[1:3] over a real [N]u8 backing
	// array) can actually be built. Tuple/array/optional/struct elements remain
	// accepted exactly as before. An enum element is a Nominal type exactly
	// like a struct element (see isEnumType) and is rejected here explicitly,
	// since enum-typed array elements are out of scope.
	if isEnumType(unit, snapshot, elementType) {
		return "", fmt.Errorf("%s declares an array-typed local of type %s whose element type %s is an enum type; enum-typed array elements are not supported yet", context, describeType(snapshot, initValue.Type), enumTypeName(elementType))
	}
	if !isSupportedSliceElementType(unit, snapshot, elementType) {
		return "", fmt.Errorf("%s declares an array-typed local of type %s whose element type is %s, want a fixed-width integer, char, bool, or an aggregate element type", context, describeType(snapshot, initValue.Type), describeType(snapshot, elementType))
	}
	scope[statement.Symbol] = localInfo{array: initValue.Type}
	if initValue.Kind == tir.ArrayRepeat {
		return buildArrayRepeatLocalDeclaration(unit, snapshot, fileSet, statement, initValue, scope, indent, context, width, length, elementType)
	}
	if len(initValue.Children) != int(length) {
		return "", fmt.Errorf("%s declares an array-typed local of type %s with %d element expression(s), want %d", context, describeType(snapshot, initValue.Type), len(initValue.Children), length)
	}
	exprs := make([]string, len(initValue.Children))
	for i, child := range initValue.Children {
		var expr string
		var err error
		if isBool(snapshot, elementType) {
			expr, err = buildBoolExpr(unit, snapshot, fileSet, child, scope, width)
		} else if isChar(snapshot, elementType) {
			expr, err = buildCharOperand(unit, snapshot, fileSet, child, scope, width)
		} else if elementWidth, integerElement := resolvedBuiltin(snapshot, elementType); integerElement && cType(elementWidth) != "" {
			// An integer element of any fixed-width builtin, not just the
			// entry's own: each element is built at the element's OWN resolved
			// width (an element of a [3]u8 array inside an i32 function builds
			// its value at u8), mirroring how buildScalarInitializeCore builds
			// a scalar local at its own declared width.
			expr, err = buildExpr(unit, snapshot, fileSet, child, scope, elementWidth, width)
		} else if isTuple(snapshot, elementType) {
			expr, err = buildNestedAggregateValue(unit, snapshot, fileSet, child, scope, elementType, context, width)
		} else if isStruct(snapshot, elementType) {
			expr, err = buildNestedAggregateValue(unit, snapshot, fileSet, child, scope, elementType, context, width)
		} else if isOptional(snapshot, elementType) {
			expr, err = buildNestedAggregateValue(unit, snapshot, fileSet, child, scope, elementType, context, width)
		} else {
			expr, err = buildExpr(unit, snapshot, fileSet, child, scope, width, width)
		}
		if err != nil {
			return "", err
		}
		exprs[i] = expr
	}
	elementCType, err := arrayElementCType(unit, snapshot, width, elementType)
	if err != nil {
		return "", fmt.Errorf("%s: %v", context, err)
	}
	return fmt.Sprintf("%s%s pebble_local_%d[%d] = { %s };\n%s(void)pebble_local_%d;", indent, elementCType, statement.Symbol, length, strings.Join(exprs, ", "), indent, statement.Symbol), nil
}

// buildArrayRepeatLocalDeclaration builds an array-typed local whose
// initializer is an ArrayRepeat ([v; N]): a single value expression repeated
// N times. The local is emitted as three C statements instead of one
// declaration line, so the repeat value is evaluated exactly once rather than
// once per slot (a naive brace-list { v, v, v } would re-evaluate v N times —
// wrong if v has any observable side effect, e.g. a checked-arithmetic panic
// or a call):
//
//	<indent>int32_t pebble_local_<sym>[<len>];
//	<indent>int32_t pebble_repeat_<sym> = <v>;
//	<indent>for (size_t pebble_i_<sym> = 0; pebble_i_<sym> < <len>; pebble_i_<sym>++) {
//	<indent>    pebble_local_<sym>[pebble_i_<sym>] = pebble_repeat_<sym>;
//	<indent>}
//	<indent>(void)pebble_local_<sym>;
//
// Both synthetic names derive from the local's own declaration symbol
// (pebble_repeat_<symbolID>, pebble_i_<symbolID>), which is guaranteed
// collision-free by construction: ArrayRepeat only ever appears as that one
// local's own initializer, so no other statement in the same function can
// reuse the symbol ID. The loop counter is size_t (C's own array-indexing
// idiom, available because pebble_rt.h includes <stddef.h>); comparing it
// against the array-length literal compiles clean under -Wall -Wextra -Werror
// (confirmed with a real cc compile), so no signed/unsigned adjustment is
// needed. The repeat value v is built by the grammar its element type selects
// — buildExpr for an element of the entry's width, buildBoolExpr for a bool
// element — and appears in the emitted C exactly once, so it is evaluated
// exactly once at runtime. The count child of an ArrayRepeat node is a
// synthesized compile-time IntegerLiteral of the snapshot's uint builtin that
// always equals the array type's own TypeKey.Array() length (confirmed
// against a real fixture: the checker builds it from the array's declared
// length in check's ir_builder_literals.go), so the loop bound comes from
// length here, and a count child that is not such a matching literal is a
// clean rejection for hand-built IR, never a guessed loop bound. The local's
// scope entry records the array type (a localInfo with array set) exactly as
// the ArrayValue path does, so element reads afterward resolve through the
// existing Load(CheckedIndexPlace) machinery unchanged — nothing about how
// the array is read changes, only how it is initialized. Like every local,
// the sequence ends with the (void) cast against -Wunused-variable.
func buildArrayRepeatLocalDeclaration(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, statement, initValue tir.Node, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind, length uint64, elementType types.TypeID) (string, error) {
	if len(initValue.Children) != 2 {
		return "", fmt.Errorf("%s declares an array-typed local from ArrayRepeat with %d child(ren), want exactly two (the repeated value and the count)", context, len(initValue.Children))
	}
	countNode, ok := unit.Node(initValue.Children[1])
	if !ok {
		return "", fmt.Errorf("%s declares an array-typed local from ArrayRepeat referencing invalid count node %d", context, initValue.Children[1])
	}
	if countNode.Kind != tir.IntegerLiteral {
		return "", fmt.Errorf("%s declares an array-typed local from ArrayRepeat whose count is a %s, want a compile-time integer literal equal to the array's declared length %d", context, countNode.Kind, length)
	}
	if countNode.Type != snapshot.Builtins().Uint {
		return "", fmt.Errorf("%s declares an array-typed local from ArrayRepeat whose count has type %s, want uint (the count is a synthesized integer literal)", context, describeType(snapshot, countNode.Type))
	}
	count, err := strconv.ParseUint(countNode.Literal.IntegerNum, 10, 64)
	if err != nil {
		return "", fmt.Errorf("%s declares an array-typed local from ArrayRepeat whose count %q is not a valid non-negative integer", context, countNode.Literal.IntegerNum)
	}
	if count != length {
		return "", fmt.Errorf("%s declares an array-typed local from ArrayRepeat whose count %d does not equal the array's declared length %d", context, count, length)
	}
	var valueExpr string
	if isBool(snapshot, elementType) {
		valueExpr, err = buildBoolExpr(unit, snapshot, fileSet, initValue.Children[0], scope, width)
	} else if isChar(snapshot, elementType) {
		valueExpr, err = buildCharOperand(unit, snapshot, fileSet, initValue.Children[0], scope, width)
	} else if elementWidth, integerElement := resolvedBuiltin(snapshot, elementType); integerElement && cType(elementWidth) != "" {
		valueExpr, err = buildExpr(unit, snapshot, fileSet, initValue.Children[0], scope, elementWidth, width)
	} else {
		valueExpr, err = buildExpr(unit, snapshot, fileSet, initValue.Children[0], scope, width, width)
	}
	if err != nil {
		return "", err
	}
	ctype, err := arrayElementCType(unit, snapshot, width, elementType)
	if err != nil {
		return "", fmt.Errorf("%s: %v", context, err)
	}
	statements := []string{
		fmt.Sprintf("%s%s pebble_local_%d[%d];", indent, ctype, statement.Symbol, length),
		fmt.Sprintf("%s%s pebble_repeat_%d = %s;", indent, ctype, statement.Symbol, valueExpr),
		fmt.Sprintf("%sfor (size_t pebble_i_%d = 0; pebble_i_%d < %d; pebble_i_%d++) {", indent, statement.Symbol, statement.Symbol, length, statement.Symbol),
		fmt.Sprintf("%s    pebble_local_%d[pebble_i_%d] = pebble_repeat_%d;", indent, statement.Symbol, statement.Symbol, statement.Symbol),
		fmt.Sprintf("%s}", indent),
		fmt.Sprintf("%s(void)pebble_local_%d;", indent, statement.Symbol),
	}
	return strings.Join(statements, "\n"), nil
}

// buildSliceLocalDeclaration builds a slice-typed local's declaration from a
// CheckedSlice initializer (a slice expression like `var s []i32 = a[1:3];`)
// or, since 10.38, a DirectCall to a slice-returning helper (`var s []i32 =
// helperReturningSlice();`). The emitted C constructs a small struct with a
// data pointer (offset from the base array by the checked start) and a len
// field (end - start). The start bound is validated by
// pebble_rt_checked_slice_start_i32/i64, which panics if the range is
// invalid. Bounds omitted in source are resolved to their defaults: 0 for
// an absent start, the base array's compile-time element count for an absent
// end. The local's scope entry records its slice type (localInfo.sliceType)
// so a later index read resolves through the slice-indexing machinery.
//
// The construction is emitted as two C statements rather than one compound-
// literal initializer because the data pointer depends on the result of the
// pebble_rt_checked_slice_start call, which cannot appear as a sub-expression
// of its own compound literal (the pointer would reference a temporary).
// Instead: first store the validated start offset in a temp, then construct
// the slice struct using the temp for both the pointer offset and the length
// computation.
func buildSliceLocalDeclaration(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, statement, initValue tir.Node, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind) (string, error) {
	if initValue.Kind == tir.DirectCall || initValue.Kind == tir.MethodCall {
		// A call to a slice-returning helper used as the direct initializer of a
		// matching slice-typed local — `let s []i32 = helperReturningSlice();` —
		// the position (10.38) in which a slice-returning helper's result lands
		// in a slice local, mirroring buildStrLocalDeclaration's own DirectCall
		// case. The call's result type is the DirectCall node's own Type, which
		// is the callee's resolved result type (confirmed against a real
		// fixture), and it must be exactly the local's declared type — double-
		// checked against the callee's declared ResultType (defense for
		// hand-built IR), so the emitted C never initializes a slice local from
		// a call returning another type. The call itself is built by
		// buildDirectCall, the same call-building machinery buildExpr's
		// DirectCall case uses. Like every local, the declaration is followed
		// by a (void) cast against -Wunused-variable.
		calleeDecl, err := findCallDeclaration(unit, initValue)
		if err != nil {
			return "", err
		}
		if calleeDecl.ResultType != initValue.Type {
			return "", fmt.Errorf("%s declares a slice-typed local of type %s initialized from a call to symbol %d whose declared result type %s does not match", context, sliceTypeName(initValue.Type), initValue.Symbol, describeType(snapshot, calleeDecl.ResultType))
		}
		callPre, callExpr, err := buildDirectCallWithPre(unit, snapshot, fileSet, initValue, scope, width)
		if err != nil {
			return "", err
		}
		scope[statement.Symbol] = localInfo{sliceType: initValue.Type}
		return withLeadingPre(callPre, indent, fmt.Sprintf("%s%s pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, sliceTypeName(initValue.Type), statement.Symbol, callExpr, indent, statement.Symbol)), nil
	}
	if initValue.Kind == tir.SliceFromRaw {
		construction, err := buildRawSliceConstruction(unit, snapshot, fileSet, initValue, scope, width, context)
		if err != nil {
			return "", err
		}
		scope[statement.Symbol] = localInfo{sliceType: initValue.Type}
		return fmt.Sprintf("%s%s pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, sliceTypeName(initValue.Type), statement.Symbol, construction, indent, statement.Symbol), nil
	}
	if initValue.Kind == tir.Load {
		// A by-value read of a slice-typed struct field used as a slice
		// local's declaration initializer — `var old_entries = self.entries;`,
		// the std/hmap.peb rehash shape — lowered by the checker to a Load of
		// a FieldPlace naming the slice field (the same Load(FieldPlace)
		// shape a slice field read in any other value position uses). The
		// emitted C is a whole-struct copy declaration,
		// `pebble_slice_<typeID>_t pebble_local_<symbol> = <lvalue>;`, where
		// the lvalue is the field projection built by buildPlaceLValue, and
		// its resolved type must be exactly the local's declared type
		// (defense for hand-built IR). Like every local, the declaration is
		// followed by a (void) cast against -Wunused-variable.
		if len(initValue.Children) != 1 {
			return "", fmt.Errorf("%s declares a slice-typed local of type %s initialized from a Load with %d child(ren), want exactly one place", context, sliceTypeName(initValue.Type), len(initValue.Children))
		}
		lvalue, elementType, err := buildPlaceLValue(unit, snapshot, fileSet, initValue.Children[0], scope, width)
		if err != nil {
			return "", fmt.Errorf("%s slice-field read: %v", context, err)
		}
		if elementType != initValue.Type {
			return "", fmt.Errorf("%s declares a slice-typed local of type %s initialized from a read of element type %s", context, sliceTypeName(initValue.Type), describeType(snapshot, elementType))
		}
		scope[statement.Symbol] = localInfo{sliceType: initValue.Type}
		return fmt.Sprintf("%s%s pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, sliceTypeName(initValue.Type), statement.Symbol, lvalue, indent, statement.Symbol), nil
	}
	tempDecl, constructionExpr, err := buildSliceConstruction(unit, snapshot, fileSet, initValue, scope, indent, context, width, fmt.Sprintf("pebble_slice_start_%d", statement.Symbol))
	if err != nil {
		return "", err
	}
	scope[statement.Symbol] = localInfo{sliceType: initValue.Type}
	return strings.Join([]string{
		tempDecl,
		fmt.Sprintf("%s%s pebble_local_%d = %s;", indent, sliceTypeName(initValue.Type), statement.Symbol, constructionExpr),
		fmt.Sprintf("%s(void)pebble_local_%d;", indent, statement.Symbol),
	}, "\n"), nil
}

func buildRawSliceConstruction(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, node tir.Node, scope map[symbol.SymbolID]localInfo, width types.BuiltinKind, context string) (string, error) {
	if len(node.Children) != 2 {
		return "", fmt.Errorf("%s SliceFromRaw has %d children, want two", context, len(node.Children))
	}
	ptr, err := buildExpr(unit, snapshot, fileSet, node.Children[0], scope, width, width)
	if err != nil {
		return "", err
	}
	countNode, ok := unit.Node(node.Children[1])
	if !ok {
		return "", fmt.Errorf("%s SliceFromRaw references invalid count node", context)
	}
	var count string
	if countNode.Kind == tir.SymbolValue {
		if _, declared := scope[countNode.Symbol]; !declared {
			return "", fmt.Errorf("%s slice count references symbol %d outside the current scope", context, countNode.Symbol)
		}
		count = fmt.Sprintf("pebble_local_%d", countNode.Symbol)
	} else if countNode.Kind == tir.IntegerLiteral {
		litWidth, _ := resolvedBuiltin(snapshot, countNode.Type)
		count = integerLiteralText(countNode.Literal.IntegerNum, litWidth)
	} else {
		count, err = buildUintExpr(unit, snapshot, fileSet, node.Children[1], scope, width)
		if err != nil {
			return "", err
		}
	}
	return fmt.Sprintf("(%s){ .data = %s, .len = (size_t)(%s) }", sliceTypeName(node.Type), ptr, count), nil
}

func buildUintExpr(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, id tir.NodeID, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind) (string, error) {
	node, ok := unit.Node(id)
	if !ok || !isUint(snapshot, node.Type) {
		return "", fmt.Errorf("uint expression has invalid node or type")
	}
	switch node.Kind {
	case tir.SourceAlias:
		if len(node.Children) != 1 {
			return "", fmt.Errorf("uint source alias has %d children", len(node.Children))
		}
		return buildUintExpr(unit, snapshot, fileSet, node.Children[0], locals, width)
	case tir.IntegerLiteral:
		litWidth, _ := resolvedBuiltin(snapshot, node.Type)
		return integerLiteralText(node.Literal.IntegerNum, litWidth), nil
	case tir.SizeofType:
		// The result of `sizeof T` for a uint expression. The emitted C is
		// `sizeof(<T's C type>)` where the C type is resolved from the
		// SizeofType's TypeArg: a fixed-width integer at its own C type
		// (int32_t for int/i32, int64_t for i64, uint64_t for uint/u64 — the
		// original three-width dispatch), or any aggregate's own typedef name
		// — the std/hmap.peb rehash/with_capacity shape (`new_cap * (sizeof
		// Entry[K, V])`) where the TypeArg is the Entry STRUCT type, which
		// must lower to sizeof(pebble_struct_<typeID>_t), never the default
		// sizeof(uint64_t) the builtin-only three-width dispatch would fall
		// through to (that would size the allocation 8 bytes per Entry
		// instead of 12, corrupting the rehash table).
		typeName, err := sizeofCTypeName(unit, snapshot, node.TypeArg)
		if err != nil {
			return "", err
		}
		return "sizeof(" + typeName + ")", nil
	case tir.SymbolValue:
		if _, ok := locals[node.Symbol]; !ok {
			return "", fmt.Errorf("uint expression references unknown symbol %d", node.Symbol)
		}
		return fmt.Sprintf("pebble_local_%d", node.Symbol), nil
	case tir.IntegerCast:
		// An integer value cast to uint (`x as uint`), the std/hmap.peb
		// insert/get shape (`(hash as uint) % self.cap`). The destination is
		// uint by construction (buildUintExpr's gate above already required
		// node.Type to be uint), so the whole cast lowers to a plain C cast
		// to uint's own C type (uint64_t). The single child is built at its
		// OWN width: a uint-typed child recurses into this same builder (a
		// uint-to-uint cast is a no-op but must still be accepted), and any
		// other fixed-width integer child — u64 for hmap's hash (the
		// checker anchors the hash to u64, so the child is a u64-typed
		// SymbolValue or call) — is built by buildExpr at the child's own
		// resolved width, since u64 (and every other non-uint integer)
		// flows through the general path.
		if len(node.Children) != 1 {
			return "", fmt.Errorf("uint expression contains an IntegerCast with %d children, want exactly one", len(node.Children))
		}
		child, ok := unit.Node(node.Children[0])
		if !ok {
			return "", fmt.Errorf("uint expression IntegerCast references invalid child node %d", node.Children[0])
		}
		childWidth, integerChild := resolvedBuiltin(snapshot, child.Type)
		var childExpr string
		var err error
		if isUint(snapshot, child.Type) {
			childExpr, err = buildUintExpr(unit, snapshot, fileSet, node.Children[0], locals, width)
		} else if integerChild && cType(childWidth) != "" {
			childExpr, err = buildExpr(unit, snapshot, fileSet, node.Children[0], locals, childWidth, width)
		} else {
			return "", fmt.Errorf("uint expression IntegerCast child has type %s, want a fixed-width integer", describeType(snapshot, child.Type))
		}
		if err != nil {
			return "", err
		}
		return "(" + cType(types.Uint) + ")(" + childExpr + ")", nil
	case tir.PointerToInteger:
		// A pointer value cast to uint (`ptr as uint`), the uint twin of
		// buildExpr's PointerToInteger case: the whole cast lowers to a plain
		// C cast to uint's own C type (uint64_t) of the single child pointer
		// expression, built by buildExpr (whose pointer branch handles every
		// pointer-value shape). The destination is uint by construction
		// (buildUintExpr's gate above already required node.Type to be uint).
		if len(node.Children) != 1 {
			return "", fmt.Errorf("uint expression contains a PointerToInteger with %d children, want exactly one", len(node.Children))
		}
		childExpr, err := buildExpr(unit, snapshot, fileSet, node.Children[0], locals, width, width)
		if err != nil {
			return "", err
		}
		return "(" + cType(types.Uint) + ")(" + childExpr + ")", nil
	case tir.Load:
		// A by-value read of a uint-typed place — a uint struct field read
		// (`var old_cap = self.cap;`, the std/hmap.peb rehash shape, lowered
		// to Load(FieldPlace)), a uint-typed tuple element, or a uint element
		// of a slice/array — used as a uint value. The Load's Type is already
		// gated to uint by buildUintExpr's entry check, so the whole read is
		// the place's C lvalue built by buildPlaceLValue (the same projection
		// a uint field write or address-of targets), whose resolved type must
		// be uint (defense for hand-built IR).
		if len(node.Children) != 1 {
			return "", fmt.Errorf("uint expression contains a Load with %d children, want exactly one place", len(node.Children))
		}
		lvalue, placeType, err := buildPlaceLValue(unit, snapshot, fileSet, node.Children[0], locals, width)
		if err != nil {
			return "", fmt.Errorf("uint expression place: %v", err)
		}
		if !isUint(snapshot, placeType) {
			return "", fmt.Errorf("uint expression reads a place of type %s, want uint", describeType(snapshot, placeType))
		}
		return lvalue, nil
	case tir.CheckedOptionalUnwrap:
		// A force-unwrap of an optional whose payload is uint (`tombstone_index!`
		// where tombstone_index is `?uint`, the std/hmap.peb insert shape). The
		// unwrap is bounds-checked via the runtime helper selected from the
		// PAYLOAD's own type (optionalUnwrapSuffix maps a uint payload to the
		// u64 helper, since uint's .value field is uint64_t) — passing the
		// optional local's has_value and value fields. The single child is a
		// SymbolValue naming the optional-typed local.
		unwrapSuffix := optionalUnwrapSuffix(snapshot, node.Type)
		if unwrapSuffix == "" {
			return "", fmt.Errorf("uint expression contains a CheckedOptionalUnwrap of a %s payload, which has no runtime unwrap helper", describeType(snapshot, node.Type))
		}
		if len(node.Children) != 1 {
			return "", fmt.Errorf("uint expression contains a CheckedOptionalUnwrap with %d child(ren), want exactly one (the optional value being unwrapped)", len(node.Children))
		}
		child, ok := unit.Node(node.Children[0])
		if !ok {
			return "", fmt.Errorf("uint expression contains a CheckedOptionalUnwrap referencing invalid child node %d", node.Children[0])
		}
		if child.Kind != tir.SymbolValue {
			return "", fmt.Errorf("uint expression contains a CheckedOptionalUnwrap whose child is a %s, want a SymbolValue naming an optional-typed local", child.Kind)
		}
		if info, declared := locals[child.Symbol]; !declared || info.optional == 0 {
			return "", fmt.Errorf("uint expression contains a CheckedOptionalUnwrap of symbol %d, which is not an optional-typed local", child.Symbol)
		}
		return fmt.Sprintf("pebble_rt_checked_unwrap_%s(pebble_local_%d.has_value, pebble_local_%d.value, %s)", unwrapSuffix, child.Symbol, child.Symbol, buildSourceLoc(fileSet, node.Span)), nil
	case tir.CheckedArithmetic:
		if len(node.Children) != 2 {
			return "", fmt.Errorf("uint arithmetic has %d operands", len(node.Children))
		}
		left, err := buildUintExpr(unit, snapshot, fileSet, node.Children[0], locals, width)
		if err != nil {
			return "", err
		}
		right, err := buildUintExpr(unit, snapshot, fileSet, node.Children[1], locals, width)
		if err != nil {
			return "", err
		}
		op, ok := arithmeticOperator(node.Operator)
		if !ok {
			return "", fmt.Errorf("unsupported uint arithmetic operator %s", node.Operator)
		}
		return fmt.Sprintf("(%s %s %s)", left, op, right), nil
	case tir.DirectCall:
		// A call to a uint-returning helper used as a uint value (`var n =
		// get_count();`, or std/io.peb's read_line shape `var bytes = read(
		// file, &ch as *void, 1);` — bytes is inferred uint from read's own
		// declared result type, so its initializer is a DirectCall node the
		// checker routes through this builder). The emitted C is
		// `pebble_fn_<callee>(ctx, ...)` — a C function call whose return
		// type IS the callee's declared uint64_t result (helperSignature
		// declares a uint-result helper with cType(types.Uint) == uint64_t,
		// the exact C type a uint value uses, the same pattern the
		// PointerToInteger/IntegerCast cases above use for their own uint-cast
		// wrapping), so the expression is directly a uint value, no cast
		// needed. The callee's declared result type is double-checked to be
		// uint (defense for hand-built IR — buildUintExpr's entry gate already
		// required node.Type to be uint, matching the defensive result-type
		// checks other cases here do, e.g. CheckedOptionalUnwrap's payload
		// check). The call is built by buildDirectCallWithPre, and a
		// non-empty pre (an inline slice-construction argument, whose temp
		// declaration a pure expression position cannot place) is a clean
		// rejection, never silently dropped: buildUintExpr returns (string,
		// error) with no pre-threading and is called from pure expression
		// positions throughout this file (local declarations, returns,
		// compound assignments, slice/array indices, struct field values,
		// and more), so the pre-bearing shape is deliberately out of scope
		// rather than a signature-widening refactor across all its call
		// sites.
		calleeDecl, err := findCallDeclaration(unit, node)
		if err != nil {
			return "", err
		}
		if !isUint(snapshot, calleeDecl.ResultType) {
			return "", fmt.Errorf("uint expression contains a call to symbol %d whose declared result type %s is not uint", node.Symbol, describeType(snapshot, calleeDecl.ResultType))
		}
		callPre, callExpr, err := buildDirectCallWithPre(unit, snapshot, fileSet, node, locals, width)
		if err != nil {
			return "", err
		}
		if callPre != "" {
			return "", fmt.Errorf("uint expression contains a call to symbol %d whose argument is an inline slice construction (a CheckedSlice), which is not supported in this uint expression position: a pure expression position has nowhere to place the temp-declaration statement the slice construction needs; bind the slice into a local first", node.Symbol)
		}
		return callExpr, nil
	default:
		return "", fmt.Errorf("unsupported uint expression node %s", node.Kind)
	}
}

func arithmeticOperator(op syntax.TokenKind) (string, bool) {
	switch op {
	case syntax.Plus:
		return "+", true
	case syntax.Minus:
		return "-", true
	case syntax.Star:
		return "*", true
	case syntax.Slash:
		return "/", true
	case syntax.Percent:
		return "%", true
	default:
		return "", false
	}
}

// buildSliceConstruction validates one CheckedSlice node (a slice expression
// `a[start:end]`) and builds the two pieces of C text its construction needs:
// a temp-declaration statement holding the checked-start result, and the
// compound-literal construction expression that uses that temp for both its
// .data pointer offset and its .len subtraction. The two-statement shape is
// required because the temp can't be a sub-expression of the very compound
// literal it initializes (the pointer offset would reference a value not yet
// computed in a well-defined order within one expression) — the same
// construction shape 10.37 established for a slice-typed local's declaration,
// kept here so both callers share one source of truth rather than two copies
// that could drift. tempName is the deterministic C identifier of the temp
// variable, derived by the caller from a stable identity (a slice local's own
// declaration symbol for a local declaration; the return value node's NodeID
// for a slice-returning helper's tail return). The declaration statement (with
// indent) and the construction expression (unindented, a C99 compound literal)
// are returned separately so each caller assembles them into its own statement
// shape: buildSliceLocalDeclaration embeds the expression in a local
// declaration statement, and buildSliceReturnValue hands the declaration back
// to buildBlock/buildSwitchCaseBody to thread in as an extra pre-return
// statement before the final return line. The construction reuses the exact
// validation 10.37 established: the base must be an array-typed local in
// scope, the slice's element type must equal the base array's element type,
// and that element type must be the entry's resolved width or bool.
func buildSliceConstruction(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, initValue tir.Node, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind, tempName string) (string, string, error) {
	if initValue.Kind != tir.CheckedSlice {
		return "", "", fmt.Errorf("%s slice construction is a %s, want a CheckedSlice", context, initValue.Kind)
	}
	if len(initValue.Children) < 1 {
		return "", "", fmt.Errorf("%s CheckedSlice has %d child(ren), want at least one (the base array)", context, len(initValue.Children))
	}
	baseNode, ok := unit.Node(initValue.Children[0])
	if !ok {
		return "", "", fmt.Errorf("%s CheckedSlice references invalid base node %d", context, initValue.Children[0])
	}
	// Two base shapes are accepted:
	//
	// 1. A SymbolValue naming an ARRAY-typed local in scope (the original,
	//    unchanged path): the array local decays to a pointer to its first
	//    element, and the array's length is a compile-time constant
	//    (arrayLengthLiteral) used both as the default end bound and as the
	//    upper bound the runtime helper validates the range against.
	//
	// 2. A Load of a SLICE-typed place — re-slicing an EXISTING slice value
	//    (`self.data[:self.len]`, std/string.peb's String::as_slice, lowered
	//    to a Load(FieldPlace) reading the slice-typed struct field). There is
	//    no compile-time length for a slice base: the new slice's .data
	//    pointer must offset the EXISTING slice's own runtime .data, and the
	//    upper bound is the EXISTING slice's runtime .len field.
	isSliceBase := baseNode.Kind == tir.Load && isSlice(snapshot, baseNode.Type)
	var baseInfo localInfo
	if !isSliceBase {
		if baseNode.Kind != tir.SymbolValue {
			return "", "", fmt.Errorf("%s slice base is a %s, want a SymbolValue naming an array local", context, baseNode.Kind)
		}
		var declared bool
		baseInfo, declared = scope[baseNode.Symbol]
		if !declared {
			return "", "", fmt.Errorf("%s slice base references symbol %d, which is not a local in scope", context, baseNode.Symbol)
		}
		if baseInfo.array == 0 {
			return "", "", fmt.Errorf("%s slice base is not an array-typed local", context)
		}
	}
	sliceType := initValue.Type
	sliceKey, ok := snapshot.Key(sliceType)
	if !ok {
		return "", "", fmt.Errorf("%s slice type %d is not in the type snapshot", context, sliceType)
	}
	sliceElementType, ok := sliceKey.Child()
	if !ok {
		return "", "", fmt.Errorf("%s slice type %s has no element type", context, describeType(snapshot, sliceType))
	}
	if !isSupportedSliceElementType(unit, snapshot, sliceElementType) {
		return "", "", fmt.Errorf("%s slice element type is %s, want a fixed-width integer, char, bool, tuple, optional, or struct", context, describeType(snapshot, sliceElementType))
	}
	// Per-base resolve the element type the new slice's element type must
	// match, plus the base-specific length and data-pointer expressions. The
	// array base's validations are exactly the original ones; the slice base
	// is the parallel Load(FieldPlace) path.
	var lengthLiteral, defaultEnd, dataExpr string
	if !isSliceBase {
		arrayKey, ok := snapshot.Key(baseInfo.array)
		if !ok {
			return "", "", fmt.Errorf("%s base array type %d is not in the type snapshot", context, baseInfo.array)
		}
		length, arrayElementType, ok := arrayKey.Array()
		if !ok {
			return "", "", fmt.Errorf("%s base is not an array type", context)
		}
		if sliceElementType != arrayElementType {
			return "", "", fmt.Errorf("%s slice element type %s does not match base array element type %s", context, describeType(snapshot, sliceElementType), describeType(snapshot, arrayElementType))
		}
		if _, err := arrayLengthLiteral(length, width); err != nil {
			return "", "", fmt.Errorf("%s: %v", context, err)
		}
		lengthLiteral, _ = arrayLengthLiteral(length, width)
		defaultEnd = fmt.Sprintf("%d", length)
		dataExpr = fmt.Sprintf("pebble_local_%d", baseNode.Symbol)
	} else {
		// A Load of a slice-typed place: the base slice value's own C lvalue
		// (built by buildPlaceLValue, the same projection a slice-typed struct
		// field read in any other value position uses). Its .data and .len
		// sub-fields are the base slice's own storage pointer and runtime
		// length.
		if len(baseNode.Children) != 1 {
			return "", "", fmt.Errorf("%s slice base Load has %d child(ren), want exactly one place", context, len(baseNode.Children))
		}
		baseLvalue, baseType, err := buildPlaceLValue(unit, snapshot, fileSet, baseNode.Children[0], scope, width)
		if err != nil {
			return "", "", fmt.Errorf("%s slice base read: %v", context, err)
		}
		if !isSlice(snapshot, baseType) {
			return "", "", fmt.Errorf("%s slice base Load reads a place of type %s, want a slice-typed place", context, describeType(snapshot, baseType))
		}
		baseSliceKey, ok := snapshot.Key(baseType)
		if !ok {
			return "", "", fmt.Errorf("%s base slice type %d is not in the type snapshot", context, baseType)
		}
		baseSliceElementType, ok := baseSliceKey.Child()
		if !ok {
			return "", "", fmt.Errorf("%s base slice type %s has no element type", context, describeType(snapshot, baseType))
		}
		if sliceElementType != baseSliceElementType {
			return "", "", fmt.Errorf("%s slice element type %s does not match base slice element type %s", context, describeType(snapshot, sliceElementType), describeType(snapshot, baseSliceElementType))
		}
		lengthLiteral = baseLvalue + ".len"
		defaultEnd = baseLvalue + ".len"
		dataExpr = baseLvalue + ".data"
	}
	// Extract start and end bounds from children. Children layout is
	// [base, start?, end?] with presence determined by
	// SliceStartPresent/SliceEndPresent.
	childIdx := 1
	var startExpr, endExpr string
	if initValue.SliceStartPresent {
		if childIdx >= len(initValue.Children) {
			return "", "", fmt.Errorf("%s CheckedSlice claims start present but has no start child", context)
		}
		startExpr = buildSliceBoundExpr(unit, snapshot, fileSet, initValue.Children[childIdx], scope, width, context)
		if startExpr == "" {
			return "", "", fmt.Errorf("%s failed to build slice start bound", context)
		}
		childIdx++
	} else {
		startExpr = "0"
	}
	if initValue.SliceEndPresent {
		if childIdx >= len(initValue.Children) {
			return "", "", fmt.Errorf("%s CheckedSlice claims end present but has no end child", context)
		}
		endExpr = buildSliceBoundExpr(unit, snapshot, fileSet, initValue.Children[childIdx], scope, width, context)
		if endExpr == "" {
			return "", "", fmt.Errorf("%s failed to build slice end bound", context)
		}
		childIdx++
	} else {
		endExpr = defaultEnd
	}
	if _, err := sliceElementCType(unit, snapshot, width, sliceElementType); err != nil {
		return "", "", fmt.Errorf("%s: %v", context, err)
	}
	startArg := startExpr
	if !initValue.SliceStartPresent {
		startArg = "0"
	}
	endArg := endExpr
	if !initValue.SliceEndPresent {
		endArg = lengthLiteral
	}
	sliceCType := sliceTypeName(sliceType)
	// Emit as two statements: first the checked-start call stored in a temp,
	// then the struct construction using the temp. The temp is declared at the
	// entry's own resolved width (cType(width)), matching whichever of
	// pebble_rt_checked_slice_start_i32/_i64 checkedSuffix(width) selects —
	// declaring it as a fixed int32_t regardless of width would silently
	// narrow an i64 entry's checked-start result.
	tempDecl := fmt.Sprintf("%s%s %s = pebble_rt_checked_slice_start_%s(%s, %s, %s, %s);", indent, cType(width), tempName, checkedSuffix(width), startArg, endArg, lengthLiteral, buildSourceLoc(fileSet, initValue.Span))
	constructionExpr := fmt.Sprintf("(%s){ .data = %s + %s, .len = (size_t)(%s - %s) }", sliceCType, dataExpr, tempName, endExpr, tempName)
	return tempDecl, constructionExpr, nil
}

// buildSliceBoundExpr builds the C expression for one slice bound (start or
// end). The bound may be an integer literal or a reference to a local.
func buildSliceBoundExpr(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, nodeID tir.NodeID, scope map[symbol.SymbolID]localInfo, width types.BuiltinKind, context string) string {
	boundNode, ok := unit.Node(nodeID)
	if !ok {
		return ""
	}
	if boundNode.Kind == tir.IntegerLiteral && boundNode.Type == snapshot.Builtins().Int {
		return boundNode.Literal.IntegerNum
	}
	if boundNode.Kind == tir.SymbolValue {
		if _, declared := scope[boundNode.Symbol]; declared {
			return fmt.Sprintf("pebble_local_%d", boundNode.Symbol)
		}
	}
	if isUint(snapshot, boundNode.Type) {
		// A uint-typed slice bound (a range-loop iterator or uint local whose
		// type the checker anchored to uint): built by the dedicated uint
		// grammar, mirroring the slice/array index dispatch — the general
		// buildExpr path rejects non-entry-width types.
		expr, err := buildUintExpr(unit, snapshot, fileSet, nodeID, scope, width)
		if err != nil {
			return ""
		}
		return expr
	}
	expr, err := buildExpr(unit, snapshot, fileSet, nodeID, scope, width, width)
	if err != nil {
		return ""
	}
	return expr
}

func arrayElementCType(unit *tir.Unit, snapshot *types.Snapshot, width types.BuiltinKind, id types.TypeID) (string, error) {
	if isBool(snapshot, id) {
		return "bool", nil
	}
	if isChar(snapshot, id) {
		return "int32_t", nil
	}
	if isTuple(snapshot, id) {
		return tupleTypeName(id), nil
	}
	if isOptional(snapshot, id) {
		return optionalTypeName(id), nil
	}
	if isStruct(snapshot, id) {
		if isEnumType(unit, snapshot, id) {
			return "", fmt.Errorf("array element type %s is an enum type; enum-typed array elements are not supported yet", enumTypeName(id))
		}
		return structTypeName(id), nil
	}
	// A scalar integer element resolves to its OWN width, not the ambient
	// entry width: a [3]u8 array inside an i32 function must declare its C
	// storage as uint8_t[3] (and a slice over it as a uint8_t* data pointer),
	// mirroring sliceElementCType's own element-own-width resolution.
	if elementWidth, integerElement := resolvedBuiltin(snapshot, id); integerElement && cType(elementWidth) != "" {
		return cType(elementWidth), nil
	}
	return "", fmt.Errorf("array element type %s is not supported", describeType(snapshot, id))
}

// sliceElementCType resolves the C pointer target type for a slice's data
// field: the element's C type. Any fixed-width integer builtin (resolved to its
// own width by resolvedBuiltin/cType), char (the fixed int32_t), bool, tuple
// (the tuple's own typedef), optional (the optional's own typedef), and struct
// (the struct's own typedef — an enum element is a Nominal type exactly like a
// struct element and is rejected here explicitly, since enum-typed slice
// elements are out of scope) are supported slice element types, matching
// isSupportedSliceElementType and arrayElementCType's own element handling.
// Any other element type is a clean rejection naming what was found.
func sliceElementCType(unit *tir.Unit, snapshot *types.Snapshot, width types.BuiltinKind, id types.TypeID) (string, error) {
	if isBool(snapshot, id) {
		return "bool", nil
	}
	if isChar(snapshot, id) {
		return "int32_t", nil
	}
	if isTuple(snapshot, id) {
		return tupleTypeName(id), nil
	}
	if isOptional(snapshot, id) {
		return optionalTypeName(id), nil
	}
	if isStruct(snapshot, id) {
		if isEnumType(unit, snapshot, id) {
			return "", fmt.Errorf("slice element type %s is an enum type; enum-typed slice elements are not supported yet", enumTypeName(id))
		}
		return structTypeName(id), nil
	}
	if elementWidth, integerElement := resolvedBuiltin(snapshot, id); integerElement && cType(elementWidth) != "" {
		return cType(elementWidth), nil
	}
	return "", fmt.Errorf("slice element type %s is not supported", describeType(snapshot, id))
}

// buildOptionalLocalDeclaration builds one optional-typed local's declaration:
// a `pebble_optional_<typeID>_t pebble_local_<symbol> = { .has_value = true,
// .value = <expr> };` for a SomeOptional initializer, or
// `{ .has_value = false, .value = 0 }` for a NoneOptional (`none` — the
// payload value is irrelevant when absent, so zero is fine), or — since the
// OptionalIntegerToEnum slice — a two-statement
// temp-declaration-plus-declaration fragment for an integer-to-optional-enum
// cast initializer (`var c ?Color = 5 as ?Color;`), built by
// buildOptionalIntegerToEnumDeclaration.
// The payload expression is built by the grammar its own type selects —
// buildExpr for an integer payload, buildBoolExpr for a bool payload — exactly
// like the tuple and array element builders. The local's scope entry records
// its optional type (a localInfo with optional set), so a later force-unwrap
// resolves the optional type being unwrapped. Every payload type must be exactly
// the entry's width or bool; anything else is a clean rejection naming the
// payload type, since this backend emits exactly those two C types as the value
// field. Like every scalar local, the declaration is followed by a (void) cast
// against -Wunused-variable.
func buildOptionalLocalDeclaration(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, statement, initValue tir.Node, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind, id tir.NodeID) (string, error) {
	key, ok := snapshot.Key(initValue.Type)
	if !ok {
		return "", fmt.Errorf("%s declares an optional-typed local whose type %d is not in the type snapshot", context, initValue.Type)
	}
	payloadType, ok := key.Child()
	if !ok {
		return "", fmt.Errorf("%s declares an optional-typed local of type %s, which has no payload type", context, optionalTypeName(initValue.Type))
	}
	switch initValue.Kind {
	case tir.SomeOptional, tir.OptionalInject:
		// SomeOptional (an explicit `some <expr>`) and OptionalInject (an
		// implicit injection, e.g. `var o ?int = 5;` with no `some` keyword)
		// both carry exactly one child, the payload expression, and lower to
		// the identical C: `{ .has_value = true, .value = <payload> }` — the
		// two TIR node kinds only distinguish authored syntax, not runtime
		// behavior, so they share this whole case body.
		if len(initValue.Children) != 1 {
			return "", fmt.Errorf("%s declares an optional-typed local from %s with %d child(ren), want exactly one payload expression", context, initValue.Kind, len(initValue.Children))
		}
		var valueExpr string
		payloadWidth, integerPayload := resolvedBuiltin(snapshot, payloadType)
		switch {
		case integerPayload && cType(payloadWidth) != "" && !isUint(snapshot, payloadType):
			// Any fixed-width integer payload other than uint (uint flows
			// through its own dedicated grammar below) is built at its OWN
			// resolved width, mirroring buildCallArgument/buildComparisonOperand.
			expr, err := buildExpr(unit, snapshot, fileSet, initValue.Children[0], scope, payloadWidth, width)
			if err != nil {
				return "", err
			}
			valueExpr = expr
		case isUint(snapshot, payloadType):
			expr, err := buildUintExpr(unit, snapshot, fileSet, initValue.Children[0], scope, width)
			if err != nil {
				return "", err
			}
			valueExpr = expr
		case isBool(snapshot, payloadType):
			expr, err := buildBoolExpr(unit, snapshot, fileSet, initValue.Children[0], scope, width)
			if err != nil {
				return "", err
			}
			valueExpr = expr
		case isTuple(snapshot, payloadType):
			expr, err := buildNestedAggregateValue(unit, snapshot, fileSet, initValue.Children[0], scope, payloadType, context, width)
			if err != nil {
				return "", err
			}
			valueExpr = expr
		case isEnumType(unit, snapshot, payloadType):
			// An enum-payload optional initialized from `some <variant>` — the
			// only enum-payload optional initializer this backend supports is
			// an integer-to-optional-enum cast (`5 as ?Color`, see the
			// OptionalIntegerToEnum case below); a some-initialized enum
			// payload is a clean rejection naming the shape.
			return "", fmt.Errorf("%s declares an optional-typed local of type %s initialized from some with an enum payload %s; the only supported enum-payload optional initializer is an integer-to-optional-enum cast (e.g. 5 as ?Color)", context, optionalTypeName(initValue.Type), enumTypeName(payloadType))
		case isStruct(snapshot, payloadType):
			expr, err := buildNestedAggregateValue(unit, snapshot, fileSet, initValue.Children[0], scope, payloadType, context, width)
			if err != nil {
				return "", err
			}
			valueExpr = expr
		case isPointer(snapshot, payloadType):
			// A pointer payload's value is built by the same buildExpr path a
			// pointer-typed value takes anywhere else (AddressOf, NilPointer,
			// a pointer-typed local reference, or a pointer-returning call) —
			// buildExpr's isPointer bypass handles the shape regardless of the
			// ambient width args, so no width is threaded here.
			expr, err := buildExpr(unit, snapshot, fileSet, initValue.Children[0], scope, width, width)
			if err != nil {
				return "", err
			}
			valueExpr = expr
		default:
			return "", fmt.Errorf("%s declares an optional-typed local of type %s whose payload is %s, want a fixed-width integer, bool, tuple, struct, or enum", context, optionalTypeName(initValue.Type), describeType(snapshot, payloadType))
		}
		scope[statement.Symbol] = localInfo{optional: initValue.Type}
		return fmt.Sprintf("%s%s pebble_local_%d = { .has_value = true, .value = %s };\n%s(void)pebble_local_%d;", indent, optionalTypeName(initValue.Type), statement.Symbol, valueExpr, indent, statement.Symbol), nil
	case tir.NoneOptional:
		// NoneOptional has zero children and the payload value is irrelevant
		// when absent — but the zero-value literal's own shape still has to
		// match the payload's own C type: a bare 0 is fine for a scalar
		// (int/bool/enum) .value field, but a struct/tuple .value field needs
		// the aggregate zero-initializer {0} instead — a bare 0 there
		// triggers -Wmissing-field-initializers/-Wmissing-braces under this
		// project's -Werror (see zeroOptionalPayloadLiteral).
		scope[statement.Symbol] = localInfo{optional: initValue.Type}
		return fmt.Sprintf("%s%s pebble_local_%d = { .has_value = false, .value = %s };\n%s(void)pebble_local_%d;", indent, optionalTypeName(initValue.Type), statement.Symbol, zeroOptionalPayloadLiteral(unit, snapshot, payloadType), indent, statement.Symbol), nil
	case tir.DirectCall, tir.MethodCall:
		// A call to an optional-returning helper used as the direct
		// initializer of a matching optional-typed local: `var o ?int =
		// f();` or a method call (`let ptr = self.get_by_ref(key);`, the
		// std/hmap.peb get shape — a MethodCall whose result type is the
		// optional type) — the one position (alongside some/none and the
		// integer-to-optional-enum cast) in which an optional-typed local
		// may be initialized, mirroring the tuple/struct aggregate-call
		// initializer shape (buildAggregateCallInitializer).
		return buildOptionalCallInitializer(unit, snapshot, fileSet, statement, initValue, scope, indent, context, width)
	case tir.OptionalIntegerToEnum:
		// An integer cast to an optional enum used directly as a local
		// declaration's initializer (`var c ?Color = 5 as ?Color;`). The
		// cast must evaluate its source integer exactly once and derive both
		// the has_value bool and the enum value from that single evaluation,
		// so the source is hoisted into an int64_t temp (see
		// buildOptionalIntegerToEnumDeclaration) — the one position this
		// backend supports the cast in, because a declaration statement has a
		// natural place to prepend the temp's own statement line.
		pre, core, err := buildOptionalIntegerToEnumDeclaration(unit, snapshot, fileSet, statement, initValue, scope, context, id, width)
		if err != nil {
			return "", err
		}
		scope[statement.Symbol] = localInfo{optional: initValue.Type}
		return fmt.Sprintf("%s%s\n%s%s;\n%s(void)pebble_local_%d;", indent, pre, indent, core, indent, statement.Symbol), nil
	default:
		return "", fmt.Errorf("%s declares an optional-typed local of type %s initialized from a %s, want some <expr> or none", context, optionalTypeName(initValue.Type), initValue.Kind)
	}
}

// zeroOptionalPayloadLiteral is the C literal a NoneOptional's irrelevant
// .value field is initialized with — a bare 0 for a scalar payload type
// (int/bool/enum, all of which accept a plain 0 as a valid, warning-clean
// initializer), or the aggregate zero-initializer {0} for a struct/tuple
// payload (a bare 0 there triggers -Wmissing-field-initializers /
// -Wmissing-braces under -Werror, since the .value field's own C type is a
// struct, not a scalar).
func zeroOptionalPayloadLiteral(unit *tir.Unit, snapshot *types.Snapshot, payloadType types.TypeID) string {
	if isTuple(snapshot, payloadType) {
		return "{0}"
	}
	// isStruct also reports true for an enum payload (both are Nominal keys,
	// indistinguishable at this level) — an enum's C type is a plain scalar
	// C enum, not a struct, so it must NOT take the aggregate {0} literal (a
	// brace-enclosed initializer around a scalar is itself a clean
	// -Wmissing-braces target). isEnumType resolves the ambiguity the same
	// way every other struct/enum-payload site in this file already does.
	if isStruct(snapshot, payloadType) && !isEnumType(unit, snapshot, payloadType) {
		return "{0}"
	}
	return "0"
}

func buildOptionalValueExpr(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, node tir.Node, scope map[symbol.SymbolID]localInfo, context string, width types.BuiltinKind) (string, error) {
	key, ok := snapshot.Key(node.Type)
	if !ok {
		return "", fmt.Errorf("%s optional value type %d is not in the type snapshot", context, node.Type)
	}
	payload, ok := key.Child()
	if !ok {
		return "", fmt.Errorf("%s optional value has no payload type", context)
	}
	if node.Kind == tir.NoneOptional {
		return fmt.Sprintf("(%s){ .has_value = false, .value = %s }", optionalTypeName(node.Type), zeroOptionalPayloadLiteral(unit, snapshot, payload)), nil
	}
	if (node.Kind != tir.SomeOptional && node.Kind != tir.OptionalInject) || len(node.Children) != 1 {
		return "", fmt.Errorf("%s contains a %s, want some, none, or an implicit-injection (some-without-the-keyword) optional value", context, node.Kind)
	}
	var value string
	var err error
	payloadWidth, integerPayload := resolvedBuiltin(snapshot, payload)
	switch {
	case integerPayload && cType(payloadWidth) != "" && !isUint(snapshot, payload):
		value, err = buildExpr(unit, snapshot, fileSet, node.Children[0], scope, payloadWidth, width)
	case isUint(snapshot, payload):
		value, err = buildUintExpr(unit, snapshot, fileSet, node.Children[0], scope, width)
	case isBool(snapshot, payload):
		value, err = buildBoolExpr(unit, snapshot, fileSet, node.Children[0], scope, width)
	case isTuple(snapshot, payload):
		value, err = buildTupleValueExpr(unit, snapshot, fileSet, mustNode(unit, node.Children[0]), scope, context, width)
	case isStruct(snapshot, payload):
		value, err = buildStructValueExpr(unit, snapshot, fileSet, mustNode(unit, node.Children[0]), scope, context, width)
	case isPointer(snapshot, payload):
		// A pointer payload's value is built by the same buildExpr path a
		// pointer-typed value takes anywhere else (AddressOf, NilPointer, a
		// pointer-typed local reference, or a pointer-returning call), whose
		// isPointer bypass ignores the ambient width args.
		value, err = buildExpr(unit, snapshot, fileSet, node.Children[0], scope, width, width)
	default:
		return "", fmt.Errorf("%s optional payload %s is unsupported", context, describeType(snapshot, payload))
	}
	if err != nil {
		return "", err
	}
	return fmt.Sprintf("(%s){ .has_value = true, .value = %s }", optionalTypeName(node.Type), value), nil
}

func mustNode(unit *tir.Unit, id tir.NodeID) tir.Node { n, _ := unit.Node(id); return n }

// buildStructLocalDeclaration builds one struct-typed local's declaration: a
// `pebble_struct_<typeID>_t pebble_local_<symbol> = { .pebble_field_<m0> =
// <e0>, .pebble_field_<m1> = <e1> };` whose field initializers are the
// RecordConstruct's Fields, each value built by the grammar its own type
// selects — buildExpr for a field of the entry's width, buildBoolExpr for a
// bool field. The initializer is a C99 designated-initializer brace list
// (`.pebble_field_<member> = <expr>`), not a positional brace list, so the
// construction-site field order a RecordConstruct's Fields carry (which need
// not match the struct's declared order — a site may write Point.{ y = 2, x =
// 1 }) needs no reordering: each designated initializer places its value
// under exactly the C field its member symbol names, regardless of either
// order. Designated initializers are standard C99 and compile clean under
// -Wall -Wextra -Werror (confirmed by a real cc compile through this test
// suite's own harness). Every field type must be exactly the entry's width or
// bool; anything else (a str field, a nested struct field) is a clean
// rejection naming the field position, since this backend emits exactly those
// two C field types. Three initializer shapes are supported (10.26): a
// RecordConstruct (a struct literal), emitted as a designated-initializer
// brace list, a DirectCall to a struct-returning helper whose result type
// matches the local's declared type, emitted by the same call-building
// machinery buildExpr's DirectCall case uses (see buildAggregateCallInitializer),
// and — since the slice-of-struct-element slice — a Load of a CheckedIndexPlace,
// a by-value read of one struct element of an array or slice local (`let e =
// old_entries[j];`, a whole-struct copy of the bounds-checked element lvalue).
// Initializing a struct local from any other value — a whole-struct
// copy of another local, anything else — is a clean rejection. The
// local's scope entry records its struct type (a localInfo with structType
// set), so a later field read resolves the struct type being projected. Like
// every scalar local, the declaration is followed by a (void) cast against
// -Wunused-variable.
func buildStructLocalDeclaration(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, statement, initValue tir.Node, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind) (string, error) {
	if initValue.Kind == tir.DirectCall || initValue.Kind == tir.MethodCall {
		// A call to a struct-returning helper used as the direct initializer of
		// a matching struct-typed local — `let p Point =
		// helperReturningPoint();` — the one position (10.26) in which calling
		// a struct-returning helper is supported.
		return buildAggregateCallInitializer(unit, snapshot, fileSet, statement, initValue, scope, indent, context, width, false)
	}
	if initValue.Kind == tir.Load {
		// A by-value read of one struct element of an array or slice local —
		// `let e = old_entries[j];`, the std/hmap.peb rehash shape — lowered by
		// the checker to a Load of a CheckedIndexPlace whose single child is the
		// StoragePlace naming the array/slice local (the exact shape a scalar
		// element read uses, confirmed against a real fixture). The emitted C is
		// a whole-struct copy declaration, `pebble_struct_<typeID>_t
		// pebble_local_<symbol> = <lvalue>;`, where the lvalue is the
		// bounds-checked element projection built by buildPlaceLValue (the same
		// lvalue an address-of or field-write through a slice index lowers to),
		// and its resolved element type must be exactly the local's declared
		// type (defense for hand-built IR). Like every local, the declaration is
		// followed by a (void) cast against -Wunused-variable.
		if len(initValue.Children) != 1 {
			return "", fmt.Errorf("%s declares a struct-typed local of type %s initialized from a Load with %d child(ren), want exactly one place", context, structTypeName(initValue.Type), len(initValue.Children))
		}
		place, ok := unit.Node(initValue.Children[0])
		if !ok {
			return "", fmt.Errorf("%s declares a struct-typed local of type %s initialized from a Load referencing invalid place node %d", context, structTypeName(initValue.Type), initValue.Children[0])
		}
		if place.Kind != tir.CheckedIndexPlace {
			return "", fmt.Errorf("%s declares a struct-typed local of type %s initialized from a Load whose place is a %s, want a CheckedIndexPlace (a by-value struct-element read)", context, structTypeName(initValue.Type), place.Kind)
		}
		lvalue, elementType, err := buildPlaceLValue(unit, snapshot, fileSet, initValue.Children[0], scope, width)
		if err != nil {
			return "", fmt.Errorf("%s struct-element read: %v", context, err)
		}
		if elementType != initValue.Type {
			return "", fmt.Errorf("%s declares a struct-typed local of type %s initialized from a read of element type %s", context, structTypeName(initValue.Type), describeType(snapshot, elementType))
		}
		scope[statement.Symbol] = localInfo{structType: initValue.Type}
		return fmt.Sprintf("%s%s pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, structTypeName(initValue.Type), statement.Symbol, lvalue, indent, statement.Symbol), nil
	}
	if initValue.Kind != tir.RecordConstruct {
		return "", fmt.Errorf("%s declares a struct-typed local of type %s initialized from a %s, want a RecordConstruct (a struct literal) or a call to a struct-returning helper; initializing a struct local from another value is not supported yet", context, structTypeName(initValue.Type), initValue.Kind)
	}
	preStatements, braceList, err := buildStructBraceList(unit, snapshot, fileSet, initValue, scope, indent, context, width)
	if err != nil {
		return "", err
	}
	scope[statement.Symbol] = localInfo{structType: initValue.Type}
	declaration := fmt.Sprintf("%s%s pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, structTypeName(initValue.Type), statement.Symbol, braceList, indent, statement.Symbol)
	if preStatements != "" {
		// A slice-typed field constructed from an inline CheckedSlice needs its
		// temp-declaration statement threaded ahead of the declaration line (the
		// brace list itself has nowhere to put it) — the same statement-position
		// shape buildSliceReturnValue demonstrates for a slice return.
		return preStatements + "\n" + declaration, nil
	}
	return declaration, nil
}

// buildStructBraceList validates one RecordConstruct node's field list and
// builds its brace-list content, `{ .pebble_field_<m0> = <e0>, ... }`, a C99
// designated-initializer brace list with one designated initializer per
// constructed field. Each field's value is built by the grammar its own type
// selects — buildExpr for a field of the entry's width, buildBoolExpr for a
// bool field. The designated form places each value under exactly the C field
// its member symbol names, so the construction-site field order a
// RecordConstruct's Fields carry (which need not match the struct's declared
// order — a site may write Point.{ y = 2, x = 1 }) needs no reordering.
// Every field type must be exactly the entry's width or bool; anything else
// (a str field, a nested struct field) is a clean rejection naming the field
// position, since this backend emits exactly those two C field types. context
// names the enclosing construct in error messages. The function is shared by
// the two places a RecordConstruct's fields are built (10.25): a struct-typed
// local's declaration initializer (buildStructLocalDeclaration embeds the
// returned brace list in the declaration statement) and a freshly-constructed
// struct built inline as a call argument (buildStructValueExpr wraps the same
// brace list in a compound-literal cast), so field-type validation and the
// buildExpr/buildBoolExpr dispatch live in exactly one place.
//
// The return is two pieces of C text rather than one so a slice-typed field
// whose construction value is an inline slice construction (`Bag.{ items =
// arr[:] }`, a bare CheckedSlice, or `slice ptr, n`, a SliceFromRaw) can be
// supported. A CheckedSlice construction needs the same two-statement
// temp-then-construction shape a slice local's declaration and a slice return
// use: the checked-start result is stored in a temp statement first, then the
// compound-literal construction uses that temp for both its pointer offset and
// its length subtraction, so the potentially-aborting checked call (and any
// side-effecting bound expression) is evaluated exactly once. That temp
// declaration has no place inside a brace list, so it is returned separately
// as preStatements (already indented) for a caller in a statement position
// (buildStructLocalDeclaration) to thread ahead of the declaration line; a
// caller in a pure expression position (buildStructValueExpr) must reject a
// non-empty preStatements rather than drop it, the same discipline
// buildSliceArgument applies to an inline slice construction passed as a call
// argument. A SliceFromRaw field value is a single expression with no temp
// (buildRawSliceConstruction emits the compound literal directly), so it needs
// no pre-statement. A slice-typed field whose construction value is a
// SymbolValue naming an already-declared slice-typed local is the same
// single-expression forward as before, with empty preStatements. indent indents
// the temp declarations to match the enclosing declaration line.
func buildStructBraceList(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, node tir.Node, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind) (string, string, error) {
	key, ok := snapshot.Key(node.Type)
	if !ok {
		return "", "", fmt.Errorf("%s contains a struct value whose type %d is not in the type snapshot", context, node.Type)
	}
	decl, _, ok := key.Nominal()
	if !ok {
		return "", "", fmt.Errorf("%s contains a struct value of type %s, which has no nominal declaration", context, structTypeName(node.Type))
	}
	typeDecl, ok := findTypeDeclaration(unit, decl)
	if !ok {
		return "", "", fmt.Errorf("%s contains a struct value of type %s whose declaration symbol %d has no TypeDeclaration in the unit", context, structTypeName(node.Type), decl)
	}
	members := typeDecl.Members
	if len(node.Fields) != len(members) {
		return "", "", fmt.Errorf("%s contains a struct value of type %s with %d field initializer(s), want %d (one per declared field)", context, structTypeName(node.Type), len(node.Fields), len(members))
	}
	inits := make([]string, len(node.Fields))
	var pres []string
	for i, field := range node.Fields {
		declared := false
		for _, member := range members {
			if member == field.Field {
				declared = true
				break
			}
		}
		if !declared {
			return "", "", fmt.Errorf("%s contains a struct value of type %s with an initializer for symbol %d, which is not one of its declared fields", context, structTypeName(node.Type), field.Field)
		}
		valueNode, ok := unit.Node(field.Value)
		if !ok {
			return "", "", fmt.Errorf("%s contains a struct value of type %s referencing invalid field value node %d", context, structTypeName(node.Type), field.Value)
		}
		fieldType, found := declaredFieldType(unit, snapshot, node.Type, field.Field)
		if !found {
			fieldType = valueNode.Type
		}
		var expr string
		fieldWidth, integerField := resolvedBuiltin(snapshot, fieldType)
		switch {
		case integerField && cType(fieldWidth) != "" && !isUint(snapshot, fieldType):
			// Any fixed-width integer field other than uint (uint flows
			// through its own dedicated grammar below) is built at its OWN
			// resolved width, mirroring buildCallArgument/buildComparisonOperand
			// and the optional-payload widening (d737242) — so an i64 field
			// inside an i32 function builds its value at i64, not i32.
			built, err := buildExpr(unit, snapshot, fileSet, field.Value, scope, fieldWidth, width)
			if err != nil {
				return "", "", err
			}
			expr = built
		case isUint(snapshot, fieldType):
			built, err := buildUintExpr(unit, snapshot, fileSet, field.Value, scope, width)
			if err != nil {
				return "", "", err
			}
			expr = built
		case isBool(snapshot, fieldType):
			built, err := buildBoolExpr(unit, snapshot, fileSet, field.Value, scope, width)
			if err != nil {
				return "", "", err
			}
			expr = built
		case isTuple(snapshot, fieldType):
			built, err := buildNestedAggregateValue(unit, snapshot, fileSet, field.Value, scope, fieldType, context, width)
			if err != nil {
				return "", "", err
			}
			expr = built
		case isOptional(snapshot, fieldType):
			built, err := buildNestedAggregateValue(unit, snapshot, fileSet, field.Value, scope, fieldType, context, width)
			if err != nil {
				return "", "", err
			}
			expr = built
		case runtimeType(unit, snapshot, fieldType) != 0:
			// A field of a compiler-builtin runtime type (Allocator, Context) is
			// initialized from the same runtime-value grammar a runtime-typed
			// local's declaration uses (buildRuntimeValue: `context.default_allocator`,
			// a runtime-typed local, or a load of a runtime-typed field) — NOT the
			// nested-aggregate grammar the isStruct case below would send it to
			// (Allocator is Nominal like a struct, but its value is never a
			// RecordConstruct). The C field's type is PebbleAllocator /
			// PebbleContext (see structFieldCType), so the built expression matches
			// the field type with no cast.
			built, err := buildRuntimeValueNode(unit, snapshot, fileSet, field.Value, scope, width)
			if err != nil {
				return "", "", err
			}
			expr = built
		case isEnumType(unit, snapshot, fieldType):
			// An enum-typed field's construction value (Entry's `state = .Empty`,
			// std/hmap.peb's insert) is a variant literal — an EnumVariantValue or
			// a zero-payload VariantConstruct — built by the same buildEnumValue an
			// enum-typed local's declaration uses, NOT the nested-aggregate grammar
			// the isStruct case below would send it to (a plain enum is Nominal
			// exactly like a struct — see isEnumType — but its value is never a
			// RecordConstruct, so buildNestedAggregateValue's SymbolValue-or-nested-
			// construction dispatch would mishandle it). The variant literal lowers
			// to the variant's C enum constant, whose type matches the field's own
			// pebble_enum_<typeID>_t C type with no cast needed.
			built, err := buildEnumValue(unit, snapshot, fileSet, field.Value, scope, width)
			if err != nil {
				return "", "", err
			}
			expr = built
		case isStruct(snapshot, fieldType):
			built, err := buildNestedAggregateValue(unit, snapshot, fileSet, field.Value, scope, fieldType, context, width)
			if err != nil {
				return "", "", err
			}
			expr = built
		case isSlice(snapshot, fieldType):
			// A slice-typed field's construction value is one of three shapes
			// (all confirmed against real fixtures): a SymbolValue naming an
			// already-declared slice-typed local in scope of exactly the
			// field's type (a single-expression forward, the shape that always
			// worked); a bare CheckedSlice (`arr[:]` used directly as the
			// field's value — the general struct-field-construction gap this
			// case closes, reachable in both non-generic and generic struct
			// constructions), which needs the same two-statement
			// temp-then-construction shape a slice local's declaration uses,
			// so its temp declaration is returned as a pre-statement; or a
			// bare SliceFromRaw (`slice ptr, n` — restricted to std-package
			// source, where the raw-pointer slice builtin is available),
			// whose construction is a single expression (buildRawSliceConstruction
			// needs no temp) and needs no pre-statement. Anything else is a
			// clean rejection naming what was found.
			fieldValue, ok := unit.Node(field.Value)
			if !ok {
				return "", "", fmt.Errorf("%s contains a struct value of type %s referencing invalid field value node %d", context, structTypeName(node.Type), field.Value)
			}
			switch fieldValue.Kind {
			case tir.SymbolValue:
				local, declared := scope[fieldValue.Symbol]
				if !declared || local.sliceType != fieldType {
					return "", "", fmt.Errorf("%s contains a slice field %d initialized from a nonmatching local", context, field.Field)
				}
				expr = fmt.Sprintf("pebble_local_%d", fieldValue.Symbol)
			case tir.CheckedSlice:
				if fieldValue.Type != fieldType {
					return "", "", fmt.Errorf("%s contains a slice field %d initialized from a CheckedSlice of type %s, not a slice-typed value of type %s", context, field.Field, describeType(snapshot, fieldValue.Type), sliceTypeName(fieldType))
				}
				// The temp name derives from the field value node's own NodeID
				// — the only stable identity in hand here (a struct field has
				// no local symbol to name it from), distinct from the
				// pebble_slice_start_<symbol> temps a slice local's declaration
				// uses and the pebble_slice_ret_<nodeID> temps a slice return
				// uses, so the three can never collide even when a symbol ID
				// numerically equals a node ID.
				tempDecl, constructionExpr, err := buildSliceConstruction(unit, snapshot, fileSet, fieldValue, scope, indent, context, width, fmt.Sprintf("pebble_field_slice_%d", field.Value))
				if err != nil {
					return "", "", err
				}
				pres = append(pres, tempDecl)
				expr = constructionExpr
			case tir.SliceFromRaw:
				if fieldValue.Type != fieldType {
					return "", "", fmt.Errorf("%s contains a slice field %d initialized from a SliceFromRaw of type %s, not a slice-typed value of type %s", context, field.Field, describeType(snapshot, fieldValue.Type), sliceTypeName(fieldType))
				}
				construction, err := buildRawSliceConstruction(unit, snapshot, fileSet, fieldValue, scope, width, context)
				if err != nil {
					return "", "", err
				}
				expr = construction
			default:
				return "", "", fmt.Errorf("%s contains a slice field %d initialized from a %s, want a slice local or a fresh slice construction (a CheckedSlice or a slice-from-raw)", context, field.Field, fieldValue.Kind)
			}
		case isPointer(snapshot, fieldType):
			built, err := buildExpr(unit, snapshot, fileSet, field.Value, scope, width, width)
			if err != nil {
				return "", "", err
			}
			expr = built
		case isFunctionType(snapshot, fieldType):
			// A function-typed field's construction value (`Table.{ op = add }`,
			// function-types slice 2): built by the same buildFunctionValue a
			// function-typed local's declaration and the general indirect call's
			// callee already use (slice 1) — a bare top-level function reference
			// (HoistedFunctionValue) or a reference to an in-scope function-typed
			// local (SymbolValue), whose C value already matches the field's own
			// pebble_fnptr_<typeID>_t C type with no cast needed.
			built, err := buildFunctionValue(unit, snapshot, fileSet, valueNode, scope, context, width)
			if err != nil {
				return "", "", err
			}
			expr = built
		default:
			return "", "", fmt.Errorf("%s contains a struct value of type %s whose field %d is %s, want a fixed-width integer, bool, tuple, struct, enum, pointer, slice, or function type", context, structTypeName(node.Type), field.Field, describeType(snapshot, fieldType))
		}
		inits[i] = fmt.Sprintf(".pebble_field_%d = %s", field.Field, expr)
	}
	preStatements := ""
	if len(pres) > 0 {
		preStatements = strings.Join(pres, "\n")
	}
	return preStatements, "{ " + strings.Join(inits, ", ") + " }", nil
}

// buildStructValueExpr builds a freshly-constructed struct value as an
// ordinary C expression (10.25): a RecordConstruct node lowered to a
// designated-initializer C99 compound literal,
// `(pebble_struct_<typeID>_t){ .pebble_field_<m0> = <e0>, ... }`. The field
// list is built and validated by buildStructBraceList (the same logic a
// struct-typed local's declaration initializer uses), so a construction
// site's field order still need not match the struct's declared order — the
// designated-initializer form handles the ordering in this position exactly
// as it does in a declaration — and a field of any type other than the entry's
// width or bool is rejected the same way it would be in a declaration. The
// cast makes the compound literal a value usable anywhere a struct-typed
// value is needed — in this slice, only as a call argument for a
// struct-typed parameter (buildAggregateArgument). The node must be a
// RecordConstruct; the caller already guarantees this, so the kind check is
// defense for hand-built IR.
func buildStructValueExpr(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, node tir.Node, scope map[symbol.SymbolID]localInfo, context string, width types.BuiltinKind) (string, error) {
	if node.Kind != tir.RecordConstruct {
		return "", fmt.Errorf("%s contains a %s, want a RecordConstruct (a struct literal)", context, node.Kind)
	}
	preStatements, braceList, err := buildStructBraceList(unit, snapshot, fileSet, node, scope, "", context, width)
	if err != nil {
		return "", err
	}
	if preStatements != "" {
		// A slice-typed field whose construction value is an inline CheckedSlice
		// needs its temp-declaration statement, and this is a pure expression
		// position (a call argument, a return value, or a nested aggregate
		// field) with nowhere to place it — the same reason buildSliceArgument
		// rejects an inline slice construction passed as a call argument. The
		// slice-typed-local-reference shape (empty preStatements) is unaffected.
		return "", fmt.Errorf("%s is a struct value with a slice field initialized from an inline slice construction, which is not supported in this position: a C expression has nowhere to place the temp-declaration statement the slice construction needs; construct the slice into a local first and reference that local", context)
	}
	return fmt.Sprintf("(%s)%s", structTypeName(node.Type), braceList), nil
}

// buildEnumLocalDeclaration builds one plain enum-typed local's declaration: a
// `pebble_enum_<typeID>_t pebble_local_<symbol> = <initializer>;` whose
// initializer is a variant literal — an EnumVariantValue (Color.green, the
// member-access form) or a zero-payload VariantConstruct (Color.red(), the
// parenthesized-call form, which a plain enum's payload-less variants also
// produce — confirmed against a real fixture) — or, since CheckedIntegerToEnum
// support landed, an integer cast to the enum (`5 as Color`, built by
// buildCheckedIntegerToEnumExpr through the checked runtime primitive, e.g.
// `pebble_local_<sym> = (pebble_enum_<typeID>_t)pebble_rt_checked_int_to_enum(...);`).
// A variant literal lowers to the variant's C
// enum constant, whose value is the variant's ordinal in the enum's declared
// order (the C typedef emits one named constant per variant in TypeDecl order,
// so the constant and the typedef agree by construction). A payload-carrying
// initializer — an EnumVariantValue or VariantConstruct with one or more
// children — is a tagged-union (union enum) construction, which real source
// routes to buildUnionLocalDeclaration instead (the type is a tagged union
// whenever any reachable construction carries a payload); this payload
// rejection is defense for hand-built IR where such a construction reaches
// this plain-enum builder, never guessed at. The
// initializer's variant symbol must be one of the enum's declared variants, and
// the enum type must actually be a plain enum (not a struct that shares the
// Nominal key shape — isEnumType distinguishes them). The local's scope entry
// records its enum type (a localInfo with enumType set), so a later reference,
// reassignment, switch subject, or comparison resolves the enum type being
// used. Like every scalar local, the declaration is followed by a (void) cast
// against -Wunused-variable.
func buildEnumLocalDeclaration(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, statement, initValue tir.Node, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind) (string, error) {
	switch initValue.Kind {
	case tir.EnumVariantValue:
		if len(initValue.Children) == 1 {
			return "", fmt.Errorf("%s declares an enum-typed local initialized from an enum variant with a payload; a tagged-union (union enum) construction routes through buildUnionLocalDeclaration, never a plain enum declaration", context)
		}
	case tir.VariantConstruct:
		if len(initValue.Children) >= 1 {
			return "", fmt.Errorf("%s declares an enum-typed local initialized from a variant construction with %d payload(s); a tagged-union (union enum) construction routes through buildUnionLocalDeclaration, never a plain enum declaration", context, len(initValue.Children))
		}
	case tir.CheckedIntegerToEnum:
		// An integer cast to an enum (`let c Color = 5 as Color;`) is built by
		// buildCheckedIntegerToEnumExpr below — the value is produced by the
		// checked runtime primitive, not a variant constant.
	default:
		return "", fmt.Errorf("%s declares an enum-typed local of type %s initialized from a %s, want a variant literal (e.g. Color.green) or an integer cast to the enum type (e.g. 5 as Color); initializing an enum local from another value is not supported yet", context, enumTypeName(initValue.Type), initValue.Kind)
	}
	info, err := resolveEnumInfo(unit, snapshot, initValue.Type)
	if err != nil {
		return "", err
	}
	scope[statement.Symbol] = localInfo{enumType: initValue.Type}
	if initValue.Kind == tir.CheckedIntegerToEnum {
		castExpr, err := buildCheckedIntegerToEnumExpr(unit, snapshot, fileSet, initValue, scope, context, width)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("%s%s pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, enumTypeName(initValue.Type), statement.Symbol, castExpr, indent, statement.Symbol), nil
	}
	if !containsVariant(info.variants, initValue.Member) {
		return "", fmt.Errorf("%s declares an enum-typed local of type %s initialized from variant symbol %d, which is not one of its declared variants", context, enumTypeName(initValue.Type), initValue.Member)
	}
	return fmt.Sprintf("%s%s pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, enumTypeName(initValue.Type), statement.Symbol, enumVariantName(initValue.Member), indent, statement.Symbol), nil
}

// buildEnumValue builds the C expression text for a plain enum value node of
// six shapes (all confirmed against real fixtures): an EnumVariantValue
// (Color.green, a variant literal with no payload), a zero-payload
// VariantConstruct (Color.red(), the parenthesized-call form of a plain
// enum's payload-less variant), a SymbolValue naming an enum-typed local
// declared earlier in the body (emitted as its pebble_local_<symbolID> C name),
// a SourceAlias (transparent grouped-expression parens, e.g. `(2 as Color)`,
// unwrapped to its single child), a Load of an enum-typed struct field
// (`entry.state`, the enum-typed-struct-field shape — the projection carries
// the field's own pebble_enum_<typeID>_t C type), and — since
// CheckedIntegerToEnum support landed — an integer cast to an enum (`5 as
// Color`, built by
// buildCheckedIntegerToEnumExpr through the checked runtime primitive). A
// variant literal emits its C enum constant
// pebble_variant_<member>, whose value is the variant's ordinal in the enum's
// declared order. A payload-carrying variant — an EnumVariantValue or
// VariantConstruct with one or more children — is a tagged-union construction,
// which real source routes to buildUnionConstruction instead; this rejection is
// defense for hand-built IR where such a construction reaches this plain-enum
// builder. Anything else is a clean
// rejection, never a guessed lowering. This is the one shared builder for an
// enum value wherever one is needed this slice: an enum-typed local's
// declaration initializer, a reassignment's new value, an enum switch's
// subject, and an enum comparison's operand.
func buildEnumValue(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, id tir.NodeID, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind) (string, error) {
	node, ok := unit.Node(id)
	if !ok {
		return "", fmt.Errorf("entry function body expression references invalid node %d", id)
	}
	switch node.Kind {
	case tir.EnumVariantValue:
		if len(node.Children) == 1 {
			return "", fmt.Errorf("entry function body expression constructs enum variant symbol %d with a payload; a tagged-union (union enum) construction routes through buildUnionConstruction, never a plain enum value", node.Member)
		}
		return enumVariantName(node.Member), nil
	case tir.VariantConstruct:
		if len(node.Children) >= 1 {
			return "", fmt.Errorf("entry function body expression constructs enum variant symbol %d with %d payload(s); a tagged-union (union enum) construction routes through buildUnionConstruction, never a plain enum value", node.Member, len(node.Children))
		}
		return enumVariantName(node.Member), nil
	case tir.SymbolValue:
		info, declared := locals[node.Symbol]
		if !declared || info.enumType == 0 {
			return "", fmt.Errorf("entry function body expression references symbol %d, which is not an enum-typed local declared earlier in the body", node.Symbol)
		}
		return fmt.Sprintf("pebble_local_%d", node.Symbol), nil
	case tir.SourceAlias:
		// A SourceAlias is transparent — it records grouped-expression parens
		// (e.g. `(2 as Color) == Color.blue`) and nothing else — so it is
		// unwrapped and its single child built, exactly as buildBoolExpr and
		// buildFloatExpr already unwrap it.
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains a SourceAlias with %d child(ren), want exactly one", len(node.Children))
		}
		return buildEnumValue(unit, snapshot, fileSet, node.Children[0], locals, width)
	case tir.CheckedIntegerToEnum:
		return buildCheckedIntegerToEnumExpr(unit, snapshot, fileSet, node, locals, "entry function body expression", width)
	case tir.CheckedOptionalUnwrap:
		// A force-unwrap of an enum-payload optional (`c!` where c is
		// ?Color), used as an enum value — the read-back path for an
		// OptionalIntegerToEnum-constructed local (`5 as ?Color`), which is
		// how the in-range round trip (`c! as i32 == 1`) is observable end to
		// end. The unwrap reads the optional local's has_value and value
		// fields through the checked-unwrap runtime helper at the C enum's
		// underlying width (a C enum is int-compatible, so
		// pebble_rt_checked_unwrap_i32's value parameter and int32_t result
		// both accept the enum's representation directly), and the int32_t
		// result is narrowed back to the enum typedef. The child must name an
		// optional-typed local whose payload is exactly the enum the unwrap
		// yields.
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap with %d child(ren), want exactly one (the optional value being unwrapped)", len(node.Children))
		}
		child, ok := unit.Node(node.Children[0])
		if !ok {
			return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap referencing invalid child node %d", node.Children[0])
		}
		if child.Kind != tir.SymbolValue {
			return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap of an enum-payload optional whose child is a %s, want a SymbolValue naming an optional-typed local", child.Kind)
		}
		info, declared := locals[child.Symbol]
		if !declared {
			return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap referencing symbol %d, which is not a local declared earlier in the entry body", child.Symbol)
		}
		if info.optional == 0 {
			return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap of symbol %d, which is not an optional-typed local", child.Symbol)
		}
		payloadKey, ok := snapshot.Key(info.optional)
		if !ok || payloadKey.Kind() != types.Optional {
			return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap of symbol %d with unresolvable optional type %d", child.Symbol, info.optional)
		}
		payload, ok := payloadKey.Child()
		if !ok || !isEnumType(unit, snapshot, payload) || payload != node.Type {
			return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap of symbol %d whose payload %s is not the enum type %s the unwrap yields", child.Symbol, describeType(snapshot, payload), enumTypeName(node.Type))
		}
		return fmt.Sprintf("(%s)pebble_rt_checked_unwrap_i32(pebble_local_%d.has_value, pebble_local_%d.value, %s)", enumTypeName(node.Type), child.Symbol, child.Symbol, buildSourceLoc(fileSet, node.Span)), nil
	case tir.Load:
		// A struct field read of an enum-typed field (`entry.state`, the
		// std/hmap.peb insert comparison shape) — lowered by the checker to a
		// Load of a FieldPlace whose single child names the struct local,
		// exactly the shape buildExpr's Load case routes to
		// buildStructFieldRead. The projection
		// pebble_local_<sym>.pebble_field_<m> carries the field's own
		// pebble_enum_<typeID>_t C type, which is directly comparable to
		// another enum value's constant. The Load's own Type must be the enum
		// type (the checker guarantees it for real source; a mismatch is a
		// clean rejection for hand-built IR).
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains a Load with %d child(ren), want exactly one place", len(node.Children))
		}
		place, ok := unit.Node(node.Children[0])
		if !ok {
			return "", fmt.Errorf("entry function body expression contains a Load referencing invalid place node %d", node.Children[0])
		}
		if place.Kind != tir.FieldPlace {
			return "", fmt.Errorf("entry function body expression contains a Load whose place is a %s, want a FieldPlace (an enum-typed struct field read)", place.Kind)
		}
		if !isEnumType(unit, snapshot, node.Type) {
			return "", fmt.Errorf("entry function body expression contains a Load of type %s, want an enum type", describeType(snapshot, node.Type))
		}
		return buildStructFieldRead(unit, snapshot, fileSet, place, locals, width, false)
	default:
		return "", fmt.Errorf("entry function body expression contains a %s, want an enum variant literal (an EnumVariantValue) or a reference to an enum-typed local", node.Kind)
	}
}

// buildCheckedIntegerToEnumExpr builds the C expression text for one integer-to-
// enum cast (a tir.CheckedIntegerToEnum, e.g. `5 as Color`), the shared builder
// behind every enum-value position the cast can occupy: an enum-typed local's
// declaration initializer, a reassignment's new value, an enum switch's
// subject, and an enum comparison's operand. The destination enum type is the
// node's own Type (a plain enum — the checker only ever routes an integer to a
// NominalEnum destination; a tagged-union destination is impossible), and the
// emitted C is
//
//	(<destination enum C type>)pebble_rt_checked_int_to_enum((int64_t)(<child expr>), <variant_count>, <source loc>)
//
// The single child is the ordinary integer being cast, built by the integer
// expression builder (buildExpr) at its own declared width, not by
// buildEnumValue (which is for enum-typed operands). The variant count comes
// from the destination enum's TypeDecl.Members length: Pebble enums are
// ordinal, variant Members[i] gets the C enum value i, so an integer names a
// real variant exactly when 0 <= value < variant_count — the runtime primitive
// enforces exactly that bound (see pebble_rt.h), SAFE panicking out-of-range
// and RELEASE returning the value unchecked.
func buildCheckedIntegerToEnumExpr(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, node tir.Node, locals map[symbol.SymbolID]localInfo, context string, entryWidth types.BuiltinKind) (string, error) {
	if node.Kind != tir.CheckedIntegerToEnum {
		return "", fmt.Errorf("%s contains a %s, want a CheckedIntegerToEnum", context, node.Kind)
	}
	if len(node.Children) != 1 {
		return "", fmt.Errorf("%s contains a CheckedIntegerToEnum with %d children, want exactly one", context, len(node.Children))
	}
	info, err := resolveEnumInfo(unit, snapshot, node.Type)
	if err != nil {
		return "", fmt.Errorf("%s integer-to-enum cast: %v", context, err)
	}
	if len(info.variants) == 0 {
		return "", fmt.Errorf("%s integer-to-enum cast targets enum %s, which has no declared variants", context, enumTypeName(node.Type))
	}
	child, ok := unit.Node(node.Children[0])
	if !ok {
		return "", fmt.Errorf("%s integer-to-enum cast references invalid child node %d", context, node.Children[0])
	}
	childType, ok := snapshot.Key(child.Type)
	if !ok {
		return "", fmt.Errorf("%s integer-to-enum cast child has invalid type %d", context, child.Type)
	}
	childWidth, ok := childType.Builtin()
	if !ok || cType(childWidth) == "" {
		return "", fmt.Errorf("%s integer-to-enum cast child has non-integer type %s", context, describeType(snapshot, child.Type))
	}
	childExpr, err := buildExpr(unit, snapshot, fileSet, node.Children[0], locals, childWidth, entryWidth)
	if err != nil {
		return "", fmt.Errorf("%s integer-to-enum cast child: %v", context, err)
	}
	return fmt.Sprintf("(%s)pebble_rt_checked_int_to_enum((int64_t)(%s), %d, %s)", enumTypeName(node.Type), childExpr, len(info.variants), buildSourceLoc(fileSet, node.Span)), nil
}

// buildOptionalIntegerToEnumDeclaration builds the two-part C fragment a local
// declaration whose initializer is an OptionalIntegerToEnum (`var c ?Color =
// 5 as ?Color;`) needs. Unlike its sibling CheckedIntegerToEnum, the cast must
// produce an optional STRUCT VALUE with two fields — `{ .has_value = <bool>,
// .value = <enum value> }` — both derived from the SAME source integer, so the
// source must be evaluated exactly ONCE and both fields read from that single
// evaluation. Naively embedding the source expression twice would evaluate it
// twice, wrong whenever the source has a side effect (a function call, for
// instance). This backend has no expression-level "evaluate once, reuse twice"
// mechanism (buildExpr returns a plain `(string, error)` with no pre-statement
// threading), so the cast is supported ONLY as a local variable declaration's
// initializer — the two call sites that already emit a single indent-prefixed
// statement are the natural places to prepend one more line — and is cleanly
// rejected everywhere else. The returned pre string is the one-time
// evaluation statement, and core the local declaration WITHOUT leading indent
// and WITHOUT trailing `;`:
//
//	pre:  int64_t pebble_temp_<id> = (int64_t)(<child expr>);
//	core: pebble_optional_<typeID>_t pebble_local_<symbol> = { .has_value =
//	      pebble_rt_int_to_enum_is_valid(pebble_temp_<id>, <variant_count>),
//	      .value = (<enum C type>)pebble_temp_<id> }
//
// so the two callers assemble them into the two-line leading-statement form
// (buildOptionalLocalDeclaration, which prepends the indent to each line and
// appends `;` plus the (void) cast) and the leading-statement-plus-header form
// (buildForInitClause, which emits pre as a statement before the for and uses
// core as the for-header's init clause, where the header's own `;` terminates
// it) — mirroring buildCompoundStore's (pre, core, err) shape exactly. The
// temp name is derived from the Initialize statement's own node id, mirroring
// the pebble_compound_ptr_<id> naming scheme's uniqueness-per-call-site. The
// destination enum type and variant count are resolved exactly as
// buildCheckedIntegerToEnumExpr resolves them, except the node's own Type is
// the optional `?Color` — the destination enum is its payload, unwrapped via
// TypeKey.Child() the same way buildOptionalLocalDeclaration unwraps an
// optional-typed initializer's payload.
func buildOptionalIntegerToEnumDeclaration(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, statement, initValue tir.Node, scope map[symbol.SymbolID]localInfo, context string, id tir.NodeID, width types.BuiltinKind) (string, string, error) {
	if initValue.Kind != tir.OptionalIntegerToEnum {
		return "", "", fmt.Errorf("%s contains a %s, want an OptionalIntegerToEnum", context, initValue.Kind)
	}
	if len(initValue.Children) != 1 {
		return "", "", fmt.Errorf("%s contains an OptionalIntegerToEnum with %d child(ren), want exactly one integer value", context, len(initValue.Children))
	}
	optionalKey, ok := snapshot.Key(initValue.Type)
	if !ok {
		return "", "", fmt.Errorf("%s integer-to-optional-enum cast has invalid destination type %d", context, initValue.Type)
	}
	if optionalKey.Kind() != types.Optional {
		return "", "", fmt.Errorf("%s integer-to-optional-enum cast destination %s is not an optional type", context, describeType(snapshot, initValue.Type))
	}
	enumType, ok := optionalKey.Child()
	if !ok {
		return "", "", fmt.Errorf("%s integer-to-optional-enum cast destination %s has no payload type", context, describeType(snapshot, initValue.Type))
	}
	info, err := resolveEnumInfo(unit, snapshot, enumType)
	if err != nil {
		return "", "", fmt.Errorf("%s integer-to-optional-enum cast: %v", context, err)
	}
	if len(info.variants) == 0 {
		return "", "", fmt.Errorf("%s integer-to-optional-enum cast targets enum %s, which has no declared variants", context, enumTypeName(enumType))
	}
	child, ok := unit.Node(initValue.Children[0])
	if !ok {
		return "", "", fmt.Errorf("%s integer-to-optional-enum cast references invalid child node %d", context, initValue.Children[0])
	}
	childType, ok := snapshot.Key(child.Type)
	if !ok {
		return "", "", fmt.Errorf("%s integer-to-optional-enum cast child has invalid type %d", context, child.Type)
	}
	childWidth, ok := childType.Builtin()
	if !ok || cType(childWidth) == "" {
		return "", "", fmt.Errorf("%s integer-to-optional-enum cast child has non-integer type %s", context, describeType(snapshot, child.Type))
	}
	childExpr, err := buildExpr(unit, snapshot, fileSet, initValue.Children[0], scope, childWidth, width)
	if err != nil {
		return "", "", fmt.Errorf("%s integer-to-optional-enum cast child: %v", context, err)
	}
	tempName := fmt.Sprintf("pebble_temp_%d", id)
	pre := fmt.Sprintf("int64_t %s = (int64_t)(%s);", tempName, childExpr)
	core := fmt.Sprintf("%s pebble_local_%d = { .has_value = pebble_rt_int_to_enum_is_valid(%s, %d), .value = (%s)%s }", optionalTypeName(initValue.Type), statement.Symbol, tempName, len(info.variants), enumTypeName(enumType), tempName)
	return pre, core, nil
}

// buildUnionLocalDeclaration builds one tagged-union-typed local's declaration:
// a `pebble_union_<typeID>_t pebble_local_<symbol> = <construction>;` whose
// initializer is a variant construction — a payload-carrying VariantConstruct
// (Choice.value(5)), a payload-less EnumVariantValue (Choice.empty), or a
// zero-payload VariantConstruct (Choice.empty()) — built by
// buildUnionConstruction as a C99 compound literal. The union type is the
// initializer value's own Type (the Initialize node carries no Type itself,
// confirmed against a real fixture — same as every other local kind), and the
// type must be a tagged union in this program (the caller's unions map,
// collected by collectUnionTypes from reachable payload-carrying
// constructions); a type that is enum-shaped but not in the union map routes
// here's sibling buildEnumLocalDeclaration instead. The construction is
// validated by buildUnionConstruction, which requires the constructed variant's
// symbol to be one of the union's declared variants, so the emitted C's tag
// value and payload member always exist in the union's typedef. The
// local's scope entry records its union type (a localInfo with enumType set —
// a tagged union is enum-shaped exactly like a plain enum), so a later switch
// subject, reassignment, or reference resolves the union type being used.
// Like every local, the declaration is followed by a (void) cast against
// -Wunused-variable.
func buildUnionLocalDeclaration(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, statement, initValue tir.Node, scope map[symbol.SymbolID]localInfo, indent, context string, unions map[types.TypeID]unionInfo, width types.BuiltinKind) (string, error) {
	if _, ok := unions[initValue.Type]; !ok {
		return "", fmt.Errorf("%s declares an enum-typed local of type %s, which is not a tagged-union type in this program", context, describeType(snapshot, initValue.Type))
	}
	construction, err := buildUnionConstruction(unit, snapshot, fileSet, initValue, scope, context, unions, width)
	if err != nil {
		return "", err
	}
	scope[statement.Symbol] = localInfo{enumType: initValue.Type}
	return fmt.Sprintf("%s%s pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, unionTypeName(initValue.Type), statement.Symbol, construction, indent, statement.Symbol), nil
}

// buildUnionConstruction builds the C expression text for one tagged-union
// variant construction, of three shapes (all confirmed against real fixtures):
// a payload-carrying VariantConstruct (Choice.value(5), the variant's payload
// expression as its one child), a payload-less EnumVariantValue (Choice.empty,
// the member-access form), and a zero-payload VariantConstruct (Choice.empty(),
// the parenthesized-call form). All three lower to a C99 compound literal of
// the union's own struct typedef:
//
//	(pebble_union_<typeID>_t){ .tag = pebble_variant_<member> }
//	(pebble_union_<typeID>_t){ .tag = pebble_variant_<member>, .payload = { .pebble_field_<member> = <payload expr> } }
//
// The tag is the variant's C enum constant (the same pebble_variant_<member>
// name a plain enum uses — the discriminant ordinal scheme is identical), so a
// payload-less construction leaves the payload union unspecified, which is
// legal C: the tag alone determines which member, if any, is meaningful. A
// payload-carrying construction's payload expression is built by the grammar
// its own type selects — buildExpr for a payload of the entry's width,
// buildBoolExpr for a bool payload, buildStrOperand for a str payload — and
// the payload union member is named
// pebble_field_<member> exactly as the union's typedef declares it. The node's
// Type is the union type and its Member the variant symbol (both confirmed
// against real fixtures); the member must be one of the union's declared
// variants, and a payload-carrying construction must name a variant whose
// payload member the union's typedef declares (both guaranteed for real source
// by the checker; the checks are defense for hand-built IR). Any other node
// kind is a clean rejection, never a guessed lowering.
func buildUnionConstruction(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, node tir.Node, scope map[symbol.SymbolID]localInfo, context string, unions map[types.TypeID]unionInfo, width types.BuiltinKind) (string, error) {
	info, ok := unions[node.Type]
	if !ok {
		return "", fmt.Errorf("%s constructs an enum-typed value of type %s, which is not a tagged-union type in this program", context, describeType(snapshot, node.Type))
	}
	if !containsVariant(info.variants, node.Member) {
		return "", fmt.Errorf("%s constructs variant symbol %d, which is not one of the union %s's declared variants", context, node.Member, unionTypeName(node.Type))
	}
	tag := enumVariantName(node.Member)
	switch node.Kind {
	case tir.EnumVariantValue:
		if len(node.Children) != 0 {
			return "", fmt.Errorf("%s constructs union variant symbol %d with %d payload(s), want zero (a payload-less member access)", context, node.Member, len(node.Children))
		}
		return fmt.Sprintf("(%s){ .tag = %s }", unionTypeName(node.Type), tag), nil
	case tir.VariantConstruct:
		if len(node.Children) == 0 {
			return fmt.Sprintf("(%s){ .tag = %s }", unionTypeName(node.Type), tag), nil
		}
		if len(node.Children) != 1 {
			return "", fmt.Errorf("%s constructs union variant symbol %d with %d payload(s), want exactly one (a tagged-union variant carries exactly one payload)", context, node.Member, len(node.Children))
		}
		payloadNode, ok := unit.Node(node.Children[0])
		if !ok {
			return "", fmt.Errorf("%s constructs union variant symbol %d referencing invalid payload node %d", context, node.Member, node.Children[0])
		}
		memberType, hasMember := unionMemberType(info.members, node.Member)
		if !hasMember {
			return "", fmt.Errorf("%s constructs union variant symbol %d, whose payload type is not resolved (no construction of it is collected as a union member)", context, node.Member)
		}
		if payloadNode.Type != memberType {
			return "", fmt.Errorf("%s constructs union variant symbol %d with a payload of type %s, want %s (the variant's resolved payload type)", context, node.Member, describeType(snapshot, payloadNode.Type), describeType(snapshot, memberType))
		}
		var payloadExpr string
		var err error
		if isBool(snapshot, payloadNode.Type) {
			payloadExpr, err = buildBoolExpr(unit, snapshot, fileSet, node.Children[0], scope, width)
		} else if isStr(snapshot, payloadNode.Type) {
			// A str-typed payload (`Result[int, str].{ Err = "bad" }`): built
			// by the str grammar (buildStrOperand — a string literal, a
			// reference to a str-typed local, or a call to a str-returning
			// helper), emitted as a PebbleStr value, the same C type the
			// union's payload member is declared with (see unionMemberCType),
			// so the designated initializer needs no cast.
			payloadExpr, err = buildStrOperand(unit, snapshot, fileSet, node.Children[0], scope, width)
		} else {
			payloadExpr, err = buildExpr(unit, snapshot, fileSet, node.Children[0], scope, width, width)
		}
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("(%s){ .tag = %s, .payload = { .pebble_field_%d = %s } }", unionTypeName(node.Type), tag, node.Member, payloadExpr), nil
	default:
		return "", fmt.Errorf("%s constructs a %s, want a union variant construction (a VariantConstruct) or a member access (an EnumVariantValue)", context, node.Kind)
	}
}

// unionMemberType returns the resolved payload type of one of a tagged-union
// type's constructed members, by member symbol. The members list carries the
// payload types resolved by collectUnionTypes from construction sites, so a
// construction of a variant that was never collected as a union member reports
// false.
func unionMemberType(members []unionMemberInfo, member symbol.SymbolID) (types.TypeID, bool) {
	for _, m := range members {
		if m.member == member {
			return m.payloadType, true
		}
	}
	return 0, false
}

// buildStrLocalDeclaration builds one str-typed local's declaration: a
// `PebbleStr pebble_local_<symbol> = { .data = (const uint8_t *)"<escaped>",
// .len = <N> };` whose initializer is a StringLiteral (a string literal) or,
// since 10.36, a call to a str-returning helper (a DirectCall whose result
// type is str — `let s str = g();`). PebbleStr is the
// runtime ABI's length-prefixed string type (runtime/include/pebble_rt.h), a
// fixed runtime type rather than a program-specific shape, so the local is
// declared directly as PebbleStr with no typedef. .data points at the
// literal's bytes re-escaped into a safe C string literal by escapeCString
// (the decoded content is not assumed simple — a control character, a quote,
// or a backslash anywhere in it is escaped correctly, with every non-
// printable byte emitted as a fixed-width octal escape so a following digit
// can never be swallowed by C's maximal-munch escape rules); .len is the
// decoded byte length, a compile-time constant known from the literal itself,
// so no runtime strlen is involved. The initializer must be a StringLiteral
// or a matching str-returning DirectCall:
// initializing a str local from any other value — a copy of another str
// local, anything else — is a clean rejection, keeping this slice's
// supported initializer exactly the string literal (or a call to a
// str-returning helper). The local's scope entry
// records isStr, so a later str ==/!= comparison, reassignment, or
// str-returning function return resolves the operand as a
// str local. Like every scalar local, the declaration is followed by a (void)
// cast against -Wunused-variable.
func buildStrLocalDeclaration(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, statement, initValue tir.Node, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind) (string, error) {
	if initValue.Kind == tir.DirectCall || initValue.Kind == tir.MethodCall {
		// A call to a str-returning helper used as the direct initializer of a
		// matching str-typed local — `let s str = helperReturningStr();` — the
		// one position (10.36) in which calling a str-returning helper is
		// supported for declaring a str local. The call's result type is the
		// DirectCall node's own Type, which is the callee's resolved result
		// type (confirmed against a real fixture), and it must be exactly the
		// local's declared type — double-checked against the callee's declared
		// ResultType (defense for hand-built IR), so the emitted C never
		// initializes a str local from a call returning another type. The call
		// itself is built by buildDirectCall, the same call-building machinery
		// buildExpr's DirectCall case uses, so context and argument handling
		// are identical to a scalar call — only the result type differs. Like
		// every local, the declaration is followed by a (void) cast against
		// -Wunused-variable.
		calleeDecl, err := findCallDeclaration(unit, initValue)
		if err != nil {
			return "", err
		}
		if calleeDecl.ResultType != initValue.Type {
			return "", fmt.Errorf("%s declares a str-typed local of type %s initialized from a call to symbol %d whose declared result type %s does not match", context, describeType(snapshot, initValue.Type), initValue.Symbol, describeType(snapshot, calleeDecl.ResultType))
		}
		callPre, callExpr, err := buildDirectCallWithPre(unit, snapshot, fileSet, initValue, scope, width)
		if err != nil {
			return "", err
		}
		scope[statement.Symbol] = localInfo{isStr: true}
		return withLeadingPre(callPre, indent, fmt.Sprintf("%sPebbleStr pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, statement.Symbol, callExpr, indent, statement.Symbol)), nil
	}
	if initValue.Kind != tir.StringLiteral {
		return "", fmt.Errorf("%s declares a str-typed local initialized from a %s, want a StringLiteral (a string literal) or a call to a str-returning helper; initializing a str local from another value is not supported yet", context, initValue.Kind)
	}
	valueText, err := buildStrLiteralValue(initValue)
	if err != nil {
		return "", fmt.Errorf("%s: %v", context, err)
	}
	scope[statement.Symbol] = localInfo{isStr: true}
	return fmt.Sprintf("%sPebbleStr pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, statement.Symbol, valueText, indent, statement.Symbol), nil
}

// buildFunctionLocalDeclaration builds one function-typed local's declaration:
// a `pebble_fnptr_<typeID>_t pebble_local_<symbol> = <init_expr>;` whose
// initializer is a function value — a bare top-level function reference (a
// HoistedFunctionValue, e.g. `var f fn(int, int) int = add;`) or a reference
// to an in-scope function-typed local (a SymbolValue, a function-to-function
// copy) — both built by buildFunctionValue. The local's C type is the function
// type's own pointer typedef, pebble_fnptr_<typeID>_t (see buildFunctionTypedef),
// so a bare function name (which decays to a function pointer of exactly that
// typedef's shape) needs no cast at the declaration site. The function type's
// own signature — its calling convention, parameter list, and result — is
// validated against this slice's supported shapes by
// validateFunctionTypeSignature before the typedef or declaration is emitted
// (a function type whose parameters/result mention anything other than the
// entry's width, uint, bool, char, or str parameters and the entry's width,
// bool, char, or void result is a clean rejection naming what is unsupported).
// The scope entry records functionType so a later reference, reassignment, or
// indirect call resolves the local's declared function type. Like every local,
// the declaration is followed by a (void) cast against -Wunused-variable.
func buildFunctionLocalDeclaration(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, statement, initValue tir.Node, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind) (string, error) {
	fnType := initValue.Type
	if !isFunctionType(snapshot, fnType) {
		return "", fmt.Errorf("%s declares a local of type %s, want a function type", context, describeType(snapshot, fnType))
	}
	if err := validateFunctionTypeSignature(snapshot, width, fnType); err != nil {
		return "", fmt.Errorf("%s: %v", context, err)
	}
	valueText, err := buildFunctionValue(unit, snapshot, fileSet, initValue, scope, context, width)
	if err != nil {
		return "", err
	}
	scope[statement.Symbol] = localInfo{functionType: fnType}
	return fmt.Sprintf("%s%s pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, functionTypeName(fnType), statement.Symbol, valueText, indent, statement.Symbol), nil
}

// buildPointerLocalDeclaration builds one pointer-typed local's declaration: a
// `<pointee_c_type> * pebble_local_<symbol> = <init_expr>;` whose initializer
// is an AddressOf expression (`let p *i32 = &y;`), another pointer-typed local
// (pointer copy), or a nil literal. The local's C type is the pointee's own
// C type name followed by ` *` (int32_t * for *i32, pebble_struct_<id>_t *
// for *Point, etc.), resolved by pointerTypeName from the pointer type's
// pointee. The scope entry records pointerType so a later dereference
// (*p) or address-of (&y) resolves the pointer type correctly. Like every
// scalar local, the declaration is followed by a (void) cast against
// -Wunused-variable.
func buildPointerLocalDeclaration(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, statement, initValue tir.Node, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind) (string, error) {
	pointerTypeID := initValue.Type
	pointeeTypeID, ok := pointerPointeeType(snapshot, pointerTypeID)
	if !ok {
		return "", fmt.Errorf("%s declares a pointer-typed local with invalid pointer type", context)
	}
	ctypeName := pointerTypeName(snapshot, pointeeTypeID)
	if ctypeName == "" {
		return "", fmt.Errorf("%s declares a pointer-typed local with unsupported pointee type %s", context, describeType(snapshot, pointeeTypeID))
	}
	switch initValue.Kind {
	case tir.Load:
		fieldText, err := buildRuntimeValue(unit, snapshot, fileSet, initValue, scope, width)
		if err != nil {
			return "", err
		}
		scope[statement.Symbol] = localInfo{pointerType: pointerTypeID}
		return fmt.Sprintf("%s%s pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, ctypeName, statement.Symbol, fieldText, indent, statement.Symbol), nil
	case tir.AddressOf:
		// An address-of expression: `let p *i32 = &y;`. The AddressOf node
		// has one child (the place being addressed). The emitted C is
		// `<ctype> pebble_local_<sym> = &<place_lvalue>;`.
		if len(initValue.Children) != 1 {
			return "", fmt.Errorf("%s address-of initializer has %d children, want exactly one", context, len(initValue.Children))
		}
		placeLValue, _, err := buildPlaceLValue(unit, snapshot, fileSet, initValue.Children[0], scope, width)
		if err != nil {
			return "", fmt.Errorf("%s address-of place: %v", context, err)
		}
		scope[statement.Symbol] = localInfo{pointerType: pointerTypeID}
		return fmt.Sprintf("%s%s pebble_local_%d = &%s;\n%s(void)pebble_local_%d;", indent, ctypeName, statement.Symbol, placeLValue, indent, statement.Symbol), nil
	case tir.SymbolValue:
		// A reference to another pointer-typed local: `let q *i32 = p;`.
		// The emitted C is a plain assignment.
		if _, declared := scope[initValue.Symbol]; !declared {
			return "", fmt.Errorf("%s references symbol %d, which is not a local in scope", context, initValue.Symbol)
		}
		scope[statement.Symbol] = localInfo{pointerType: pointerTypeID}
		return fmt.Sprintf("%s%s pebble_local_%d = pebble_local_%d;\n%s(void)pebble_local_%d;", indent, ctypeName, statement.Symbol, initValue.Symbol, indent, statement.Symbol), nil
	case tir.NilPointer:
		// A nil literal: `let p *i32 = nil;`. The emitted C uses NULL.
		scope[statement.Symbol] = localInfo{pointerType: pointerTypeID}
		return fmt.Sprintf("%s%s pebble_local_%d = NULL;\n%s(void)pebble_local_%d;", indent, ctypeName, statement.Symbol, indent, statement.Symbol), nil
	case tir.DirectCall, tir.MethodCall:
		// A call to a pointer-returning helper used as the direct
		// initializer of a matching pointer-typed local: `let p *i32 =
		// helperReturningPointer();`.
		callPre, callText, err := buildDirectCallWithPre(unit, snapshot, fileSet, initValue, scope, width)
		if err != nil {
			return "", err
		}
		scope[statement.Symbol] = localInfo{pointerType: pointerTypeID}
		return withLeadingPre(callPre, indent, fmt.Sprintf("%s%s pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, ctypeName, statement.Symbol, callText, indent, statement.Symbol)), nil
	case tir.CheckedOptionalUnwrap:
		// A force-unwrap of an optional whose payload is a pointer used as
		// the direct initializer of a matching pointer-typed local:
		// `let p *i32 = o!;`. When the unwrapped optional is a call result
		// (`let p *i32 = find(x)!;`), the call must be evaluated exactly once
		// (a call result is a C struct read back by value; naively embedding
		// it twice would run the call twice), so its result is hoisted into a
		// pebble_temp_<id> optional local first — the same evaluate-once /
		// reuse-twice pattern buildOptionalIntegerToEnumDeclaration uses for
		// its hoisted source temp, and only possible here because a local
		// declaration is a statement position with room for a preceding line.
		// A SymbolValue/Load child (an unwrap of an optional-typed local)
		// needs no hoisting and unwraps inline via buildExpr's pointer-branch
		// CheckedOptionalUnwrap case.
		if len(initValue.Children) != 1 {
			return "", fmt.Errorf("%s force-unwrap initializer has %d child(ren), want exactly one optional value", context, len(initValue.Children))
		}
		child, ok := unit.Node(initValue.Children[0])
		if !ok {
			return "", fmt.Errorf("%s force-unwrap initializer references invalid child node %d", context, initValue.Children[0])
		}
		if child.Kind == tir.DirectCall || child.Kind == tir.MethodCall {
			callPre, callText, err := buildDirectCallWithPre(unit, snapshot, fileSet, child, scope, width)
			if err != nil {
				return "", err
			}
			tempName := fmt.Sprintf("pebble_temp_%d", statement.Children[0])
			unwrapText := fmt.Sprintf("pebble_rt_checked_unwrap_ptr(%s.has_value, %s.value, %s)", tempName, tempName, buildSourceLoc(fileSet, initValue.Span))
			scope[statement.Symbol] = localInfo{pointerType: pointerTypeID}
			return withLeadingPre(callPre, indent, fmt.Sprintf("%s%s %s = %s;\n%s%s pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, optionalTypeName(child.Type), tempName, callText, indent, ctypeName, statement.Symbol, unwrapText, indent, statement.Symbol)), nil
		}
		unwrapText, err := buildExpr(unit, snapshot, fileSet, statement.Children[0], scope, width, width)
		if err != nil {
			return "", err
		}
		scope[statement.Symbol] = localInfo{pointerType: pointerTypeID}
		return fmt.Sprintf("%s%s pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, ctypeName, statement.Symbol, unwrapText, indent, statement.Symbol), nil
	case tir.IndirectCall:
		callText, err := buildIndirectCall(unit, snapshot, fileSet, initValue, scope, width)
		if err != nil {
			return "", err
		}
		scope[statement.Symbol] = localInfo{pointerType: pointerTypeID}
		return fmt.Sprintf("%s%s pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, ctypeName, statement.Symbol, callText, indent, statement.Symbol), nil
	case tir.PointerCast:
		// An explicit pointer-to-pointer cast: `let q *void = p as *void;`.
		// The PointerCast node has one child (the source pointer value) and
		// its Type is the destination pointer type. The emitted C is a
		// simple assignment since C pointer types are already named.
		if len(initValue.Children) != 1 {
			return "", fmt.Errorf("%s pointer cast initializer has %d children, want exactly one", context, len(initValue.Children))
		}
		childText, err := buildExpr(unit, snapshot, fileSet, initValue.Children[0], scope, width, width)
		if err != nil {
			return "", fmt.Errorf("%s pointer cast child: %v", context, err)
		}
		scope[statement.Symbol] = localInfo{pointerType: pointerTypeID}
		return fmt.Sprintf("%s%s pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, ctypeName, statement.Symbol, childText, indent, statement.Symbol), nil
	default:
		return "", fmt.Errorf("%s declares a pointer-typed local initialized from a %s, want an AddressOf expression, another pointer local, a pointer-returning call, a pointer-to-pointer cast, a pointer-payload optional force-unwrap, or nil", context, initValue.Kind)
	}
}

// buildStrLiteralValue builds the C text constructing a PebbleStr value from a
// StringLiteral node's decoded bytes: the `{ .data = (const uint8_t *)
// "<escaped>", .len = <N> }` brace text every str value is built from. It is
// the single source of the string-literal-to-PebbleStr construction text,
// shared byte-for-byte by the three places a str value is built from a
// literal: a str-typed local's declaration initializer (buildStrLocalDeclaration
// embeds it in `PebbleStr pebble_local_<id> = <text>;`), a comparison operand
// with no local behind it (buildStrOperand wraps it in a (PebbleStr) compound
// literal), and a str-typed local's reassignment (buildStoreCore wraps it the
// same way) — so a declaration and a later reassignment from the same literal
// emit byte-identical PebbleStr construction text. The escaping is
// escapeCString's fixed-width-octal scheme (a \NNN octal escape for every
// non-printable byte, so C's maximal-munch escape rules can never swallow a
// following digit) and the length is the literal's compile-time decoded byte
// length, so no runtime strlen is involved. A StringLiteral whose literal kind
// is not a decoded string is a clean rejection.
func buildStrLiteralValue(node tir.Node) (string, error) {
	if node.Literal.Kind != tir.LiteralString {
		return "", fmt.Errorf("contains a StringLiteral with literal kind %s, want a decoded string", node.Literal.Kind)
	}
	text := node.Literal.String
	return fmt.Sprintf("{ .data = (const uint8_t *)\"%s\", .len = %d }", escapeCString(text), len(text)), nil
}

// buildCharLiteralValue builds the C text for one CharLiteral node: its
// decoded rune emitted as an int32_t decimal literal, `(int32_t)97`. A char's
// C representation is always the fixed int32_t — a Unicode scalar value fits
// in 21 bits, so no emitted literal ever overflows a signed 32-bit constant,
// regardless of the entry's resolved integer width (the two are unrelated
// concepts: the entry's width picks integer arithmetic's size; a char's size
// is fixed by the Unicode scalar value range). The decimal text comes from the
// literal's Char field (a Go rune, an int32 alias) with no escaping and no
// width splitting, so a non-ASCII value like 'é' (233) or an emoji such as
// '😀' (128512) emits its full scalar value, not a truncated byte. A
// CharLiteral whose literal kind is not a decoded character is a clean
// rejection.
func buildCharLiteralValue(node tir.Node) (string, error) {
	if node.Literal.Kind != tir.LiteralChar {
		return "", fmt.Errorf("contains a CharLiteral with literal kind %s, want a decoded character", node.Literal.Kind)
	}
	return fmt.Sprintf("(int32_t)%d", node.Literal.Char), nil
}

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

// buildCondition builds the C text for one if/while condition. It dispatches
// on the condition node's shape: a direct integer comparison (tir.BinaryValue)
// keeps the existing buildComparison path unchanged, while a bare bool value —
// a bool literal, a reference to an in-scope bool local, a unary ! negation of
// one of those (tir.PrefixValue with the Bang operator), a comparison used as
// a bool operand, or a && / || combination of any of these (a
// tir.ShortCircuitValue) — is routed through buildBoolExpr. Anything else is
// rejected by whichever builder it reaches.
func buildCondition(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, id tir.NodeID, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind) (string, error) {
	node, ok := unit.Node(id)
	if !ok {
		return "", fmt.Errorf("entry function body condition references invalid node %d", id)
	}
	if node.Kind == tir.BinaryValue {
		return buildComparison(unit, snapshot, fileSet, id, locals, width)
	}
	return buildBoolExpr(unit, snapshot, fileSet, id, locals, width)
}

// buildComparison builds the C text for an if condition. It accepts exactly a
// tir.BinaryValue with two operands and one of the six comparison operators
// (<, <=, >, >=, ==, !=), and emits the plain C operator directly — comparing
// two integers, two char values, or two bools with ==/!=, cannot overflow, so
// no runtime helper
// is needed. The operand grammar is decided from the operands' own resolved
// types, not assumed to be integers: when both operands carry the snapshot's
// str builtin, they are an equality between two str values built by
// buildStrOperand and lowered to the runtime helper
// pebble_rt_str_eq(<left>, <right>) (==) or its negation (!=) — ordering
// comparisons between strs are rejected cleanly, since the checker does not
// reject them from source (confirmed against a real fixture). When both
// operands carry the snapshot's
// char builtin, they are two char values built by buildCharOperand (a char
// literal, a char local reference, or a call to a char-returning helper), and
// all six operators are legal — comparing Unicode scalar values numerically
// is well-defined, and the checker accepts ordering comparisons between chars
// (confirmed against a real fixture) — emitted as the plain C operator with
// no runtime helper. When both
// operands carry the snapshot's
// bool builtin, they are built by buildBoolExpr (a bool comparison result, a
// bool local, a bool literal, a ! negation, or a && / || combination — the
// wrapped-comparison shape (1 < 2) == (3 < 4) is exactly this, its two
// SourceAlias-wrapped comparison operands being bool values), and only the
// ==/!= operators are legal for bool operands — the checker itself rejects an
// ordering comparison between bools (C0603, confirmed against a real fixture),
// so that ordering guard is defense for hand-built IR, not a reachable source
// shape. Both bool operands are parenthesized in the emitted C so a bool
// operand that is itself a comparison cannot chain associatively with the
// outer operator (e.g. (a == b) == (c == d) must not collapse to a left-to-
// right a == b == c == d). Otherwise each operand is built by
// buildComparisonOperand (an int-typed integer literal, an unanchored
// loop-iterator symbol, or any fixed-width-integer expression buildExpr
// accepts at the operand's own resolved width, including non-entry-width
// integers like u64, i64, or u8). Any other node kind, or any other operator on a
// BinaryValue (bitwise), is a clean rejection. The && / || that lower to
// ShortCircuitValue nodes are not this function's concern — buildCondition
// routes them to buildBoolExpr.
func buildComparison(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, id tir.NodeID, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind) (string, error) {
	node, ok := unit.Node(id)
	if !ok {
		return "", fmt.Errorf("entry function body if condition references invalid node %d", id)
	}
	if node.Kind != tir.BinaryValue {
		return "", fmt.Errorf("entry function body if condition is a %s, want a direct integer comparison or a ==/!= between two bool values (<, <=, >, >=, ==, or !=)", node.Kind)
	}
	if len(node.Children) != 2 {
		return "", fmt.Errorf("entry function body if condition has %d operand(s), want exactly two operands", len(node.Children))
	}
	op, ok := comparisonOperator(node.Operator)
	if !ok {
		return "", fmt.Errorf("entry function body if condition uses operator %s, want one of <, <=, >, >=, ==, or !=", node.Operator)
	}
	leftOperand, ok := unit.Node(node.Children[0])
	if !ok {
		return "", fmt.Errorf("entry function body if condition references invalid operand node %d", node.Children[0])
	}
	rightOperand, ok := unit.Node(node.Children[1])
	if !ok {
		return "", fmt.Errorf("entry function body if condition references invalid operand node %d", node.Children[1])
	}
	if isStr(snapshot, leftOperand.Type) && isStr(snapshot, rightOperand.Type) {
		// A comparison between two str values: ==, !=, <, <=, >, >=. Equality
		// and inequality are lowered via the runtime helper pebble_rt_str_eq
		// (byte-for-byte, length-prefixed — no strlen, no NUL-termination
		// dependence): == emits the call directly and != emits its negation.
		// Ordering comparisons are lowered via pebble_rt_str_cmp, which
		// returns negative/zero/positive like C's memcmp/strcmp, and the result
		// is compared against 0 using the source operator translated to its C
		// spelling. Each operand is built by buildStrOperand — a reference to
		// an in-scope str local, or a string literal embedded as a PebbleStr
		// compound literal — so a literal operand participates in a comparison
		// without needing a declared local.
		left, err := buildStrOperand(unit, snapshot, fileSet, node.Children[0], locals, width)
		if err != nil {
			return "", err
		}
		right, err := buildStrOperand(unit, snapshot, fileSet, node.Children[1], locals, width)
		if err != nil {
			return "", err
		}
		if node.Operator == syntax.Equal {
			return "pebble_rt_str_eq(" + left + ", " + right + ")", nil
		}
		if node.Operator == syntax.NotEqual {
			return "!pebble_rt_str_eq(" + left + ", " + right + ")", nil
		}
		// Ordering operators: <, <=, >, >= — the runtime helper
		// pebble_rt_str_cmp returns negative/zero/positive and the source
		// operator is translated to its C spelling by comparisonOperator,
		// which has already validated the token kind above.
		return "pebble_rt_str_cmp(" + left + ", " + right + ") " + op + " 0", nil
	}
	if isChar(snapshot, leftOperand.Type) && isChar(snapshot, rightOperand.Type) {
		// A comparison between two char values — c == 'a', c != d, and all
		// four ordering operators (c < d and so on), all confirmed
		// checker-reachable against real fixtures: a char is a Unicode scalar
		// value, and comparing two scalar values numerically is well-defined
		// for every one of the six operators, so the plain C operator is a
		// direct, correct lowering — no runtime helper (this is not the str
		// case) and no overflow concern (comparisons never fault). Both
		// operands are built by buildCharOperand (a char literal, a char
		// local reference, or a call to a char-returning helper), each
		// emitted as an int32_t value, so a literal operand participates
		// without needing a declared local.
		left, err := buildCharOperand(unit, snapshot, fileSet, node.Children[0], locals, width)
		if err != nil {
			return "", err
		}
		right, err := buildCharOperand(unit, snapshot, fileSet, node.Children[1], locals, width)
		if err != nil {
			return "", err
		}
		return left + " " + op + " " + right, nil
	}
	if isBool(snapshot, leftOperand.Type) && isBool(snapshot, rightOperand.Type) {
		// Both operands are bool values, so this is an equality between bools
		// — (1 < 2) == (3 < 4), a == b, true == a, and so on. Only ==/!= make
		// sense for bool operands; an ordering comparison here is impossible
		// from real source (the checker rejects it as C0603 before typed IR
		// exists), but is rejected cleanly rather than guessed for hand-built
		// IR. The operands are built under the bool grammar by buildBoolExpr,
		// each parenthesized so a comparison operand cannot chain associatively
		// with the outer operator in the emitted C.
		if node.Operator != syntax.Equal && node.Operator != syntax.NotEqual {
			return "", fmt.Errorf("entry function body if condition compares two bool operands with operator %s, want == or !=", node.Operator)
		}
		left, err := buildBoolExpr(unit, snapshot, fileSet, node.Children[0], locals, width)
		if err != nil {
			return "", err
		}
		right, err := buildBoolExpr(unit, snapshot, fileSet, node.Children[1], locals, width)
		if err != nil {
			return "", err
		}
		return "(" + left + ") " + op + " (" + right + ")", nil
	}
	if isFloat(snapshot, leftOperand.Type) && leftOperand.Type == rightOperand.Type {
		// Float arithmetic and comparisons have defined C semantics, including
		// overflow, infinities, NaNs, and division by zero. Emit the comparison
		// directly after building both operands at their shared float width.
		left, err := buildFloatExpr(unit, snapshot, fileSet, node.Children[0], locals, resolvedFloatKind(snapshot, leftOperand.Type))
		if err != nil {
			return "", err
		}
		right, err := buildFloatExpr(unit, snapshot, fileSet, node.Children[1], locals, resolvedFloatKind(snapshot, rightOperand.Type))
		if err != nil {
			return "", err
		}
		return left + " " + op + " " + right, nil
	}
	if isEnumType(unit, snapshot, leftOperand.Type) && isEnumType(unit, snapshot, rightOperand.Type) {
		// A comparison between two plain enum values — c == Color.red,
		// c != Color.red, and (confirmed against a real fixture) the ordering
		// comparisons c < Color.red and so on, all accepted by the checker and
		// therefore reachable. Both operands are built by buildEnumValue (an
		// enum-typed local reference or a variant literal) and the plain C
		// operator is emitted directly: a C enum's value IS the variant's
		// ordinal in declared order, so comparing two enum values compares
		// their discriminants — a direct, correct lowering that cannot fault.
		// The two enum types must match (the checker guarantees it for real
		// source; mismatched operands are a clean rejection for hand-built IR).
		if leftOperand.Type != rightOperand.Type {
			return "", fmt.Errorf("entry function body if condition compares two enum values of different types %s and %s", enumTypeName(leftOperand.Type), enumTypeName(rightOperand.Type))
		}
		left, err := buildEnumValue(unit, snapshot, fileSet, node.Children[0], locals, width)
		if err != nil {
			return "", err
		}
		right, err := buildEnumValue(unit, snapshot, fileSet, node.Children[1], locals, width)
		if err != nil {
			return "", err
		}
		return left + " " + op + " " + right, nil
	}
	// Both integer operands are built at their OWN resolved integer widths
	// (see buildComparisonOperand), so a comparison between two fixed-width
	// integers of DIFFERENT widths (a u64 vs a u8, or a u64 vs an i32) is
	// rejected cleanly here rather than silently emitting mismatched C types
	// side by side — the same explicit same-type requirement the enum branch
	// above enforces. Real source never reaches here mismatched: the checker
	// requires both comparison operands to carry the identical concrete type
	// (confirmed against a real fixture — a `u64 == u8` comparison is a clean
	// T0505 "cannot unify" check-phase rejection before typed IR exists), so
	// this guard is defense for hand-built IR only, exactly like the enum
	// branch's.
	leftWidth, leftInteger := resolvedBuiltin(snapshot, leftOperand.Type)
	rightWidth, rightInteger := resolvedBuiltin(snapshot, rightOperand.Type)
	if leftInteger && rightInteger && cType(leftWidth) != "" && cType(rightWidth) != "" && leftWidth != rightWidth {
		return "", fmt.Errorf("entry function body if condition compares two integer values of different widths %s and %s", wantName(leftWidth), wantName(rightWidth))
	}
	left, err := buildComparisonOperand(unit, snapshot, fileSet, node.Children[0], locals, width)
	if err != nil {
		return "", err
	}
	right, err := buildComparisonOperand(unit, snapshot, fileSet, node.Children[1], locals, width)
	if err != nil {
		return "", err
	}
	return left + " " + op + " " + right, nil
}

// buildComparisonOperand builds one comparison operand. A bare comparison
// between two untyped integer literals defaults both operands to the
// snapshot's int builtin (confirmed against a real fixture — the same for an
// i64 entry as for an i32 one, since a bare comparison has no anchor), so an
// IntegerLiteral of type int is lowered directly as its decimal text. An
// int-typed SymbolValue operand is likewise lowered directly as its
// pebble_local_<symbol> name: in this backend's grammar such a symbol can
// only be a range loop's iterator referenced from inside its own body when
// the iterator is never used in a width-anchoring position (confirmed against
// a real fixture — `loop 0..3 : i { if i == 2 { ... } }` leaves the iterator
// as the unanchored int builtin, since the comparison anchors nothing), and
// the iterator is always declared in C at the entry's width, so its name is
// the correct C lvalue in the comparison. Every
// other integer operand is built at its OWN resolved integer width rather
// than the ambient entry width — a comparison between two non-entry-width
// integers (a u64 local compared against a literal, an i8 local, and so on)
// must build each operand at that operand's own width, since the operand's C
// local is declared at its own width and the checker anchors each operand to
// the other (both comparison operands always carry the same concrete integer
// type). This is the same per-operand width resolution buildCallArgument
// performs for a function-type parameter. uint is deliberately excluded, as
// buildCallArgument excludes it: uint is the platform-native pointer-width
// builtin the backend builds through buildUintExpr (sizeof, slice bounds,
// checked arithmetic), not the general buildExpr path. The ambient entry
// width is threaded through as buildExpr's entryWidth parameter so any
// width-requiring child (a checked runtime call) still knows the true entry
// width, not conflated with the operand's own width. Anything else must be an
// expression of the entry's width that buildExpr
// accepts — a reference to a local declared earlier in the entry body, or
// checked negation
// and checked +, -, *, /, % arithmetic — and is delegated to buildExpr, whose
// own width gate and kind switch do the rejecting.
func buildComparisonOperand(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, id tir.NodeID, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind) (string, error) {
	node, ok := unit.Node(id)
	if !ok {
		return "", fmt.Errorf("entry function body if condition references invalid operand node %d", id)
	}
	if node.Kind == tir.IntegerLiteral && node.Type == snapshot.Builtins().Int {
		text := node.Literal.IntegerNum
		if !isNonNegativeDecimal(text) {
			return "", fmt.Errorf("entry function body if condition contains an integer literal with malformed text %q", text)
		}
		return text, nil
	}
	if node.Kind == tir.SymbolValue && node.Type == snapshot.Builtins().Int {
		if _, declared := locals[node.Symbol]; !declared {
			return "", fmt.Errorf("entry function body if condition references symbol %d, which is not a local in scope", node.Symbol)
		}
		return fmt.Sprintf("pebble_local_%d", node.Symbol), nil
	}
	operandWidth, integerOperand := resolvedBuiltin(snapshot, node.Type)
	if integerOperand && cType(operandWidth) != "" && !isUint(snapshot, node.Type) {
		return buildExpr(unit, snapshot, fileSet, id, locals, operandWidth, width)
	}
	if isUint(snapshot, node.Type) {
		return buildUintExpr(unit, snapshot, fileSet, id, locals, width)
	}
	return buildExpr(unit, snapshot, fileSet, id, locals, width, width)
}

// buildStrOperand builds one str value in a position that accepts a str
// expression, which is exactly three shapes (each confirmed against a real
// fixture): a SymbolValue naming an in-scope str-typed local (emitted as its
// pebble_local_<symbolID> C name — a PebbleStr lvalue), a StringLiteral (a
// str value with no local behind it, emitted as a PebbleStr compound literal
// carrying the escaped bytes and their compile-time length, the same
// construction a str-typed local's declaration embeds), or — since 10.36 — a
// DirectCall to a str-returning helper (emitted as
// pebble_fn_<calleeSymbolID>(ctx, <args>) by buildDirectCall, the same
// call-building machinery buildExpr's DirectCall case uses), so a str-returning
// helper's result can be compared directly (g() == "hi") or passed to a str
// parameter (f(g())) without an intermediate local. width is the entry's
// resolved integer width, threaded through to buildDirectCall so a call's
// arguments are built at the width the callee's other parameters expect.
// Anything else — a reference to a non-str local, any other node — is a clean
// rejection, never a guessed lowering. The function is shared by the three
// positions a str value is built: a ==/!= comparison operand (buildComparison),
// a call-site argument for a str parameter (buildCallArguments), and a
// str-returning helper's tail-position return value (buildBlock /
// buildSwitchCaseBody dispatch on resultInfo.isStr).
func buildStrOperand(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, id tir.NodeID, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind) (string, error) {
	node, ok := unit.Node(id)
	if !ok {
		return "", fmt.Errorf("entry function body expression references invalid node %d", id)
	}
	switch node.Kind {
	case tir.SymbolValue:
		info, declared := locals[node.Symbol]
		if !declared || !info.isStr {
			return "", fmt.Errorf("entry function body expression references symbol %d, which is not a str-typed local declared earlier in the body", node.Symbol)
		}
		return fmt.Sprintf("pebble_local_%d", node.Symbol), nil
	case tir.StringLiteral:
		valueText, err := buildStrLiteralValue(node)
		if err != nil {
			return "", err
		}
		return "(PebbleStr)" + valueText, nil
	case tir.DirectCall, tir.MethodCall:
		// A call to a str-returning helper used directly as a str value. The
		// DirectCall's own Type is the callee's resolved result type, which
		// the reachability walk has already validated as str for a reachable
		// helper (the check here is defense for hand-built IR); the call is
		// built by the same buildDirectCall machinery a scalar-width call
		// uses, so context and argument handling are identical.
		if !isStr(snapshot, node.Type) {
			return "", fmt.Errorf("entry function body expression contains a call to symbol %d whose result type is %s, want str", node.Symbol, describeType(snapshot, node.Type))
		}
		return buildDirectCall(unit, snapshot, fileSet, node, locals, width)
	default:
		return "", fmt.Errorf("entry function body expression contains a %s, want a str-typed local reference, a string literal, or a call to a str-returning function", node.Kind)
	}
}

// buildCharOperand builds one char value in a position that accepts a char
// expression, which is exactly four shapes (each confirmed against a real
// fixture): a CharLiteral (a char value with no local behind it, emitted as an
// int32_t decimal literal), a SymbolValue naming an in-scope char-typed local
// (emitted as its pebble_local_<symbolID> C name — an int32_t lvalue), a
// DirectCall to a char-returning helper (emitted as
// pebble_fn_<calleeSymbolID>(ctx, <args>) by buildDirectCall, the same
// call-building machinery buildExpr's DirectCall case uses), so a
// char-returning helper's result can be compared directly (g() == 'a') or
// passed to a char parameter (f(g())) without an intermediate local, and —
// since 10.42 — a tir.CheckedIndex, str indexing s[i], whose Children are
// [base, index]: the base is a str value built by buildStrOperand and the
// read is emitted as the runtime's UTF-8 decoder
// pebble_rt_str_char_at_<suffix>(<base>, <index>). width is
// the entry's resolved integer width, threaded through to buildDirectCall so a
// call's arguments are built at the width the callee's other parameters
// expect. Anything else — a reference to a non-char local, any other node — is
// a clean rejection, never a guessed lowering. The function is shared by the
// six positions a char value is built: a comparison operand (buildComparison),
// a char-typed local's declaration initializer (buildScalarInitializeCore), a
// char-typed local's reassignment new value (buildStoreCore), a call-site
// argument for a char parameter (buildCallArguments), and a char-returning
// helper's tail-position return value (buildBlock / buildSwitchCaseBody
// dispatch on resultInfo.isChar).
func buildCharOperand(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, id tir.NodeID, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind) (string, error) {
	node, ok := unit.Node(id)
	if !ok {
		return "", fmt.Errorf("entry function body expression references invalid node %d", id)
	}
	switch node.Kind {
	case tir.CharLiteral:
		valueText, err := buildCharLiteralValue(node)
		if err != nil {
			return "", err
		}
		return valueText, nil
	case tir.SymbolValue:
		info, declared := locals[node.Symbol]
		if !declared || !info.isChar {
			return "", fmt.Errorf("entry function body expression references symbol %d, which is not a char-typed local declared earlier in the body", node.Symbol)
		}
		return fmt.Sprintf("pebble_local_%d", node.Symbol), nil
	case tir.DirectCall, tir.MethodCall:
		// A call to a char-returning helper used directly as a char value. The
		// DirectCall's own Type is the callee's resolved result type, which
		// the reachability walk has already validated as char for a reachable
		// helper (the check here is defense for hand-built IR); the call is
		// built by the same buildDirectCall machinery a scalar-width call
		// uses, so context and argument handling are identical.
		if !isChar(snapshot, node.Type) {
			return "", fmt.Errorf("entry function body expression contains a call to symbol %d whose result type is %s, want char", node.Symbol, describeType(snapshot, node.Type))
		}
		return buildDirectCall(unit, snapshot, fileSet, node, locals, width)
	case tir.IndirectCall:
		// A call through a function-typed value whose result is char
		// (confirmed checker-reachable: `let c char = f('a');` lowers the
		// initializer to a char-typed IndirectCall). The call itself is built
		// by the same buildIndirectCall machinery a scalar-width indirect call
		// uses — the callee and every argument are built under the callee's
		// own function type — and the result is an int32_t, the same C type a
		// char value uses everywhere.
		if !isChar(snapshot, node.Type) {
			return "", fmt.Errorf("entry function body expression contains an indirect call whose result type is %s, want char", describeType(snapshot, node.Type))
		}
		return buildIndirectCall(unit, snapshot, fileSet, node, locals, width)
	case tir.Load:
		// A char-typed element read of a char-element slice (`let c char =
		// s[0];`). The checker lowers s[i] to Load(CheckedIndexPlace), exactly
		// as an integer or bool element read does, and the Load's Type is the
		// slice's char element type (already gated to char above). The read is
		// emitted by the same buildArrayPlaceRead machinery an integer slice
		// element read uses — .data[pebble_rt_checked_index_i32/_i64(...)] at
		// the ENTRY width — whose resulting C type int32_t is the char value's
		// C type everywhere.
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains a Load with %d child(ren), want exactly one place", len(node.Children))
		}
		place, ok := unit.Node(node.Children[0])
		if !ok {
			return "", fmt.Errorf("entry function body expression contains a Load referencing invalid place node %d", node.Children[0])
		}
		if place.Kind != tir.CheckedIndexPlace {
			return "", fmt.Errorf("entry function body expression contains a char Load whose place is a %s, want a CheckedIndexPlace (a char-element slice read)", place.Kind)
		}
		return buildArrayPlaceRead(unit, snapshot, fileSet, place, locals, width, false)
	case tir.CheckedIndex:
		// String indexing s[i]. The checker produces a bare tir.CheckedIndex —
		// not Load(CheckedIndexPlace), the node array/slice indexing uses —
		// exactly when the indexed value has no addressable place: a str's
		// byte-level content is not addressable the way array/slice element
		// storage is, so str indexing is a pure decode-to-value operation
		// (confirmed against a real fixture: the node's Children are [base,
		// index] and its Type is the snapshot's char builtin). The base is a
		// str value built by buildStrOperand — a reference to an in-scope str
		// local, a bare string literal, or a call to a str-returning helper,
		// all three confirmed reachable against real fixtures ("hi"[0] and
		// g()[0] both lower to this exact shape) — and the index is built by
		// the same dispatch buildArrayPlaceRead uses: an int-typed
		// IntegerLiteral (a literal index is the unanchored int builtin even
		// in an i64 entry, confirmed against a real fixture) or int-typed
		// SymbolValue (a range loop's iterator used directly as the index, the
		// same unanchored-int case) lowered directly, anything else (a
		// width-typed local reference, checked arithmetic) via buildExpr. The
		// read is emitted as the runtime's UTF-8 decoder
		// pebble_rt_str_char_at_<suffix>(<base>, <index>): s[i] is a
		// Unicode-scalar-value index, not a byte offset, so the runtime walks
		// and decodes the variable-width UTF-8 byte sequence from the start,
		// panicking on a negative or out-of-range index or on malformed UTF-8
		// (pebble_rt.h declares _i32 and _i64 variants; the index parameter's
		// width varies by the entry's, the int32_t result does not — a char
		// always fits in 32 bits, so the width-selected helper returns a char
		// either way).
		//
		// Since the slice-index slice, a CheckedIndex whose base is NOT a str
		// — a slice-element read of a value with no addressable place, the
		// same char result (`view()[1]` where view returns []char) — is also
		// accepted, built by the shared buildSliceIndexValue. This char
		// builder returns only an expression (no leading-statement slot), so a
		// pure-projection base (a slice-typed local or place, which needs no
		// temp) is emitted directly; a freshly-computed base (a call result,
		// which needs a temp-declaration statement) is a clean rejection here —
		// the positions that CAN host a temp (a print, a local declaration, a
		// return) intercept the shape before reaching this case and thread
		// buildSliceIndexValue's pre. A CheckedIndex whose base does not
		// resolve to a str or a slice type is a clean rejection naming what
		// was found, never a guessed lowering.
		strBase, err := checkedIndexBaseIsStr(unit, snapshot, node)
		if err != nil {
			return "", err
		}
		if !strBase {
			pre, read, err := buildSliceIndexValue(unit, snapshot, fileSet, id, node, locals, width, false)
			if err != nil {
				return "", err
			}
			if pre != "" {
				base, ok := unit.Node(node.Children[0])
				if !ok {
					return "", fmt.Errorf("entry function body expression contains a CheckedIndex referencing invalid base node %d", node.Children[0])
				}
				return "", fmt.Errorf("entry function body expression indexes a %s of type %s in a pure-expression position with nowhere to place the temp-declaration statement the freshly-computed slice value needs; bind the slice into a local first", base.Kind, describeType(snapshot, base.Type))
			}
			return read, nil
		}
		if len(node.Children) != 2 {
			return "", fmt.Errorf("entry function body expression contains a CheckedIndex with %d child(ren), want exactly two (the str value being indexed and the index)", len(node.Children))
		}
		base, err := buildStrOperand(unit, snapshot, fileSet, node.Children[0], locals, width)
		if err != nil {
			return "", err
		}
		indexNode, ok := unit.Node(node.Children[1])
		if !ok {
			return "", fmt.Errorf("entry function body expression contains a CheckedIndex referencing invalid index node %d", node.Children[1])
		}
		var index string
		if indexNode.Kind == tir.IntegerLiteral && indexNode.Type == snapshot.Builtins().Int {
			if !isNonNegativeDecimal(indexNode.Literal.IntegerNum) {
				return "", fmt.Errorf("str index contains an integer literal with malformed text %q", indexNode.Literal.IntegerNum)
			}
			index = indexNode.Literal.IntegerNum
		} else if indexNode.Kind == tir.SymbolValue && indexNode.Type == snapshot.Builtins().Int {
			// An int-typed SymbolValue index is a range loop's iterator
			// referenced directly (the same unanchored-int case
			// buildComparisonOperand and buildArrayPlaceRead handle), and the
			// iterator is always declared in C at the entry's width, so its
			// name is the correct C lvalue for the index.
			if _, declared := locals[indexNode.Symbol]; !declared {
				return "", fmt.Errorf("str index references symbol %d, which is not a local in scope", indexNode.Symbol)
			}
			index = fmt.Sprintf("pebble_local_%d", indexNode.Symbol)
		} else if isUint(snapshot, indexNode.Type) {
			// A uint-typed str index (a uint-typed local or loop iterator):
			// built by the dedicated uint grammar, mirroring the slice/array
			// index dispatch.
			index, err = buildUintExpr(unit, snapshot, fileSet, node.Children[1], locals, width)
			if err != nil {
				return "", fmt.Errorf("str index: %v", err)
			}
		} else {
			index, err = buildExpr(unit, snapshot, fileSet, node.Children[1], locals, width, width)
			if err != nil {
				return "", fmt.Errorf("str index: %v", err)
			}
		}
		return "pebble_rt_str_char_at_" + checkedSuffix(width) + "(" + base + ", " + index + ", " + buildSourceLoc(fileSet, node.Span) + ")", nil
	default:
		return "", fmt.Errorf("entry function body expression contains a %s, want a char literal, a reference to a char-typed local declared earlier in the body, a call to a char-returning function, or a str index", node.Kind)
	}
}

// comparisonOperator maps the six comparison token kinds this backend lowers
// to their plain C spellings. These map 1:1 to C syntax — no runtime helper is
// involved, since comparing two integer values (the entry's width, or the
// int-typed literal case) cannot overflow. Any other
// operator is deliberately not mapped and rejected by the caller.
func comparisonOperator(op syntax.TokenKind) (string, bool) {
	switch op {
	case syntax.Less:
		return "<", true
	case syntax.LessEqual:
		return "<=", true
	case syntax.Greater:
		return ">", true
	case syntax.GreaterEqual:
		return ">=", true
	case syntax.Equal:
		return "==", true
	case syntax.NotEqual:
		return "!=", true
	default:
		return "", false
	}
}

// buildExpr builds the C expression text for an integer value node of the
// entry's resolved width, recursing into its operands. width (types.Int,
// types.I32, or types.I64) is the width resolved once in Emit; every node in an accepted
// tree must carry exactly that width's builtin — a node carrying the other
// width (an i32 local referenced inside an i64 entry, or vice versa) is a
// clean width-mismatch rejection, never a coercion. locals is the set of
// symbols in scope at this point in the
// entry body (a map is deliberately used, not a slice, so membership is a
// constant-time check); it is read-only for a SymbolValue reference and is
// otherwise threaded through unchanged. In addition to the scalar and call
// forms below, it accepts plain bitwise &, |, ^, and ~ expressions, which are
// safe to emit directly as C operators:
//
//   - IntegerLiteral — its decimal text (defensively validated, exactly as
//     10.3 validated a bare literal return), given a "u" suffix when the
//     literal's width is unsigned so a large value is an unsigned C constant.
//   - CheckedNegate with exactly one operand of the entry's width —
//     pebble_rt_checked_neg_<suffix>.
//   - CheckedArithmetic with exactly two operands of the entry's width and
//     operator +, -, *, /, or % — pebble_rt_checked_add_<suffix> /
//     pebble_rt_checked_sub_<suffix> / pebble_rt_checked_mul_<suffix> /
//     pebble_rt_checked_div_<suffix> / pebble_rt_checked_mod_<suffix>.
//   - BinaryValue with exactly two operands of the entry's width and operator
//     &, |, or ^ — the parenthesized plain C operator expression.
//   - PrefixValue with exactly one operand of the entry's width and operator
//     ~ — the parenthesized plain C bitwise-not expression.
//   - SymbolValue whose Symbol is in locals — pebble_local_<symbol ID>, the C
//     name buildBlock gave that local's declaration.
//   - DirectCall — a call to another Pebble-convention function whose result
//     is the entry's width (validated by the reachability walk in
//     discoverReachableHelpers). Each call-site argument is built by the
//     grammar its callee parameter resolves to — the entry's width for an
//     integer parameter (this builder), bool for a bool parameter
//     (buildBoolExpr) — so the call emits pebble_fn_<calleeSymbolID>(ctx,
//     <arg0>, <arg1>, ...), with the ctx argument prepended by this backend
//     since the typed IR threads context via ContextAction rather than as an
//     explicit child.
//
// CheckedArithmetic with any other operator (the integral operators that build
// this node but are not yet lowered) is rejected, not guessed. BinaryValue or
// PrefixValue with any other operator, including shifts, is also rejected. A SymbolValue
// referencing anything not in locals (a global, a symbol from an
// outer/different scope — none of which are reachable from this narrow body
// shape, but checked defensively rather than assumed) is a clean rejection.
// Any other node kind at any position — a non-integer
// operand, CheckedShift, and so on — is a clean rejection naming what was
// found.
// Emitting the checked runtime helpers (rather than raw C operators) is what
// keeps the IR nodes' real overflow and divide-by-zero semantics from silently
// disappearing in the emitted program.
//
// width is the width the expression tree is being built AT — the entry's own
// width for a top-level expression, but a value's own resolved width (u8, i64,
// ...) when buildScalarInitializeCore, buildCallArgument, buildStoreCore, or
// an IntegerCast child re-anchors the grammar. entryWidth is the width of the
// function being emitted, threaded unchanged through every recursive call so a
// width-suffixed runtime call buried in the tree (a slice/array index's
// pebble_rt_checked_index_i32/_i64, whose helper exists only at i32/i64) can
// always pick the entry's helper rather than the element's possibly-empty
// checkedSuffix.
func buildExpr(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, id tir.NodeID, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind, entryWidth types.BuiltinKind) (string, error) {
	node, ok := unit.Node(id)
	if !ok {
		return "", fmt.Errorf("entry function body expression references invalid node %d", id)
	}
	if node.Kind == tir.ContextValue || runtimeType(unit, snapshot, node.Type) != 0 {
		if node.Kind == tir.ContextValue {
			return "(*ctx)", nil
		}
		if node.Kind == tir.SymbolValue {
			if node.Symbol == unit.Runtime().Context {
				return "(*ctx)", nil
			}
			if _, declared := locals[node.Symbol]; !declared {
				return "", fmt.Errorf("runtime symbol %d is not a local", node.Symbol)
			}
			return fmt.Sprintf("pebble_local_%d", node.Symbol), nil
		}
	}
	if node.Kind == tir.IndirectCall {
		return buildIndirectCall(unit, snapshot, fileSet, node, locals, width)
	}
	if node.Kind == tir.SliceFromRaw {
		return buildRawSliceConstruction(unit, snapshot, fileSet, node, locals, width, "entry function body expression")
	}
	// A pointer-typed node's Type is never the entry's width, so it must
	// bypass the width gate below. This covers every shape a pointer value
	// can take: freshly constructed (AddressOf, NilPointer), a reference to
	// an existing pointer-typed local (SymbolValue), or the result of a
	// pointer-returning helper call (DirectCall) — not just the construction
	// forms, since a pointer local is very commonly read back by name rather
	// than always rebuilt at each use site.
	if isPointer(snapshot, node.Type) {
		switch node.Kind {
		case tir.AddressOf:
			if len(node.Children) != 1 {
				return "", fmt.Errorf("entry function body expression contains an AddressOf with %d children, want exactly one", len(node.Children))
			}
			placeLValue, _, err := buildPlaceLValue(unit, snapshot, fileSet, node.Children[0], locals, entryWidth)
			if err != nil {
				return "", fmt.Errorf("entry function body address-of place: %v", err)
			}
			pointeeTypeID, ok := pointerPointeeType(snapshot, node.Type)
			if !ok {
				return "", fmt.Errorf("entry function body expression contains an AddressOf with unsupported pointer type %s", describeType(snapshot, node.Type))
			}
			return "(" + pointerTypeName(snapshot, pointeeTypeID) + ")(&" + placeLValue + ")", nil
		case tir.NilPointer:
			pointeeTypeID, ok := pointerPointeeType(snapshot, node.Type)
			if !ok {
				return "", fmt.Errorf("entry function body expression contains a NilPointer with unsupported pointer type %s", describeType(snapshot, node.Type))
			}
			return "(" + pointerTypeName(snapshot, pointeeTypeID) + ")(NULL)", nil
		case tir.SymbolValue:
			if _, declared := locals[node.Symbol]; !declared {
				return "", fmt.Errorf("entry function body expression references symbol %d, which is not a local declared earlier in the entry body", node.Symbol)
			}
			return fmt.Sprintf("pebble_local_%d", node.Symbol), nil
		case tir.Load:
			if len(node.Children) == 1 {
				place, ok := unit.Node(node.Children[0])
				if ok && place.Kind == tir.FieldPlace {
					return buildStructFieldRead(unit, snapshot, fileSet, place, locals, width, false)
				}
			}
			return "", fmt.Errorf("entry function body expression contains an unsupported pointer Load")
		case tir.DirectCall:
			return buildDirectCall(unit, snapshot, fileSet, node, locals, width)
		case tir.CheckedOptionalUnwrap:
			// A force-unwrap of an optional whose payload is a pointer
			// (`let p *i32 = o!;` or an inline `*(o!)`): the unwrap is only
			// the has_value check via the runtime helper (no overflow/width
			// concerns for a pointer payload), selected from the PAYLOAD's own
			// type by optionalUnwrapSuffix ("ptr" here). The helper's void *
			// result converts implicitly to any object pointer type in C.
			unwrapSuffix := optionalUnwrapSuffix(snapshot, node.Type)
			if unwrapSuffix == "" {
				return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap of a %s payload, which has no runtime unwrap helper", describeType(snapshot, node.Type))
			}
			if len(node.Children) != 1 {
				return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap with %d child(ren), want exactly one (the optional value being unwrapped)", len(node.Children))
			}
			child, ok := unit.Node(node.Children[0])
			if !ok {
				return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap referencing invalid child node %d", node.Children[0])
			}
			if child.Kind == tir.Load && len(child.Children) == 1 {
				if _, ok := unit.Node(child.Children[0]); !ok {
					return "", fmt.Errorf("invalid optional place")
				}
				expr, typ, err := buildPlaceLValue(unit, snapshot, fileSet, child.Children[0], locals, width)
				if err != nil {
					return "", err
				}
				if !isOptional(snapshot, typ) {
					return "", fmt.Errorf("optional unwrap base is not optional")
				}
				return fmt.Sprintf("pebble_rt_checked_unwrap_%s(%s.has_value, %s.value, %s)", unwrapSuffix, expr, expr, buildSourceLoc(fileSet, node.Span)), nil
			}
			if child.Kind != tir.SymbolValue {
				return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap whose child is a %s, want a SymbolValue naming an optional-typed local", child.Kind)
			}
			if info, declared := locals[child.Symbol]; !declared || info.optional == 0 {
				return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap of symbol %d, which is not an optional-typed local", child.Symbol)
			}
			return fmt.Sprintf("pebble_rt_checked_unwrap_%s(pebble_local_%d.has_value, pebble_local_%d.value, %s)", unwrapSuffix, child.Symbol, child.Symbol, buildSourceLoc(fileSet, node.Span)), nil
		case tir.SourceAlias:
			// A SourceAlias is transparent — it records grouped-expression
			// parens — so a pointer-typed one unwraps to its single child,
			// exactly as the general-value SourceAlias case below does.
			if len(node.Children) != 1 {
				return "", fmt.Errorf("entry function body expression contains a pointer-typed SourceAlias with %d child(ren), want exactly one", len(node.Children))
			}
			return buildExpr(unit, snapshot, fileSet, node.Children[0], locals, width, entryWidth)
		case tir.PointerCast:
			if len(node.Children) != 1 {
				return "", fmt.Errorf("entry function body expression contains a PointerCast with %d children, want exactly one", len(node.Children))
			}
			child, err := buildExpr(unit, snapshot, fileSet, node.Children[0], locals, width, entryWidth)
			if err != nil {
				return "", fmt.Errorf("entry function body pointer cast child: %v", err)
			}
			pointeeTypeID, ok := pointerPointeeType(snapshot, node.Type)
			if !ok {
				return "", fmt.Errorf("entry function body expression contains a PointerCast with unsupported pointer type %s", describeType(snapshot, node.Type))
			}
			return "(" + pointerTypeName(snapshot, pointeeTypeID) + ")(" + child + ")", nil
		default:
			return "", fmt.Errorf("entry function body expression contains a %s of pointer type %s, which this backend does not lower", node.Kind, describeType(snapshot, node.Type))
		}
	}
	// A function-typed node's Type is never the entry's width, so it must
	// bypass the width gate below the same way a pointer-typed node does. This
	// covers the shapes a function value can take: a reference to an
	// existing function-typed local or parameter (SymbolValue), a bare
	// top-level function reference (HoistedFunctionValue), a generic function
	// referenced as a value (GenericFunctionValue), a call to a
	// function-returning helper (DirectCall), a function-typed struct field
	// read (FieldValue/Load), and (since function-types slice 2) a bare
	// function reference used as a struct field's construction value — all
	// built by buildFunctionValue. (The
	// general indirect call through such a value is handled by buildIndirectCall
	// at the top of this function, whose callee child is a function-typed node
	// routed through this same bypass.)
	if isFunctionType(snapshot, node.Type) {
		return buildFunctionValue(unit, snapshot, fileSet, node, locals, "entry function body expression", width)
	}
	// A node carrying the abstract `int` builtin (types.Int) is accepted at any
	// integer width. The checker deliberately leaves a value at this unanchored
	// type when nothing pins it to a fixed width — an untyped integer literal,
	// a range-loop iterator never used in a width-anchoring position, or a
	// `let`-declared global constant with an untyped literal initializer — and
	// this backend already treats such nodes as width-compatible in the narrow
	// positions buildRangeBound/buildComparisonOperand/buildCharOperand
	// special-case. `int` shares i32's C type (int32_t), so an `int`-typed node
	// is emitted at whatever integer width the surrounding position requests
	// (a literal as its decimal text, a SymbolValue as its already-declared C
	// name) with no cast needed; the gate below still rejects a genuinely
	// mismatched fixed-width node (an i64 value inside an i32 context), which
	// is the mismatch it exists to catch. Symmetrically, isCompatibleIntegerWidth
	// accepts a node carrying ANY concrete fixed-width integer builtin whose C
	// representation matches the requested width (an i32-typed value inside an
	// `int` context — the shape a generic specialization produces), for the same
	// reason: the two share a C type, so the value is emitted as-is with no
	// cast.
	if node.Kind != tir.CheckedIntegerToEnum && node.Kind != tir.OptionalIntegerToEnum && !isWidth(snapshot, width, node.Type) && !(isAbstractInt(snapshot, node.Type) && cType(width) != "") && !isCompatibleIntegerWidth(snapshot, width, node.Type) {
		wantName, _ := builtinName(width)
		return "", fmt.Errorf("entry function body expression contains a %s of type %s, want %s", node.Kind, describeType(snapshot, node.Type), wantName)
	}
	switch node.Kind {
	case tir.IntegerLiteral:
		text := node.Literal.IntegerNum
		if !isNonNegativeDecimal(text) {
			return "", fmt.Errorf("entry function body expression contains an integer literal with malformed text %q", text)
		}
		return integerLiteralText(text, width), nil
	case tir.IntegerCast:
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains an IntegerCast with %d children, want exactly one", len(node.Children))
		}
		destination, ok := snapshot.Key(node.Type)
		if !ok {
			return "", fmt.Errorf("entry function body expression contains an IntegerCast with invalid destination type %d", node.Type)
		}
		destinationWidth, ok := destination.Builtin()
		if !ok || cType(destinationWidth) == "" {
			return "", fmt.Errorf("entry function body expression contains an IntegerCast with non-integer destination type %s", describeType(snapshot, node.Type))
		}
		child, ok := unit.Node(node.Children[0])
		if !ok {
			return "", fmt.Errorf("entry function body expression contains an IntegerCast referencing invalid child node %d", node.Children[0])
		}
		childType, ok := snapshot.Key(child.Type)
		if !ok {
			return "", fmt.Errorf("entry function body IntegerCast child has invalid type %d", child.Type)
		}
		childWidth, ok := childType.Builtin()
		if !ok || cType(childWidth) == "" {
			return "", fmt.Errorf("entry function body IntegerCast child has non-integer type %s", describeType(snapshot, child.Type))
		}
		childExpr, err := buildExpr(unit, snapshot, fileSet, node.Children[0], locals, childWidth, entryWidth)
		if err != nil {
			return "", fmt.Errorf("entry function body integer cast child: %v", err)
		}
		return "(" + cType(destinationWidth) + ")(" + childExpr + ")", nil
	case tir.EnumToInteger:
		// An enum value cast to an integer (`Color.green as i32`), lowered as a
		// plain, unchecked C cast of the enum value's expression to the
		// destination integer type. An enum value once constructed is always a
		// valid member of its enum's declared variant set — no well-typed Pebble
		// program can observe an "invalid" enum value, unlike the reverse
		// integer-to-enum direction, which needs a runtime validity check — so
		// reading out the enum's underlying integer representation is always
		// well-defined and needs no runtime helper. The destination width is
		// resolved from the node's own Type exactly as IntegerCast resolves its
		// own (and the width gate above has already required it to be the
		// surrounding context's width); the single child is the enum value being
		// cast, built by buildEnumValue (an enum-typed local reference, a
		// variant literal, or a zero-payload variant construction), and the
		// emitted C is `(<destination C type>)(<enum value expression>)`. A
		// C enum's value IS the variant's ordinal in declared order and casts to
		// an integer type directly and trivially, so no intermediate step
		// through the enum's own typedef is needed. The reverse direction,
		// CheckedIntegerToEnum, is implemented in this file's sibling case (and
		// via buildCheckedIntegerToEnumExpr); only the optional-destination form,
		// OptionalIntegerToEnum, remains out of scope.
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains an EnumToInteger with %d children, want exactly one", len(node.Children))
		}
		destination, ok := snapshot.Key(node.Type)
		if !ok {
			return "", fmt.Errorf("entry function body expression contains an EnumToInteger with invalid destination type %d", node.Type)
		}
		destinationWidth, ok := destination.Builtin()
		if !ok || cType(destinationWidth) == "" {
			return "", fmt.Errorf("entry function body expression contains an EnumToInteger with non-integer destination type %s", describeType(snapshot, node.Type))
		}
		childExpr, err := buildEnumValue(unit, snapshot, fileSet, node.Children[0], locals, entryWidth)
		if err != nil {
			return "", err
		}
		return "(" + cType(destinationWidth) + ")(" + childExpr + ")", nil
	case tir.CharToInteger:
		// A char value cast to an integer (`c as u32`), lowered as a plain,
		// unchecked C cast of the char value's expression to the destination
		// integer type. A char is a Unicode scalar value, and every valid
		// codepoint (max 0x10FFFF) fits identically in every integer width
		// Pebble has, so no well-typed Pebble program can construct a char
		// value whose codepoint overflows the destination width — reading out
		// the char's codepoint as an integer is always well-defined and needs
		// no runtime helper, exactly as enum-to-integer (EnumToInteger) casts
		// need none. The destination width is resolved from the node's own Type
		// exactly as IntegerCast resolves its own (and the width gate above has
		// already required it to be the surrounding context's width); the single
		// child is the char value being cast, built by buildCharOperand (a char
		// literal, a char-typed local reference, a char-returning call, or a
		// char element read), and the emitted C is `(<destination C type>)
		// (<char value expression>)`. The reverse direction, integer-to-char,
		// is out of scope (an arbitrary integer is not necessarily a valid
		// Unicode scalar, so that direction needs a validity-checked design of
		// its own).
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains a CharToInteger with %d children, want exactly one", len(node.Children))
		}
		destination, ok := snapshot.Key(node.Type)
		if !ok {
			return "", fmt.Errorf("entry function body expression contains a CharToInteger with invalid destination type %d", node.Type)
		}
		destinationWidth, ok := destination.Builtin()
		if !ok || cType(destinationWidth) == "" {
			return "", fmt.Errorf("entry function body expression contains a CharToInteger with non-integer destination type %s", describeType(snapshot, node.Type))
		}
		childExpr, err := buildCharOperand(unit, snapshot, fileSet, node.Children[0], locals, entryWidth)
		if err != nil {
			return "", err
		}
		return "(" + cType(destinationWidth) + ")(" + childExpr + ")", nil
	case tir.PointerToInteger:
		// A pointer value cast to an integer (`ptr as u64`), lowered as a
		// plain, unchecked C cast of the pointer value's expression to the
		// destination integer type. This mirrors EnumToInteger/CharToInteger
		// exactly: the reverse direction, integer-to-pointer, stays forbidden
		// (compatibleForbidden) by the checker, so this node can only ever
		// represent a well-typed pointer whose bit pattern is being read out
		// as an integer address — always well-defined and needing no runtime
		// helper. The destination width is resolved from the node's own Type;
		// the single child is the pointer value being cast, built by buildExpr
		// (which routes any pointer-typed child through its pointer branch),
		// and the emitted C is `(<destination C type>)(<pointer expression>)`.
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains a PointerToInteger with %d children, want exactly one", len(node.Children))
		}
		destination, ok := snapshot.Key(node.Type)
		if !ok {
			return "", fmt.Errorf("entry function body expression contains a PointerToInteger with invalid destination type %d", node.Type)
		}
		destinationWidth, ok := destination.Builtin()
		if !ok || cType(destinationWidth) == "" {
			return "", fmt.Errorf("entry function body expression contains a PointerToInteger with non-integer destination type %s", describeType(snapshot, node.Type))
		}
		childExpr, err := buildExpr(unit, snapshot, fileSet, node.Children[0], locals, width, entryWidth)
		if err != nil {
			return "", err
		}
		return "(" + cType(destinationWidth) + ")(" + childExpr + ")", nil
	case tir.CheckedIntegerToEnum:
		// An integer cast to an enum (`5 as Color`), lowered through the
		// single canonical-width runtime primitive
		// pebble_rt_checked_int_to_enum. The destination enum type is the
		// node's own Type (an enum-typed value, exactly as the surrounding
		// width gate bypasses it — the enum check precedes the width gate by
		// design); its variant count is the destination enum's TypeDecl.Members
		// length (variant Members[i] gets the C enum value i, so an integer
		// names a real variant exactly when 0 <= value < variant_count). The
		// single child is the ordinary integer being cast, built by the
		// integer expression builder at its own declared width — not
		// buildEnumValue, which is for enum-typed operands. The emitted C is
		// `(<destination enum C type>)pebble_rt_checked_int_to_enum((int64_t)
		// (<child expr>), <variant_count>, <source loc>)`: the source is
		// widened to the primitive's int64_t input (sign-extending a negative
		// signed source, zero-extending any unsigned source up to 63 bits, and
		// bit-reinterpreting a u64 >= 2^63 as negative — all recovered
		// correctly by the primitive's single unsigned bounds comparison), and
		// the primitive's result is narrowed back to the enum type. SAFE mode
		// panics out-of-range; RELEASE returns the value unchanged (unchecked,
		// trusting the input). The reverse direction, OptionalIntegerToEnum
		// (`5 as ?Color`), is handled in the sibling case below.
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains a CheckedIntegerToEnum with %d children, want exactly one", len(node.Children))
		}
		return buildCheckedIntegerToEnumExpr(unit, snapshot, fileSet, node, locals, "entry function body expression", entryWidth)
	case tir.OptionalIntegerToEnum:
		// An integer cast to an optional enum (`5 as ?Color`): the ONE
		// supported position is a local variable declaration's initializer
		// (see buildOptionalIntegerToEnumDeclaration), where the backend can
		// emit a pre-statement to evaluate the source integer exactly once
		// before building both the has_value bool and the enum value from it.
		// Every expression position reaches here only through a plain
		// `(string, error)` builder with no pre-statement threading, so there
		// is nowhere to hoist the source's single evaluation — embedding the
		// source twice would evaluate it twice, wrong for a side-effecting
		// source. Rather than emit a wrong double-evaluated C or reach for a
		// GNU statement-expression, the cast is cleanly rejected here naming
		// what was found.
		return "", fmt.Errorf("entry function body expression contains an integer-to-optional-enum cast to %s, which is only supported as a local variable declaration's initializer; a C expression position has nowhere to place the temp-declaration statement that evaluates the source integer exactly once, so the cast is not supported here", describeType(snapshot, node.Type))
	case tir.FloatToInteger:
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains a FloatToInteger with %d children, want exactly one", len(node.Children))
		}
		destination, ok := snapshot.Key(node.Type)
		if !ok {
			return "", fmt.Errorf("entry function body expression contains a FloatToInteger with invalid destination type %d", node.Type)
		}
		destinationWidth, ok := destination.Builtin()
		// The float-to-integer conversion helpers exist only for i32 and i64
		// destinations. A u64 destination is cleanly rejected here rather than
		// emitted as a call to a nonexistent pebble_rt_checked_f*_to_u64
		// (checkedSuffix admits u64 for the +, -, * arithmetic family this
		// slice adds, but the float-conversion family has no u64 twin yet).
		if !ok || checkedSuffix(destinationWidth) == "" || destinationWidth == types.U64 {
			return "", fmt.Errorf("entry function body expression contains a FloatToInteger with non-integer destination type %s", describeType(snapshot, node.Type))
		}
		child, ok := unit.Node(node.Children[0])
		if !ok {
			return "", fmt.Errorf("entry function body expression contains a FloatToInteger referencing invalid child node %d", node.Children[0])
		}
		childWidth := resolvedFloatKind(snapshot, child.Type)
		if childWidth == 0 {
			return "", fmt.Errorf("entry function body FloatToInteger child has non-float type %s", describeType(snapshot, child.Type))
		}
		childExpr, err := buildFloatExpr(unit, snapshot, fileSet, node.Children[0], locals, childWidth)
		if err != nil {
			return "", fmt.Errorf("entry function body float-to-integer cast child: %v", err)
		}
		helper := "pebble_rt_checked_" + childFloatSuffix(childWidth) + "_to_" + checkedSuffix(destinationWidth)
		return helper + "(" + childExpr + ", " + buildSourceLoc(fileSet, node.Span) + ")", nil
	case tir.CheckedNegate:
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains a CheckedNegate with %d operand(s), want exactly one", len(node.Children))
		}
		if node.Operator != syntax.Minus {
			return "", fmt.Errorf("entry function body expression contains a CheckedNegate with operator %s, want -", node.Operator)
		}
		child, err := buildExpr(unit, snapshot, fileSet, node.Children[0], locals, width, entryWidth)
		if err != nil {
			return "", err
		}
		return "pebble_rt_checked_neg_" + checkedSuffix(width) + "(" + child + ", " + buildSourceLoc(fileSet, node.Span) + ")", nil
	case tir.CheckedArithmetic:
		if len(node.Children) != 2 {
			return "", fmt.Errorf("entry function body expression contains a CheckedArithmetic with %d operand(s), want exactly two", len(node.Children))
		}
		helper, ok := checkedArithmeticHelper(node.Operator, width)
		if !ok {
			return "", fmt.Errorf("entry function body expression contains a CheckedArithmetic with operator %s, want +, -, *, /, or %% (at u64, only +, -, and * have a checked runtime helper)", node.Operator)
		}
		left, err := buildExpr(unit, snapshot, fileSet, node.Children[0], locals, width, entryWidth)
		if err != nil {
			return "", err
		}
		right, err := buildExpr(unit, snapshot, fileSet, node.Children[1], locals, width, entryWidth)
		if err != nil {
			return "", err
		}
		return helper + "(" + left + ", " + right + ", " + buildSourceLoc(fileSet, node.Span) + ")", nil
	case tir.CheckedShift:
		if len(node.Children) != 2 {
			return "", fmt.Errorf("entry function body expression contains a CheckedShift with %d operand(s), want exactly two", len(node.Children))
		}
		helper, ok := checkedShiftHelper(node.Operator, width)
		if !ok {
			return "", fmt.Errorf("entry function body expression contains a CheckedShift with operator %s, want << or >>", node.Operator)
		}
		left, err := buildExpr(unit, snapshot, fileSet, node.Children[0], locals, width, entryWidth)
		if err != nil {
			return "", err
		}
		amountNode, ok := unit.Node(node.Children[1])
		if !ok {
			return "", fmt.Errorf("entry function body expression references invalid shift amount node %d", node.Children[1])
		}
		amountType, ok := snapshot.Key(amountNode.Type)
		if !ok {
			return "", fmt.Errorf("entry function body shift amount has invalid type %d", amountNode.Type)
		}
		amountWidth, ok := amountType.Builtin()
		if !ok || cType(amountWidth) == "" {
			return "", fmt.Errorf("entry function body shift amount has non-integer type %s", describeType(snapshot, amountNode.Type))
		}
		amount, err := buildExpr(unit, snapshot, fileSet, node.Children[1], locals, amountWidth, entryWidth)
		if err != nil {
			return "", err
		}
		if amountWidth != width {
			amount = "(" + cType(width) + ")(" + amount + ")"
		}
		return helper + "(" + left + ", " + amount + ", " + buildSourceLoc(fileSet, node.Span) + ")", nil
	case tir.BinaryValue:
		if len(node.Children) != 2 {
			return "", fmt.Errorf("entry function body expression contains a BinaryValue with %d operand(s), want exactly two", len(node.Children))
		}
		op, ok := bitwiseOperator(node.Operator)
		if !ok {
			return "", fmt.Errorf("entry function body expression contains a BinaryValue with operator %s, want &, |, or ^", node.Operator)
		}
		left, err := buildExpr(unit, snapshot, fileSet, node.Children[0], locals, width, entryWidth)
		if err != nil {
			return "", err
		}
		right, err := buildExpr(unit, snapshot, fileSet, node.Children[1], locals, width, entryWidth)
		if err != nil {
			return "", err
		}
		return "(" + left + " " + op + " " + right + ")", nil
	case tir.PrefixValue:
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains a PrefixValue with %d operand(s), want exactly one", len(node.Children))
		}
		if node.Operator != syntax.Tilde {
			return "", fmt.Errorf("entry function body expression contains a PrefixValue with operator %s, want ~", node.Operator)
		}
		child, err := buildExpr(unit, snapshot, fileSet, node.Children[0], locals, width, entryWidth)
		if err != nil {
			return "", err
		}
		return "~(" + child + ")", nil
	case tir.SymbolValue:
		if _, declared := locals[node.Symbol]; !declared {
			return "", fmt.Errorf("entry function body expression references symbol %d, which is not a local declared earlier in the entry body", node.Symbol)
		}
		return fmt.Sprintf("pebble_local_%d", node.Symbol), nil
	case tir.CheckedOptionalUnwrap:
		// A force-unwrap of an optional-typed local (x!). The child is a
		// SymbolValue naming the optional local, and this node's Type is the
		// unwrapped result type (the payload type, already gated above to an
		// integer builtin this grammar accepts). The unwrap is bounds-checked
		// via the runtime helper — selected from the PAYLOAD's own type (see
		// optionalUnwrapSuffix), so a uint/u64 payload routes to
		// pebble_rt_checked_unwrap_u64 rather than the entry-width helper —
		// passing the optional local's has_value and value fields.
		unwrapSuffix := optionalUnwrapSuffix(snapshot, node.Type)
		if unwrapSuffix == "" {
			return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap of a %s payload, which has no runtime unwrap helper", describeType(snapshot, node.Type))
		}
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap with %d child(ren), want exactly one (the optional value being unwrapped)", len(node.Children))
		}
		child, ok := unit.Node(node.Children[0])
		if !ok {
			return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap referencing invalid child node %d", node.Children[0])
		}
		if child.Kind == tir.Load && len(child.Children) == 1 {
			expr, typ, err := buildPlaceLValue(unit, snapshot, fileSet, child.Children[0], locals, width)
			if err != nil {
				return "", err
			}
			if !isOptional(snapshot, typ) {
				return "", fmt.Errorf("optional unwrap base is not optional")
			}
			return fmt.Sprintf("pebble_rt_checked_unwrap_%s(%s.has_value, %s.value, %s)", unwrapSuffix, expr, expr, buildSourceLoc(fileSet, node.Span)), nil
		}
		if child.Kind != tir.SymbolValue {
			return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap whose child is a %s, want a SymbolValue naming an optional-typed local", child.Kind)
		}
		info, declared := locals[child.Symbol]
		if !declared {
			return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap referencing symbol %d, which is not a local declared earlier in the entry body", child.Symbol)
		}
		if info.optional == 0 {
			return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap of symbol %d, which is not an optional-typed local", child.Symbol)
		}
		return fmt.Sprintf("pebble_rt_checked_unwrap_%s(pebble_local_%d.has_value, pebble_local_%d.value, %s)", unwrapSuffix, child.Symbol, child.Symbol, buildSourceLoc(fileSet, node.Span)), nil
	case tir.Load:
		// A tuple element or struct field read. Reading one element of a
		// tuple-typed local (`t.1`) is lowered by the checker to a Load of a
		// TuplePlace whose single child is the StoragePlace naming the tuple
		// local, and reading one field of a struct-typed local (`point.x`) to
		// a Load of a FieldPlace whose single child is the StoragePlace naming
		// the struct local (both confirmed against real fixtures); these are
		// the only shapes real source produces for reading an element/field of
		// a compound local (a plain local read is a SymbolValue, not a Load).
		// The Load's Type is the element/field's own type, already gated to
		// the entry's width above, so the element/field must resolve to the
		// entry's width here. The emitted C is
		// pebble_local_<symbol>._<ordinal> for a tuple and
		// pebble_local_<symbol>.pebble_field_<member> for a struct.
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains a Load with %d child(ren), want exactly one place", len(node.Children))
		}
		place, ok := unit.Node(node.Children[0])
		if !ok {
			return "", fmt.Errorf("entry function body expression contains a Load referencing invalid place node %d", node.Children[0])
		}
		if place.Kind != tir.TuplePlace {
			if place.Kind == tir.CheckedIndexPlace {
				return buildArrayPlaceRead(unit, snapshot, fileSet, place, locals, entryWidth, false)
			}
			if place.Kind == tir.FieldPlace {
				return buildStructFieldRead(unit, snapshot, fileSet, place, locals, width, false)
			}
			if place.Kind == tir.DereferencePlace {
				return buildDereferencePlaceRead(unit, snapshot, fileSet, place, locals, width, node.Span, false)
			}
			return "", fmt.Errorf("entry function body expression contains a Load whose place is a %s, want a TuplePlace, CheckedIndexPlace, FieldPlace, or DereferencePlace", place.Kind)
		}
		return buildTuplePlaceRead(unit, snapshot, fileSet, place, locals, width, false)
	case tir.TupleElementValue:
		// The checker produces a TupleElementValue only when a tuple literal is
		// indexed directly — (1, 2).1 — whose child is the TupleValue being
		// indexed and whose element type comes out as the unanchored `int`
		// builtin (confirmed against a real fixture); that shape is out of
		// scope, and its int-typed element fails the width gate above before
		// reaching this case. The only in-scope element read of a tuple local
		// is Load(TuplePlace). This case is therefore defense for hand-built
		// IR matching the local-read shape: a TupleElementValue whose single
		// child is a SymbolValue naming a tuple-typed local is emitted exactly
		// like the Load(TuplePlace) read, and any other base is a clean
		// rejection.
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains a TupleElementValue with %d child(ren), want exactly one (the tuple value being indexed)", len(node.Children))
		}
		base, ok := unit.Node(node.Children[0])
		if !ok {
			return "", fmt.Errorf("entry function body expression contains a TupleElementValue referencing invalid node %d", node.Children[0])
		}
		if base.Kind == tir.Load && len(base.Children) == 1 {
			place, ok := unit.Node(base.Children[0])
			if !ok || place.Kind != tir.TuplePlace {
				return "", fmt.Errorf("tuple element base is not a tuple place")
			}
			return buildTuplePlaceRead(unit, snapshot, fileSet, place, locals, width, false)
		}
		if base.Kind == tir.SourceAlias && len(base.Children) == 1 {
			inner, ok := unit.Node(base.Children[0])
			if ok && inner.Kind == tir.Load && len(inner.Children) == 1 {
				place, ok := unit.Node(inner.Children[0])
				if ok && place.Kind == tir.TuplePlace && len(place.Children) == 1 {
					baseExpr, _, err := buildPlaceLValue(unit, snapshot, fileSet, place.Children[0], locals, width)
					if err != nil {
						return "", err
					}
					return fmt.Sprintf("%s._%d._%d", baseExpr, place.Ordinal, node.Ordinal), nil
				}
			}
		}
		if base.Kind != tir.SymbolValue {
			return "", fmt.Errorf("entry function body expression reads element %d of a %s, want a SymbolValue naming a tuple-typed local (indexing a tuple literal is not supported)", node.Ordinal, base.Kind)
		}
		return buildTupleElement(unit, snapshot, base.Symbol, node.Ordinal, locals, width, false)
	case tir.CheckedIndex:
		// A bare CheckedIndex in a pure scalar-expression position — a
		// slice-element read of a value with no addressable place
		// (`view()[0]` in a comparison, arithmetic, or call argument), whose
		// element type is the entry's width (the width gate above has already
		// required it). The base here can only be a pure projection safe to
		// reference twice — a SymbolValue naming a slice-typed local, or a
		// Load of a slice-typed place — since a freshly-computed base (a call
		// result) needs a temp-declaration statement this pure-expression
		// position has nowhere to place (the positions that CAN host a temp —
		// a print, a local declaration, a return — intercept the shape before
		// reaching this case and thread buildSliceIndexValue's pre). A str
		// index's result is char, never the entry's integer width, so a str
		// base cannot reach this case from real source; if one does (hand-built
		// IR) it is a clean rejection.
		strBase, err := checkedIndexBaseIsStr(unit, snapshot, node)
		if err != nil {
			return "", err
		}
		if strBase {
			return "", fmt.Errorf("entry function body expression contains a str index whose result type is %s, want %s", describeType(snapshot, node.Type), wantName(width))
		}
		pre, read, err := buildSliceIndexValue(unit, snapshot, fileSet, id, node, locals, width, false)
		if err != nil {
			return "", err
		}
		if pre != "" {
			base, ok := unit.Node(node.Children[0])
			if !ok {
				return "", fmt.Errorf("entry function body expression contains a CheckedIndex referencing invalid base node %d", node.Children[0])
			}
			return "", fmt.Errorf("entry function body expression indexes a %s of type %s in a pure-expression position with nowhere to place the temp-declaration statement the freshly-computed slice value needs; bind the slice into a local first", base.Kind, describeType(snapshot, base.Type))
		}
		return read, nil
	case tir.SourceAlias:
		if len(node.Children) == 1 {
			child, ok := unit.Node(node.Children[0])
			if ok && child.Kind == tir.TupleElementValue {
				return buildExpr(unit, snapshot, fileSet, node.Children[0], locals, width, entryWidth)
			}
			return buildExpr(unit, snapshot, fileSet, node.Children[0], locals, width, entryWidth)
		}
		return "", fmt.Errorf("entry function body expression contains a SourceAlias, which is not supported")
	case tir.DirectCall, tir.MethodCall:
		// A call to another Pebble-convention function whose result is the
		// entry's own width. The width gate above already
		// checked node.Type (the call's result type, which is the callee's
		// resolved result type) is the entry's width. The call itself is built
		// by buildDirectCall, the single call-building machinery shared with an
		// aggregate-typed call used as a matching local's declaration
		// initializer (buildAggregateCallInitializer) — context and argument
		// handling are identical there; only the result type differs from the
		// scalar case.
		return buildDirectCall(unit, snapshot, fileSet, node, locals, width)
	default:
		return "", fmt.Errorf("entry function body expression contains a %s, want an integer literal, a reference to a local declared earlier in the body, checked +, -, *, /, %% arithmetic, bitwise &, |, ^, ~, or a call to another function", node.Kind)
	}
}

// indirectCalleePlace unwraps an IndirectCall's callee child (Children[0])
// past any SourceAlias (grouped-expression parens) and a single Load, and
// reports whether the unwrapped node is specifically the built-in Allocator's
// alloc/realloc/free field — NOT merely "any FieldPlace/FieldValue," which is
// too broad: a function-typed STRUCT FIELD (e.g. `t.op` where `op fn(int,
// int) int;`, slice 2 of the function-types feature) produces the exact same
// FieldValue node shape a real allocator field access does (confirmed via a
// real fixture) — the two are distinguished only by checking the field's
// actual owner type and member symbol against the runtime's own
// AllocatorAlloc/AllocatorRealloc/AllocatorFree identities via
// runtimeFieldName, exactly like the pre-existing allocator-only code already
// did before this function existed. Both buildIndirectCall and
// collectFunctionTypesWalk call this so the two agree on which case any given
// IndirectCall is — confirmed via a real fixture that the checker sets
// IndirectCall.FunctionType on BOTH the allocator case and the general case
// (not just the general one, contrary to an earlier, unverified assumption),
// so FunctionType alone cannot distinguish them.
func indirectCalleePlace(unit *tir.Unit, node tir.Node) (placeNode tir.Node, isAllocator bool, ok bool) {
	if len(node.Children) < 1 {
		return tir.Node{}, false, false
	}
	calleeNode, found := unit.Node(node.Children[0])
	if !found {
		return tir.Node{}, false, false
	}
	placeNode = calleeNode
	for placeNode.Kind == tir.SourceAlias && len(placeNode.Children) == 1 {
		placeNode, _ = unit.Node(placeNode.Children[0])
	}
	if placeNode.Kind == tir.Load && len(placeNode.Children) == 1 {
		placeNode, _ = unit.Node(placeNode.Children[0])
	}
	var owner types.TypeID
	var member symbol.SymbolID
	switch {
	case placeNode.Kind == tir.FieldPlace && len(placeNode.Children) == 1:
		if base, baseOK := unit.Node(placeNode.Children[0]); baseOK {
			owner, member = base.Type, placeNode.Member
		}
	case placeNode.Kind == tir.FieldValue && len(placeNode.Children) == 1:
		if base, baseOK := unit.Node(placeNode.Children[0]); baseOK {
			owner, member = base.Type, placeNode.Member
		}
	}
	if owner != 0 && member != 0 {
		if _, mapped := runtimeFieldName(unit, owner, member); mapped {
			isAllocator = member == unit.Runtime().AllocatorAlloc || member == unit.Runtime().AllocatorRealloc || member == unit.Runtime().AllocatorFree
		}
	}
	return placeNode, isAllocator, true
}

func buildIndirectCall(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, node tir.Node, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind) (string, error) {
	if len(node.Children) < 1 || node.ContextAction != tir.ContextForward {
		return "", fmt.Errorf("indirect call has invalid callee or context action")
	}
	placeNode, allocatorCallee, ok := indirectCalleePlace(unit, node)
	if !ok {
		return "", fmt.Errorf("indirect call has invalid callee")
	}
	var base string
	var owner types.TypeID
	var member symbol.SymbolID
	if allocatorCallee && placeNode.Kind == tir.FieldPlace {
		var err error
		base, owner, err = buildPlaceLValue(unit, snapshot, fileSet, placeNode.Children[0], locals, width)
		if err != nil {
			return "", err
		}
		member = placeNode.Member
	} else if allocatorCallee && placeNode.Kind == tir.FieldValue && len(placeNode.Children) == 1 {
		receiver, ok := unit.Node(placeNode.Children[0])
		if !ok {
			return "", fmt.Errorf("invalid allocator receiver")
		}
		var err error
		base, err = buildRuntimeValueNode(unit, snapshot, fileSet, placeNode.Children[0], locals, width)
		if err != nil {
			return "", err
		}
		owner, member = receiver.Type, placeNode.Member
	}
	if !allocatorCallee {
		// The general indirect call: the callee is an ordinary function-typed
		// value, not an allocator function field. This is the f(1, 2) shape —
		// a call through a function-typed local (or a bare function value),
		// distinct from the allocator-specific FieldPlace/FieldValue shape
		// above. Both the callee and every argument are built under the
		// callee's own function type (buildFunctionIndirectCall), and the
		// allocator path above is completely untouched.
		return buildFunctionIndirectCall(unit, snapshot, fileSet, node, placeNode, locals, width)
	}
	field, mapped := runtimeFieldName(unit, owner, member)
	if !mapped || (member != unit.Runtime().AllocatorAlloc && member != unit.Runtime().AllocatorRealloc && member != unit.Runtime().AllocatorFree) {
		return "", fmt.Errorf("indirect call callee is not an allocator function field")
	}
	args := make([]string, 0, len(node.Children)-1)
	for _, id := range node.Children[1:] {
		arg, err := buildRuntimeCallArg(unit, snapshot, fileSet, id, locals, width)
		if err != nil {
			return "", err
		}
		args = append(args, arg)
	}
	cast := "PebbleFreeFn"
	if member == unit.Runtime().AllocatorAlloc {
		cast = "PebbleAllocFn"
	}
	if member == unit.Runtime().AllocatorRealloc {
		cast = "PebbleReallocFn"
	}
	if len(args) > 0 {
		args[0] = "(PebbleContext *)" + args[0]
	}
	return fmt.Sprintf("((%s)(%s.%s))(%s)", cast, base, field, strings.Join(args, ", ")), nil
}

// buildFunctionIndirectCall builds the C expression text for the GENERAL
// indirect call through an ordinary function-typed value — the f(1, 2) shape —
// where the callee (Children[0], already unwrapped past SourceAlias/Load by
// buildIndirectCall) is a function-typed value rather than an allocator
// function field, and every argument (Children[1:]) is built under the
// callee's own function type's parameter list, which REPLACES the
// tir.Node.Parameters an ordinary fixed function declaration would have. The
// callee's function type is the IndirectCall's FunctionType field (its own
// Type is the call's RESULT type, distinct from the callee's type — confirmed
// against a real fixture). The emitted C is
//
//	<callee-expr>(ctx, <arg0>, <arg1>, ...)
//
// for a Pebble-convention function type: the callee value's own C type IS
// pebble_fnptr_<typeID>_t (a function-typed local is declared as exactly that
// typedef), so calling it directly with the threaded context and its declared
// parameters is trivially valid C, no cast needed. Each argument's grammar is
// decided by buildCallArgument from its parameter's resolved type — the exact
// same per-parameter-type dispatch an ordinary fixed function's call uses.
// calleeNode is the unwrapped callee value node (a SymbolValue naming a
// function-typed local, or a HoistedFunctionValue), built by buildFunctionValue.
func buildFunctionIndirectCall(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, node, calleeNode tir.Node, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind) (string, error) {
	calleeExpr, err := buildFunctionValue(unit, snapshot, fileSet, calleeNode, locals, "entry function body indirect call", width)
	if err != nil {
		return "", err
	}
	fnType := node.FunctionType
	if !isFunctionType(snapshot, fnType) {
		return "", fmt.Errorf("indirect call has a callee of type %s, want a function type", describeType(snapshot, fnType))
	}
	if err := validateFunctionTypeSignature(snapshot, width, fnType); err != nil {
		return "", err
	}
	key, ok := snapshot.Key(fnType)
	if !ok {
		return "", fmt.Errorf("indirect call callee type %d is not in the type snapshot", fnType)
	}
	_, parameters, _, _, ok := key.Function()
	if !ok {
		return "", fmt.Errorf("indirect call callee type %s is not a function type", describeType(snapshot, fnType))
	}
	if len(node.Children)-1 != len(parameters) {
		return "", fmt.Errorf("indirect call passes %d argument(s), want %d (the callee's function type %s declares %d parameter(s))", len(node.Children)-1, len(parameters), describeType(snapshot, fnType), len(parameters))
	}
	args := make([]string, 0, len(parameters))
	// A C-convention indirect callee (an extern function referenced as a
	// value, or a C-convention function pointer) declares its str parameters
	// against the libc header, so a str argument must be lowered to
	// `const char *` exactly like a direct C-convention call's (see
	// buildCallArguments); the callee's own convention decides.
	convention, _, _, _, _ := key.Function()
	for i, id := range node.Children[1:] {
		pre, arg, err := buildCallArgument(unit, snapshot, fileSet, node.Symbol, i, id, tir.Parameter{Type: parameters[i]}, convention == types.C, locals, width)
		if err != nil {
			return "", err
		}
		if pre != "" {
			// An indirect call is a pure expression position (it may appear
			// anywhere a function value's result is consumed), so an inline
			// slice-construction argument's temp declaration has nowhere to
			// go; reject cleanly rather than drop it.
			return "", fmt.Errorf("entry function body indirect call argument %d is an inline slice construction, which is not supported as an indirect call argument: a C expression has nowhere to place the temp-declaration statement the slice construction needs; bind the slice into a local first", i)
		}
		args = append(args, arg)
	}
	if len(args) == 0 {
		return fmt.Sprintf("%s(ctx)", calleeExpr), nil
	}
	return fmt.Sprintf("%s(ctx, %s)", calleeExpr, strings.Join(args, ", ")), nil
}

// buildFunctionValue builds the C expression text for one function-typed VALUE:
// a reference to an in-scope function-typed local (a SymbolValue, emitted as
// pebble_local_<symbol>, whose C type is the local's own pebble_fnptr_<typeID>_t)
// or a bare top-level function reference (a HoistedFunctionValue, emitted as
// the referenced function's own C name pebble_fn_<symbolID> — a bare C function
// name naturally decays to a function pointer of the exact fnptr typedef type,
// so no cast is needed at a declaration site). A SourceAlias (grouped-expression
// parens) is transparently unwrapped. It is the single builder shared by the
// three positions a function value can appear in: a function-typed local's
// declaration initializer (buildFunctionLocalDeclaration), a function-typed
// local's reassignment (buildStoreCore), and the general indirect call's callee
// (buildFunctionIndirectCall, also reachable through buildExpr's function-type
// bypass for a fn-typed node used as a value). Any other shape is a clean
// rejection naming what was found.
func buildFunctionValue(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, node tir.Node, locals map[symbol.SymbolID]localInfo, context string, width types.BuiltinKind) (string, error) {
	switch node.Kind {
	case tir.SymbolValue:
		info, declared := locals[node.Symbol]
		if !declared || info.functionType == 0 {
			return "", fmt.Errorf("%s references symbol %d, which is not a function-typed local declared earlier in the body", context, node.Symbol)
		}
		return fmt.Sprintf("pebble_local_%d", node.Symbol), nil
	case tir.HoistedFunctionValue:
		decl, err := findFunctionDeclaration(unit, node.Symbol, "function value")
		if err != nil {
			return "", err
		}
		return helperCName(decl), nil
	case tir.GenericFunctionValue:
		// A generic function referenced as a first-class value
		// (`var f fn(int) int = identity[int];`, function-types slice 3):
		// the same resolution a generic function CALL uses
		// (findCalledFunctionDeclaration with the value's own TypeArgs —
		// node.Symbol is the generic function's declaration symbol and
		// node.TypeArgs the concrete specialization's type arguments), and the
		// specialized declaration's own C name emitted exactly like a
		// HoistedFunctionValue's — a bare C function name that decays to the
		// exact fnptr typedef type, no cast needed. The specialization is
		// discovered as reachable by the same collectDirectCalls case added
		// for this node kind, so its pebble_fn_<symbol>_<function> definition
		// is always emitted.
		decl, err := findCalledFunctionDeclaration(unit, node.Symbol, node.TypeArgs)
		if err != nil {
			return "", err
		}
		return helperCName(decl), nil
	case tir.DirectCall:
		// A call to a function-returning helper used as a function-typed value
		// (`var f fn(int, int) int = chooseOp();`, `return chooseOp();`,
		// `apply(chooseOp(), 1, 2)`, `f = chooseOp();`, or a struct field's
		// construction value `Table.{ op = chooseOp() }` — all confirmed
		// checker-reachable, all the same DirectCall-with-a-function-result
		// node whose Type is the function type): built by buildDirectCall, the
		// same call builder every other direct call uses. The emitted C is
		// `pebble_fn_<callee>(ctx, ...)` — a C function call whose return
		// type IS the callee's declared fnptr typedef, so the expression is a
		// function pointer of the exact value's C type, no cast needed.
		return buildDirectCall(unit, snapshot, fileSet, node, locals, width)
	case tir.SourceAlias:
		if len(node.Children) != 1 {
			return "", fmt.Errorf("%s contains a SourceAlias with %d child(ren), want exactly one", context, len(node.Children))
		}
		child, ok := unit.Node(node.Children[0])
		if !ok {
			return "", fmt.Errorf("%s contains a SourceAlias referencing invalid node %d", context, node.Children[0])
		}
		return buildFunctionValue(unit, snapshot, fileSet, child, locals, context, width)
	case tir.FieldValue:
		// A function-typed struct field read (`t.op`, function-types slice 2;
		// `self.hash_fn`/`self.eq_fn`, the std/hmap.peb insert/get shapes):
		// the receiver is a struct-typed local in scope (a SymbolValue),
		// emitted as its own pebble_local_<symbol> C name, and the field is
		// read as pebble_field_<member> — the exact same designated-field-name
		// convention every other struct field this backend emits already uses
		// (see buildStructBraceList's ".pebble_field_%d" inits and
		// buildFieldPlaceRead's identical trailing access), just reached
		// through buildFunctionValue instead of the width/bool-only field
		// -read path (buildFieldPlaceRead), since a function-typed field's
		// value isn't a scalar. A pointer-to-struct receiver (`self.hash_fn`
		// where self is a `*HashMap` method parameter) is supported too: the
		// C projection uses `->` instead of `.`, exactly as buildPlaceLValue's
		// FieldPlace case and buildStructFieldRead resolve a pointer receiver.
		// Anything else is a clean rejection naming what was found.
		if len(node.Children) != 1 {
			return "", fmt.Errorf("%s contains a FieldValue with %d child(ren), want exactly one (the struct receiver)", context, len(node.Children))
		}
		receiver, ok := unit.Node(node.Children[0])
		if !ok {
			return "", fmt.Errorf("%s contains a FieldValue referencing invalid receiver node %d", context, node.Children[0])
		}
		if receiver.Kind != tir.SymbolValue {
			return "", fmt.Errorf("%s reads a function-typed field from a %s receiver, want a reference to a struct-typed local declared earlier in the body", context, receiver.Kind)
		}
		info, declared := locals[receiver.Symbol]
		if !declared {
			return "", fmt.Errorf("%s reads a function-typed field from symbol %d, which is not a local declared earlier in the body", context, receiver.Symbol)
		}
		access := "."
		if info.structType == 0 && info.runtimeType == 0 {
			// A pointer-to-struct receiver: the pointer's pointee must be a
			// struct type (info.pointerType records the pointer type, and
			// pointerPointeeType extracts its pointee), and the field read
			// uses the `->` access the C pointer gives.
			if _, ok := pointerPointeeType(snapshot, info.pointerType); !ok {
				return "", fmt.Errorf("%s reads a function-typed field from symbol %d, which is not a struct-typed local declared earlier in the body", context, receiver.Symbol)
			}
			access = "->"
		}
		return fmt.Sprintf("pebble_local_%d%spebble_field_%d", receiver.Symbol, access, node.Member), nil
	case tir.Load:
		// A function-typed field read used as an rvalue in a position other
		// than an indirect call's direct callee (e.g. a local declaration's
		// initializer, `var f fn(int, int) int = t.op;`) is wrapped in a
		// Load node by the checker (confirmed via a real fixture: Load's
		// single child here is a FieldPlace, not the bare FieldValue the
		// direct-callee position produces) — transparently unwrapped to its
		// single child.
		if len(node.Children) != 1 {
			return "", fmt.Errorf("%s contains a Load with %d child(ren), want exactly one", context, len(node.Children))
		}
		child, ok := unit.Node(node.Children[0])
		if !ok {
			return "", fmt.Errorf("%s contains a Load referencing invalid node %d", context, node.Children[0])
		}
		return buildFunctionValue(unit, snapshot, fileSet, child, locals, context, width)
	case tir.FieldPlace:
		// The lvalue-place form of a function-typed struct field read (the
		// shape a Load's child takes, as opposed to FieldValue's rvalue
		// form) — the receiver is a StoragePlace naming a struct-typed local
		// in scope, and the field is read the same
		// pebble_local_<symbol>.pebble_field_<member> way FieldValue's case
		// does. A pointer-to-struct receiver (`self.hash_fn` where self is a
		// `*HashMap` method parameter) is supported too, using `->` access,
		// exactly as the FieldValue case resolves it.
		if len(node.Children) != 1 {
			return "", fmt.Errorf("%s contains a FieldPlace with %d child(ren), want exactly one (the struct receiver)", context, len(node.Children))
		}
		receiverPlace, ok := unit.Node(node.Children[0])
		if !ok {
			return "", fmt.Errorf("%s contains a FieldPlace referencing invalid receiver node %d", context, node.Children[0])
		}
		if receiverPlace.Kind != tir.StoragePlace {
			return "", fmt.Errorf("%s reads a function-typed field from a %s receiver, want a reference to a struct-typed local declared earlier in the body", context, receiverPlace.Kind)
		}
		info, declared := locals[receiverPlace.Symbol]
		if !declared {
			return "", fmt.Errorf("%s reads a function-typed field from symbol %d, which is not a local declared earlier in the body", context, receiverPlace.Symbol)
		}
		access := "."
		if info.structType == 0 && info.runtimeType == 0 {
			if _, ok := pointerPointeeType(snapshot, info.pointerType); !ok {
				return "", fmt.Errorf("%s reads a function-typed field from symbol %d, which is not a struct-typed local declared earlier in the body", context, receiverPlace.Symbol)
			}
			access = "->"
		}
		return fmt.Sprintf("pebble_local_%d%spebble_field_%d", receiverPlace.Symbol, access, node.Member), nil
	default:
		return "", fmt.Errorf("%s contains a %s, want a reference to a function-typed local or a bare function value", context, node.Kind)
	}
}

func buildRuntimeCallArg(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, id tir.NodeID, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind) (string, error) {
	node, ok := unit.Node(id)
	if !ok {
		return "", fmt.Errorf("invalid indirect-call argument")
	}
	if node.Kind == tir.IntegerLiteral {
		litWidth, _ := resolvedBuiltin(snapshot, node.Type)
		return integerLiteralText(node.Literal.IntegerNum, litWidth), nil
	}
	if isUint(snapshot, node.Type) {
		return buildUintExpr(unit, snapshot, fileSet, id, locals, width)
	}
	return buildExpr(unit, snapshot, fileSet, id, locals, width, width)
}

// buildDirectCall builds the C expression text for one tir.DirectCall: a call
// to another Pebble-convention function emitted as
// pebble_fn_<calleeSymbolID>(ctx, <arg0>, <arg1>, ...). Context threading is
// not an explicit IR child — the DirectCall records it as ContextAction
// (ContextForward for a Pebble-convention call) — so, exactly as the old
// backend textually injected `context`, this backend prepends ctx as the first
// C argument itself, the same way pebble_user_main receives it. Each argument
// is built by buildCallArguments, which decides each child's grammar from the
// callee's declared parameter type (the reachability walk has already resolved
// and validated the callee, so the checks here are defense against hand-built
// IR, matching the file's style). The function is shared (10.26) by the two
// call-building sites whose result type differs: buildExpr's DirectCall case
// (a scalar-width call) and buildAggregateCallInitializer (a tuple/struct-
// returning call used as a matching local's declaration initializer) — the
// context and argument handling are identical; only the call's result type
// differs, and that is decided by the caller, never here.
//
// This wrapper is the EXPRESSION-position entry point every call site not
// itself in a leading-statement position uses (19 call sites). It delegates to
// buildDirectCallWithPre and rejects a non-empty pre: an inline slice
// construction passed as a call argument (f(a[1:3])) needs a temp-declaration
// statement before the call, which a pure C expression position has nowhere to
// place, so in such a position the call is a clean rejection naming what was
// found (the checker has already coerced the argument, so this only fires for
// real source — confirmed via the return f(a[1:3]) fixture).
func buildDirectCall(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, node tir.Node, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind) (string, error) {
	pre, expr, err := buildDirectCallWithPre(unit, snapshot, fileSet, node, locals, width)
	if err != nil {
		return "", err
	}
	if pre != "" {
		return "", fmt.Errorf("entry function body expression contains a call to symbol %d whose argument is an inline slice construction (a CheckedSlice), which is not supported in this expression position: a C function argument is a pure expression position with nowhere to place the temp-declaration statement the slice construction needs; bind the slice into a local first", node.Symbol)
	}
	return expr, nil
}

// buildDirectCallWithPre is the leading-statement-position twin of
// buildDirectCall: it builds the identical call expression but ALSO returns an
// indent-free pre-statement that the caller must emit BEFORE the call whenever
// an argument is an inline slice construction (f(a[1:3])) — the
// temp-declaration statement the slice construction needs (the same
// two-statement temp-then-construction shape 10.37's slice local declaration
// uses). Only the leading-statement call sites call this — a bare call
// statement (buildExpressionStatement) and each local's declaration-initializer
// builder — positions that already have a natural place for a preceding
// statement, mirroring how buildScalarInitializeCore's pre mechanism threads a
// force-unwrap temp. Every other call site keeps calling buildDirectCall, which
// rejects a non-empty pre. The returned pre has no indent; the caller prepends
// its own.
func buildDirectCallWithPre(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, node tir.Node, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind) (string, string, error) {
	// A C-convention call (direct call to an extern fn declaration) is
	// lowered differently from a Pebble-convention call: no context parameter
	// is threaded, the callee is called by its real C name (malloc, not
	// pebble_fn_<symbolID>), and the result is the function's own C return
	// type (which the consuming expression — a cast, assignment, or
	// discarded-statement — already matches).
	if node.Convention == types.C {
		if node.ContextAction != tir.ContextNone {
			return "", "", fmt.Errorf("entry function body expression contains a C-convention call that records ContextAction %s, want NoContext", node.ContextAction)
		}
		var calleeDecl tir.Node
		var err error
		if len(node.TypeArgs) != 0 {
			calleeDecl, err = findCalledFunctionDeclaration(unit, node.Symbol, node.TypeArgs)
		} else {
			calleeDecl, err = findFunctionDeclaration(unit, node.Symbol, "called function")
			if err != nil {
				calleeDecl, err = findCalledFunctionByResult(unit, node.Symbol, node.Type)
			}
		}
		if err != nil {
			if len(node.TypeArgs) != 0 {
				return "", "", fmt.Errorf("entry function body expression contains a generic call with no matching specialization")
			}
			return "", "", err
		}
		if calleeDecl.Kind != tir.ExternDeclaration {
			return "", "", fmt.Errorf("entry function body expression contains a C-convention call to symbol %d, which is not an extern declaration", calleeDecl.Symbol)
		}
		calleeName, err := externCName(calleeDecl)
		if err != nil {
			return "", "", err
		}
		callPre, callArgs, err := buildCallArguments(unit, snapshot, fileSet, node, calleeDecl, locals, width)
		if err != nil {
			return "", "", err
		}
		if callArgs == "" {
			return callPre, fmt.Sprintf("%s()", calleeName), nil
		}
		return callPre, fmt.Sprintf("%s(%s)", calleeName, callArgs), nil
	}
	if node.Convention != types.Pebble {
		return "", "", fmt.Errorf("entry function body expression contains a call using the %s calling convention, want Pebble", callingConventionName(node.Convention))
	}
	if node.ContextAction != tir.ContextForward {
		return "", "", fmt.Errorf("entry function body expression contains a call that records ContextAction %s, want ForwardCurrentContext (this backend only lowers Pebble-convention calls that thread the context)", node.ContextAction)
	}
	// The callee's own declaration supplies the parameter list that decides
	// each argument's grammar below (the reachability walk in
	// discoverReachableHelpers has already resolved and validated this
	// callee, so the checks here are defense against hand-built IR,
	// matching the file's style).
	var calleeDecl tir.Node
	var err error
	if len(node.TypeArgs) != 0 {
		calleeDecl, err = findCalledFunctionDeclaration(unit, node.Symbol, node.TypeArgs)
	} else {
		calleeDecl, err = findFunctionDeclaration(unit, node.Symbol, "called function")
		if err != nil {
			calleeDecl, err = findCalledFunctionByResult(unit, node.Symbol, node.Type)
		}
	}
	if err != nil {
		if len(node.TypeArgs) != 0 {
			return "", "", fmt.Errorf("entry function body expression contains a generic call with no matching specialization")
		}
		return "", "", err
	}
	callPre, callArgs, err := buildCallArguments(unit, snapshot, fileSet, node, calleeDecl, locals, width)
	if err != nil {
		return "", "", err
	}
	calleeName := helperCName(calleeDecl)
	if callArgs == "" {
		return callPre, fmt.Sprintf("%s(ctx)", calleeName), nil
	}
	return callPre, fmt.Sprintf("%s(ctx, %s)", calleeName, callArgs), nil
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

// buildTuplePlaceRead builds the C text for reading one element of a tuple
// local through the Load(TuplePlace) shape the checker actually produces for
// `t.<ordinal>` (confirmed against a real fixture): the TuplePlace carries the
// element Ordinal and its single child is the StoragePlace naming the tuple
// local. wantBool selects which grammar the element must satisfy — bool (the
// buildBoolExpr path) or the entry's width (the buildExpr path) — matching how
// the Load's own Type was already gated by the caller's builder. The emitted C
// is pebble_local_<symbol>._<ordinal>.
func buildTuplePlaceRead(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, place tir.Node, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind, wantBool bool) (string, error) {
	if len(place.Children) != 1 {
		return "", fmt.Errorf("tuple place wants one base")
	}
	expr, typ, err := buildPlaceLValue(unit, snapshot, fileSet, place.Children[0], locals, width)
	if err != nil {
		return "", err
	}
	key, ok := snapshot.Key(typ)
	if !ok {
		return "", fmt.Errorf("tuple place type %d is not in the type snapshot", typ)
	}
	elements, ok := key.Elements()
	if !ok || place.Ordinal >= uint32(len(elements)) {
		return "", fmt.Errorf("tuple element %d is out of range", place.Ordinal)
	}
	elem := elements[place.Ordinal]
	if wantBool && !isBool(snapshot, elem) {
		return "", fmt.Errorf("tuple element %d is not bool", place.Ordinal)
	}
	if !wantBool && !isWidth(snapshot, width, elem) {
		return "", fmt.Errorf("tuple element %d is not %s", place.Ordinal, wantName(width))
	}
	return fmt.Sprintf("%s._%d", expr, place.Ordinal), nil
}

// buildArrayPlaceRead lowers Load(CheckedIndexPlace) for an array or slice
// local. The index is built as an integer expression and checked with the
// runtime helper selected by the ENTRY width (width — the only width with a
// checked-index primitive) before it is used as the C subscript. For a slice
// base, the subscript uses .data and .len instead of the base array directly.
// The element value grammar is decided by the element's own type: bool for the
// buildBoolExpr path (wantBool), char, or any fixed-width integer at its own
// resolved width — the caller (buildExpr/buildBoolExpr/buildCharOperand) has
// already gated the Load's type against its consuming grammar, so this element
// check is defense for hand-built IR rather than a re-gate on the entry width.
func buildArrayPlaceRead(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, place tir.Node, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind, wantBool bool) (string, error) {
	if len(place.Children) != 2 {
		return "", fmt.Errorf("CheckedIndexPlace wants two children")
	}
	baseExpr, arrayType, err := buildPlaceLValue(unit, snapshot, fileSet, place.Children[0], locals, width)
	if err != nil {
		return "", err
	}
	// Check if the base is a slice-typed local.
	baseNode, ok := unit.Node(place.Children[0])
	if ok && (baseNode.Kind == tir.StoragePlace || isSlice(snapshot, arrayType)) {
		if info, declared := locals[baseNode.Symbol]; isSlice(snapshot, arrayType) || (declared && info.sliceType != 0) {
			sliceType := arrayType
			if sliceType == 0 {
				sliceType = info.sliceType
			}
			// Slice-typed base: use .data[checked_index(idx, (width_type).len)].
			sliceKey, ok := snapshot.Key(sliceType)
			if !ok {
				return "", fmt.Errorf("slice type %d is not in the type snapshot", sliceType)
			}
			element, ok := sliceKey.Child()
			if !ok {
				return "", fmt.Errorf("slice type %s has no element type", describeType(snapshot, info.sliceType))
			}
			if wantBool {
				if !isBool(snapshot, element) {
					return "", fmt.Errorf("slice element type is %s, want bool", describeType(snapshot, element))
				}
			} else if !isSupportedSliceElementType(unit, snapshot, element) {
				return "", fmt.Errorf("slice element type is %s, want a fixed-width integer, char, bool, tuple, optional, or struct", describeType(snapshot, element))
			}
			indexNode, ok := unit.Node(place.Children[1])
			if !ok {
				return "", fmt.Errorf("slice index references invalid node %d", place.Children[1])
			}
			var index string
			if indexNode.Kind == tir.IntegerLiteral && indexNode.Type == snapshot.Builtins().Int {
				if !isNonNegativeDecimal(indexNode.Literal.IntegerNum) {
					return "", fmt.Errorf("slice index contains an integer literal with malformed text %q", indexNode.Literal.IntegerNum)
				}
				index = indexNode.Literal.IntegerNum
			} else if indexNode.Kind == tir.SymbolValue && indexNode.Type == snapshot.Builtins().Int {
				if _, declared := locals[indexNode.Symbol]; !declared {
					return "", fmt.Errorf("slice index references symbol %d, which is not a local in scope", indexNode.Symbol)
				}
				index = fmt.Sprintf("pebble_local_%d", indexNode.Symbol)
			} else if isUint(snapshot, indexNode.Type) {
				// A uint-typed slice index (the loop iterator from a
				// `loop 0..new_cap : i { ... }` whose bounds the checker
				// anchored to uint, or a uint-typed local used as an index):
				// built by the dedicated uint grammar, not the general
				// buildExpr path which rejects non-entry-width types.
				index, err = buildUintExpr(unit, snapshot, fileSet, place.Children[1], locals, width)
				if err != nil {
					return "", fmt.Errorf("slice index: %v", err)
				}
			} else {
				index, err = buildExpr(unit, snapshot, fileSet, place.Children[1], locals, width, width)
				if err != nil {
					return "", fmt.Errorf("slice index: %v", err)
				}
			}
			return fmt.Sprintf("%s.data[pebble_rt_checked_index_%s(%s, (%s)%s.len, %s)]", baseExpr, checkedSuffix(width), index, cType(width), baseExpr, buildSourceLoc(fileSet, place.Span)), nil
		}
	}
	// Array-typed base: original path.
	key, ok := snapshot.Key(arrayType)
	if !ok {
		return "", fmt.Errorf("array type %d is not in the type snapshot", arrayType)
	}
	length, element, ok := key.Array()
	if !ok {
		return "", fmt.Errorf("checked index base is not an array")
	}
	if _, err := arrayLengthLiteral(length, width); err != nil {
		return "", err
	}
	if wantBool {
		if !isBool(snapshot, element) {
			return "", fmt.Errorf("array element type is %s, want bool", describeType(snapshot, element))
		}
	} else if !isSupportedSliceElementType(unit, snapshot, element) {
		return "", fmt.Errorf("array element type is %s, want a fixed-width integer, char, bool, tuple, optional, or struct", describeType(snapshot, element))
	}
	indexNode, ok := unit.Node(place.Children[1])
	if !ok {
		return "", fmt.Errorf("array index references invalid node %d", place.Children[1])
	}
	var index string
	if indexNode.Kind == tir.IntegerLiteral && indexNode.Type == snapshot.Builtins().Int {
		if !isNonNegativeDecimal(indexNode.Literal.IntegerNum) {
			return "", fmt.Errorf("array index contains an integer literal with malformed text %q", indexNode.Literal.IntegerNum)
		}
		index = indexNode.Literal.IntegerNum
	} else if indexNode.Kind == tir.SymbolValue && indexNode.Type == snapshot.Builtins().Int {
		// An int-typed SymbolValue index can only be a range loop's iterator
		// referenced from inside its own body when the iterator is never used
		// in a width-anchoring position (the same unanchored-int case
		// buildComparisonOperand handles), and the iterator is always declared
		// in C at the entry's width, so its name is the correct C lvalue for
		// the subscript.
		if _, declared := locals[indexNode.Symbol]; !declared {
			return "", fmt.Errorf("array index references symbol %d, which is not a local in scope", indexNode.Symbol)
		}
		index = fmt.Sprintf("pebble_local_%d", indexNode.Symbol)
	} else if isUint(snapshot, indexNode.Type) {
		// A uint-typed array index (a uint-typed local or loop iterator):
		// built by the dedicated uint grammar, not the general buildExpr
		// path which rejects non-entry-width types.
		index, err = buildUintExpr(unit, snapshot, fileSet, place.Children[1], locals, width)
		if err != nil {
			return "", fmt.Errorf("array index: %v", err)
		}
	} else {
		var err error
		index, err = buildExpr(unit, snapshot, fileSet, place.Children[1], locals, width, width)
		if err != nil {
			return "", fmt.Errorf("array index: %v", err)
		}
	}
	literal, _ := arrayLengthLiteral(length, width)
	return fmt.Sprintf("%s[pebble_rt_checked_index_%s(%s, %s, %s)]", baseExpr, checkedSuffix(width), index, literal, buildSourceLoc(fileSet, place.Span)), nil
}

// buildSliceIndexValue builds the C text for a bare tir.CheckedIndex whose
// base is a slice-typed VALUE with no addressable place — the checker lowers
// foo()[i] (indexing a call's slice result directly) to a bare CheckedIndex
// because a call result has no place to address, exactly the reason str
// indexing lowers to a bare CheckedIndex rather than Load(CheckedIndexPlace),
// and indexing a method call's slice result inline inside a print statement is
// an ordinary operation real code hits. Unlike str indexing — whose element
// read is a stateless UTF-8 decode function, pebble_rt_str_char_at_<suffix>,
// safely callable on the base expression directly even repeatedly — a
// slice/array element read needs the base's .data pointer AND its .len (for
// the bounds check), so a freshly-computed base (a call result) must be
// materialized ONCE into a temp local before the read: evaluating the base
// twice (once for .len, once for .data) would run the underlying call twice —
// wrong (side effects run twice) and wasteful. The returned (pre, expr) pair
// follows the two-statement shape buildSliceConstruction established for
// exactly this "evaluate an expression once, then use its pieces multiple
// times" problem: pre is a leading temp-declaration statement the caller must
// place in its enclosing statement sequence before the expression (empty when
// the base is a pure projection safe to reference twice — a SymbolValue
// naming a slice-typed local, or a Load of a slice-typed place, both
// side-effect-free lvalues, the same reasoning the slice-of-slice re-slicing
// fix used), expr is the indexed element read.
//
// Exactly four base shapes are accepted:
//
//   - a SymbolValue naming a slice-typed local in scope — emitted as the
//     local's own pebble_local_<symbol> C name, no temp (a pure projection);
//   - a Load of a slice-typed place (a slice-typed struct field read off an
//     addressable receiver) — emitted as the place's own lvalue
//     (buildPlaceLValue), no temp;
//   - a DirectCall/MethodCall whose result type is a slice — built ONCE via
//     buildDirectCallWithPre into a temp local of the slice's own C type, then
//     read off the temp;
//   - a FieldValue reading a slice-typed field off a call result
//     (`make_bag().data[i]`) — the call receiver is built once and the field
//     read <receiver>.pebble_field_<member> materialized into the same temp.
//
// Anything else that resolves to a slice/array type (an array literal, an
// array-typed call result) is a clean rejection naming what was found — such a
// base is not buildable by buildExpr, so there is no way to materialize it
// without inventing a new array-temp lowering. The read is emitted as the same
// real runtime bounds check every other indexing path in this backend
// performs: <base>.data[pebble_rt_checked_index_<suffix>(<index>, (<cType>)
// <base>.len, <loc>)].
//
// The result's type is the slice's element type (node.Type), which the caller
// has already validated is what its context needs; wantBool selects the bool
// grammar exactly as buildArrayPlaceRead's wantBool does (a bool element is
// required), and any other element must satisfy isSupportedSliceElementType —
// the same restriction buildArrayPlaceRead and buildSliceConstruction enforce,
// never a new one. The index is built by the exact four-shape dispatch
// buildArrayPlaceRead and the CheckedIndexPlace path use (see
// buildSliceIndexOperand). id is the CheckedIndex node's own NodeID, used to
// name the temp (pebble_slice_index_<id>) so it can never collide with any
// other temp this backend emits. width is the entry's resolved integer width,
// used for the checked-index helper's suffix, the .len width cast, and the
// index's own width.
func buildSliceIndexValue(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, id tir.NodeID, node tir.Node, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind, wantBool bool) (string, string, error) {
	if len(node.Children) != 2 {
		return "", "", fmt.Errorf("entry function body expression contains a CheckedIndex with %d child(ren), want exactly two (the slice value being indexed and the index)", len(node.Children))
	}
	baseNode, ok := unit.Node(node.Children[0])
	if !ok {
		return "", "", fmt.Errorf("entry function body expression contains a CheckedIndex referencing invalid base node %d", node.Children[0])
	}
	indexNode, ok := unit.Node(node.Children[1])
	if !ok {
		return "", "", fmt.Errorf("entry function body expression contains a CheckedIndex referencing invalid index node %d", node.Children[1])
	}
	// Unwrap grouped-expression parens on the base (a SourceAlias): the
	// grouped base carries the same Type the SourceAlias did, so the dispatch
	// below is exactly what the checker validated.
	for baseNode.Kind == tir.SourceAlias {
		if len(baseNode.Children) != 1 {
			return "", "", fmt.Errorf("entry function body expression contains a CheckedIndex whose base SourceAlias has %d child(ren), want exactly one", len(baseNode.Children))
		}
		baseNode, ok = unit.Node(baseNode.Children[0])
		if !ok {
			return "", "", fmt.Errorf("entry function body expression contains a CheckedIndex referencing invalid base node %d", baseNode.Children[0])
		}
	}
	// Resolve the base's slice type and its C expression, plus the leading
	// temp-declaration statement a freshly-computed base needs.
	var sliceType types.TypeID
	var baseExpr, pre string
	switch baseNode.Kind {
	case tir.SymbolValue:
		info, declared := locals[baseNode.Symbol]
		if !declared || info.sliceType == 0 {
			return "", "", fmt.Errorf("entry function body expression indexes symbol %d, which is not a slice-typed local declared earlier in the body", baseNode.Symbol)
		}
		sliceType = info.sliceType
		baseExpr = fmt.Sprintf("pebble_local_%d", baseNode.Symbol)
	case tir.Load:
		if len(baseNode.Children) != 1 {
			return "", "", fmt.Errorf("entry function body expression contains a CheckedIndex whose base Load has %d child(ren), want exactly one place", len(baseNode.Children))
		}
		lvalue, placeType, err := buildPlaceLValue(unit, snapshot, fileSet, baseNode.Children[0], locals, width)
		if err != nil {
			return "", "", fmt.Errorf("entry function body expression slice-index base read: %v", err)
		}
		if !isSlice(snapshot, placeType) {
			return "", "", fmt.Errorf("entry function body expression indexes a Load of a place of type %s, want a slice-typed place", describeType(snapshot, placeType))
		}
		sliceType = placeType
		baseExpr = lvalue
	case tir.DirectCall, tir.MethodCall:
		// A slice-typed call result indexed directly (`view()[0]`,
		// `b.view()[1]`): the call is built ONCE into a temp local holding the
		// slice VALUE, and the bounds check plus element read run off the
		// temp's own .data/.len. The temp name derives from the CheckedIndex
		// node's own NodeID — the only stable identity in hand here (a bare
		// index has no local symbol to name it from), distinct from the
		// pebble_slice_start_<symbol> and pebble_slice_ret_<nodeID> temps so
		// the three can never collide.
		if !isSlice(snapshot, baseNode.Type) {
			return "", "", fmt.Errorf("entry function body expression indexes a %s whose result type is %s, want a slice-typed call result", baseNode.Kind, describeType(snapshot, baseNode.Type))
		}
		sliceType = baseNode.Type
		callPre, callExpr, err := buildDirectCallWithPre(unit, snapshot, fileSet, baseNode, locals, width)
		if err != nil {
			return "", "", err
		}
		tempName := fmt.Sprintf("pebble_slice_index_%d", id)
		pre = fmt.Sprintf("%s %s = %s;", sliceTypeName(sliceType), tempName, callExpr)
		if callPre != "" {
			pre = callPre + "\n" + pre
		}
		baseExpr = tempName
	case tir.FieldValue:
		// A slice-typed struct field read off a call result
		// (`make_bag().data[i]`): the FieldValue's single child is the struct
		// receiver, and the field is read as <receiver>.pebble_field_<member> —
		// the same designated-field naming every other struct field this
		// backend emits uses (see buildStructFieldRead). The only supported
		// receiver is a DirectCall/MethodCall (the sole checker-reachable way
		// to obtain a non-addressable struct value; a struct LOCAL's field
		// indexes as a place and never reaches a bare CheckedIndex); the field
		// read off the call result is materialized into a temp because the
		// call would otherwise run twice (once for .len, once for .data).
		if !isSlice(snapshot, baseNode.Type) {
			return "", "", fmt.Errorf("entry function body expression indexes a FieldValue of type %s, want a slice-typed struct field", describeType(snapshot, baseNode.Type))
		}
		if len(baseNode.Children) != 1 {
			return "", "", fmt.Errorf("entry function body expression contains a CheckedIndex whose base FieldValue has %d child(ren), want exactly one (the struct receiver)", len(baseNode.Children))
		}
		receiver, ok := unit.Node(baseNode.Children[0])
		if !ok {
			return "", "", fmt.Errorf("entry function body expression contains a CheckedIndex referencing invalid receiver node %d", baseNode.Children[0])
		}
		if receiver.Kind != tir.DirectCall && receiver.Kind != tir.MethodCall {
			return "", "", fmt.Errorf("entry function body expression indexes a slice-typed struct field of a %s receiver, want a call result (a non-addressable struct value only arises from a call; a struct local's field indexes as a place)", receiver.Kind)
		}
		sliceType = baseNode.Type
		callPre, callExpr, err := buildDirectCallWithPre(unit, snapshot, fileSet, receiver, locals, width)
		if err != nil {
			return "", "", err
		}
		tempName := fmt.Sprintf("pebble_slice_index_%d", id)
		pre = fmt.Sprintf("%s %s = (%s).pebble_field_%d;", sliceTypeName(sliceType), tempName, callExpr, baseNode.Member)
		if callPre != "" {
			pre = callPre + "\n" + pre
		}
		baseExpr = tempName
	default:
		return "", "", fmt.Errorf("entry function body expression indexes a %s of type %s, want a slice-typed value (a slice-typed local, a slice-typed place, a call returning a slice, or a slice-typed field of a call result); indexing an array literal or array-typed call result directly is not lowered", baseNode.Kind, describeType(snapshot, baseNode.Type))
	}
	sliceKey, ok := snapshot.Key(sliceType)
	if !ok {
		return "", "", fmt.Errorf("entry function body expression indexes a slice value whose type %d is not in the type snapshot", sliceType)
	}
	element, ok := sliceKey.Child()
	if !ok {
		return "", "", fmt.Errorf("entry function body expression indexes a slice value of type %s, which has no element type", describeType(snapshot, sliceType))
	}
	if wantBool {
		if !isBool(snapshot, element) {
			return "", "", fmt.Errorf("entry function body expression indexes a slice whose element type is %s, want bool", describeType(snapshot, element))
		}
	} else if !isSupportedSliceElementType(unit, snapshot, element) {
		return "", "", fmt.Errorf("entry function body expression indexes a slice whose element type is %s, want a fixed-width integer, char, bool, tuple, optional, or struct", describeType(snapshot, element))
	}
	index, err := buildSliceIndexOperand(unit, snapshot, fileSet, node.Children[1], indexNode, locals, width)
	if err != nil {
		return "", "", err
	}
	read := fmt.Sprintf("%s.data[pebble_rt_checked_index_%s(%s, (%s)%s.len, %s)]", baseExpr, checkedSuffix(width), index, cType(width), baseExpr, buildSourceLoc(fileSet, node.Span))
	return pre, read, nil
}

// buildSliceIndexOperand builds the C text for one slice-index expression, the
// index child of a bare CheckedIndex, using the exact four-shape dispatch
// buildArrayPlaceRead and the CheckedIndexPlace path use (the same dispatch
// this file's str-indexing case also mirrors): an int-typed IntegerLiteral is
// emitted as its decimal text, an int-typed SymbolValue (a range loop's
// iterator referenced directly — the unanchored-int case, always declared at
// the entry's width) as its pebble_local_<symbol> C name, a uint-typed index
// via the dedicated buildUintExpr grammar (the general buildExpr path rejects
// a uint-typed value), and anything else via buildExpr.
func buildSliceIndexOperand(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, indexID tir.NodeID, indexNode tir.Node, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind) (string, error) {
	if indexNode.Kind == tir.IntegerLiteral && indexNode.Type == snapshot.Builtins().Int {
		if !isNonNegativeDecimal(indexNode.Literal.IntegerNum) {
			return "", fmt.Errorf("slice index contains an integer literal with malformed text %q", indexNode.Literal.IntegerNum)
		}
		return indexNode.Literal.IntegerNum, nil
	}
	if indexNode.Kind == tir.SymbolValue && indexNode.Type == snapshot.Builtins().Int {
		if _, declared := locals[indexNode.Symbol]; !declared {
			return "", fmt.Errorf("slice index references symbol %d, which is not a local in scope", indexNode.Symbol)
		}
		return fmt.Sprintf("pebble_local_%d", indexNode.Symbol), nil
	}
	if isUint(snapshot, indexNode.Type) {
		index, err := buildUintExpr(unit, snapshot, fileSet, indexID, locals, width)
		if err != nil {
			return "", fmt.Errorf("slice index: %v", err)
		}
		return index, nil
	}
	index, err := buildExpr(unit, snapshot, fileSet, indexID, locals, width, width)
	if err != nil {
		return "", fmt.Errorf("slice index: %v", err)
	}
	return index, nil
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

// buildTupleElement builds the C text for reading one element of a tuple local
// by symbol and ordinal: pebble_local_<symbol>._<ordinal>. The symbol must be a
// local the scope records as tuple-typed (its localInfo.tuple), the ordinal
// must be in range for that tuple type's element list, and the element's own
// type must satisfy the grammar wantBool selects — bool for the buildBoolExpr
// path, the entry's width for the buildExpr path. The tuple type comes from the
// scope record, not from any node field, so a read always resolves against the
// type the local was actually declared with.
func buildTupleElement(unit *tir.Unit, snapshot *types.Snapshot, symbolID symbol.SymbolID, ordinal uint32, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind, wantBool bool) (string, error) {
	info, declared := locals[symbolID]
	if !declared || info.tuple == 0 {
		return "", fmt.Errorf("entry function body expression reads an element of symbol %d, which is not a tuple-typed local declared earlier in the entry body", symbolID)
	}
	key, ok := snapshot.Key(info.tuple)
	if !ok {
		return "", fmt.Errorf("entry function body expression reads an element of a tuple local whose type %d is not in the type snapshot", info.tuple)
	}
	elements, ok := key.Elements()
	if !ok {
		return "", fmt.Errorf("entry function body expression reads an element of tuple type %s, which has no element list", tupleTypeName(info.tuple))
	}
	if ordinal >= uint32(len(elements)) {
		return "", fmt.Errorf("entry function body expression reads tuple element %d of %s, which has only %d element(s)", ordinal, tupleTypeName(info.tuple), len(elements))
	}
	element := elements[ordinal]
	if wantBool {
		if !isBool(snapshot, element) {
			return "", fmt.Errorf("entry function body expression reads tuple element %d, whose type is %s, want bool", ordinal, describeType(snapshot, element))
		}
	} else if !isWidth(snapshot, width, element) {
		return "", fmt.Errorf("entry function body expression reads tuple element %d, whose type is %s, want %s", ordinal, describeType(snapshot, element), wantName(width))
	}
	return fmt.Sprintf("pebble_local_%d._%d", symbolID, ordinal), nil
}

// buildStructFieldRead builds the C text for reading one field of a struct
// local through the Load(FieldPlace) shape the checker actually produces for
// `point.x` (confirmed against a real fixture): the FieldPlace carries the
// field's own member symbol in Member and its single child is the StoragePlace
// naming the struct local. wantBool selects which grammar the field must
// satisfy — bool (the buildBoolExpr path) or any fixed-width integer (uint,
// u64, the entry's width, or any other — each carried by the field's own C
// type). The field's own type is resolved from the struct's declared fields by
// matching FieldPlace.Member (see declaredFieldType), not assumed from the
// place's own Type. The emitted C is
// pebble_local_<symbol>.pebble_field_<member>. Since Slice C, a FieldPlace
// whose receiver is a TAGGED UNION and whose Member is one of its constructed
// variants (the narrowed union-variant payload read `self.Ok` inside `case
// .Ok:`) is lowered instead to
// pebble_local_<symbol>.payload.pebble_field_<member> — the exact projection
// the union's construction side fills (see buildUnionConstruction).
func buildStructFieldRead(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, place tir.Node, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind, wantBool bool) (string, error) {
	baseExpr, structType, err := buildPlaceLValue(unit, snapshot, fileSet, place.Children[0], locals, width)
	if err != nil {
		return "", err
	}
	access := "."
	if key, found := snapshot.Key(structType); found && key.Kind() == types.Pointer {
		pointee, childOK := key.Child()
		if !childOK {
			return "", fmt.Errorf("field read pointer has no pointee")
		}
		structType = pointee
		access = "->"
	}
	if place.Member == tir.StructuralFieldLen || place.Member == tir.StructuralFieldData || place.Member == tir.StructuralFieldHasValue {
		name := "len"
		if place.Member == tir.StructuralFieldData {
			name = "data"
		} else if place.Member == tir.StructuralFieldHasValue {
			name = "has_value"
		}
		key, found := snapshot.Key(structType)
		if !found {
			return "", fmt.Errorf("structural field receiver type %d is not in the type snapshot", structType)
		}
		if key.Kind() == types.Slice && (name == "len" || name == "data") {
			return baseExpr + access + name, nil
		}
		if name == "len" {
			if builtin, ok := key.Builtin(); ok && builtin == types.Str {
				return baseExpr + access + "len", nil
			}
		}
		if name == "has_value" && key.Kind() == types.Optional {
			return baseExpr + access + name, nil
		}
		return "", fmt.Errorf("unsupported structural field %s", name)
	}
	// A narrowed union-variant payload read (`self.Ok` inside `case .Ok:`,
	// the checker's Slice A acceptance): the Member is one of the receiver's
	// tagged-union variants, not a real struct field. This must come BEFORE
	// declaredFieldType below, whose member lookup would MIS-report a variant
	// as a field — a TypeDeclaration's Members list carries variants and its
	// MemberTypes the variant's payload type, so the lookup would succeed with
	// the wrong kind of member and emit a pebble_field_<member> projection no
	// real struct field satisfies. The payload lives in the union typedef's
	// .payload union under pebble_field_<member> — the exact member the
	// construction side designates (see buildUnionConstruction) — so the read
	// is the same projection the tag-matched construction wrote:
	// base.payload.pebble_field_<member> (with the same . / -> access a
	// pointer receiver resolves). The read's value type is the variant's
	// payload type, already resolved onto the place node's own Type by the
	// checker — the C type of the emitted projection is the payload member's
	// declared C type (unionMemberCType), which agrees.
	if unionVariantPayloadMember(unit, snapshot, structType, place.Member) {
		return fmt.Sprintf("%s%spayload%spebble_field_%d", baseExpr, access, access, place.Member), nil
	}
	fieldType, ok := declaredFieldType(unit, snapshot, structType, place.Member)
	if runtimeType(unit, snapshot, structType) != 0 {
		fieldType = place.Type
		ok = true
	}
	if !ok {
		return "", fmt.Errorf("field %d is not declared", place.Member)
	}
	if runtimeType(unit, snapshot, structType) != 0 {
		field, found := runtimeFieldName(unit, structType, place.Member)
		if !found {
			return "", fmt.Errorf("runtime field %d is not declared", place.Member)
		}
		return fmt.Sprintf("%s%s%s", baseExpr, access, field), nil
	}
	// Reading a field whose OWN type is a compiler-builtin runtime type (e.g.
	// `let a = holder.backing;`, where backing is Allocator): the C field is
	// declared with its hand-written C type (PebbleAllocator, see
	// structFieldCType), so the read is exactly the ordinary C field projection
	// with no width/bool coercion — the runtime value's type matches the C
	// field type directly. Without this case the width/bool check below would
	// reject the field.
	if runtimeType(unit, snapshot, fieldType) != 0 {
		return fmt.Sprintf("%s%spebble_field_%d", baseExpr, access, place.Member), nil
	}
	if wantBool {
		if !isBool(snapshot, fieldType) {
			return "", fmt.Errorf("field %d has type %s, want bool", place.Member, describeType(snapshot, fieldType))
		}
		return fmt.Sprintf("%s%spebble_field_%d", baseExpr, access, place.Member), nil
	}
	// A fixed-width integer field of ANY width, not just the ambient entry's
	// own: the C field is declared at the field's own resolved width (see
	// structFieldCType — a uint or u64 field is uint64_t), so the projection
	// pebble_local_<sym>.pebble_field_<member> carries the field's own C type
	// and is valid in a surrounding context of that same width. This mirrors
	// the generic resolvedBuiltin/cType widening already applied to optional
	// payloads (d737242), slice elements, function-type parameters/results,
	// and the struct-field typedef itself.
	if fieldWidth, integerField := resolvedBuiltin(snapshot, fieldType); integerField && cType(fieldWidth) != "" {
		return fmt.Sprintf("%s%spebble_field_%d", baseExpr, access, place.Member), nil
	}
	if isPointer(snapshot, fieldType) {
		return fmt.Sprintf("%s%spebble_field_%d", baseExpr, access, place.Member), nil
	}
	if isEnumType(unit, snapshot, fieldType) {
		// A plain enum-typed field (`entry.state`): the C field is declared at
		// the field's own pebble_enum_<typeID>_t (see structFieldCType), whose
		// C representation is just the variant's ordinal (a plain C enum
		// value), so the read is exactly the ordinary C field projection with
		// no width/bool coercion — the same direct projection the pointer case
		// two lines above uses, and the shape buildComparison's enum branch
		// consumes as an enum comparison operand.
		return fmt.Sprintf("%s%spebble_field_%d", baseExpr, access, place.Member), nil
	}
	return "", fmt.Errorf("field %d has type %s, want a fixed-width integer, bool, pointer, or enum", place.Member, describeType(snapshot, fieldType))
}

// buildDereferencePlaceRead builds the C text for reading through a
// DereferencePlace: `*pebble_rt_checked_deref_ptr(<ptr_expr>, <loc>)`. The
// pointer expression is built by buildExpr, the null check is performed by the
// runtime primitive, and the dereference produces the pointee value. wantBool
// controls whether the caller expects a bool-typed result (for an `if *b` where
// b is *bool) — the C dereference of a bool pointer yields a C bool directly.
func buildDereferencePlaceRead(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, place tir.Node, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind, loadSpan source.Span, wantBool bool) (string, error) {
	if len(place.Children) != 1 {
		return "", fmt.Errorf("dereference place wants one child")
	}
	ptrExpr, err := buildExpr(unit, snapshot, fileSet, place.Children[0], locals, width, width)
	if err != nil {
		return "", fmt.Errorf("dereference pointer expression: %v", err)
	}
	checkedPtr := fmt.Sprintf("pebble_rt_checked_deref_ptr(%s, %s)", ptrExpr, buildSourceLoc(fileSet, loadSpan))
	// place.Type is already the pointee type, not the pointer type — a
	// DereferencePlace's own Type is what dereferencing produces (confirmed
	// via place_facts.go's deriveDereferencePlace, whose result is the
	// dereferenced value), the same reason it passes buildExpr's width gate
	// unmodified for a width-typed pointee.
	pointeeTypeID := place.Type
	pointeeCType := pointerTypeName(snapshot, pointeeTypeID)
	if pointeeCType == "" {
		return "", fmt.Errorf("dereference place has unsupported pointee type %s", describeType(snapshot, pointeeTypeID))
	}
	castExpr := fmt.Sprintf("*(%s)(%s)", pointeeCType, checkedPtr)
	if wantBool {
		if !isBool(snapshot, pointeeTypeID) {
			return "", fmt.Errorf("dereference read wants bool but pointee is %s", describeType(snapshot, pointeeTypeID))
		}
	}
	return castExpr, nil
}

func buildPlaceLValue(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, id tir.NodeID, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind) (string, types.TypeID, error) {
	n, ok := unit.Node(id)
	if !ok {
		return "", 0, fmt.Errorf("place %d is invalid", id)
	}
	switch n.Kind {
	case tir.StoragePlace:
		info, ok := locals[n.Symbol]
		if !ok {
			return "", 0, fmt.Errorf("symbol %d is not a local", n.Symbol)
		}
		var typ types.TypeID
		switch {
		case info.tuple != 0:
			typ = info.tuple
		case info.array != 0:
			typ = info.array
		case info.optional != 0:
			typ = info.optional
		case info.structType != 0:
			typ = info.structType
		case info.sliceType != 0:
			typ = info.sliceType
		case info.pointerType != 0:
			typ = info.pointerType
		case info.runtimeType != 0:
			typ = info.runtimeType
		default:
			// A scalar local (int, bool, char, str). buildPlaceLValue is
			// only called for address-of and aggregate field/element access;
			// for scalars the node's own Type is the correct types.TypeID.
			typ = n.Type
		}
		base := fmt.Sprintf("pebble_local_%d", n.Symbol)
		if info.array != 0 && info.arrayWrapped {
			base += ".data"
		}
		return base, typ, nil
	case tir.TuplePlace:
		if len(n.Children) != 1 {
			return "", 0, fmt.Errorf("tuple place wants one base")
		}
		base, typ, err := buildPlaceLValue(unit, snapshot, fileSet, n.Children[0], locals, width)
		if err != nil {
			return "", 0, err
		}
		key, ok := snapshot.Key(typ)
		if !ok {
			return "", 0, fmt.Errorf("tuple type missing")
		}
		elems, ok := key.Elements()
		if !ok || n.Ordinal >= uint32(len(elems)) {
			return "", 0, fmt.Errorf("tuple element out of range")
		}
		return fmt.Sprintf("%s._%d", base, n.Ordinal), elems[n.Ordinal], nil
	case tir.FieldPlace:
		if len(n.Children) != 1 {
			return "", 0, fmt.Errorf("field place wants one base")
		}
		base, typ, err := buildPlaceLValue(unit, snapshot, fileSet, n.Children[0], locals, width)
		if err != nil {
			return "", 0, err
		}
		access := "."
		if key, found := snapshot.Key(typ); found && key.Kind() == types.Pointer {
			pointee, childOK := key.Child()
			if !childOK {
				return "", 0, fmt.Errorf("field place pointer has no pointee")
			}
			typ = pointee
			access = "->"
		}
		if n.Member == tir.StructuralFieldLen || n.Member == tir.StructuralFieldData || n.Member == tir.StructuralFieldHasValue {
			// A structural field — `.len`/`.data` on a slice, `.len` on a str,
			// or `.has_value` on an optional — is a sentinel member symbol (see
			// tir.StructuralFieldLen et al.), never a real struct field index,
			// so it must be projected before declaredFieldType would reject it
			// as undeclared. This mirrors buildStructFieldRead's handling of
			// the same sentinels; the `.len`/`.data`/`.has_value` field of the
			// underlying runtime aggregate is projected with the same `.` or
			// `->` access already resolved for a pointer receiver.
			name := "len"
			if n.Member == tir.StructuralFieldData {
				name = "data"
			} else if n.Member == tir.StructuralFieldHasValue {
				name = "has_value"
			}
			key, found := snapshot.Key(typ)
			if !found {
				return "", 0, fmt.Errorf("structural field receiver type %d is not in the type snapshot", typ)
			}
			if key.Kind() == types.Slice && (name == "len" || name == "data") {
				// `.len`/`.data` on a slice: the projection's type is the
				// resolved uint / pointer-to-element the checker assigned the
				// field access itself (n.Type), so a Load of it routes through
				// buildUintExpr / the pointer grammar by its own type.
				return fmt.Sprintf("%s%s%s", base, access, name), n.Type, nil
			}
			if name == "len" {
				if builtin, ok := key.Builtin(); ok && builtin == types.Str {
					return fmt.Sprintf("%s%s%s", base, access, "len"), n.Type, nil
				}
			}
			if name == "has_value" && key.Kind() == types.Optional {
				return fmt.Sprintf("%s%s%s", base, access, name), snapshot.Builtins().Bool, nil
			}
			return "", 0, fmt.Errorf("unsupported structural field %s", name)
		}
		// A narrowed union-variant payload projection (`self.Ok` inside `case
		// .Ok:`, Slice A) used as an lvalue — the write-side twin of
		// buildStructFieldRead's read case (set_error's `self.Err = error` on
		// a *Result receiver lowers to a Store whose place is a FieldPlace of
		// the Err variant). The variant's payload lives in the union typedef's
		// .payload union under pebble_field_<member>, so the lvalue is the
		// exact projection the construction side fills. This must come before
		// declaredFieldType, whose member lookup would MIS-report a variant as
		// a real field (a TypeDeclaration's Members carries variants). The
		// lvalue's type is the variant's payload type, already resolved onto
		// the place node's own Type by the checker.
		if unionVariantPayloadMember(unit, snapshot, typ, n.Member) {
			return fmt.Sprintf("%s%spayload%spebble_field_%d", base, access, access, n.Member), n.Type, nil
		}
		ft, ok := declaredFieldType(unit, snapshot, typ, n.Member)
		if !ok {
			return "", 0, fmt.Errorf("field %d is not declared", n.Member)
		}
		if field, ok := runtimeFieldName(unit, typ, n.Member); ok {
			return fmt.Sprintf("%s%s%s", base, access, field), ft, nil
		}
		return fmt.Sprintf("%s%spebble_field_%d", base, access, n.Member), ft, nil
	case tir.CheckedIndexPlace:
		if len(n.Children) != 2 {
			return "", 0, fmt.Errorf("index place wants two children")
		}
		base, typ, err := buildPlaceLValue(unit, snapshot, fileSet, n.Children[0], locals, width)
		if err != nil {
			return "", 0, err
		}
		indexNode, ok := unit.Node(n.Children[1])
		if !ok {
			return "", 0, fmt.Errorf("invalid array index")
		}
		idx := ""
		if indexNode.Kind == tir.IntegerLiteral && indexNode.Type == snapshot.Builtins().Int {
			idx = indexNode.Literal.IntegerNum
		} else if indexNode.Kind == tir.SymbolValue && indexNode.Type == snapshot.Builtins().Int {
			if _, declared := locals[indexNode.Symbol]; !declared {
				return "", 0, fmt.Errorf("symbol %d is not a local in scope", indexNode.Symbol)
			}
			idx = fmt.Sprintf("pebble_local_%d", indexNode.Symbol)
		} else if isUint(snapshot, indexNode.Type) {
			// A uint-typed index (`self.entries[index]` where index is a uint
			// local, or the uint-typed loop iterator `new_entries[i]` from a
			// `loop 0..new_cap : i { ... }` whose bounds the checker anchored
			// to uint): the index flows through the dedicated buildUintExpr
			// grammar, exactly as every other uint value position routes —
			// the general buildExpr path's entry-width gate would reject a
			// uint-typed value with "of type uint, want <entry width>".
			idx, err = buildUintExpr(unit, snapshot, fileSet, n.Children[1], locals, width)
			if err != nil {
				return "", 0, err
			}
		} else {
			idx, err = buildExpr(unit, snapshot, fileSet, n.Children[1], locals, width, width)
			if err != nil {
				return "", 0, err
			}
		}
		if isSlice(snapshot, typ) {
			// A slice-typed base: use .data[checked_index(idx, (width_type).len)].
			sliceKey, ok := snapshot.Key(typ)
			if !ok {
				return "", 0, fmt.Errorf("slice type %d is not in the type snapshot", typ)
			}
			elem, ok := sliceKey.Child()
			if !ok {
				return "", 0, fmt.Errorf("slice type has no element type")
			}
			return fmt.Sprintf("%s.data[pebble_rt_checked_index_%s(%s, (%s)%s.len, %s)]", base, checkedSuffix(width), idx, cType(width), base, buildSourceLoc(fileSet, n.Span)), elem, nil
		}
		key, ok := snapshot.Key(typ)
		if !ok {
			return "", 0, fmt.Errorf("array type missing")
		}
		length, elem, ok := key.Array()
		if !ok {
			return "", 0, fmt.Errorf("index base is not an array")
		}
		lit, _ := arrayLengthLiteral(length, width)
		return fmt.Sprintf("%s[pebble_rt_checked_index_%s(%s, %s, %s)]", base, checkedSuffix(width), idx, lit, buildSourceLoc(fileSet, n.Span)), elem, nil
	case tir.DereferencePlace:
		// A dereference place: `*p` used as a write target (`*p = x;`). The
		// child is the pointer expression. The emitted C builds the pointer,
		// runs it through pebble_rt_checked_deref_ptr for null checking, and
		// produces `(*<checked_ptr>)` as the lvalue.
		if len(n.Children) != 1 {
			return "", 0, fmt.Errorf("dereference place wants one child")
		}
		ptrExpr, err := buildExpr(unit, snapshot, fileSet, n.Children[0], locals, width, width)
		if err != nil {
			return "", 0, fmt.Errorf("dereference pointer expression: %v", err)
		}
		checkedPtr := fmt.Sprintf("pebble_rt_checked_deref_ptr(%s, %s)", ptrExpr, buildSourceLoc(fileSet, n.Span))
		// n.Type is already the pointee type, not the pointer type (see the
		// matching comment in buildDereferencePlaceRead).
		pointeeTypeID := n.Type
		pointeeCType := pointerTypeName(snapshot, pointeeTypeID)
		if pointeeCType == "" {
			return "", 0, fmt.Errorf("dereference place has unsupported pointee type %s", describeType(snapshot, pointeeTypeID))
		}
		castExpr := fmt.Sprintf("*(%s)(%s)", pointeeCType, checkedPtr)
		return castExpr, pointeeTypeID, nil
	}
	return "", 0, fmt.Errorf("place base %s is unsupported", n.Kind)
}

// declaredFieldType resolves one field's own type from a struct type's
// declared fields, matching the field's member symbol against the struct's
// TypeDecl.Members list (the declared field order). For a generic
// instantiation the member's recorded type is one of the declaration's own
// type parameters, so the concrete field type is substituted per instantiation
// exactly as resolveStructInfo does: the member's MemberTypes entry is the
// parameter's TypeID and the struct type's Nominal arguments are the concrete
// type arguments, so Pair[K, V]'s `value V` is Bool for Pair[int, bool] while
// staying Int for Pair[int, int] in the same program. The two instantiations
// share the same field symbol, which is why the field type is resolved from
// the instantiation's own arguments rather than any per-symbol evidence —
// per-symbol node-graph evidence would let the first instantiation's Int win
// over the second's Bool. When the member's MemberTypes entry is unresolved
// (a member whose template wraps a parameter) the type is instead recovered
// from the unit's own node graph, scoped to THIS instantiation: any
// RecordConstruct whose type is exactly structType and whose Fields contain
// the member (the value node's Type is the field's resolved type), or any
// FieldPlace whose base resolves to structType and whose Member is the member
// (its Type is the field's resolved type). The RecordConstruct/FieldPlace
// scoping is what keeps two specializations of one generic struct from
// borrowing each other's field type. A member that is not in the struct's
// declared member list, or whose type cannot be resolved from the unit,
// reports false.
func declaredFieldType(unit *tir.Unit, snapshot *types.Snapshot, structType types.TypeID, member symbol.SymbolID) (types.TypeID, bool) {
	key, ok := snapshot.Key(structType)
	if !ok {
		return 0, false
	}
	decl, arguments, ok := key.Nominal()
	if !ok {
		return 0, false
	}
	typeDecl, ok := findTypeDeclaration(unit, decl)
	if !ok {
		return 0, false
	}
	for index, declared := range typeDecl.Members {
		if declared != member {
			continue
		}
		fieldType := types.TypeID(0)
		if index < len(typeDecl.MemberTypes) {
			fieldType = typeDecl.MemberTypes[index]
		}
		if fieldType != 0 {
			substitutions := structSubstitutions(unit, snapshot, decl, arguments)
			if substitutions != nil {
				if substituted, err := snapshot.Substitute(fieldType, substitutions); err == nil {
					return substituted, true
				}
			}
			return fieldType, true
		}
		break
	}
	for _, node := range unit.Nodes() {
		if node.Kind == tir.FieldPlace && node.Member == member && node.Type != 0 {
			if structOf, ok := fieldPlaceStructType(unit, node); ok && structOf == structType {
				return node.Type, true
			}
		}
		if node.Kind == tir.RecordConstruct && node.Symbol == decl && node.Type == structType {
			for _, field := range node.Fields {
				if field.Field == member {
					if value, ok := unit.Node(field.Value); ok && value.Type != 0 {
						return value.Type, true
					}
				}
			}
		}
	}
	return 0, false
}

// fieldPlaceStructType resolves the struct type a FieldPlace reads from by
// walking the place's base chain to the underlying StoragePlace or
// DereferencePlace (whose Type is the struct type). Two generic
// specializations of one struct share every field symbol, so FieldPlace
// evidence must be scoped to the struct type the read actually projects —
// matching the RecordConstruct recovery's node.Type == structType scoping —
// or the first matching FieldPlace in the unit (from another specialization)
// would supply the wrong field type.
func fieldPlaceStructType(unit *tir.Unit, node tir.Node) (types.TypeID, bool) {
	if len(node.Children) != 1 {
		return 0, false
	}
	base, ok := unit.Node(node.Children[0])
	if !ok {
		return 0, false
	}
	switch base.Kind {
	case tir.StoragePlace:
		return base.Type, true
	case tir.DereferencePlace:
		// A DereferencePlace's Type is already the pointee type, which for a
		// struct-field projection is the struct type being read.
		return base.Type, true
	case tir.FieldPlace:
		return fieldPlaceStructType(unit, base)
	}
	return 0, false
}

// buildCallArguments builds the comma-separated C argument list for a
// DirectCall's children, one expression per child in order. Each child's
// grammar is decided by the callee's corresponding parameter's resolved type
// — the entry's width parameters take buildExpr, bool parameters take
// buildBoolExpr, str parameters (since 10.36) take buildStrOperand, and
// tuple/struct parameters take buildAggregateArgument (an
// already-declared aggregate-typed local emitted as its own C name, or a
// freshly-constructed aggregate built inline as a compound-literal expression,
// see buildAggregateArgument) — so the same value grammars this backend
// already builds lower
// the arguments; the checker has already coerced each argument to its
// parameter's type, so a mismatch here is hand-built IR. The argument count
// must equal the callee's declared parameter count. A variadic callee
// (callee.Variadic — the checker's own declaration node sets it; see
// ir_builder.go's FunctionDeclaration construction) is the one exception:
// its trailing slice parameter is fed by every call-site argument from
// fixedCount (the number of fixed parameters, len(Parameters)-1, mirroring
// the checker's own FixedCount computation) onward, collected into a single
// runtime slice value built as ONE C99 compound-literal expression by
// buildVariadicSliceArgument — the array-literal element storage's automatic
// storage duration lasts until the end of the enclosing block (C11 6.5.2.5p16),
// so a callee can validly read the collected slice for the whole call — so
// the arity requirement for a variadic callee is len(call.Children) >=
// fixedCount rather than equality. Returns the joined argument text, empty
// when the callee takes no parameters (the caller then emits
// pebble_fn_<id>(ctx) with no argument list).
func buildCallArguments(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, call tir.Node, callee tir.Node, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind) (string, string, error) {
	variadic := callee.Variadic
	fixedCount := len(callee.Parameters)
	if variadic {
		if len(callee.Parameters) == 0 {
			return "", "", fmt.Errorf("entry function body expression contains a call to symbol %d whose callee is variadic but declares no parameters (a variadic callee must declare its trailing slice parameter)", call.Symbol)
		}
		fixedCount--
		if len(call.Children) < fixedCount {
			return "", "", fmt.Errorf("entry function body expression contains a call to symbol %d passing %d argument(s), want at least %d (the variadic callee declares %d fixed parameter(s) plus a trailing slice parameter)", call.Symbol, len(call.Children), fixedCount, fixedCount)
		}
	} else if len(call.Children) != len(callee.Parameters) {
		return "", "", fmt.Errorf("entry function body expression contains a call to symbol %d passing %d argument(s), want %d (the callee declares %d parameter(s))", call.Symbol, len(call.Children), len(callee.Parameters), len(callee.Parameters))
	}
	var pres []string
	args := make([]string, 0, len(call.Children))
	// A C-convention callee (an extern declaration) declares its parameters
	// against the real libc header, so a str argument must be lowered to the
	// C representation a real C function consumes — `const char *`, from the
	// PebbleStr value's .data — not passed as the whole PebbleStr struct a
	// Pebble-convention helper's own C signature accepts.
	cConvention := callee.Convention == types.C
	for i := 0; i < fixedCount; i++ {
		pre, arg, err := buildCallArgument(unit, snapshot, fileSet, call.Symbol, i, call.Children[i], callee.Parameters[i], cConvention, locals, width)
		if err != nil {
			return "", "", err
		}
		if pre != "" {
			pres = append(pres, pre)
		}
		args = append(args, arg)
	}
	if variadic {
		pre, sliceArg, err := buildVariadicSliceArgument(unit, snapshot, fileSet, call, callee.Parameters[fixedCount], call.Children[fixedCount:], locals, width)
		if err != nil {
			return "", "", err
		}
		if pre != "" {
			pres = append(pres, pre)
		}
		args = append(args, sliceArg)
	}
	return strings.Join(pres, "\n"), strings.Join(args, ", "), nil
}

// buildCallArgument builds one call-site argument expression for one callee
// parameter, deciding the child's grammar from the parameter's resolved type
// exactly as an ordinary call's per-argument loop always has — the entry's
// width parameters take buildExpr, uint parameters take buildUintExpr, bool
// parameters take buildBoolExpr, f32/f64 parameters take buildFloatExpr at the
// parameter's own float kind, char parameters take buildCharOperand, str
// parameters take buildStrOperand, tuple/struct parameters take
// buildAggregateArgument, slice parameters take buildSliceArgument, pointer
// parameters take buildExpr (which handles every pointer-value shape), and
// function-typed parameters (function-types slice 3) take buildFunctionValue
// (the shared builder every function-typed value position uses).
// The checker has already coerced the argument to the parameter's type, so a
// mismatch here is hand-built IR. position is the call-site argument index,
// used only to name the offending argument in rejection messages.
// cConvention is true when the callee is a C-convention extern declaration: a
// str argument to such a callee must be lowered to the const char * a real C
// function consumes (the PebbleStr value's .data field) rather than passed as
// the whole PebbleStr struct, so the emitted call site agrees with the libc
// header's own parameter type (fopen's path/mode, and so on).
//
// The return is a (pre, expr) pair: expr is the argument's C expression, and
// pre is an optional indent-free pre-statement the caller must emit BEFORE the
// enclosing call expression. Only an inline slice-construction argument
// (f(a[1:3]), via buildSliceArgument) ever produces a non-empty pre — the
// two-statement temp-then-construction shape the construction needs — and every
// other argument shape is a pure expression (pre == ""). The pre is only
// consumable when the enclosing call is itself in a leading-statement position;
// buildDirectCall's expression-position wrapper rejects it upstream.
func buildCallArgument(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, calleeSymbol symbol.SymbolID, position int, argID tir.NodeID, param tir.Parameter, cConvention bool, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind) (string, string, error) {
	// A parameter's own resolved integer width, when the parameter is an
	// integer builtin the backend emits (the entry's width, uint, u64, or
	// any other fixed-width integer). Deciding the argument grammar from the
	// parameter's OWN width — rather than the ambient width of the call
	// site — lets a call whose result is consumed in a different-width
	// context (a u64 result assigned into a u64 local, for instance) still
	// build each argument at the parameter's type, and likewise lets a u64
	// parameter be called from an entry-width context. uint is deliberately
	// excluded below: it is the platform-native pointer-width builtin the
	// backend builds through buildUintExpr (sizeof, slice bounds, checked
	// arithmetic), not through the general buildExpr path.
	paramWidth, integerParam := resolvedBuiltin(snapshot, param.Type)
	if integerParam && cType(paramWidth) != "" && !isUint(snapshot, param.Type) {
		expr, err := buildExpr(unit, snapshot, fileSet, argID, locals, paramWidth, width)
		if err != nil {
			return "", "", err
		}
		return "", expr, nil
	}
	switch {
	case isUint(snapshot, param.Type):
		expr, err := buildUintExpr(unit, snapshot, fileSet, argID, locals, width)
		if err != nil {
			return "", "", err
		}
		return "", expr, nil
	case isBool(snapshot, param.Type):
		expr, err := buildBoolExpr(unit, snapshot, fileSet, argID, locals, width)
		if err != nil {
			return "", "", err
		}
		return "", expr, nil
	case isFloat(snapshot, param.Type):
		// An f32/f64 parameter: the argument is a float value built by
		// buildFloatExpr at the parameter's OWN float kind — a reference to
		// a float-typed local in scope of the same kind, a float literal
		// directly (f(3.5)), or a call to a float-returning helper (f(g()))
		// — emitted at the same C float type (floatCType) the parameter is
		// declared with, so passing a float by value is trivially valid C.
		expr, err := buildFloatExpr(unit, snapshot, fileSet, argID, locals, resolvedFloatKind(snapshot, param.Type))
		if err != nil {
			return "", "", err
		}
		return "", expr, nil
	case isChar(snapshot, param.Type):
		// A char parameter: the argument is a char value built by
		// buildCharOperand — a reference to a char-typed local in scope, a
		// char literal directly (f('a')), or a call to a char-returning
		// helper (f(g())) — emitted as an int32_t value, the same C type
		// the parameter is declared with, so passing a char by value is
		// trivially valid C.
		expr, err := buildCharOperand(unit, snapshot, fileSet, argID, locals, width)
		if err != nil {
			return "", "", err
		}
		return "", expr, nil
	case isTuple(snapshot, param.Type):
		expr, err := buildAggregateArgument(unit, snapshot, fileSet, argID, locals, param.Type, true, calleeSymbol, position, width)
		if err != nil {
			return "", "", err
		}
		return "", expr, nil
	case isEnumType(unit, snapshot, param.Type):
		// An enum/union-typed parameter: the argument is a reference to an
		// already-declared enum/union-typed local in scope of exactly the
		// parameter's type (a SymbolValue — e.g. unwrap_or(a, 0) passing the
		// union local a), emitted as the local's own pebble_local_<symbol> C
		// name. The type's own typedef (pebble_enum_<typeID>_t /
		// pebble_union_<typeID>_t) makes passing the whole value by value
		// trivially valid C, matching the C type the parameter is declared
		// with (see helperSignature). An inline variant construction or a
		// reference to a nonmatching local is a clean rejection, never a
		// guessed lowering.
		argNode, ok := unit.Node(argID)
		if !ok {
			return "", "", fmt.Errorf("call to symbol %d parameter %d (symbol %d) references invalid node %d", calleeSymbol, position, param.Symbol, argID)
		}
		if argNode.Kind != tir.SymbolValue {
			return "", "", fmt.Errorf("call to symbol %d parameter %d (symbol %d) of type %s is a %s, want a reference to an enum/union-typed local of exactly that type in scope (binding the value into a local first is required)", calleeSymbol, position, param.Symbol, describeType(snapshot, param.Type), argNode.Kind)
		}
		info, declared := locals[argNode.Symbol]
		if !declared || info.enumType != param.Type {
			return "", "", fmt.Errorf("call to symbol %d parameter %d (symbol %d) passes symbol %d, which is not a local of the parameter's enum/union type %s", calleeSymbol, position, param.Symbol, argNode.Symbol, describeType(snapshot, param.Type))
		}
		return "", fmt.Sprintf("pebble_local_%d", argNode.Symbol), nil
	case isStruct(snapshot, param.Type):
		expr, err := buildAggregateArgument(unit, snapshot, fileSet, argID, locals, param.Type, false, calleeSymbol, position, width)
		if err != nil {
			return "", "", err
		}
		return "", expr, nil
	case isArray(snapshot, param.Type):
		expr, err := buildArrayArgument(unit, snapshot, fileSet, width, param.Type, argID, locals, calleeSymbol, position)
		if err != nil {
			return "", "", err
		}
		return "", expr, nil
	case isStr(snapshot, param.Type):
		// A str parameter: the argument is a str value built by
		// buildStrOperand — a reference to a str-typed local in scope, a
		// string literal directly (f("hi")), or a call to a str-returning
		// helper (f(g())) — emitted as a PebbleStr value, the same C type
		// the parameter is declared with, so passing a str by value is
		// trivially valid C. When the callee is a C-convention extern, the
		// real libc parameter is `const char *`, not a PebbleStr struct, so
		// the argument is the PebbleStr value's .data field cast to
		// const char * instead — the natural C spelling of a Pebble str (the
		// same cast buildPrint uses for a %s operand) — making the emitted
		// call site agree with the libc header (fopen's path/mode, and so on).
		expr, err := buildStrOperand(unit, snapshot, fileSet, argID, locals, width)
		if err != nil {
			return "", "", err
		}
		if cConvention {
			expr = "(const char *)(" + expr + ").data"
		}
		return "", expr, nil
	case isSlice(snapshot, param.Type):
		// A slice parameter (10.38): the argument is a reference to an
		// already-declared slice-typed local in scope of the matching type,
		// emitted as the local's own pebble_local_<symbol> C name — the
		// slice type's own struct typedef makes passing the whole slice by
		// value trivially valid C, no construction needed at the call site
		// (confirmed checker-reachable: f(s) passes a plain SymbolValue) —
		// or, since the leading-statement call-argument slice, an inline
		// slice construction (f(a[1:3])) whose temp declaration is returned
		// as the pre (see buildSliceArgument).
		return buildSliceArgument(unit, snapshot, fileSet, argID, locals, param.Type, calleeSymbol, position, width)
	case isOptional(snapshot, param.Type):
		// An optional parameter: the argument is an optional value built by
		// buildOptionalValue — a SymbolValue naming an optional-typed local
		// in scope, a fresh SomeOptional/NoneOptional/OptionalInject
		// construction (a call site's scalar implicit injection, `g(5)`,
		// arrives as an OptionalInject node, unlike a return position's bare
		// payload), or a DirectCall to another optional-returning helper —
		// emitted as the optional's own compound literal or a forwarded
		// C name/call, the same C type the parameter is declared with, so
		// passing an optional by value is trivially valid C.
		expr, err := buildOptionalValue(unit, snapshot, fileSet, argID, locals, param.Type, fmt.Sprintf("entry function body expression contains a call to symbol %d whose parameter %d (symbol %d)", calleeSymbol, position, param.Symbol), width)
		if err != nil {
			return "", "", err
		}
		return "", expr, nil
	case isPointer(snapshot, param.Type):
		// A pointer parameter: the argument is a pointer value built by
		// buildExpr, which handles every pointer-value shape (AddressOf,
		// a reference to a pointer-typed local, nil, or a call to a
		// pointer-returning helper).
		expr, err := buildExpr(unit, snapshot, fileSet, argID, locals, width, width)
		if err != nil {
			return "", "", err
		}
		return "", expr, nil
	case isFunctionType(snapshot, param.Type):
		// A function-typed parameter (function-types slice 3): the argument
		// is a function value built by buildFunctionValue — a bare top-level
		// function reference (apply(add, 1, 2), a HoistedFunctionValue), an
		// in-scope function-typed local or parameter (a SymbolValue), a
		// function-typed struct field read (a FieldValue or Load(FieldPlace),
		// combining slice 2 and slice 3), a generic function referenced as a
		// value (a GenericFunctionValue), or a call to a function-returning
		// helper (a DirectCall whose result type is the function type) —
		// emitted as a pebble_fnptr_<typeID>_t value, the same C type the
		// parameter is declared with, so passing a function by value is
		// trivially valid C.
		argNode, ok := unit.Node(argID)
		if !ok {
			return "", "", fmt.Errorf("entry function body expression contains a call to symbol %d whose parameter %d (symbol %d) references invalid argument node %d", calleeSymbol, position, param.Symbol, argID)
		}
		expr, err := buildFunctionValue(unit, snapshot, fileSet, argNode, locals, fmt.Sprintf("entry function body expression contains a call to symbol %d whose parameter %d (symbol %d)", calleeSymbol, position, param.Symbol), width)
		if err != nil {
			return "", "", err
		}
		return "", expr, nil
	default:
		// validateHelperSignature rules any unsupported parameter out
		// before a reachable helper is ever built, so this branch is
		// defense for hand-built IR only.
		return "", "", fmt.Errorf("entry function body expression contains a call to symbol %d whose parameter %d (symbol %d) has type %s, want %s, bool, char, str, f32, f64, a tuple/struct type, a slice type, or a pointer type", calleeSymbol, position, param.Symbol, describeType(snapshot, param.Type), wantName(width))
	}
}

func buildArrayArgument(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, width types.BuiltinKind, arrayType types.TypeID, argID tir.NodeID, locals map[symbol.SymbolID]localInfo, calleeSymbol symbol.SymbolID, position int) (string, error) {
	node, ok := unit.Node(argID)
	if !ok {
		return "", fmt.Errorf("entry function body expression contains a call to symbol %d whose array parameter %d references invalid argument node %d", calleeSymbol, position, argID)
	}
	key, ok := snapshot.Key(arrayType)
	if !ok {
		return "", fmt.Errorf("array parameter type %s is not in the type snapshot", describeType(snapshot, arrayType))
	}
	length, element, ok := key.Array()
	if !ok {
		return "", fmt.Errorf("array parameter type %s has no length and element type", describeType(snapshot, arrayType))
	}
	if node.Kind == tir.DirectCall {
		if node.Type != arrayType {
			return "", fmt.Errorf("array argument call has type %s, want %s", describeType(snapshot, node.Type), describeType(snapshot, arrayType))
		}
		return buildDirectCall(unit, snapshot, fileSet, node, locals, width)
	}
	if node.Kind == tir.ArrayValue {
		if uint64(len(node.Children)) != length {
			return "", fmt.Errorf("array argument has %d element(s), want %d", len(node.Children), length)
		}
		values := make([]string, len(node.Children))
		for i, childID := range node.Children {
			child, ok := unit.Node(childID)
			if !ok {
				return "", fmt.Errorf("array argument references invalid element node %d", childID)
			}
			var value string
			var err error
			if isBool(snapshot, element) {
				value, err = buildBoolExpr(unit, snapshot, fileSet, childID, locals, width)
			} else if elementWidth, integer := resolvedBuiltin(snapshot, element); integer && cType(elementWidth) != "" {
				value, err = buildExpr(unit, snapshot, fileSet, childID, locals, elementWidth, width)
			} else {
				return "", fmt.Errorf("array argument element type %s is unsupported", describeType(snapshot, child.Type))
			}
			if err != nil {
				return "", err
			}
			values[i] = value
		}
		return fmt.Sprintf("(%s){ .data = { %s } }", arrayTypeName(arrayType), strings.Join(values, ", ")), nil
	}
	if node.Kind != tir.SymbolValue {
		return "", fmt.Errorf("entry function body expression contains a call to symbol %d whose array parameter %d is a %s, want an array local or array-returning call", calleeSymbol, position, node.Kind)
	}
	info, declared := locals[node.Symbol]
	if !declared || info.array != arrayType {
		return "", fmt.Errorf("entry function body expression contains a call to symbol %d whose array parameter %d is not a local of type %s", calleeSymbol, position, describeType(snapshot, arrayType))
	}
	if info.arrayWrapped {
		return fmt.Sprintf("pebble_local_%d", node.Symbol), nil
	}
	values := make([]string, length)
	for i := range values {
		values[i] = fmt.Sprintf("pebble_local_%d[%d]", node.Symbol, i)
	}
	return fmt.Sprintf("(%s){ .data = { %s } }", arrayTypeName(arrayType), strings.Join(values, ", ")), nil
}

// buildVariadicSliceArgument builds the ONE C argument expression a variadic
// call's trailing slice parameter receives: a C99 compound literal of the
// parameter's slice type collecting every call-site argument in the variadic
// tail as an element of an inline array compound literal,
//
//	(<sliceTypeName>){ .data = (<elementCType>[]){<arg0>, <arg1>, ...}, .len = (size_t)(<count>) }
//
// built purely as an expression — no pre-statement or temp declaration is
// needed, because a C99 array compound literal has automatic storage duration
// lasting until the end of the enclosing block (C11 6.5.2.5p16), not just the
// enclosing full expression, so the callee can validly read the collected
// elements for the whole call. Each variadic call-site argument is built as an
// individual scalar expression at the trailing parameter's slice ELEMENT type
// (resolved via the slice's TypeKey.Child, the same structural step 10.37's
// slice construction and validateSliceElementType use), dispatched through the
// exact same buildCallArgument grammar a fixed parameter of that type would use
// — the only element types this backend supports for any slice at all are the
// entry's width and bool (see validateHelperSignature / sliceElementCType), so
// the practical dispatch is buildExpr for int and buildBoolExpr for bool. The
// zero-variadic-arguments case emits the codebase's established empty-slice
// shape instead of an empty array compound literal — `.data = NULL, .len =
// (size_t)0` — because a zero-size array literal is a GNU extension, not
// portable C99/C11, and won't compile under the project's strict
// -Wall -Wextra -Werror harness (the same shape a SliceFromRaw construction of
// a nil pointer with count 0, `slice ptr, 0`, already produces — see
// buildRawSliceConstruction).
func buildVariadicSliceArgument(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, call tir.Node, sliceParam tir.Parameter, variadicIDs []tir.NodeID, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind) (string, string, error) {
	sliceType := sliceParam.Type
	if !isSlice(snapshot, sliceType) {
		return "", "", fmt.Errorf("entry function body expression contains a call to symbol %d whose variadic parameter %d (symbol %d) has type %s, want a slice type", call.Symbol, len(call.Children)-1, sliceParam.Symbol, describeType(snapshot, sliceType))
	}
	key, ok := snapshot.Key(sliceType)
	if !ok {
		return "", "", fmt.Errorf("entry function body expression contains a call to symbol %d whose variadic parameter type %d is not in the type snapshot", call.Symbol, sliceType)
	}
	elementType, ok := key.Child()
	if !ok {
		return "", "", fmt.Errorf("entry function body expression contains a call to symbol %d whose variadic parameter type %s has no element type", call.Symbol, describeType(snapshot, sliceType))
	}
	elemCType, err := sliceElementCType(unit, snapshot, width, elementType)
	if err != nil {
		return "", "", fmt.Errorf("entry function body expression contains a call to symbol %d whose variadic parameter has an unsupported slice element type: %v", call.Symbol, err)
	}
	firstVariadic := len(call.Children) - len(variadicIDs)
	elems := make([]string, 0, len(variadicIDs))
	for j, argID := range variadicIDs {
		// A C-convention variadic callee is rejected upstream
		// (validateExternSignature), so a variadic slice element is always
		// built for a Pebble-convention callee and a str element stays a
		// PebbleStr value — cConvention is false by construction here.
		pre, expr, err := buildCallArgument(unit, snapshot, fileSet, call.Symbol, firstVariadic+j, argID, tir.Parameter{Symbol: sliceParam.Symbol, Type: elementType}, false, locals, width)
		if err != nil {
			return "", "", err
		}
		if pre != "" {
			// Defense: an inline slice-construction variadic element cannot
			// be placed — a slice element type is itself rejected above by
			// sliceElementCType, so no supported variadic element can ever
			// produce a pre. Reject rather than silently drop it.
			return "", "", fmt.Errorf("entry function body expression contains a call to symbol %d whose variadic argument %d requires a temp declaration, which a variadic call's collected-element compound literal cannot place", call.Symbol, firstVariadic+j)
		}
		elems = append(elems, expr)
	}
	if len(elems) == 0 {
		return "", fmt.Sprintf("(%s){ .data = NULL, .len = (size_t)0 }", sliceTypeName(sliceType)), nil
	}
	return "", fmt.Sprintf("(%s){ .data = (%s[]){ %s }, .len = (size_t)(%d) }", sliceTypeName(sliceType), elemCType, strings.Join(elems, ", "), len(elems)), nil
}

// buildAggregateArgument builds one call-site argument for a tuple- or
// struct-typed parameter. Two argument shapes are supported (10.25):
//
//   - a plain SymbolValue naming an already-declared aggregate-typed local in
//     scope whose declared type is exactly the parameter's tuple/struct type
//     (wantTuple selects which), emitted as the local's own pebble_local_<symbol>
//     C name — the aggregate's own struct typedef makes passing the whole value
//     by value trivially valid C, so no construction is needed at the call site
//     (this is 10.24's existing supported shape, unchanged);
//   - a freshly-constructed aggregate built inline at the call site — a
//     TupleValue for a tuple parameter (f((1, 2))) or a RecordConstruct for a
//     struct parameter (f(Point.{ x = 1, y = 2 })), both confirmed reachable
//     from real source and both carrying the same Children/Fields/Type shape
//     they have as a local's declaration initializer — emitted as a C99
//     compound-literal expression by buildTupleValueExpr / buildStructValueExpr,
//     which share their brace-list construction with the local-declaration
//     builders. An inline construct whose own Type is not exactly the
//     parameter's type (defense for hand-built IR — the checker coerces every
//     argument to its parameter's type and rejects a mismatch itself) is a clean
//     rejection, so the emitted C never passes a value of the wrong aggregate
//     type to a parameter. Any other argument shape is a clean rejection naming
//     what was found: a SourceAlias-wrapped argument (extra parens, e.g.
//     f(((1, 2)))), a nested aggregate whose element/field types are outside the
//     two supported grammars, or any other node kind. width is the entry's
//     resolved integer width, threaded through to the inline builders so each
//     element/field is built at the width the parameter's own typedef uses.
func buildAggregateArgument(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, argID tir.NodeID, locals map[symbol.SymbolID]localInfo, wantType types.TypeID, wantTuple bool, calleeSymbol symbol.SymbolID, position int, width types.BuiltinKind) (string, error) {
	node, ok := unit.Node(argID)
	if !ok {
		return "", fmt.Errorf("entry function body expression contains a call to symbol %d whose argument %d references invalid node %d", calleeSymbol, position, argID)
	}
	if node.Kind != tir.SymbolValue {
		context := fmt.Sprintf("entry function body expression contains a call to symbol %d whose argument %d", calleeSymbol, position)
		if wantTuple {
			if node.Kind == tir.TupleValue {
				if node.Type != wantType {
					return "", fmt.Errorf("%s is a TupleValue of type %s, not a tuple-typed value of type %s", context, describeType(snapshot, node.Type), tupleTypeName(wantType))
				}
				return buildTupleValueExpr(unit, snapshot, fileSet, node, locals, context, width)
			}
			return "", fmt.Errorf("%s is a %s, want a reference to a tuple-typed local in scope or a tuple literal (a TupleValue); only passing an already-declared tuple-typed local or constructing a fresh tuple literal inline is supported", context, node.Kind)
		}
		if node.Kind == tir.RecordConstruct {
			if node.Type != wantType {
				return "", fmt.Errorf("%s is a RecordConstruct of type %s, not a struct-typed value of type %s", context, describeType(snapshot, node.Type), structTypeName(wantType))
			}
			return buildStructValueExpr(unit, snapshot, fileSet, node, locals, context, width)
		}
		return "", fmt.Errorf("%s is a %s, want a reference to a struct-typed local in scope or a struct literal (a RecordConstruct); only passing an already-declared struct-typed local or constructing a fresh struct literal inline is supported", context, node.Kind)
	}
	info, declared := locals[node.Symbol]
	if !declared {
		return "", fmt.Errorf("entry function body expression contains a call to symbol %d whose argument %d references symbol %d, which is not a local in scope", calleeSymbol, position, node.Symbol)
	}
	if wantTuple {
		if info.tuple != wantType {
			return "", fmt.Errorf("entry function body expression contains a call to symbol %d whose argument %d passes symbol %d, which is a local of type %s, not a tuple-typed local of type %s", calleeSymbol, position, node.Symbol, describeType(snapshot, node.Type), tupleTypeName(wantType))
		}
		return fmt.Sprintf("pebble_local_%d", node.Symbol), nil
	}
	if info.structType != wantType {
		return "", fmt.Errorf("entry function body expression contains a call to symbol %d whose argument %d passes symbol %d, which is a local of type %s, not a struct-typed local of type %s", calleeSymbol, position, node.Symbol, describeType(snapshot, node.Type), structTypeName(wantType))
	}
	return fmt.Sprintf("pebble_local_%d", node.Symbol), nil
}

// buildSliceArgument builds one call-site argument for a slice-typed parameter
// (10.38). Two argument shapes are supported:
//
//   - a plain SymbolValue naming an already-declared slice-typed local in scope
//     whose declared type is exactly the parameter's slice type, emitted as the
//     local's own pebble_local_<symbol> C name — the slice type's own struct
//     typedef makes passing the whole slice by value trivially valid C, so no
//     construction is needed at the call site (confirmed checker-reachable via
//     a real fixture: f(s) passes a plain SymbolValue);
//   - an inline slice construction used directly as a call argument — f(a[1:3]),
//     a bare CheckedSlice, confirmed checker-reachable via a real fixture —
//     emitted as the same two-statement temp-then-construction shape 10.37's
//     local declaration and the return side use (a temp holding the checked-
//     start result, then the compound literal using that temp), with the temp
//     declaration returned as a separate pre-statement for the caller to place
//     before the consuming expression. This is only sound because a C function
//     argument is otherwise a pure expression position with nowhere to place
//     the temp-declaration statement, so the pre is rejected upstream in
//     buildDirectCall's expression-position wrapper (buildDirectCall) and only
//     threaded through when the enclosing call is itself in a leading-statement
//     position (a bare call statement or a local's declaration initializer) —
//     this backend still does not reach for a GNU statement-expression or any
//     other workaround to make an inline construction fit an arbitrary
//     expression position.
//
// Any other argument shape — a local that is not slice-typed, a SourceAlias-
// wrapped argument, or any other node kind — is likewise a clean rejection
// naming what was found, matching buildAggregateArgument's own discipline. The
// returned pre is indent-free (buildSliceConstruction is called with an empty
// indent); the caller prepends its own indent. width is the entry's resolved
// integer width, threaded through so the temp is declared at the correct width.
func buildSliceArgument(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, argID tir.NodeID, locals map[symbol.SymbolID]localInfo, wantType types.TypeID, calleeSymbol symbol.SymbolID, position int, width types.BuiltinKind) (string, string, error) {
	node, ok := unit.Node(argID)
	if !ok {
		return "", "", fmt.Errorf("entry function body expression contains a call to symbol %d whose argument %d references invalid node %d", calleeSymbol, position, argID)
	}
	context := fmt.Sprintf("entry function body expression contains a call to symbol %d whose argument %d", calleeSymbol, position)
	if node.Kind == tir.CheckedSlice {
		if node.Type != wantType {
			return "", "", fmt.Errorf("%s is an inline slice construction (a CheckedSlice) of type %s, not a slice-typed value of type %s", context, describeType(snapshot, node.Type), sliceTypeName(wantType))
		}
		tempDecl, constructionExpr, err := buildSliceConstruction(unit, snapshot, fileSet, node, locals, "", context, width, fmt.Sprintf("pebble_slice_arg_%d", argID))
		if err != nil {
			return "", "", err
		}
		return tempDecl, constructionExpr, nil
	}
	if node.Kind != tir.SymbolValue {
		return "", "", fmt.Errorf("%s is a %s, want a reference to a slice-typed local in scope; only passing an already-declared slice-typed local is supported", context, node.Kind)
	}
	info, declared := locals[node.Symbol]
	if !declared {
		return "", "", fmt.Errorf("%s references symbol %d, which is not a local in scope", context, node.Symbol)
	}
	if info.sliceType != wantType {
		return "", "", fmt.Errorf("%s passes symbol %d, which is a local of type %s, not a slice-typed local of type %s", context, node.Symbol, describeType(snapshot, node.Type), sliceTypeName(wantType))
	}
	return "", fmt.Sprintf("pebble_local_%d", node.Symbol), nil
}

// buildAggregateReturnValue builds the C expression text for a tuple/struct-
// returning function's tail-position return value (10.26). The enclosing
// function's result type comes from result (mutually exclusive tuple /
// structType, set by buildHelperFunctions from the helper's own ResultType),
// and the return-value shapes supported are (all confirmed against real
// fixtures):
//
//   - a plain SymbolValue naming an already-declared aggregate-typed local in
//     scope whose declared type is exactly the function's result type, emitted
//     as the local's own pebble_local_<symbol> C name — forwarding an
//     already-computed aggregate value without re-constructing it;
//   - a freshly-constructed aggregate built inline in the return — a
//     TupleValue (return (20, 22)) or a RecordConstruct (return
//     Point.{ x = 20, y = 22 }), emitted as a C99 compound-literal expression
//     by buildTupleValueExpr / buildStructValueExpr (the same 10.25 expression
//     builders an inline call argument uses), so the return statement emits
//     e.g. `return (pebble_tuple_23_t){ 20, 22 };`;
//   - (struct branch only) a DirectCall to another struct-returning helper
//     (return helperReturningStruct();), the return-forwarding shape io.peb's
//     `return string::new();` uses. The callee's declared ResultType is
//     double-checked against the function's result type (defense for hand-built
//     IR), then the call is built by buildDirectCallWithPre — the same
//     call-building machinery a struct-typed local's call initializer uses. The
//     call's argument building may require a temp-declaration pre-statement (an
//     inline slice-construction argument); since a return is a pure expression
//     position with nowhere to place that statement, the pre is returned
//     separately for the caller to thread into its statement sequence before the
//     final `return <expr>;` line, the same (pre, expr) convention
//     buildSliceReturnValue and buildScalarInitializeCore use.
//
// The function returns a (pre, expr) pair: expr is the return value's C
// expression and pre is the optional pre-statement the caller must emit BEFORE
// the `return <expr>;` line (empty for every shape but a DirectCall with a
// construction-needing argument). pre carries its own leading indent (indent,
// threaded in from the caller, is prepended to it), matching how
// buildSliceReturnValue returns its pre-indented temp declaration. An inline
// construct whose own Type is not exactly the function's result type (defense
// for hand-built IR — the checker coerces every return value to the function's
// declared result type) is a clean rejection, so the emitted C never returns a
// value of the wrong aggregate type; the struct branch's DirectCall shape is
// likewise rejected when the callee's declared ResultType differs from the
// function's result type. Any other return-value shape is a clean rejection
// naming what was found — including a DirectCall in the tuple branch, where
// only the SymbolValue and TupleValue shapes above are supported. width is the
// entry's resolved integer width, threaded through to the inline builders so
// each element/field is built at the width the result type's own typedef uses.
func buildAggregateReturnValue(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, id tir.NodeID, locals map[symbol.SymbolID]localInfo, result resultInfo, indent string, width types.BuiltinKind) (string, string, error) {
	node, ok := unit.Node(id)
	if !ok {
		return "", "", fmt.Errorf("entry function body return statement references invalid value node %d", id)
	}
	if node.Kind == tir.SymbolValue {
		info, declared := locals[node.Symbol]
		if !declared {
			return "", "", fmt.Errorf("entry function body return statement returns symbol %d, which is not a local in scope", node.Symbol)
		}
		if result.tuple != 0 {
			if info.tuple != result.tuple {
				return "", "", fmt.Errorf("entry function body return statement returns symbol %d, which is a local of type %s, not a tuple-typed local of type %s", node.Symbol, describeType(snapshot, node.Type), tupleTypeName(result.tuple))
			}
			return "", fmt.Sprintf("pebble_local_%d", node.Symbol), nil
		}
		if info.structType != result.structType {
			return "", "", fmt.Errorf("entry function body return statement returns symbol %d, which is a local of type %s, not a struct-typed local of type %s", node.Symbol, describeType(snapshot, node.Type), structTypeName(result.structType))
		}
		return "", fmt.Sprintf("pebble_local_%d", node.Symbol), nil
	}
	if result.tuple != 0 {
		context := "entry function body return statement"
		if node.Kind == tir.TupleValue {
			if node.Type != result.tuple {
				return "", "", fmt.Errorf("%s returns a TupleValue of type %s, not a tuple-typed value of type %s", context, describeType(snapshot, node.Type), tupleTypeName(result.tuple))
			}
			expr, err := buildTupleValueExpr(unit, snapshot, fileSet, node, locals, context, width)
			return "", expr, err
		}
		return "", "", fmt.Errorf("%s returns a %s, want a reference to a tuple-typed local in scope or a tuple literal (a TupleValue); only returning an already-declared tuple-typed local or constructing a fresh tuple literal inline is supported", context, node.Kind)
	}
	context := "entry function body return statement"
	if node.Kind == tir.RecordConstruct {
		if node.Type != result.structType {
			return "", "", fmt.Errorf("%s returns a RecordConstruct of type %s, not a struct-typed value of type %s", context, describeType(snapshot, node.Type), structTypeName(result.structType))
		}
		expr, err := buildStructValueExpr(unit, snapshot, fileSet, node, locals, context, width)
		return "", expr, err
	}
	if node.Kind == tir.DirectCall {
		calleeDecl, err := findCallDeclaration(unit, node)
		if err != nil {
			return "", "", err
		}
		if calleeDecl.ResultType != result.structType {
			return "", "", fmt.Errorf("%s returns a call to symbol %d whose declared result type %s does not match the function's result type %s", context, node.Symbol, describeType(snapshot, calleeDecl.ResultType), structTypeName(result.structType))
		}
		callPre, callExpr, err := buildDirectCallWithPre(unit, snapshot, fileSet, node, locals, width)
		if err != nil {
			return "", "", err
		}
		if callPre != "" {
			callPre = indent + callPre
		}
		return callPre, callExpr, nil
	}
	return "", "", fmt.Errorf("%s returns a %s, want a reference to a struct-typed local in scope, a struct literal (a RecordConstruct), or a call to a struct-returning helper (a DirectCall); only returning an already-declared struct-typed local, constructing a fresh struct literal inline, or forwarding a struct-returning helper call is supported", context, node.Kind)
}

func buildArrayReturnValue(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, id tir.NodeID, locals map[symbol.SymbolID]localInfo, arrayType types.TypeID, width types.BuiltinKind) (string, error) {
	node, ok := unit.Node(id)
	if !ok {
		return "", fmt.Errorf("array return references invalid value node %d", id)
	}
	if node.Type != arrayType {
		return "", fmt.Errorf("array return has type %s, want %s", describeType(snapshot, node.Type), describeType(snapshot, arrayType))
	}
	if node.Kind == tir.DirectCall {
		return buildDirectCall(unit, snapshot, fileSet, node, locals, width)
	}
	if node.Kind != tir.SymbolValue {
		return "", fmt.Errorf("array return is a %s, want an array local or array-returning call", node.Kind)
	}
	info, declared := locals[node.Symbol]
	if !declared || info.array != arrayType {
		return "", fmt.Errorf("array return references symbol %d, which is not a local of type %s", node.Symbol, describeType(snapshot, arrayType))
	}
	if info.arrayWrapped {
		return fmt.Sprintf("pebble_local_%d", node.Symbol), nil
	}
	key, ok := snapshot.Key(arrayType)
	if !ok {
		return "", fmt.Errorf("array return type %s is not in the type snapshot", describeType(snapshot, arrayType))
	}
	length, _, ok := key.Array()
	if !ok {
		return "", fmt.Errorf("array return type %s has no length and element type", describeType(snapshot, arrayType))
	}
	values := make([]string, length)
	for i := range values {
		values[i] = fmt.Sprintf("pebble_local_%d[%d]", node.Symbol, i)
	}
	return fmt.Sprintf("(%s){ .data = { %s } }", arrayTypeName(arrayType), strings.Join(values, ", ")), nil
}

// buildSliceReturnValue builds the C text pieces for a slice-returning
// function's tail-position return (10.38). The enclosing function's result
// type comes from result.sliceType (set by buildHelperFunctions from the
// helper's own ResultType), and exactly two return-value shapes are supported
// (both confirmed against real fixtures):
//
//   - a plain SymbolValue naming an already-declared slice-typed local — or a
//     slice-typed parameter, which seeds the callee's scope identically — in
//     scope whose declared type is exactly the function's result type, emitted
//     as the local's own pebble_local_<symbol> C name: forwarding an
//     already-computed slice value without re-constructing it, a
//     single-statement return (preReturn is empty);
//   - a fresh CheckedSlice construction (`return a[1:3];`, whose tail Return
//     child is the bare CheckedSlice node — confirmed against a real fixture).
//     This is not a single expression: the construction needs the same
//     two-statement temp-then-construction shape 10.37's local declaration
//     uses (a temp holding the checked-start result, then the compound literal
//     using that temp), but a return is a pure expression position with nowhere
//     to place the temp-declaration statement, so the temp declaration is
//     returned as a separate pre-return statement text for the caller
//     (buildBlock / buildSwitchCaseBody) to thread into its statement sequence
//     before the final `return <expr>;` line — the same mechanical shape
//     deferred statements already demonstrate, just for construction complexity
//     rather than deferred cleanup.
//
// A slice-returning helper's result may not itself be another slice-returning
// call (`return g();`, a DirectCall) — that position is confirmed reachable
// for tuple/struct returns and rejected there (10.26), and a slice call in a
// return position is a clean rejection naming what was found here. Any other
// return-value shape is likewise a clean rejection. indent indents the temp
// declaration to match the surrounding statement text. width is the entry's
// resolved integer width, threaded through so the temp is declared at the
// correct width (the i64-entry width bug found and fixed in 10.37's review).
func buildSliceReturnValue(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, id tir.NodeID, locals map[symbol.SymbolID]localInfo, result resultInfo, indent string, width types.BuiltinKind) (string, string, error) {
	node, ok := unit.Node(id)
	if !ok {
		return "", "", fmt.Errorf("entry function body return statement references invalid value node %d", id)
	}
	if node.Kind == tir.SymbolValue {
		info, declared := locals[node.Symbol]
		if !declared {
			return "", "", fmt.Errorf("entry function body return statement returns symbol %d, which is not a local in scope", node.Symbol)
		}
		if info.sliceType != result.sliceType {
			return "", "", fmt.Errorf("entry function body return statement returns symbol %d, which is a local of type %s, not a slice-typed local of type %s", node.Symbol, describeType(snapshot, node.Type), sliceTypeName(result.sliceType))
		}
		return "", fmt.Sprintf("pebble_local_%d", node.Symbol), nil
	}
	context := "entry function body return statement"
	if node.Kind == tir.CheckedSlice {
		if node.Type != result.sliceType {
			return "", "", fmt.Errorf("%s returns a CheckedSlice of type %s, not a slice-typed value of type %s", context, describeType(snapshot, node.Type), sliceTypeName(result.sliceType))
		}
		// The temp name derives from the return value node's own NodeID — the
		// only stable identity in hand here (a return has no local symbol to
		// name it from), distinct from the pebble_slice_start_<symbol> temps a
		// slice local's declaration uses so the two can never collide even when
		// a symbol ID numerically equals a node ID.
		tempDecl, constructionExpr, err := buildSliceConstruction(unit, snapshot, fileSet, node, locals, indent, context, width, fmt.Sprintf("pebble_slice_ret_%d", id))
		if err != nil {
			return "", "", err
		}
		return tempDecl, constructionExpr, nil
	}
	if node.Kind == tir.SliceFromRaw {
		if node.Type != result.sliceType {
			return "", "", fmt.Errorf("%s returns a SliceFromRaw of type %s, not %s", context, describeType(snapshot, node.Type), sliceTypeName(result.sliceType))
		}
		construction, err := buildRawSliceConstruction(unit, snapshot, fileSet, node, locals, width, context)
		return "", construction, err
	}
	return "", "", fmt.Errorf("%s returns a %s, want a reference to a slice-typed local in scope or a fresh slice construction (a CheckedSlice); only returning an already-declared slice-typed local or constructing a fresh slice from an array inline is supported", context, node.Kind)
}

// buildOptionalValue builds the C expression text for an optional-typed value
// in the one position-neutral grammar both an optional-returning function's
// tail-position return and a call site's optional-typed argument share: the
// target optional type comes in as optionalType (a return passes
// result.optionalType; a call site passes its parameter's type), and exactly
// four value shapes are supported (all confirmed against real fixtures):
//
//   - a plain SymbolValue naming an already-declared optional-typed local — or
//     an optional-typed parameter, which seeds the callee's scope identically —
//     in scope whose declared type is exactly the target optional type, emitted
//     as the local's own pebble_local_<symbol> C name: forwarding an
//     already-computed optional value without re-constructing it;
//   - a fresh SomeOptional / NoneOptional / OptionalInject construction of the
//     matching optional type (`some x;`, `none`, or an implicit injection — at
//     a call site the checker wraps even a scalar implicit injection like `g(5)`
//     in an OptionalInject node, while at a return position a scalar payload's
//     injection arrives as the bare payload value itself, the fourth shape
//     below; a tuple/struct payload's implicit injection is an OptionalInject
//     in both positions), built by the shared buildOptionalValueExpr (the
//     same builder a nested-in-aggregate optional value uses), which handles
//     every supported payload grammar and emits the optional's own compound
//     literal;
//   - a DirectCall to another optional-returning helper (`return g();` /
//     `g(f());` — the call node carries the optional result type, so it is
//     built by the same buildDirectCall machinery any call uses and forwarded
//     as the value, confirmed reachable from real source);
//   - a bare payload value whose Type is the optional's payload type rather
//     than the optional type itself — the scalar implicit-injection shape the
//     checker produces for `return 5;` in an optional-returning helper (the
//     payload is injected without an OptionalInject wrapper, unlike the
//     aggregate shapes above) — injected here as a fresh
//     `(pebble_optional_<typeID>_t){ .has_value = true, .value = <payload> }`
//     compound literal, the payload built by the grammar its own type selects.
//
// Any other value shape is a clean rejection. context names the position in
// rejection messages ("entry function body return statement" for a return, or
// "entry function body expression contains a call to symbol <N> whose
// parameter <M>" for a call argument). width is the entry's
// resolved integer width, threaded through to the inline builders so each
// payload is built at the width the target type's own typedef uses.
func buildOptionalValue(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, id tir.NodeID, locals map[symbol.SymbolID]localInfo, optionalType types.TypeID, context string, width types.BuiltinKind) (string, error) {
	node, ok := unit.Node(id)
	if !ok {
		return "", fmt.Errorf("%s references invalid value node %d", context, id)
	}
	if node.Kind == tir.SymbolValue {
		info, declared := locals[node.Symbol]
		if !declared {
			return "", fmt.Errorf("%s names symbol %d, which is not a local in scope", context, node.Symbol)
		}
		if info.optional != optionalType {
			return "", fmt.Errorf("%s names symbol %d, which is a local of type %s, not an optional-typed local of type %s", context, node.Symbol, describeType(snapshot, node.Type), optionalTypeName(optionalType))
		}
		return fmt.Sprintf("pebble_local_%d", node.Symbol), nil
	}
	if node.Kind == tir.SomeOptional || node.Kind == tir.NoneOptional || node.Kind == tir.OptionalInject {
		// A fresh optional value built inline: `some x;`, `none`, or an
		// implicit injection `(1, 2);` / a call site's `g(5)` (OptionalInject).
		// buildOptionalValueExpr shares the SomeOptional
		// and OptionalInject cases — both carry exactly one payload child
		// and lower to the identical C — and handles the NoneOptional
		// zero-child form.
		return buildOptionalValueExpr(unit, snapshot, fileSet, node, locals, context, width)
	}
	if node.Kind == tir.DirectCall {
		// A forward of another optional-returning helper's result:
		// `return g();` or `g(f());`. The call's own result type is validated
		// by validateHelperSignature (the callee's declared ResultType must be
		// an optional type) and the checker coerces the value to the target
		// optional type, so a plain buildDirectCall of
		// the call node is the whole lowering — the call already returns
		// the optional's own C type.
		return buildDirectCall(unit, snapshot, fileSet, node, locals, width)
	}
	// The remaining supported shape is the bare scalar payload the checker
	// produces for implicit injection in a return (`return 5;` in a ?int
	// helper): the value node carries the payload type, not the optional
	// type. Inject it here — the same C a SomeOptional/OptionalInject of
	// this payload would emit, with the payload built by the grammar its own
	// type selects.
	key, ok := snapshot.Key(optionalType)
	if !ok {
		return "", fmt.Errorf("%s names a value of type %s whose target optional type %d is not in the type snapshot", context, describeType(snapshot, node.Type), optionalType)
	}
	payload, ok := key.Child()
	if !ok {
		return "", fmt.Errorf("%s names a value of type %s whose target optional type %s has no payload type", context, describeType(snapshot, node.Type), optionalTypeName(optionalType))
	}
	if node.Type != payload {
		return "", fmt.Errorf("%s names a %s, want an optional-typed value of type %s (a reference to an optional-typed local, some/none, a call to an optional-returning helper) or a payload value of type %s", context, describeType(snapshot, node.Type), optionalTypeName(optionalType), describeType(snapshot, payload))
	}
	var value string
	var err error
	payloadWidth, integerPayload := resolvedBuiltin(snapshot, payload)
	switch {
	case integerPayload && cType(payloadWidth) != "" && !isUint(snapshot, payload):
		value, err = buildExpr(unit, snapshot, fileSet, id, locals, payloadWidth, width)
	case isUint(snapshot, payload):
		value, err = buildUintExpr(unit, snapshot, fileSet, id, locals, width)
	case isBool(snapshot, payload):
		value, err = buildBoolExpr(unit, snapshot, fileSet, id, locals, width)
	case isPointer(snapshot, payload):
		// A bare pointer payload implicitly injected into a ?*T optional (a
		// return/argument whose payload value is the pointer itself, e.g. a
		// `return &y;` inside a ?*int helper with no explicit some keyword) —
		// built by the same buildExpr pointer path any pointer-typed value
		// takes, whose isPointer bypass ignores the ambient width args.
		value, err = buildExpr(unit, snapshot, fileSet, id, locals, width, width)
	default:
		return "", fmt.Errorf("%s implicitly injects a payload value of type %s, want a fixed-width integer or bool", context, describeType(snapshot, payload))
	}
	if err != nil {
		return "", err
	}
	return fmt.Sprintf("(%s){ .has_value = true, .value = %s }", optionalTypeName(optionalType), value), nil
}

// buildBoolExpr builds the C text for a bool value node, used both for a bool
// local's initializer/reassignment value and for a bare bool if/while
// condition (via buildCondition). The bool grammar is genuinely different from
// the integer one buildExpr handles: there is no checked arithmetic — bools
// are combined, compared, and negated with plain C, which cannot fault — so it
// is a separate builder rather than a mode on buildExpr. width is the entry's
// resolved integer width, threaded through to the comparison path so a
// comparison used as a bool value's operand builds its own integer operands at
// the entry's width. It accepts exactly seven node kinds, each carrying the
// snapshot's bool builtin:
//
//   - BoolLiteral — the C literal true/false (requires #include <stdbool.h>).
//
//   - SymbolValue whose Symbol is a bool local in scope (the locals map
//     records types.Bool for it) — pebble_local_<symbol ID>, the same C name
//     buildLeadingStatement gave that local's declaration.
//
//   - PrefixValue with operator ! (syntax.Bang, confirmed against a real
//     fixture — a bool `!` is a PrefixValue, not the CheckedNegate integer
//     negation uses) and exactly one operand that is itself a bool value in
//     this grammar — !(<operand>), plain C negation. The operand is built by
//     recursing into this same builder, so a negated comparison (e.g.
//     !(i < 5)) is now accepted: its operand is a SourceAlias wrapping a
//     BinaryValue, both handled below.
//
//   - BinaryValue with one of the six comparison operators — delegated to
//     buildComparison, the same path a top-level if/while condition uses, so
//     a comparison can serve as an operand of && / || as well as stand alone.
//     buildComparison decides the operand grammar from the operands' resolved
//     types: integer operands take the integer comparison path, and two bool
//     operands — e.g. (1 < 2) == (3 < 4), whose SourceAlias-wrapped
//     comparison operands are bool values — take the bool-equality path.
//     (A BinaryValue with any other operator is rejected by buildComparison's
//     operator check.)
//
//   - ShortCircuitValue with operator && (syntax.LogicalAnd) or ||
//     (syntax.LogicalOr) — <(left) && (right)> / <(left) || (right)>,
//     parenthesized so nested combinations produce unambiguous C regardless of
//     depth. Both operands are built by recursing into this same builder, so
//     && and || combine literals, bool locals, ! negations, comparisons,
//     bool-returning calls, and nested && / || freely. Plain C && and || are
//     the correct lowering: both languages short-circuit the same way, so a
//     call operand is evaluated exactly when Pebble would evaluate it (a
//     skipped right operand is skipped in both languages). The operand tree
//     already encodes the language's &&-vs-|| precedence (confirmed: Pebble's
//     grammar gives || precedence 1 and && precedence 2), so this builder
//     never re-derives precedence.
//
//   - SourceAlias — a transparent wrapper (the grouped-expression parens), so
//     it is unwrapped and its single child built by recursing into this same
//     builder. A parenthesized comparison operand of && / || is exactly this
//     shape (confirmed against a real fixture: flag && (1 < 2) has the
//     comparison wrapped in a SourceAlias, while the unparenthesized
//     1 < 2 && 3 < 4 wraps nothing).
//
//   - Load of a TuplePlace — a tuple-typed local's bool element read (`t.1`
//     in a bool position), the same Load(TuplePlace) shape buildExpr's Load
//     case handles but with the element's own type gated to bool here, so the
//     read emits pebble_local_<symbol>._<ordinal> via buildTupleElement. (A
//     plain bool local read is a SymbolValue, not a Load.)
//
//   - DirectCall / MethodCall — a call to a bool-returning helper function or
//     method used directly as a bool value (confirmed checker-reachable: a
//     plain helper with a bool result type is admitted by
//     validateHelperSignature, so `if id(true) { ... }` lowers the condition
//     to a bool-typed DirectCall, and a bool-returning method call lowers to a
//     bool-typed MethodCall). The call is built by the same buildDirectCall
//     machinery a scalar-width call uses, and its result is a C bool, which
//     this position accepts directly.
//
// A SymbolValue referencing anything else — an integer local, a global, a
// parameter — and any other node kind at any position is a clean rejection
// naming what was found.
func buildBoolExpr(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, id tir.NodeID, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind) (string, error) {
	node, ok := unit.Node(id)
	if !ok {
		return "", fmt.Errorf("entry function body expression references invalid node %d", id)
	}
	if !isBool(snapshot, node.Type) {
		return "", fmt.Errorf("entry function body expression contains a %s of type %s, want bool", node.Kind, describeType(snapshot, node.Type))
	}
	switch node.Kind {
	case tir.BoolLiteral:
		if node.Literal.Bool {
			return "true", nil
		}
		return "false", nil
	case tir.SymbolValue:
		if locals[node.Symbol].kind != types.Bool {
			return "", fmt.Errorf("entry function body expression references symbol %d, which is not a bool local declared earlier in the entry body", node.Symbol)
		}
		return fmt.Sprintf("pebble_local_%d", node.Symbol), nil
	case tir.DirectCall, tir.MethodCall:
		return buildDirectCall(unit, snapshot, fileSet, node, locals, width)
	case tir.IndirectCall:
		// A call through a function-typed value whose result is bool
		// (confirmed checker-reachable: `if f(true) { ... }` lowers the
		// condition to a bool-typed IndirectCall). The call itself is built by
		// the same buildIndirectCall machinery a scalar-width indirect call
		// uses — the callee and every argument are built under the callee's
		// own function type — and the result is a C bool, which this position
		// accepts directly.
		return buildIndirectCall(unit, snapshot, fileSet, node, locals, width)
	case tir.CheckedOptionalUnwrap:
		// A force-unwrap of an optional-typed local with a bool payload (x!).
		// The child is a SymbolValue naming the optional local, and this
		// node's Type is bool (already gated above). The unwrap is
		// bounds-checked via the runtime helper, passing the optional local's
		// has_value and value fields.
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap with %d child(ren), want exactly one (the optional value being unwrapped)", len(node.Children))
		}
		child, ok := unit.Node(node.Children[0])
		if !ok {
			return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap referencing invalid child node %d", node.Children[0])
		}
		if child.Kind == tir.Load && len(child.Children) == 1 {
			if _, ok := unit.Node(child.Children[0]); !ok {
				return "", fmt.Errorf("invalid optional place")
			}
			expr, typ, err := buildPlaceLValue(unit, snapshot, fileSet, child.Children[0], locals, width)
			if err != nil {
				return "", err
			}
			if !isOptional(snapshot, typ) {
				return "", fmt.Errorf("optional unwrap base is not optional")
			}
			return fmt.Sprintf("pebble_rt_checked_unwrap_%s(%s.has_value, %s.value, %s)", checkedSuffix(width), expr, expr, buildSourceLoc(fileSet, node.Span)), nil
		}
		if child.Kind != tir.SymbolValue {
			return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap whose child is a %s, want a SymbolValue naming an optional-typed local", child.Kind)
		}
		info, declared := locals[child.Symbol]
		if !declared {
			return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap referencing symbol %d, which is not a local declared earlier in the entry body", child.Symbol)
		}
		if info.optional == 0 {
			return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap of symbol %d, which is not an optional-typed local", child.Symbol)
		}
		return fmt.Sprintf("pebble_rt_checked_unwrap_bool(pebble_local_%d.has_value, pebble_local_%d.value, %s)", child.Symbol, child.Symbol, buildSourceLoc(fileSet, node.Span)), nil
	case tir.Load:
		// A tuple-typed local's bool element read or a struct-typed local's
		// bool field read (see buildExpr's Load case for the shape
		// confirmation; here the Load's Type is the element/field's bool type,
		// already gated above).
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains a Load with %d child(ren), want exactly one place", len(node.Children))
		}
		place, ok := unit.Node(node.Children[0])
		if !ok {
			return "", fmt.Errorf("entry function body expression contains a Load referencing invalid place node %d", node.Children[0])
		}
		if place.Kind != tir.TuplePlace {
			if place.Kind == tir.CheckedIndexPlace {
				return buildArrayPlaceRead(unit, snapshot, fileSet, place, locals, width, true)
			}
			if place.Kind == tir.FieldPlace {
				return buildStructFieldRead(unit, snapshot, fileSet, place, locals, width, true)
			}
			if place.Kind == tir.DereferencePlace {
				return buildDereferencePlaceRead(unit, snapshot, fileSet, place, locals, width, node.Span, true)
			}
			return "", fmt.Errorf("entry function body expression contains a Load whose place is a %s, want a TuplePlace, CheckedIndexPlace, FieldPlace, or DereferencePlace", place.Kind)
		}
		return buildTuplePlaceRead(unit, snapshot, fileSet, place, locals, width, true)
	case tir.CheckedIndex:
		// A bare CheckedIndex in a pure bool-expression position — a
		// slice-element read of a bool-typed value with no addressable place
		// (`boolview()[0]` as a condition, a && / || operand, or a call
		// argument), the bool twin of buildExpr's CheckedIndex case. Same
		// pre-threading rule: a pure-projection base (a slice-typed local or
		// place, which needs no temp) is emitted directly; a freshly-computed
		// base (a call result) needs a temp-declaration statement this
		// position has nowhere to place and is a clean rejection — the
		// positions that CAN host a temp (a print, a local declaration, a
		// return) intercept the shape before reaching this case.
		strBase, err := checkedIndexBaseIsStr(unit, snapshot, node)
		if err != nil {
			return "", err
		}
		if strBase {
			return "", fmt.Errorf("entry function body expression contains a str index whose result type is %s, want bool", describeType(snapshot, node.Type))
		}
		pre, read, err := buildSliceIndexValue(unit, snapshot, fileSet, id, node, locals, width, true)
		if err != nil {
			return "", err
		}
		if pre != "" {
			base, ok := unit.Node(node.Children[0])
			if !ok {
				return "", fmt.Errorf("entry function body expression contains a CheckedIndex referencing invalid base node %d", node.Children[0])
			}
			return "", fmt.Errorf("entry function body expression indexes a %s of type %s in a pure-expression position with nowhere to place the temp-declaration statement the freshly-computed slice value needs; bind the slice into a local first", base.Kind, describeType(snapshot, base.Type))
		}
		return read, nil
	case tir.TupleElementValue:
		// Defense for hand-built IR, exactly like buildExpr's TupleElementValue
		// case: the checker never produces this shape for a bool element read of
		// a tuple local (that is a Load of a TuplePlace); a TupleElementValue
		// whose single child is a SymbolValue naming a tuple-typed local is
		// accepted here, anything else is a clean rejection.
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains a TupleElementValue with %d child(ren), want exactly one (the tuple value being indexed)", len(node.Children))
		}
		base, ok := unit.Node(node.Children[0])
		if !ok {
			return "", fmt.Errorf("entry function body expression contains a TupleElementValue referencing invalid node %d", node.Children[0])
		}
		if base.Kind != tir.SymbolValue {
			return "", fmt.Errorf("entry function body expression reads element %d of a %s, want a SymbolValue naming a tuple-typed local (indexing a tuple literal is not supported)", node.Ordinal, base.Kind)
		}
		return buildTupleElement(unit, snapshot, base.Symbol, node.Ordinal, locals, width, true)
	case tir.PrefixValue:
		if node.Operator != syntax.Bang {
			return "", fmt.Errorf("entry function body expression contains a PrefixValue with operator %s, want !", node.Operator)
		}
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains a PrefixValue with %d operand(s), want exactly one", len(node.Children))
		}
		child, err := buildBoolExpr(unit, snapshot, fileSet, node.Children[0], locals, width)
		if err != nil {
			return "", err
		}
		return "!(" + child + ")", nil
	case tir.BinaryValue:
		// A comparison used as a bool value (an operand of && / ||, or the
		// condition routed here by buildCondition) is the same BinaryValue
		// shape buildComparison already lowers for a top-level condition, so it
		// is delegated unchanged. Non-comparison operators and non-integer
		// operands are rejected by buildComparison itself.
		return buildComparison(unit, snapshot, fileSet, id, locals, width)
	case tir.ShortCircuitValue:
		if len(node.Children) != 2 {
			return "", fmt.Errorf("entry function body expression contains a ShortCircuitValue with %d operand(s), want exactly two", len(node.Children))
		}
		op, ok := shortCircuitOperator(node.Operator)
		if !ok {
			return "", fmt.Errorf("entry function body expression contains a ShortCircuitValue with operator %s, want && or ||", node.Operator)
		}
		left, err := buildBoolExpr(unit, snapshot, fileSet, node.Children[0], locals, width)
		if err != nil {
			return "", err
		}
		right, err := buildBoolExpr(unit, snapshot, fileSet, node.Children[1], locals, width)
		if err != nil {
			return "", err
		}
		return "(" + left + " " + op + " " + right + ")", nil
	case tir.SourceAlias:
		// A SourceAlias is transparent — it records grouped-expression parens
		// and nothing else — so it is unwrapped and its single child built.
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains a SourceAlias with %d child(ren), want exactly one", len(node.Children))
		}
		return buildBoolExpr(unit, snapshot, fileSet, node.Children[0], locals, width)
	default:
		return "", fmt.Errorf("entry function body expression contains a %s, want a bool literal, a reference to a bool local declared earlier in the body, a comparison, a && / || combination, or a ! negation", node.Kind)
	}
}

// buildFloatExpr builds one float value in a position that accepts a float
// expression, the float grammar's counterpart of buildBoolExpr: the float
// builtins' scalar shapes, built on top of the same locals/scope machinery
// buildExpr uses. Stage A supports exactly three node kinds, and no others,
// corresponding to declaring a float local, reading it, and (for a float-
// returning main) returning it:
//
//   - tir.FloatLiteral — a float literal (e.g. 3.14), emitted as its C
//     float/double constant text verbatim (the checker's validated decimal
//     text is already a valid C floating constant — a decimal point and/or
//     exponent are always present — and needs no suffix for either float
//     width, since assigning a double constant to a float is not a warning
//     under this suite's -Wall -Wextra -Werror). The text is defensively
//     re-validated before being trusted, mirroring how buildExpr's
//     IntegerLiteral case validates its own literal text.
//   - tir.SymbolValue — a reference to an in-scope float-typed local of the
//     same float kind, emitted as pebble_local_<symbolID> (the reader of a
//     float local).
//   - tir.SourceAlias — Pebble's grouped-expression parens, transparently
//     unwrapped (exactly one child), the same distinction buildExpr and
//     buildBoolExpr make for parenthesized float expressions.
//
// Width must be one of the two float builtins (F32 for an f32 position, F64
// for an f64 position) and every node in an accepted expression tree must
// carry exactly that builtin — a node carrying the other float kind, or a
// non-float value, is a clean rejection naming the wanted kind, never a
// coercion. Since this slice widened float helper parameters and results, a
// DirectCall/MethodCall to a float-returning helper is a supported float
// value (a float local's call initializer, a float call argument, a float
// comparison operand, a print operand, or a float-returning helper's
// tail-position return forward) built by the same buildDirectCall machinery a
// scalar-width call uses. Float arithmetic, comparisons, and casts are
// likewise supported. Shared by the positions a float value can appear in: a
// float local's declaration initializer (buildScalarInitializeCore), a float
// local's reassignment (buildStoreCore), a float call argument
// (buildCallArgument), a float comparison operand (buildComparison), a print
// operand (buildPrint), and a float-returning entry's or helper's
// tail-position return value (buildBlock / buildSwitchCaseBody dispatch on
// resultInfo.kind).
func buildFloatExpr(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, id tir.NodeID, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind) (string, error) {
	node, ok := unit.Node(id)
	if !ok {
		return "", fmt.Errorf("entry function body expression references invalid node %d", id)
	}
	if !isFloat(snapshot, node.Type) {
		return "", fmt.Errorf("entry function body expression contains a %s of type %s, want %s", node.Kind, describeType(snapshot, node.Type), wantName(width))
	}
	builtin, _ := resolvedBuiltin(snapshot, node.Type)
	if builtin != width {
		return "", fmt.Errorf("entry function body expression contains a %s of type %s, want %s", node.Kind, describeType(snapshot, node.Type), wantName(width))
	}
	switch node.Kind {
	case tir.FloatLiteral:
		text := node.Literal.Float
		if !isValidFloatLiteralText(text) {
			return "", fmt.Errorf("entry function body expression contains a float literal with malformed text %q", text)
		}
		return text, nil
	case tir.SymbolValue:
		info, declared := locals[node.Symbol]
		if !declared || info.kind != width {
			return "", fmt.Errorf("entry function body expression references symbol %d, which is not a %s local declared earlier in the body", node.Symbol, wantName(width))
		}
		return fmt.Sprintf("pebble_local_%d", node.Symbol), nil
	case tir.SourceAlias:
		// A SourceAlias is transparent — it records grouped-expression parens
		// and nothing else — so it is unwrapped and its single child built.
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains a SourceAlias with %d child(ren), want exactly one", len(node.Children))
		}
		return buildFloatExpr(unit, snapshot, fileSet, node.Children[0], locals, width)
	case tir.PrefixValue:
		// A unary minus on a float (`-x`, `-3.5`) arrives as a PrefixValue
		// with operator -: the checker only lowers a negate to CheckedNegate
		// when an operand is an integer, so a float negate is a PrefixValue.
		// Float negation is a defined C operation (the IEEE 754 sign flip;
		// only the -0.0 boundary is sign-flipped, which is exactly the
		// intended semantics), so the plain C `-` operator is a direct,
		// correct lowering of the built child.
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains a PrefixValue with %d operand(s), want exactly one", len(node.Children))
		}
		if node.Operator != syntax.Minus {
			return "", fmt.Errorf("entry function body expression contains a PrefixValue with operator %s, want -", node.Operator)
		}
		child, err := buildFloatExpr(unit, snapshot, fileSet, node.Children[0], locals, width)
		if err != nil {
			return "", err
		}
		return "(-" + child + ")", nil
	case tir.DirectCall, tir.MethodCall:
		// A call to a float-returning helper used directly as a float value
		// (a float local's call initializer, a float call argument, a float
		// comparison operand, a print operand, or a float-returning helper's
		// tail-position return forward). The call node's own Type is the
		// callee's resolved result type, gated to the same float kind above;
		// the call is built by the same buildDirectCall machinery a
		// scalar-width call uses, so context and argument handling are
		// identical.
		return buildDirectCall(unit, snapshot, fileSet, node, locals, width)
	case tir.BinaryValue:
		if len(node.Children) != 2 {
			return "", fmt.Errorf("entry function body float arithmetic has %d operands, want exactly two", len(node.Children))
		}
		op, ok := arithmeticOperator(node.Operator)
		if !ok || node.Operator == syntax.Percent {
			return "", fmt.Errorf("entry function body float arithmetic uses operator %s, want +, -, *, or /", node.Operator)
		}
		left, err := buildFloatExpr(unit, snapshot, fileSet, node.Children[0], locals, width)
		if err != nil {
			return "", err
		}
		right, err := buildFloatExpr(unit, snapshot, fileSet, node.Children[1], locals, width)
		if err != nil {
			return "", err
		}
		return "(" + left + " " + op + " " + right + ")", nil
	case tir.IntegerToFloat:
		// An integer value cast to a float (`x as f64` where x is an integer).
		// The result is a float (this node's Type is the destination float
		// builtin, already gated to width above), and the single child is the
		// integer being cast. The child is built via buildExpr — NOT
		// buildFloatExpr — at its own resolved integer width, mirroring how
		// buildExpr's IntegerCast case resolves the child's own width
		// independently of the ambient width, because a cast's whole point is
		// that its operand's width differs from it. The lowering is a plain,
		// unchecked C cast `(<destination float type>)(<child>)`: C's
		// integer-to-float conversion is well-defined for every input (no
		// undefined behavior, no range fault), so — exactly like IntegerCast —
		// no checked runtime primitive is needed.
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains an IntegerToFloat with %d children, want exactly one", len(node.Children))
		}
		destination, ok := snapshot.Key(node.Type)
		if !ok {
			return "", fmt.Errorf("entry function body expression contains an IntegerToFloat with invalid destination type %d", node.Type)
		}
		destinationWidth, ok := destination.Builtin()
		if !ok || floatCType(destinationWidth) == "" {
			return "", fmt.Errorf("entry function body expression contains an IntegerToFloat with non-float destination type %s", describeType(snapshot, node.Type))
		}
		child, ok := unit.Node(node.Children[0])
		if !ok {
			return "", fmt.Errorf("entry function body expression contains an IntegerToFloat referencing invalid child node %d", node.Children[0])
		}
		childType, ok := snapshot.Key(child.Type)
		if !ok {
			return "", fmt.Errorf("entry function body IntegerToFloat child has invalid type %d", child.Type)
		}
		childWidth, ok := childType.Builtin()
		if !ok || cType(childWidth) == "" {
			return "", fmt.Errorf("entry function body IntegerToFloat child has non-integer type %s", describeType(snapshot, child.Type))
		}
		childExpr, err := buildExpr(unit, snapshot, fileSet, node.Children[0], locals, childWidth, width)
		if err != nil {
			return "", fmt.Errorf("entry function body integer-to-float cast child: %v", err)
		}
		return "(" + floatCType(destinationWidth) + ")(" + childExpr + ")", nil
	case tir.FloatCast:
		// A float value cast to a different float width (`x as f32` where x is
		// an f64, or vice versa). The result is a float (this node's Type is
		// the destination float builtin, already gated to width above), and the
		// single child is the float being cast. The child is built via a
		// recursive buildFloatExpr call at the CHILD's own resolved float width
		// (not the destination width), the same "recurse at the child's own
		// width" principle as IntegerCast. The lowering is a plain, unchecked C
		// cast `(<destination float type>)(<child>)`: C's float-to-float
		// conversion is well-defined for every input, so no checked runtime
		// primitive is needed.
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains a FloatCast with %d children, want exactly one", len(node.Children))
		}
		destination, ok := snapshot.Key(node.Type)
		if !ok {
			return "", fmt.Errorf("entry function body expression contains a FloatCast with invalid destination type %d", node.Type)
		}
		destinationWidth, ok := destination.Builtin()
		if !ok || floatCType(destinationWidth) == "" {
			return "", fmt.Errorf("entry function body expression contains a FloatCast with non-float destination type %s", describeType(snapshot, node.Type))
		}
		child, ok := unit.Node(node.Children[0])
		if !ok {
			return "", fmt.Errorf("entry function body expression contains a FloatCast referencing invalid child node %d", node.Children[0])
		}
		childWidth := resolvedFloatKind(snapshot, child.Type)
		if childWidth == 0 {
			return "", fmt.Errorf("entry function body expression contains a FloatCast with non-float child type %s", describeType(snapshot, child.Type))
		}
		childExpr, err := buildFloatExpr(unit, snapshot, fileSet, node.Children[0], locals, childWidth)
		if err != nil {
			return "", fmt.Errorf("entry function body float cast child: %v", err)
		}
		return "(" + floatCType(destinationWidth) + ")(" + childExpr + ")", nil
	default:
		return "", fmt.Errorf("entry function body expression contains a %s, want a float literal, a reference to a %s local declared earlier in the body, float arithmetic, a negation, or a call to a float-returning function", node.Kind, wantName(width))
	}
}

func resolvedFloatKind(snapshot *types.Snapshot, id types.TypeID) types.BuiltinKind {
	key, ok := snapshot.Key(id)
	if !ok {
		return 0
	}
	kind, ok := key.Builtin()
	if !ok || (kind != types.F32 && kind != types.F64) {
		return 0
	}
	return kind
}

// shortCircuitOperator maps the two logical-combination token kinds a
// ShortCircuitValue may carry to their plain C spellings. Both C and Pebble
// && and || short-circuit their right operand, and both sides of the operator
// are side-effect-free in this backend's grammar, so the plain C operator is a
// direct, correct lowering. Any other operator is deliberately not mapped and
// rejected by the caller.
func shortCircuitOperator(op syntax.TokenKind) (string, bool) {
	switch op {
	case syntax.LogicalAnd:
		return "&&", true
	case syntax.LogicalOr:
		return "||", true
	default:
		return "", false
	}
}

// bitwiseOperator maps the unchecked integral operators whose C semantics are
// defined for every bit pattern. Shifts are deliberately excluded: their
// CheckedShift nodes require checked runtime semantics and are not plain C
// operators in this backend.
func bitwiseOperator(op syntax.TokenKind) (string, bool) {
	switch op {
	case syntax.Ampersand:
		return "&", true
	case syntax.Pipe:
		return "|", true
	case syntax.Caret:
		return "^", true
	default:
		return "", false
	}
}

// checkedArithmeticHelper maps the +, -, *, /, % operators a CheckedArithmetic
// node may carry to the runtime helper that implements their checked semantics,
// at the entry's resolved width (width's checkedSuffix picks the _i32 or _i64
// function-name suffix).
// Division and modulo map to pebble_rt_checked_div_i32 / pebble_rt_checked_mod_i32
// (or their _i64 twins), which handle both the divide-by-zero fault (in every
// mode) and the one
// division overflow input, INT32_MIN / -1 (INT64_MIN / -1 at the wider
// width). At u64 the +, -, * family exists (pebble_rt_checked_add/sub/mul_u64,
// this slice), but the / and % family has no u64 twin yet, so a u64 division
// or modulo is cleanly rejected rather than emitted as a call to a nonexistent
// pebble_rt_checked_div_u64/mod_u64. Any other operator (bitwise, etc.)
// is deliberately not mapped and rejected by the caller.
func checkedArithmeticHelper(op syntax.TokenKind, width types.BuiltinKind) (string, bool) {
	var base string
	switch op {
	case syntax.Plus:
		base = "pebble_rt_checked_add"
	case syntax.Minus:
		base = "pebble_rt_checked_sub"
	case syntax.Star:
		base = "pebble_rt_checked_mul"
	case syntax.Slash:
		base = "pebble_rt_checked_div"
	case syntax.Percent:
		base = "pebble_rt_checked_mod"
	default:
		return "", false
	}
	if width == types.U64 && (op == syntax.Slash || op == syntax.Percent) {
		return "", false
	}
	return base + "_" + checkedSuffix(width), true
}

func checkedShiftHelper(op syntax.TokenKind, width types.BuiltinKind) (string, bool) {
	var base string
	switch op {
	case syntax.ShiftLeft:
		base = "pebble_rt_checked_shl"
	case syntax.ShiftRight:
		base = "pebble_rt_checked_shr"
	default:
		return "", false
	}
	// Every width with a runtime shift-helper pair is admitted: the original
	// i32/i64 pair plus every narrower fixed-width integer (u8, u16, i8, i16,
	// u32) added when shifts were widened. A u64 (or uint) shift stays a clean
	// rejection here rather than being emitted as a call to a nonexistent
	// pebble_rt_checked_shl_u64/shr_u64 (checkedSuffix admits u64 for the +,
	// -, * arithmetic family this slice adds, but the shift family has no u64
	// twin yet).
	suffix := checkedShiftSuffix(width)
	if suffix == "" {
		return "", false
	}
	return base + "_" + suffix, true
}

// checkedShiftSuffix returns the pebble_rt_checked_shl/shr_* function-name
// suffix for the given width: "i32" for an int or i32 entry, "i64" for an
// i64 entry, and the width's own name for every narrower fixed-width integer
// (u8, u16, i8, i16, u32 — each of which has its own runtime shift-helper
// pair). It is the shift-specific twin of checkedSuffix, which deliberately
// stays narrow: the OTHER checked helper families (arithmetic, index, slice
// start, float-to-integer) admit only the i32/i64/u64 widths, so widening
// checkedSuffix globally would emit calls to nonexistent helpers for them
// (e.g. pebble_rt_checked_add_u8 does not exist). Any width without a
// runtime shift helper — u64 and uint, whose C representation is uint64_t —
// yields "", a clean rejection for the caller.
func checkedShiftSuffix(width types.BuiltinKind) string {
	switch width {
	case types.Int, types.I32:
		return "i32"
	case types.I64:
		return "i64"
	case types.U8:
		return "u8"
	case types.U16:
		return "u16"
	case types.U32:
		return "u32"
	case types.I8:
		return "i8"
	case types.I16:
		return "i16"
	}
	return ""
}

// isWidth reports whether id is the snapshot's builtin for the entry's
// resolved integer width (types.Int, types.I32, or types.I64). The checked helpers this
// backend emits operate on exactly one width per entry, so every node in an
// accepted expression tree must carry exactly this type — a node carrying the
// other width is a clean rejection, never a coercion, since there is no
// cast/coercion lowering yet.
func isWidth(snapshot *types.Snapshot, width types.BuiltinKind, id types.TypeID) bool {
	if snapshot == nil {
		return false
	}
	key, ok := snapshot.Key(id)
	if !ok {
		return false
	}
	builtin, ok := key.Builtin()
	return ok && builtin == width && cType(width) != ""
}

// isAbstractInt reports whether id is the snapshot's abstract `int` builtin
// (types.Int) — the unanchored keyword type the checker leaves a value at when
// nothing pins it to a fixed width. It is a DISTINCT builtin from types.I32
// (each is its own BuiltinKind), but it shares i32's C representation
// (int32_t), so an `int`-typed value is emitted at whatever integer width the
// surrounding position requests without a cast. The width gate in buildExpr
// admits such a node at any integer width (see there); isWidth itself stays
// exact-match so every other call site keeps its existing meaning.
func isAbstractInt(snapshot *types.Snapshot, id types.TypeID) bool {
	return snapshot != nil && id == snapshot.Builtins().Int
}

// isCompatibleIntegerWidth reports whether id resolves to an integer builtin
// (ANY fixed-width integer, not just the abstract `int` one) whose C
// representation shares width's own: cType(builtin) == cType(width). It is the
// symmetric twin of the isAbstractInt leniency buildExpr's width gate applies
// for the reverse direction: isAbstractInt lets an abstract-`int`-typed VALUE
// be emitted where a concrete width is requested, and this predicate lets a
// CONCRETE-width-typed value/parameter (typically the substitution a generic
// specialization performed — `clamp[T]` instantiated at `i32` has i32-typed
// parameters) be accepted where the surrounding context's own width — an
// `int`-declared entry's resolved `int` builtin — is expected, since `int` and
// `i32` share the int32_t representation. The gate deliberately does NOT accept
// a genuinely mismatched fixed width (an i64 value in an int/i32 context), which
// is the mismatch it exists to catch. isWidth itself stays exact-match so every
// other call site keeps its existing meaning.
func isCompatibleIntegerWidth(snapshot *types.Snapshot, width types.BuiltinKind, id types.TypeID) bool {
	if snapshot == nil || cType(width) == "" {
		return false
	}
	builtin, ok := resolvedBuiltin(snapshot, id)
	return ok && cType(builtin) != "" && cType(builtin) == cType(width)
}

func isUint(snapshot *types.Snapshot, id types.TypeID) bool {
	return snapshot != nil && id == snapshot.Builtins().Uint
}

// isU64 reports whether id is the snapshot's u64 builtin. It is the u64 twin
// of isUint: u64 is a DISTINCT builtin from uint in this compiler
// (snapshot.Builtins().U64 vs snapshot.Builtins().Uint), so it needs its own
// predicate rather than being folded into isUint, just as isUint is separate
// from isWidth. Unlike uint (the platform-native pointer-width type that the
// backend builds through buildUintExpr for its sizeof/slice-bounds/checked
// arithmetic), u64 is an ordinary fixed-width integer that flows through the
// general buildExpr path at its own resolved width.
func isU64(snapshot *types.Snapshot, id types.TypeID) bool {
	return snapshot != nil && id == snapshot.Builtins().U64
}

// isBool reports whether id is the snapshot's bool builtin. It is the bool
// twin of isWidth: every node in an accepted bool expression tree must carry
// exactly the bool builtin, since this backend has no cast/coercion lowering
// between bool and anything else.
func isBool(snapshot *types.Snapshot, id types.TypeID) bool {
	if snapshot == nil {
		return false
	}
	return id == snapshot.Builtins().Bool
}

// isStr reports whether id is the snapshot's str builtin. A str value is a
// builtin like bool, but unlike bool (or the entry's integer width) it has no
// arithmetic grammar this backend builds — a str local is initialized from a
// string literal (or, since 10.36, a call to a str-returning helper), may be
// reassigned from a string literal, and a str value is an operand of a ==/!=
// comparison, a call-site argument, or a str-returning function's return value
// — so it is recognized by this distinct predicate rather than by a shared
// scalar-builder switch.
func isStr(snapshot *types.Snapshot, id types.TypeID) bool {
	if snapshot == nil {
		return false
	}
	return id == snapshot.Builtins().Str
}

// isChar reports whether id is the snapshot's char builtin. A char value is a
// builtin like bool, but like str it has no width grammar this backend builds —
// its C representation is the fixed int32_t (the language's char is a full
// Unicode scalar value, which always fits in 32 bits, regardless of the entry's
// resolved integer width), so a char local is initialized from a char literal,
// a char-typed local reference, or a call to a char-returning helper, may be
// reassigned the same ways, and a char value is an operand of any of the six
// comparisons, a call-site argument, or a char-returning function's return
// value — recognized by this distinct predicate rather than by a shared
// scalar-builder switch.
func isChar(snapshot *types.Snapshot, id types.TypeID) bool {
	if snapshot == nil {
		return false
	}
	return id == snapshot.Builtins().Char
}

// isFloat reports whether id is one of the snapshot's two float builtins
// (f32 or f64). It is the float cousin of isBool: every node in an accepted
// float expression tree must carry exactly one of the two float builtins, and
// — unlike bool, which has just one type — which float builtin a node carries
// must also match the specific float kind the surrounding position wants, so
// buildFloatExpr additionally checks the resolved kind against its width
// argument rather than accepting either float builtin interchangeably.
func isFloat(snapshot *types.Snapshot, id types.TypeID) bool {
	if snapshot == nil {
		return false
	}
	return id == snapshot.Builtins().F32 || id == snapshot.Builtins().F64
}

// isVoid reports whether id is the snapshot's void builtin. A void result is
// the third accepted result kind for a reachable helper (alongside the entry's
// width and a tuple/struct type), recognized so validateHelperSignature can
// admit a void-returning callee and buildHelperFunctions can declare it with
// the C return type "void"; a void-returning call is then built only in the
// bare discarded-expression statement position (buildExpressionStatement),
// never as a value.
func isVoid(snapshot *types.Snapshot, id types.TypeID) bool {
	if snapshot == nil {
		return false
	}
	return id == snapshot.Builtins().Void
}

// isTuple reports whether id resolves to a tuple type in the snapshot. It is
// how the emitter recognizes a tuple-typed local's declaration without
// consulting the builtin table: a tuple is not a types.BuiltinKind, so
// resolvedBuiltin returns no kind for it and the caller must ask whether the
// type is a tuple instead.
func isTuple(snapshot *types.Snapshot, id types.TypeID) bool {
	if snapshot == nil {
		return false
	}
	key, ok := snapshot.Key(id)
	if !ok {
		return false
	}
	return key.Kind() == types.Tuple
}

// isArray reports whether id resolves to a fixed-length array type.
func isArray(snapshot *types.Snapshot, id types.TypeID) bool {
	if snapshot == nil {
		return false
	}
	key, ok := snapshot.Key(id)
	return ok && key.Kind() == types.Array
}

// isSlice reports whether id resolves to a slice type in the snapshot.
func isSlice(snapshot *types.Snapshot, id types.TypeID) bool {
	if snapshot == nil {
		return false
	}
	key, ok := snapshot.Key(id)
	return ok && key.Kind() == types.Slice
}

// isOptional reports whether id resolves to an optional type in the snapshot.
func isOptional(snapshot *types.Snapshot, id types.TypeID) bool {
	if snapshot == nil {
		return false
	}
	key, ok := snapshot.Key(id)
	return ok && key.Kind() == types.Optional
}

// isStruct reports whether id resolves to a struct type in the snapshot. It is
// how the emitter recognizes a struct-typed local's declaration without
// consulting the builtin table: a struct is a Nominal type, not a
// types.BuiltinKind, so resolvedBuiltin returns no kind for it and the caller
// must ask whether the type is a struct instead. A generic struct's
// monomorphized instance is also Nominal (its Nominal arguments are the
// concrete type arguments), so it is recognized the same way; this backend
// never inspects the argument list, so a generic instance is emitted exactly
// like a non-generic struct of the same shape.
func isStruct(snapshot *types.Snapshot, id types.TypeID) bool {
	if snapshot == nil {
		return false
	}
	key, ok := snapshot.Key(id)
	return ok && key.Kind() == types.Nominal
}

// isPointer reports whether id resolves to a pointer type in the snapshot. A
// pointer-typed local is declared with the pointee's own C type followed by
// ` *`, and its initializer is most commonly an AddressOf expression. The
// pointer type is recognized by this distinct predicate rather than by a
// shared scalar-builder switch, since a pointer is not a types.BuiltinKind.
func isPointer(snapshot *types.Snapshot, id types.TypeID) bool {
	if snapshot == nil {
		return false
	}
	key, ok := snapshot.Key(id)
	return ok && key.Kind() == types.Pointer
}

// isFunctionType reports whether id resolves to a function type in the
// snapshot. A function-typed local is declared with the function type's own
// pointer typedef (pebble_fnptr_<typeID>_t) and its value is a function
// pointer; the function type is recognized by this distinct predicate rather
// than by a shared scalar-builder switch, since a function type is not a
// types.BuiltinKind (mirroring how isTuple / isSlice / isOptional recognize
// their own kinds).
func isFunctionType(snapshot *types.Snapshot, id types.TypeID) bool {
	if snapshot == nil {
		return false
	}
	key, ok := snapshot.Key(id)
	return ok && key.Kind() == types.Function
}

// pointerPointeeType returns the pointee type of a pointer type. It is the
// single way to extract the child of a pointer type, mirroring how
// key.Child() works for Slice/Optional but restricted to Pointer kinds for
// clarity at call sites.
func pointerPointeeType(snapshot *types.Snapshot, pointerType types.TypeID) (types.TypeID, bool) {
	key, ok := snapshot.Key(pointerType)
	if !ok {
		return 0, false
	}
	if key.Kind() != types.Pointer {
		return 0, false
	}
	return key.Child()
}

// opaqueExternTypeName reports whether id is a Nominal type declared by an
// `extern { type Name; }` with no body — an OPAQUE extern type, meaning "this
// exists in C, I'm not describing its layout" — and, if so, returns its real
// authored C name (FILE, DIR, ...). The mapping is resolved the same way
// externCName resolves a function's real C name: a Nominal type's key carries
// its declaring symbol.SymbolID (see types.NominalKey), and the symbol table
// threaded into Emit (emitSymbols) classifies that declaration as
// SymbolExternType and holds the exact identifier written after `type` in the
// source. An ordinary Pebble struct/enum/union is a SymbolType symbol, never
// SymbolExternType, so the kind check is what distinguishes an opaque extern
// type from a real struct (both are Nominal in the type snapshot). A nil or
// missing symbol table yields ok=false: without the table the real name cannot
// be known, and the caller must not guess a pebble_struct_<id>_t name for a
// type whose layout it does not describe.
func opaqueExternTypeName(snapshot *types.Snapshot, id types.TypeID) (string, bool) {
	if snapshot == nil || emitSymbols == nil || emitSymbols.Symbols == nil {
		return "", false
	}
	key, ok := snapshot.Key(id)
	if !ok || key.Kind() != types.Nominal {
		return "", false
	}
	decl, _, ok := key.Nominal()
	if !ok {
		return "", false
	}
	s, ok := emitSymbols.Symbols.Symbol(decl)
	if !ok || s.Kind != symbol.SymbolExternType {
		return "", false
	}
	return s.Name, true
}

// isOpaqueExternType reports whether id is an opaque extern type (a
// `type Name;` inside an extern block, no body). It is the bool-only form of
// opaqueExternTypeName, used by struct-type collection to exclude such a type
// from the synthesized struct-typedef machinery: an opaque extern type has no
// layout of its own to emit — its real C name (FILE, DIR) is supplied by the
// libc header the preamble already includes — so collecting it as a struct
// would both emit a bogus empty typedef and break resolveStructInfo's
// field-resolution assumptions.
func isOpaqueExternType(snapshot *types.Snapshot, id types.TypeID) bool {
	_, ok := opaqueExternTypeName(snapshot, id)
	return ok
}

// pointerTypeName returns the full C type name for a pointer to the given
// pointee type: `int32_t *` for *i32, `bool *` for *bool, `pebble_struct_<id>_t *`
// for *Point, `pebble_tuple_<id>_t *` for a tuple pointer, `FILE *` for a
// pointer to an opaque extern type (its real C name, declared by the <stdio.h>
// preamble hasCExterns includes), etc. The pointee
// type must be a valid type in the snapshot. Returns "" for any unsupported
// pointee kind (defense for hand-built IR).
func pointerTypeName(snapshot *types.Snapshot, pointee types.TypeID) string {
	if snapshot == nil {
		return ""
	}
	if builtin, ok := snapshot.Key(pointee); ok {
		if bk, ok := builtin.Builtin(); ok {
			// cType only maps the fixed-width integer kinds (Int/I32/I64) —
			// it's meant for width-typed locals, not every possible pointee.
			// void/bool/char are real, common pointee kinds (*void is
			// pervasive in std/libc.peb and std/mem.peb) with their own C
			// spellings that don't go through cType's narrower convention.
			switch bk {
			case types.Void:
				return "void *"
			case types.Bool:
				return "bool *"
			case types.Char:
				// Matches the existing convention: a char value/local is
				// always declared as int32_t in emitted C (see the
				// char-typed-parameter case in buildHelperFunctions).
				return "int32_t *"
			}
			if ctype := cType(bk); ctype != "" {
				return ctype + " *"
			}
			return ""
		}
	}
	if isStr(snapshot, pointee) {
		return "PebbleStr *"
	}
	if isTuple(snapshot, pointee) {
		return tupleTypeName(pointee) + " *"
	}
	// An opaque extern type (type FILE;) is Nominal like a struct, so the
	// extern-type case must come BEFORE the isStruct fall-through: the real C
	// type name (FILE, from the already-included header) replaces the
	// synthesized pebble_struct_<id>_t a struct-typed pointee would get, and
	// the C that results actually agrees with the libc declaration of every
	// function that takes or returns such a pointer.
	if name, ok := opaqueExternTypeName(snapshot, pointee); ok {
		return name + " *"
	}
	if isStruct(snapshot, pointee) {
		return structTypeName(pointee) + " *"
	}
	if isSlice(snapshot, pointee) {
		return sliceTypeName(pointee) + " *"
	}
	return ""
}

// arrayLengthLiteral validates that the compile-time length can be passed to
// the width-specific checked-index helper without a narrowing conversion.
func arrayLengthLiteral(length uint64, width types.BuiltinKind) (string, error) {
	max := uint64(^uint32(0) >> 1)
	if width == types.I64 {
		max = uint64(^uint64(0) >> 1)
	}
	if length > max {
		return "", fmt.Errorf("array length %d does not fit the %s checked-index helper", length, wantName(width))
	}
	return fmt.Sprintf("%d", length), nil
}

// tupleTypeName is the deterministic C name of one distinct tuple type's
// struct typedef: pebble_tuple_<typeID>_t, derived from the tuple type's own
// stable types.TypeID (stable within one Emit call), mirroring the
// pebble_fn_<symbolID> / pebble_local_<symbolID> naming discipline of reusing
// a stable IR identity rather than a counter.
func tupleTypeName(id types.TypeID) string {
	return fmt.Sprintf("pebble_tuple_%d_t", id)
}

func arrayTypeName(id types.TypeID) string {
	return fmt.Sprintf("pebble_array_%d_t", id)
}

// optionalTypeName is the deterministic C name of one distinct optional type's
// struct typedef: pebble_optional_<typeID>_t, derived from the optional
// type's own stable types.TypeID, mirroring the tuple naming discipline.
func optionalTypeName(id types.TypeID) string {
	return fmt.Sprintf("pebble_optional_%d_t", id)
}

// structTypeName is the deterministic C name of one distinct struct type's
// struct typedef: pebble_struct_<typeID>_t, derived from the struct type's own
// stable types.TypeID, mirroring the tuple naming discipline.
func structTypeName(id types.TypeID) string {
	return fmt.Sprintf("pebble_struct_%d_t", id)
}

// functionTypeName is the deterministic C name of one distinct function type's
// pointer typedef: pebble_fnptr_<typeID>_t, derived from the function type's
// own stable types.TypeID, following the same TypeID-based naming discipline
// as pebble_slice_<typeID>_t / pebble_optional_<typeID>_t /
// pebble_struct_<typeID>_t (NOT v1's ad-hoc canonical-name-string scheme).
func functionTypeName(id types.TypeID) string {
	return fmt.Sprintf("pebble_fnptr_%d_t", id)
}

func runtimeType(unit *tir.Unit, snapshot *types.Snapshot, id types.TypeID) symbol.RuntimeType {
	if unit == nil || snapshot == nil {
		return 0
	}
	key, ok := snapshot.Key(id)
	if !ok {
		return 0
	}
	decl, _, ok := key.Nominal()
	if !ok {
		return 0
	}
	info := unit.Runtime()
	switch decl {
	case info.Allocator:
		if decl != 0 {
			return symbol.RuntimeAllocator
		}
	case info.Context:
		if decl != 0 {
			return symbol.RuntimeContext
		}
	}
	return 0
}

func runtimeTypeName(unit *tir.Unit, snapshot *types.Snapshot, id types.TypeID) string {
	switch runtimeType(unit, snapshot, id) {
	case symbol.RuntimeAllocator:
		return "PebbleAllocator"
	case symbol.RuntimeContext:
		return "PebbleContext"
	default:
		return structTypeName(id)
	}
}

func runtimeFieldName(unit *tir.Unit, owner types.TypeID, member symbol.SymbolID) (string, bool) {
	info := unit.Runtime()
	if runtimeType(unit, unit.Snapshot(), owner) == symbol.RuntimeAllocator {
		switch member {
		case info.AllocatorPtr:
			return "state", true
		case info.AllocatorAlloc:
			return "alloc", true
		case info.AllocatorRealloc:
			return "realloc", true
		case info.AllocatorFree:
			return "free", true
		}
	}
	if runtimeType(unit, unit.Snapshot(), owner) == symbol.RuntimeContext && member == info.ContextDefaultAllocator {
		return "allocator", true
	}
	return "", false
}

// sliceTypeName is the deterministic C name of one distinct slice type's
// struct typedef: pebble_slice_<typeID>_t, derived from the slice type's own
// stable types.TypeID, mirroring the tuple/struct/optional naming discipline.
func sliceTypeName(id types.TypeID) string {
	return fmt.Sprintf("pebble_slice_%d_t", id)
}

// enumTypeName is the deterministic C name of one distinct plain enum type's
// enum typedef: pebble_enum_<typeID>_t, derived from the enum type's own
// stable types.TypeID, mirroring the pebble_struct_<typeID>_t / pebble_tuple_
// <typeID>_t naming discipline of reusing a stable IR identity rather than a
// counter.
func enumTypeName(id types.TypeID) string {
	return fmt.Sprintf("pebble_enum_%d_t", id)
}

// sizeofCTypeName resolves one type to the C type name a `sizeof T` lowers to
// — the type's own C storage type: a fixed-width integer at its own cType
// (int32_t for int/i32, int64_t for i64, uint64_t for uint/u64, and so on),
// bool as bool, char as the fixed int32_t, str as PebbleStr, a tuple/optional/
// struct/slice/enum as its own typedef name (tupleTypeName/optionalTypeName/
// structTypeName/sliceTypeName/enumTypeName), and a runtime type as its
// hand-written C type (runtimeTypeName). This is the general form of the
// SizeofType case's original builtin-only three-width dispatch; it exists so
// `sizeof` of an aggregate type (std/hmap.peb's `sizeof Entry[K, V]`, where
// the TypeArg is the Entry struct type) sizes the storage by the aggregate's
// OWN typedef, never the fallback sizeof(uint64_t) the builtin-only dispatch
// would produce. Anything without a C type this backend emits is a clean
// rejection naming the type.
func sizeofCTypeName(unit *tir.Unit, snapshot *types.Snapshot, id types.TypeID) (string, error) {
	if isBool(snapshot, id) {
		return "bool", nil
	}
	if isChar(snapshot, id) {
		return "int32_t", nil
	}
	if isStr(snapshot, id) {
		return "PebbleStr", nil
	}
	if width, integer := resolvedBuiltin(snapshot, id); integer && cType(width) != "" {
		return cType(width), nil
	}
	if runtimeType(unit, snapshot, id) != 0 {
		return runtimeTypeName(unit, snapshot, id), nil
	}
	if isTuple(snapshot, id) {
		return tupleTypeName(id), nil
	}
	if isOptional(snapshot, id) {
		return optionalTypeName(id), nil
	}
	if isSlice(snapshot, id) {
		return sliceTypeName(id), nil
	}
	if isEnumType(unit, snapshot, id) {
		return enumTypeName(id), nil
	}
	if isStruct(snapshot, id) {
		return structTypeName(id), nil
	}
	if isPointer(snapshot, id) {
		if pointee, ok := pointerPointeeType(snapshot, id); ok {
			if name := pointerTypeName(snapshot, pointee); name != "" {
				return name, nil
			}
		}
	}
	return "", fmt.Errorf("sizeof of type %s is not supported, want a fixed-width integer, bool, char, str, tuple, optional, slice, enum, struct, or pointer", describeType(snapshot, id))
}

// unionTypeName is the deterministic C name of one distinct tagged-union type's
// struct typedef: pebble_union_<typeID>_t, derived from the union type's own
// stable types.TypeID, mirroring the pebble_struct_<typeID>_t / pebble_enum_
// <typeID>_t naming discipline of reusing a stable IR identity rather than a
// counter. The discriminant enum typedef the struct's tag field uses is
// pebble_enum_<typeID>_t (see enumTypeName) — the two names share the type ID
// suffix and never collide, since one spells "enum" and the other "union".
func unionTypeName(id types.TypeID) string {
	return fmt.Sprintf("pebble_union_%d_t", id)
}

// enumVariantName is the deterministic C name of one plain enum variant's
// enum constant: pebble_variant_<memberSymbolID>, derived from the variant's
// own stable symbol.SymbolID (mirroring the pebble_field_<memberSymbolID>
// naming discipline struct fields use, and the pebble_local_<symbolID> /
// pebble_fn_<symbolID> discipline everywhere else), so a C constant name can
// never collide with another identifier even if a source variant name were a C
// keyword.
func enumVariantName(member symbol.SymbolID) string {
	return fmt.Sprintf("pebble_variant_%d", member)
}

// tupleElementCType is the C field type a tuple element of the given type is
// declared with in its tuple's struct typedef: int32_t / int64_t for an
// element of the entry's resolved width, bool for a bool element. Any other
// element type is a clean rejection naming what was found, since this backend
// emits exactly those two C types as tuple fields.
func tupleElementCType(unit *tir.Unit, snapshot *types.Snapshot, width types.BuiltinKind, id types.TypeID) (string, error) {
	if isWidth(snapshot, width, id) {
		return cType(width), nil
	}
	if isBool(snapshot, id) {
		return "bool", nil
	}
	if isTuple(snapshot, id) {
		return tupleTypeName(id), nil
	}
	if isOptional(snapshot, id) {
		return optionalTypeName(id), nil
	}
	if isStruct(snapshot, id) {
		if isEnumType(unit, snapshot, id) {
			return "", fmt.Errorf("element type %s is an enum type; enum-typed tuple elements are not supported yet", enumTypeName(id))
		}
		return structTypeName(id), nil
	}
	if builtin, ok := resolvedBuiltin(snapshot, id); ok {
		if name, ok := builtinName(builtin); ok {
			return "", fmt.Errorf("element type %s is not supported, want %s or bool", name, wantName(width))
		}
	}
	return "", fmt.Errorf("element type %s is not supported, want %s or bool", describeType(snapshot, id), wantName(width))
}

// buildTupleTypedefs builds the C text of one struct typedef per tuple type in
// ids, in order, each joined by a newline. The caller (Emit) supplies ids in
// first-encountered order from the tuple-type collection pass, so every tuple
// type the emitted program references has exactly one typedef here, written
// before any function definition in the final output.
func buildTupleTypedefs(unit *tir.Unit, snapshot *types.Snapshot, width types.BuiltinKind, ids []types.TypeID) (string, error) {
	texts := make([]string, 0, len(ids))
	for _, id := range ids {
		text, err := buildTupleTypedef(unit, snapshot, width, id, nil)
		if err != nil {
			return "", err
		}
		texts = append(texts, text)
	}
	return strings.Join(texts, "\n"), nil
}

func buildArrayTypedefs(unit *tir.Unit, snapshot *types.Snapshot, width types.BuiltinKind, ids []types.TypeID) (string, error) {
	texts := make([]string, 0, len(ids))
	for _, id := range ids {
		key, ok := snapshot.Key(id)
		if !ok {
			return "", fmt.Errorf("array type %d is not in the type snapshot", id)
		}
		length, element, ok := key.Array()
		if !ok {
			return "", fmt.Errorf("type %s is not an array type", describeType(snapshot, id))
		}
		ctype, err := arrayElementCType(unit, snapshot, width, element)
		if err != nil {
			return "", fmt.Errorf("array type %s: %v", describeType(snapshot, id), err)
		}
		texts = append(texts, fmt.Sprintf("typedef struct {\n    %s data[%d];\n} %s;", ctype, length, arrayTypeName(id)))
	}
	return strings.Join(texts, "\n"), nil
}

func buildAggregateTypedefs(unit *tir.Unit, snapshot *types.Snapshot, width types.BuiltinKind, ids []types.TypeID, infos []structInfo, tagged map[types.TypeID]bool) (string, error) {
	structs := make(map[types.TypeID]structInfo, len(infos))
	for _, info := range infos {
		structs[info.typ] = info
	}
	texts := make([]string, 0, len(ids))
	for _, id := range ids {
		var text string
		var err error
		switch {
		case isTuple(snapshot, id):
			text, err = buildTupleTypedef(unit, snapshot, width, id, tagged)
		case isOptional(snapshot, id):
			text, err = buildOptionalTypedef(unit, snapshot, width, id, tagged)
		case isStruct(snapshot, id):
			text, err = buildStructTypedef(unit, snapshot, width, structs[id], tagged)
		}
		if err != nil {
			return "", err
		}
		if text != "" {
			texts = append(texts, text)
		}
	}
	return strings.Join(texts, "\n"), nil
}

// buildTupleTypedef builds the C text of one tuple type's struct typedef, with
// positional fields `_0`, `_1`, ... in element order (mirroring the old
// backend's own tuple-field naming convention, without the old 9-field cap):
//
//	typedef struct {
//	    int32_t _0;
//	    bool _1;
//	} pebble_tuple_<typeID>_t;
//
// Each field's C type comes from tupleElementCType, which validates the
// element is the entry's width or bool. A TypeID that is not a tuple type in
// the snapshot is a clean rejection, not a guessed layout.
func buildTupleTypedef(unit *tir.Unit, snapshot *types.Snapshot, width types.BuiltinKind, id types.TypeID, tagged map[types.TypeID]bool) (string, error) {
	key, ok := snapshot.Key(id)
	if !ok {
		return "", fmt.Errorf("tuple type %d is not in the type snapshot", id)
	}
	if key.Kind() != types.Tuple {
		return "", fmt.Errorf("type %s is a %v, want a tuple type", tupleTypeName(id), key.Kind())
	}
	elements, ok := key.Elements()
	if !ok {
		return "", fmt.Errorf("tuple type %s has no element list", tupleTypeName(id))
	}
	fields := make([]string, len(elements))
	for i, element := range elements {
		ctype, err := tupleElementCType(unit, snapshot, width, element)
		if err != nil {
			return "", fmt.Errorf("tuple type %s: %v", tupleTypeName(id), err)
		}
		fields[i] = "    " + ctype + fmt.Sprintf(" _%d;", i)
	}
	head := "typedef struct"
	if tagged[id] {
		head = "typedef struct " + strings.TrimSuffix(tupleTypeName(id), "_t")
	}
	return fmt.Sprintf("%s {\n%s\n} %s;", head, strings.Join(fields, "\n"), tupleTypeName(id)), nil
}

// buildOptionalTypedefs builds the C text of one struct typedef per optional
// type in ids, in order, each joined by a newline. The caller (Emit) supplies
// ids in first-encountered order from the optional-type collection pass, so
// every optional type the emitted program references has exactly one typedef
// here, written before any function definition in the final output.
func buildOptionalTypedefs(unit *tir.Unit, snapshot *types.Snapshot, width types.BuiltinKind, ids []types.TypeID) (string, error) {
	texts := make([]string, 0, len(ids))
	for _, id := range ids {
		text, err := buildOptionalTypedef(unit, snapshot, width, id, nil)
		if err != nil {
			return "", err
		}
		texts = append(texts, text)
	}
	return strings.Join(texts, "\n"), nil
}

// buildOptionalTypedef builds the C text of one optional type's struct typedef:
//
//	typedef struct {
//	    bool has_value;
//	    int32_t value;
//	} pebble_optional_<typeID>_t;
//
// The value field's C type is the payload's own type (int32_t/int64_t for the
// entry's width, bool for a bool payload, or the payload's enum typedef for an
// enum payload). A TypeID that is not an optional type in the snapshot is a
// clean rejection, not a guessed layout.
func buildOptionalTypedef(unit *tir.Unit, snapshot *types.Snapshot, width types.BuiltinKind, id types.TypeID, tagged map[types.TypeID]bool) (string, error) {
	key, ok := snapshot.Key(id)
	if !ok {
		return "", fmt.Errorf("optional type %d is not in the type snapshot", id)
	}
	if key.Kind() != types.Optional {
		return "", fmt.Errorf("type %s is a %v, want an optional type", optionalTypeName(id), key.Kind())
	}
	payloadType, ok := key.Child()
	if !ok {
		return "", fmt.Errorf("optional type %s has no payload type", optionalTypeName(id))
	}
	valueCType, err := optionalPayloadCType(unit, snapshot, width, payloadType)
	if err != nil {
		return "", fmt.Errorf("optional type %s: %v", optionalTypeName(id), err)
	}
	head := "typedef struct"
	if tagged[id] {
		head = "typedef struct " + strings.TrimSuffix(optionalTypeName(id), "_t")
	}
	return fmt.Sprintf("%s {\n    bool has_value;\n    %s value;\n} %s;", head, valueCType, optionalTypeName(id)), nil
}

// buildStructTypedefs builds the C text of one struct typedef per struct type
// in infos, in order, each joined by a newline. The caller (Emit) supplies
// infos in first-encountered order from the struct-type collection pass, so
// every struct type the emitted program references has exactly one typedef
// here, written before any function definition in the final output.
func buildStructTypedefs(unit *tir.Unit, snapshot *types.Snapshot, width types.BuiltinKind, infos []structInfo) (string, error) {
	texts := make([]string, 0, len(infos))
	for _, info := range infos {
		text, err := buildStructTypedef(unit, snapshot, width, info, nil)
		if err != nil {
			return "", err
		}
		texts = append(texts, text)
	}
	return strings.Join(texts, "\n"), nil
}

// buildStructTypedef builds the C text of one struct type's struct typedef,
// with one field per declared struct field, in the struct's *declared* order
// (from the TypeDecl's Members list, resolved by collectStructTypes — never
// the construction-site order a RecordConstruct's Fields carry), each named
// deterministically from the field's own stable symbol.SymbolID:
//
//	typedef struct {
//	    int32_t pebble_field_25;
//	    bool pebble_field_26;
//	} pebble_struct_<typeID>_t;
//
// Naming each C field from the field's symbol ID (mirroring the
// pebble_local_<symbolID> / pebble_fn_<symbolID> discipline) makes a C-field
// name collision impossible even if a source field name were a C keyword or
// duplicated another identifier. Each field's C type comes from
// structFieldCType, which validates the field is a fixed-width integer, bool,
// a supported compound type, or a runtime type.
// A structInfo whose TypeID is not a Nominal type in the snapshot is a clean
// rejection, not a guessed layout (defense for hand-built IR; collectStructTypes
// has already resolved every collected TypeID through resolveStructInfo).
func buildStructTypedef(unit *tir.Unit, snapshot *types.Snapshot, width types.BuiltinKind, info structInfo, tagged map[types.TypeID]bool) (string, error) {
	key, ok := snapshot.Key(info.typ)
	if !ok {
		return "", fmt.Errorf("struct type %d is not in the type snapshot", info.typ)
	}
	if key.Kind() != types.Nominal {
		return "", fmt.Errorf("type %s is a %v, want a struct type", structTypeName(info.typ), key.Kind())
	}
	fields := make([]string, len(info.fields))
	for i, field := range info.fields {
		ctype, err := structFieldCType(unit, snapshot, width, field.typ)
		if err != nil {
			return "", fmt.Errorf("struct type %s: %v", structTypeName(info.typ), err)
		}
		fields[i] = "    " + ctype + fmt.Sprintf(" pebble_field_%d;", field.member)
	}
	head := "typedef struct"
	if tagged[info.typ] {
		head = "typedef struct " + strings.TrimSuffix(structTypeName(info.typ), "_t")
	}
	return fmt.Sprintf("%s {\n%s\n} %s;", head, strings.Join(fields, "\n"), structTypeName(info.typ)), nil
}

// structFieldCType is the C field type a struct field of the given type is
// declared with in its struct's typedef: any fixed-width integer builtin (the
// entry's resolved width, uint, u64, or any other fixed-width integer, each
// resolved to its OWN width by the generic resolvedBuiltin/cType pattern — so
// a uint or u64 field is uint64_t), bool for a bool field, the field's own
// tuple/optional/struct/pointer/slice/function-type typedef, a plain enum
// field's own enum typedef (pebble_enum_<typeID>_t, the same C type an
// enum-typed local/parameter/result is declared with), or a
// compiler-builtin runtime type's hand-written C type. Any other field type —
// a str field, a char field — is a clean rejection naming what
// was found, since this backend emits exactly those C types as struct fields.
func structFieldCType(unit *tir.Unit, snapshot *types.Snapshot, width types.BuiltinKind, id types.TypeID) (string, error) {
	if fieldWidth, integerField := resolvedBuiltin(snapshot, id); integerField && cType(fieldWidth) != "" {
		return cType(fieldWidth), nil
	}
	if isBool(snapshot, id) {
		return "bool", nil
	}
	if isTuple(snapshot, id) {
		return tupleTypeName(id), nil
	}
	if isOptional(snapshot, id) {
		return optionalTypeName(id), nil
	}
	// A field of a compiler-builtin runtime type (Allocator, Context) is
	// declared with its hand-written C type (PebbleAllocator / PebbleContext,
	// defined in pebble_rt.h, never emitted as a pebble_struct_<id>_t typedef —
	// orderAggregateTypes skips runtime types for exactly this reason), so it
	// must be resolved before the isStruct case, which would otherwise name a
	// nonexistent pebble_struct_<id>_t. Mirrors how a runtime-typed local,
	// parameter, and result are declared via runtimeTypeName.
	if runtimeType(unit, snapshot, id) != 0 {
		return runtimeTypeName(unit, snapshot, id), nil
	}
	if isStruct(snapshot, id) {
		if isEnumType(unit, snapshot, id) {
			return enumTypeName(id), nil
		}
		return structTypeName(id), nil
	}
	if isPointer(snapshot, id) {
		pointee, ok := pointerPointeeType(snapshot, id)
		if !ok {
			return "", fmt.Errorf("field type %s has no pointer pointee", describeType(snapshot, id))
		}
		if name := pointerTypeName(snapshot, pointee); name != "" {
			return name, nil
		}
	}
	if isSlice(snapshot, id) {
		return sliceTypeName(id), nil
	}
	if isFunctionType(snapshot, id) {
		// A function-typed field (`op fn(int, int) int;`, function-types
		// slice 2): declared with the function type's own pointer typedef,
		// pebble_fnptr_<typeID>_t (see functionTypeName / buildFunctionTypedef,
		// slice 1) — the same C type a function-typed local or value uses, so
		// storing a function value in the field is trivially valid C. The
		// field's signature is validated by validateFunctionTypeSignature
		// wherever the field's construction/read value is built
		// (buildFunctionValue), mirroring how a slice field's element type is
		// validated separately from this resolver.
		if err := validateFunctionTypeSignature(snapshot, width, id); err != nil {
			return "", fmt.Errorf("field type %s: %v", describeType(snapshot, id), err)
		}
		return functionTypeName(id), nil
	}
	if builtin, ok := resolvedBuiltin(snapshot, id); ok {
		if name, ok := builtinName(builtin); ok {
			return "", fmt.Errorf("field type %s is not supported, want a fixed-width integer, bool, tuple, struct, enum, pointer, slice, function type, or runtime type", name)
		}
	}
	return "", fmt.Errorf("field type %s is not supported, want a fixed-width integer, bool, tuple, struct, enum, pointer, slice, function type, or runtime type", describeType(snapshot, id))
}

// optionalPayloadCType is the C field type an optional payload of the given
// type is declared with in its optional's struct typedef: any fixed-width
// integer builtin (the entry's resolved width, uint, u64, or any other
// fixed-width integer, each resolved to its OWN width by the generic
// resolvedBuiltin/cType pattern — so a uint or u64 payload's .value field is
// uint64_t), bool for a bool payload, the payload's own tuple/struct typedef
// name, and, since the OptionalIntegerToEnum slice, the payload's own enum
// typedef (pebble_enum_<typeID>_t) for an enum payload — the destination
// shape of an integer cast to an optional enum (`5 as ?Color`), whose optional
// struct must carry the enum value field. A pointer payload (the std/hmap.peb
// get_by_ref shape, `?*V`) is declared with the pointee's own pointer C type,
// `<pointee> *` via pointerTypeName. Any other payload type is a clean
// rejection naming what was found, since this backend emits exactly those C
// types as optional value fields.
func optionalPayloadCType(unit *tir.Unit, snapshot *types.Snapshot, width types.BuiltinKind, id types.TypeID) (string, error) {
	if payloadWidth, integerPayload := resolvedBuiltin(snapshot, id); integerPayload && cType(payloadWidth) != "" {
		return cType(payloadWidth), nil
	}
	if isBool(snapshot, id) {
		return "bool", nil
	}
	if isTuple(snapshot, id) {
		return tupleTypeName(id), nil
	}
	if isStruct(snapshot, id) {
		if isEnumType(unit, snapshot, id) {
			return enumTypeName(id), nil
		}
		return structTypeName(id), nil
	}
	if isPointer(snapshot, id) {
		pointee, ok := pointerPointeeType(snapshot, id)
		if !ok {
			return "", fmt.Errorf("payload type %s has no pointer pointee", describeType(snapshot, id))
		}
		if name := pointerTypeName(snapshot, pointee); name != "" {
			return name, nil
		}
		return "", fmt.Errorf("payload type %s has a pointee %s whose C type is unsupported", describeType(snapshot, id), describeType(snapshot, pointee))
	}
	if builtin, ok := resolvedBuiltin(snapshot, id); ok {
		if name, ok := builtinName(builtin); ok {
			return "", fmt.Errorf("payload type %s is not supported, want a fixed-width integer, bool, tuple, struct, or enum", name)
		}
	}
	return "", fmt.Errorf("payload type %s is not supported, want a fixed-width integer, bool, tuple, struct, or enum", describeType(snapshot, id))
}

// buildUnionTypedefs builds the C text of one tagged-union typedef pair per
// union type in infos, in order, each joined by a newline. Each pair is the
// discriminant enum typedef followed by the tagged struct typedef (in that
// order, since the struct typedef's tag field references the enum typedef by
// name — C requires a type fully defined before use). The caller (Emit)
// supplies infos in first-encountered order from the union-type collection
// pass, so every union type the emitted program references has exactly one
// pair here, written before any function definition in the final output.
func buildUnionTypedefs(unit *tir.Unit, snapshot *types.Snapshot, width types.BuiltinKind, infos []unionInfo) (string, error) {
	texts := make([]string, 0, len(infos))
	for _, info := range infos {
		text, err := buildUnionTypedef(unit, snapshot, width, info)
		if err != nil {
			return "", err
		}
		texts = append(texts, text)
	}
	return strings.Join(texts, "\n"), nil
}

// buildUnionTypedef builds the C text of one tagged-union type's typedef pair:
// the discriminant enum typedef (reused verbatim from buildEnumTypedef over the
// union's variants in declared order — the declared order IS the discriminant,
// exactly like a plain enum, so the switch case labels and the stored tag
// values agree with the typedef by construction) followed by the tagged struct
// typedef:
//
//	typedef enum {
//	    pebble_variant_25,
//	    pebble_variant_26,
//	} pebble_enum_23_t;
//	typedef struct {
//	    pebble_enum_23_t tag;
//	    union {
//	        int32_t pebble_field_26;
//	    } payload;
//	} pebble_union_23_t;
//
// The tag field is typed as the discriminant enum typedef, the union's
// identity carrier: a tagged union's value IS its discriminant plus the
// payload union, and the discriminant ordinal scheme is identical to a plain
// enum's. Each payload union member is named pebble_field_<memberSymbolID>
// from the variant's own stable symbol.SymbolID, exactly the naming discipline
// struct fields use (see buildStructTypedef) — deliberately distinct from
// pebble_variant_<memberSymbolID>, which names the *enum constant* (the tag
// value), not a union member, so the two can never collide. One member is
// declared per non-void variant actually constructed somewhere in the reachable
// program (the unionInfo's members, resolved by resolveUnionInfo); a variant
// never constructed has no member, since no payload for it is ever read or
// written. A unionInfo whose TypeID is not an enum-shaped Nominal type in the
// snapshot is a clean rejection, not a guessed layout (defense for hand-built
// IR; collectUnionTypes has already resolved every collected TypeID through
// resolveUnionInfo, which requires a tagged-union type).
func buildUnionTypedef(unit *tir.Unit, snapshot *types.Snapshot, width types.BuiltinKind, info unionInfo) (string, error) {
	key, ok := snapshot.Key(info.typ)
	if !ok {
		return "", fmt.Errorf("union type %d is not in the type snapshot", info.typ)
	}
	if key.Kind() != types.Nominal {
		return "", fmt.Errorf("type %s is a %v, want a tagged-union type", unionTypeName(info.typ), key.Kind())
	}
	enumText, err := buildEnumTypedef(snapshot, enumInfo{typ: info.typ, decl: info.decl, variants: info.variants})
	if err != nil {
		return "", err
	}
	members := make([]string, len(info.members))
	for i, member := range info.members {
		ctype, err := unionMemberCType(unit, snapshot, width, member.payloadType)
		if err != nil {
			return "", fmt.Errorf("union type %s: %v", unionTypeName(info.typ), err)
		}
		members[i] = "        " + ctype + fmt.Sprintf(" pebble_field_%d;", member.member)
	}
	structText := fmt.Sprintf("typedef struct {\n    %s tag;\n    union {\n%s\n    } payload;\n} %s;", enumTypeName(info.typ), strings.Join(members, "\n"), unionTypeName(info.typ))
	return enumText + "\n" + structText, nil
}

// unionMemberCType is the C type one tagged-union payload member of the given
// payload type is declared with in its union's struct typedef: int32_t /
// int64_t for a payload of the entry's resolved width, bool for a bool payload,
// and the runtime ABI's fixed PebbleStr for a str payload. Any other payload
// type is a clean rejection naming what was found, since this backend emits
// exactly those three C types as union members.
func unionMemberCType(unit *tir.Unit, snapshot *types.Snapshot, width types.BuiltinKind, id types.TypeID) (string, error) {
	if isWidth(snapshot, width, id) {
		return cType(width), nil
	}
	if isBool(snapshot, id) {
		return "bool", nil
	}
	if isStr(snapshot, id) {
		return "PebbleStr", nil
	}
	if builtin, ok := resolvedBuiltin(snapshot, id); ok {
		if name, ok := builtinName(builtin); ok {
			return "", fmt.Errorf("payload type %s is not supported, want %s, bool, or str", name, wantName(width))
		}
	}
	return "", fmt.Errorf("payload type %s is not supported, want %s, bool, or str", describeType(snapshot, id), wantName(width))
}

// buildEnumTypedefs builds the C text of one enum typedef per plain enum type
// in infos, in order, each joined by a newline. The caller (Emit) supplies
// infos in first-encountered order from the enum-type collection pass, so
// every enum type the emitted program references has exactly one typedef here,
// written before any function definition in the final output.
func buildEnumTypedefs(snapshot *types.Snapshot, infos []enumInfo) (string, error) {
	texts := make([]string, 0, len(infos))
	for _, info := range infos {
		text, err := buildEnumTypedef(snapshot, info)
		if err != nil {
			return "", err
		}
		texts = append(texts, text)
	}
	return strings.Join(texts, "\n"), nil
}

// buildEnumTypedef builds the C text of one plain enum type's enum typedef,
// with one named constant per declared variant, in the enum's *declared* order
// (from the TypeDecl's Members list, resolved by collectEnumTypes — the same
// ordering a struct typedef resolves its fields by), each named
// deterministically from the variant's own stable symbol.SymbolID:
//
//	typedef enum {
//	    pebble_variant_25,
//	    pebble_variant_26,
//	    pebble_variant_27,
//	} pebble_enum_23_t;
//
// The declared order IS the discriminant: C assigns the constants the ordinal
// values 0, 1, 2, ... in declaration order, so variant Members[i] is the value
// i — the natural, stable discriminant the switch case labels and the values
// stored in enum-typed locals agree with by construction. Naming each constant
// from the variant's symbol ID (mirroring the pebble_field_<memberSymbolID>
// discipline) makes a C constant-name collision impossible even if a source
// variant name were a C keyword or duplicated another identifier. An enumInfo
// whose TypeID is not a Nominal type in the snapshot is a clean rejection, not
// a guessed layout (defense for hand-built IR; collectEnumTypes has already
// resolved every collected TypeID through resolveEnumInfo, which requires a
// plain enum).
func buildEnumTypedef(snapshot *types.Snapshot, info enumInfo) (string, error) {
	key, ok := snapshot.Key(info.typ)
	if !ok {
		return "", fmt.Errorf("enum type %d is not in the type snapshot", info.typ)
	}
	if key.Kind() != types.Nominal {
		return "", fmt.Errorf("type %s is a %v, want an enum type", enumTypeName(info.typ), key.Kind())
	}
	if len(info.variants) == 0 {
		return "", fmt.Errorf("enum type %s has no declared variants", enumTypeName(info.typ))
	}
	constants := make([]string, len(info.variants))
	for i, variant := range info.variants {
		constants[i] = "    " + enumVariantName(variant) + ","
	}
	return fmt.Sprintf("typedef enum {\n%s\n} %s;", strings.Join(constants, "\n"), enumTypeName(info.typ)), nil
}

// sliceElementForwardDeclaredAggregates reports, for every slice type in
// infos, which aggregate element types (a struct, tuple, or optional — an enum
// element is excluded: it is rejected by sliceElementCType before any typedef
// could be emitted) its .data pointer names in the slice's typedef text. Those
// aggregate typedef names must be DECLARED — even incompletely — before the
// slice typedef that points at them, because the slice typedef block is
// emitted before the aggregate block that fully defines them; C resolves a
// pointer to a declared-but-incomplete type. The returned set marks exactly
// those aggregate types whose FULL definition must carry the matching struct
// tag (buildStructTypedef / buildTupleTypedef / buildOptionalTypedef emit
// `typedef struct pebble_<kind>_<id> {` for them), so the forward declaration
// and the definition complete the same C type. A slice whose element is a
// scalar (integer, char, bool) needs no forward declaration: its data field
// names a builtin C type that needs no typedef.
func sliceElementForwardDeclaredAggregates(snapshot *types.Snapshot, infos []sliceInfo) map[types.TypeID]bool {
	out := make(map[types.TypeID]bool)
	for _, info := range infos {
		if isStruct(snapshot, info.elementType) || isTuple(snapshot, info.elementType) || isOptional(snapshot, info.elementType) {
			out[info.elementType] = true
		}
	}
	return out
}

// buildAggregateForwardDeclarations builds the C text of one incomplete typedef
// declaration per aggregate type in tagged, ordered by TypeID for deterministic
// output (each declaration is self-contained, so order does not matter
// semantically):
//
//	typedef struct pebble_struct_<typeID> pebble_struct_<typeID>_t;
//	typedef struct pebble_tuple_<typeID> pebble_tuple_<typeID>_t;
//	typedef struct pebble_optional_<typeID> pebble_optional_<typeID>_t;
//
// Each declares the struct TAG pebble_<kind>_<typeID> and the typedef name
// pebble_<kind>_<typeID>_t together, so a later full definition of the same
// tag (emitted with the matching tag by buildStructTypedef /
// buildTupleTypedef / buildOptionalTypedef) completes the type, and a pointer
// field in a slice typedef emitted between the two (`pebble_struct_<typeID>_t
// *data;`) is valid C against the incomplete declaration.
func buildAggregateForwardDeclarations(snapshot *types.Snapshot, tagged map[types.TypeID]bool) string {
	ids := make([]types.TypeID, 0, len(tagged))
	for id := range tagged {
		ids = append(ids, id)
	}
	sort.Slice(ids, func(i, j int) bool { return ids[i] < ids[j] })
	decls := make([]string, 0, len(ids))
	for _, id := range ids {
		name := structTypeName(id)
		if isTuple(snapshot, id) {
			name = tupleTypeName(id)
		} else if isOptional(snapshot, id) {
			name = optionalTypeName(id)
		}
		tag := strings.TrimSuffix(name, "_t")
		decls = append(decls, fmt.Sprintf("typedef struct %s %s;", tag, name))
	}
	return strings.Join(decls, "\n")
}

// buildSliceTypedefs builds the C text for every distinct slice type, one
// typedef per slice type, joining them with newlines. Each slice type is a
// small C struct with a data pointer and a length field.
func buildSliceTypedefs(unit *tir.Unit, snapshot *types.Snapshot, infos []sliceInfo, width types.BuiltinKind) (string, error) {
	texts := make([]string, 0, len(infos))
	for _, info := range infos {
		text, err := buildSliceTypedef(unit, snapshot, info, width)
		if err != nil {
			return "", err
		}
		texts = append(texts, text)
	}
	return strings.Join(texts, "\n"), nil
}

// buildSliceTypedef builds the C text of one slice type's struct typedef:
//
//	typedef struct {
//	    int32_t *data;
//	    size_t len;
//	} pebble_slice_<typeID>_t;
//
// Field names data/len match PebbleStrSlice's own naming in pebble_rt.h.
func buildSliceTypedef(unit *tir.Unit, snapshot *types.Snapshot, info sliceInfo, width types.BuiltinKind) (string, error) {
	if info.elementType == 0 {
		return "", fmt.Errorf("slice type %s has no element type", sliceTypeName(info.typ))
	}
	elemCType, err := sliceElementCType(unit, snapshot, width, info.elementType)
	if err != nil {
		return "", fmt.Errorf("slice type %s: %v", sliceTypeName(info.typ), err)
	}
	return fmt.Sprintf("typedef struct {\n    %s *data;\n    size_t len;\n} %s;", elemCType, sliceTypeName(info.typ)), nil
}

// buildFunctionTypedefs builds the C text of one function-pointer typedef per
// function type in ids, in order, each joined by a newline. The caller (Emit)
// supplies ids in first-encountered order from the function-type collection
// pass, so every function type the emitted program references as a first-class
// value has exactly one typedef here, written before any function definition
// in the final output.
func buildFunctionTypedefs(snapshot *types.Snapshot, width types.BuiltinKind, ids []types.TypeID) (string, error) {
	texts := make([]string, 0, len(ids))
	for _, id := range ids {
		text, err := buildFunctionTypedef(snapshot, width, id)
		if err != nil {
			return "", err
		}
		texts = append(texts, text)
	}
	return strings.Join(texts, "\n"), nil
}

// buildFunctionTypedef builds the C text of one function type's pointer
// typedef, mirroring v1's TYPE_FUNCTION typedef shape adapted to v2's
// TypeID-based naming and ctx-threading convention:
//
//	typedef <result-c-type> (*pebble_fnptr_<typeID>_t)(PebbleContext *ctx, <param-c-types>...);
//
// The function type's own signature is resolved via types.TypeKey.Function()
// (convention, parameters, result, variadic), and every parameter/result C
// type is resolved by functionTypeParamCType / functionTypeResultCType — the
// same C types buildHelperFunctions declares an ordinary Pebble-convention
// helper's parameters and result with, so a hoisted function's C name and the
// fnptr typedef always agree exactly. Only Pebble-convention function types
// are reachable as first-class values in this slice (a C-convention one is a
// clean rejection — see validateFunctionTypeSignature), so the typedef always
// carries the trailing PebbleContext *ctx parameter. Every parameter/result C
// type is self-contained (the entry's cType, uint64_t, bool, int32_t,
// PebbleStr, a pointer's own `<pointee> *` spelling via pointerTypeName, or
// void), so the typedef never references an aggregate typedef
// that might be emitted after it.
func buildFunctionTypedef(snapshot *types.Snapshot, width types.BuiltinKind, id types.TypeID) (string, error) {
	if err := validateFunctionTypeSignature(snapshot, width, id); err != nil {
		return "", err
	}
	key, ok := snapshot.Key(id)
	if !ok {
		return "", fmt.Errorf("function type %d is not in the type snapshot", id)
	}
	_, parameters, result, _, ok := key.Function()
	if !ok {
		return "", fmt.Errorf("type %s is not a function type", describeType(snapshot, id))
	}
	resultCType, err := functionTypeResultCType(snapshot, width, result)
	if err != nil {
		return "", err
	}
	paramCTypes := make([]string, len(parameters))
	for i, parameter := range parameters {
		paramCType, err := functionTypeParamCType(snapshot, width, parameter)
		if err != nil {
			return "", err
		}
		paramCTypes[i] = paramCType
	}
	if len(paramCTypes) == 0 {
		return fmt.Sprintf("typedef %s (*pebble_fnptr_%d_t)(PebbleContext *ctx);", resultCType, id), nil
	}
	return fmt.Sprintf("typedef %s (*pebble_fnptr_%d_t)(PebbleContext *ctx, %s);", resultCType, id, strings.Join(paramCTypes, ", ")), nil
}

// functionTypeParamCType resolves one function type's parameter to the C type
// an ordinary Pebble-convention helper's parameter of that type is declared
// with (see buildHelperFunctions): the entry's cType(width) for a width
// parameter, uint64_t for uint, bool for bool, int32_t for char, PebbleStr
// for str, and the pointee's own `<pointee> *` via pointerTypeName for a
// pointer parameter (the same spelling helperSignature gives an ordinary
// helper's pointer parameter) — the exact self-contained set
// validateFunctionTypeSignature admits.
// Anything else is a clean rejection, defense for hand-built IR (the
// validation has already ruled every reachable parameter shape out).
func functionTypeParamCType(snapshot *types.Snapshot, width types.BuiltinKind, param types.TypeID) (string, error) {
	switch {
	case isWidth(snapshot, width, param):
		return cType(width), nil
	case isUint(snapshot, param):
		return "uint64_t", nil
	case isU64(snapshot, param):
		return "uint64_t", nil
	case isBool(snapshot, param):
		return "bool", nil
	case isChar(snapshot, param):
		return "int32_t", nil
	case isStr(snapshot, param):
		return "PebbleStr", nil
	case isPointer(snapshot, param):
		// A pointer parameter is spelled the same way helperSignature spells
		// an ordinary helper's pointer parameter: pointerTypeName takes the
		// pointee, not the pointer type itself (it appends " *" to the
		// pointee's own C type), so the pointee must be extracted first.
		pointeeTypeID, ok := pointerPointeeType(snapshot, param)
		if !ok {
			return "", fmt.Errorf("function type parameter type %s has no pointer pointee", describeType(snapshot, param))
		}
		if ctypeName := pointerTypeName(snapshot, pointeeTypeID); ctypeName != "" {
			return ctypeName, nil
		}
		return "", fmt.Errorf("function type parameter type %s has a pointee %s whose C type is unsupported", describeType(snapshot, param), describeType(snapshot, pointeeTypeID))
	}
	return "", fmt.Errorf("function type parameter type %s is not supported, want %s, uint, bool, char, str, or a pointer type", describeType(snapshot, param), wantName(width))
}

// functionTypeResultCType resolves one function type's result to the C return
// type an ordinary Pebble-convention helper with that result is declared with
// (see buildHelperFunctions): the entry's cType(width) for a width result,
// bool, int32_t for char, the pointee's own `<pointee> *` via pointerTypeName
// for a pointer result (the same spelling helperSignature gives an ordinary
// helper's pointer result), and void — the exact self-contained set
// validateFunctionTypeSignature admits. Anything else is a clean rejection,
// defense for hand-built IR (the validation has already ruled every reachable
// result shape out).
func functionTypeResultCType(snapshot *types.Snapshot, width types.BuiltinKind, result types.TypeID) (string, error) {
	switch {
	case isWidth(snapshot, width, result):
		return cType(width), nil
	case isU64(snapshot, result):
		return "uint64_t", nil
	case isBool(snapshot, result):
		return "bool", nil
	case isChar(snapshot, result):
		return "int32_t", nil
	case isVoid(snapshot, result):
		return "void", nil
	case isPointer(snapshot, result):
		// A pointer result is spelled the same way helperSignature spells an
		// ordinary helper's pointer result: pointerTypeName takes the
		// pointee, not the pointer type itself (it appends " *" to the
		// pointee's own C type), so the pointee must be extracted first.
		pointeeTypeID, ok := pointerPointeeType(snapshot, result)
		if !ok {
			return "", fmt.Errorf("function type result type %s has no pointer pointee", describeType(snapshot, result))
		}
		if ctypeName := pointerTypeName(snapshot, pointeeTypeID); ctypeName != "" {
			return ctypeName, nil
		}
		return "", fmt.Errorf("function type result type %s has a pointee %s whose C type is unsupported", describeType(snapshot, result), describeType(snapshot, pointeeTypeID))
	}
	return "", fmt.Errorf("function type result type %s is not supported, want %s, bool, char, void, or a pointer type", describeType(snapshot, result), wantName(width))
}

// joinTypedefs joins two typedef text blocks into a single block, with a blank
// line between them when both are non-empty. Either may be empty; the result is
// empty when both are empty. Emit chains it twice (tuple joined with optional,
// then the result joined with struct) so the three typedef families form one
// block in a fixed order.
func joinTypedefs(tupleTypedefs, optionalTypedefs string) string {
	if tupleTypedefs == "" {
		return optionalTypedefs
	}
	if optionalTypedefs == "" {
		return tupleTypedefs
	}
	return tupleTypedefs + "\n" + optionalTypedefs
}

// resolvedBuiltin resolves a TypeID to the builtin kind it names, if it names
// one. It is how the emitter decides what a value node's type means — the
// entry's integer width for an integer local's initializer, or bool for a bool
// local's — without re-deriving anything.
func resolvedBuiltin(snapshot *types.Snapshot, id types.TypeID) (types.BuiltinKind, bool) {
	if snapshot == nil {
		return 0, false
	}
	key, ok := snapshot.Key(id)
	if !ok {
		return 0, false
	}
	return key.Builtin()
}

// wantName returns the human-readable name of the entry's resolved integer
// width ("i32" or "i64") for error messages that name the wanted type.
func wantName(width types.BuiltinKind) string {
	name, _ := builtinName(width)
	return name
}

// cType returns the fixed-width C integer type corresponding to a Pebble
// integer builtin. Int and uint use Pebble's platform-independent 32-bit and
// 64-bit representations respectively.
func cType(width types.BuiltinKind) string {
	switch width {
	case types.Int:
		return "int32_t"
	case types.Uint:
		return "uint64_t"
	case types.I8:
		return "int8_t"
	case types.I16:
		return "int16_t"
	case types.I32:
		return "int32_t"
	case types.I64:
		return "int64_t"
	case types.U8:
		return "uint8_t"
	case types.U16:
		return "uint16_t"
	case types.U32:
		return "uint32_t"
	case types.U64:
		return "uint64_t"
	}
	return ""
}

// floatCType returns the C floating-point type corresponding to a Pebble
// float builtin: float for f32, double for f64. It is deliberately a separate
// helper from cType rather than an extension of it: cType is integer-specific
// by name and doc-comment, and its ""-means-not-an-integer contract is relied
// on by several integer-only paths (validateHelperSignature, the
// buildScalarInitializeCore fall-through), so overloading it with float kinds
// would change what those paths mean. Anything that is not a float builtin
// returns "", matching cType's convention.
func floatCType(width types.BuiltinKind) string {
	switch width {
	case types.F32:
		return "float"
	case types.F64:
		return "double"
	}
	return ""
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

// optionalUnwrapSuffix returns the pebble_rt_checked_unwrap_* helper suffix
// for an optional payload of the given type: "i32" for an int/i32 payload,
// "i64" for an i64 payload, "u64" for a uint or u64 payload (both carry the
// C type uint64_t, so one runtime helper reads both back at their true
// width), and "bool" for a bool payload. Any other payload type (a narrower
// fixed-width integer without a runtime unwrap helper yet, a char, str,
// tuple, or struct) yields "", a clean rejection for the caller. The helper
// must be selected from the PAYLOAD's own type rather than the ambient entry
// width: a uint payload's .value field is uint64_t, which only
// pebble_rt_checked_unwrap_u64 reads back at its true width, not the
// entry-width helper.
func optionalUnwrapSuffix(snapshot *types.Snapshot, id types.TypeID) string {
	if isBool(snapshot, id) {
		return "bool"
	}
	if isPointer(snapshot, id) {
		// A pointer payload's .value field is `<pointee> *` and the unwrap is
		// only the has_value check (a null payload value is a perfectly valid
		// unwrap result, so there is no dereference at this point) — one
		// void *-based helper serves every pointee.
		return "ptr"
	}
	payloadWidth, ok := resolvedBuiltin(snapshot, id)
	if !ok {
		return ""
	}
	switch payloadWidth {
	case types.Int, types.I32:
		return "i32"
	case types.I64:
		return "i64"
	case types.Uint, types.U64:
		return "u64"
	}
	return ""
}

// checkedSuffix returns the pebble_rt_checked_* function-name suffix for the
// given width: "i32" for an int or i32 entry, "i64" for an i64 entry, "u64"
// for a u64 entry. It is exactly the type's name for the fixed-width entries,
// but named for what it selects — the width-specific runtime helper family.
func checkedSuffix(width types.BuiltinKind) string {
	switch width {
	case types.Int:
		return "i32"
	case types.I32:
		return "i32"
	case types.I64:
		return "i64"
	case types.U64:
		return "u64"
	}
	return ""
}

func childFloatSuffix(width types.BuiltinKind) string {
	switch width {
	case types.F32:
		return "f32"
	case types.F64:
		return "f64"
	}
	return ""
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

// helperFunction is the C text of one reachable helper function: a static
// function named deterministically pebble_fn_<symbolID> from the callee's
// stable IR identity (mirroring the pebble_local_<symbolID> naming
// discipline — never a counter), taking the Pebble context the same way
// pebble_user_main does plus one parameter declaration per callee parameter,
// each named pebble_local_<paramSymbol>. %s is the C return type for the
// entry's resolved width (cType), %d the callee's symbol ID, the third %s the
// comma-separated parameter declaration list (", <cType> pebble_local_<id>",
// empty for a zero-parameter callee), the fourth %s one
// `    (void)pebble_local_<id>;` per parameter (suppressing the confirmed
// -Wunused-parameter warning for a parameter the body never reads, the same
// discipline the (void)ctx; below applies to the context), and the last %s the
// helper's body statements built by buildBlock at depth 0 (4-space indent,
// exactly like the entry's own body).
const helperFunction = `static %s %s(PebbleContext *ctx%s) {
    (void)ctx;
%s%s
}`

// helperPrototype is the C text of one reachable helper's forward
// declaration: the same `static <return> pebble_fn_<symbolID>(PebbleContext
// *ctx, ...)` shape as its definition (helperFunction) but terminated by a
// semicolon — a C prototype. %s is the C return type, %d the callee's symbol
// ID, and the third %s the comma-separated parameter declaration list (the
// same ", <cType> pebble_local_<id>" list the definition uses, empty for a
// zero-parameter callee). Every reachable helper gets a prototype BEFORE any
// definition, which is the standard C fix for recursive and
// mutually-recursive functions: a call anywhere in the file (including inside
// a function whose own definition comes earlier) always has a preceding
// prototype in scope, so no call ever references an undeclared identifier.
const helperPrototype = `static %s %s(PebbleContext *ctx%s);`

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

// describeType renders a TypeID into a short human-readable spelling for
// error messages. It only needs to be good enough to name what was found.
func describeType(snapshot *types.Snapshot, id types.TypeID) string {
	if snapshot == nil {
		return fmt.Sprintf("type %d", id)
	}
	key, ok := snapshot.Key(id)
	if !ok {
		return fmt.Sprintf("type %d", id)
	}
	switch key.Kind() {
	case types.Builtin:
		if builtin, ok := key.Builtin(); ok {
			if name, ok := builtinName(builtin); ok {
				return name
			}
		}
	case types.Pointer:
		if child, ok := key.Child(); ok {
			return "*" + describeType(snapshot, child)
		}
	case types.Array:
		if length, child, ok := key.Array(); ok {
			return fmt.Sprintf("[%d]%s", length, describeType(snapshot, child))
		}
	case types.Slice:
		if child, ok := key.Child(); ok {
			return "[]" + describeType(snapshot, child)
		}
	case types.Tuple:
		if elements, ok := key.Elements(); ok {
			parts := make([]string, len(elements))
			for i, element := range elements {
				parts[i] = describeType(snapshot, element)
			}
			return "(" + strings.Join(parts, ", ") + ")"
		}
	case types.Optional:
		if child, ok := key.Child(); ok {
			return "?" + describeType(snapshot, child)
		}
	case types.Function:
		if _, parameters, result, _, ok := key.Function(); ok {
			parts := make([]string, len(parameters))
			for i, parameter := range parameters {
				parts[i] = describeType(snapshot, parameter)
			}
			return "fn(" + strings.Join(parts, ", ") + ") " + describeType(snapshot, result)
		}
	case types.Nominal:
		if declaration, _, ok := key.Nominal(); ok {
			return fmt.Sprintf("nominal(symbol %d)", declaration)
		}
	case types.TypeParameter:
		if declaration, ok := key.TypeParameter(); ok {
			return fmt.Sprintf("type-parameter(symbol %d)", declaration)
		}
	}
	return fmt.Sprintf("type %d", id)
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
