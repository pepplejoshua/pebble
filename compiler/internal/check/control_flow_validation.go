package check

import (
	"sort"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

type controlExitKind uint8

const (
	exitFallthrough controlExitKind = iota + 1
	exitReturn
	exitBreak
	exitContinue
	exitDiverge
)

type controlExit struct {
	kind   controlExitKind
	target controlID
}

// validateControlFlow performs the tree-free flow and statement legality pass.
// It deliberately does not validate switch categories or defer edges; those
// consumers use the same exit representation in later validation passes.
func validateControlFlow(handoff *solveHandoff, records *solvedRecords, diagnostics *diagnostic.DiagnosticSet, config Config) bool {
	if !canContinueWithPartial(handoff, config) || records == nil || handoff.Semantics == nil {
		return true
	}
	reporter := newValidationReporter(diagnostics, normalizeConfig(config).MaxDiagnostics)
	failed := false
	report := func(ctrl *controlRecord, code diagnostic.Code, severity diagnostic.Severity, message string) {
		if severity == diagnostic.Error {
			failed = true
		}
		var primary diagnostic.Label
		if ctrl != nil {
			primary = diagnostic.Label{Span: ctrl.Header.Span}
		}
		reporter.add(diagnostic.Diagnostic{Severity: severity, Code: code, Message: message, Primary: primary})
	}

	controls := handoff.Records.Controls()
	retained := handoff.Records.Records()
	byRegion := make(map[controlID][]*controlRecord)
	owner := make(map[controlID]*controlRecord)
	bySyntax := make(map[symbol.SyntaxRef]*controlRecord)
	variantBySyntax := collectVariantBySyntax(handoff)
	for i := range retained {
		if retained[i].Control == nil || !activeOperatorRecord(handoff, retained[i].Header) {
			continue
		}
		ctrl := retained[i].Control
		byRegion[ctrl.Region] = append(byRegion[ctrl.Region], ctrl)
		bySyntax[ctrl.Header.Syntax] = ctrl
		if regionOwningControl(ctrl.Kind) {
			owner[ctrl.Region] = ctrl
		}
	}

	contains := func(exits []controlExit, want controlExit) bool {
		for _, exit := range exits {
			if exit == want {
				return true
			}
		}
		return false
	}
	addExit := func(exits []controlExit, exit controlExit) []controlExit {
		if !contains(exits, exit) {
			return append(exits, exit)
		}
		return exits
	}
	ancestor := func(from, target controlID) bool {
		if from == 0 || target == 0 || uint64(from) > uint64(len(controls)) || uint64(target) > uint64(len(controls)) {
			return false
		}
		for current := from; current != 0; current = controls[current-1].Parent {
			if current == target {
				return true
			}
		}
		return false
	}

	var evalRegion func(controlID, bool) []controlExit
	var evalRecord func(*controlRecord, bool) []controlExit
	var evalSequence func([]*controlRecord, bool) []controlExit
	warningSuppressed := 0

	resultType := func(ctrl *controlRecord) (types.TypeID, bool) {
		if ctrl == nil || ctrl.Callable.Symbol == 0 {
			return 0, false
		}
		signature, ok := handoff.Semantics.Signature(ctrl.Callable.Symbol)
		if !ok {
			return 0, false
		}
		template, ok := handoff.Semantics.Template(signature.Result)
		return template.Known, ok && template.Kind == infer.TemplateKnown
	}
	isVoidResult := func(ctrl *controlRecord) bool {
		result, ok := resultType(ctrl)
		return ok && result == handoff.Semantics.Types().Builtins().Void
	}

	scalarPrintable := func(typeID types.TypeID) bool {
		key, ok := handoff.Semantics.Types().Key(typeID)
		if !ok {
			return false
		}
		builtin, ok := key.Builtin()
		return ok && (builtin == types.Bool || builtin == types.Char || builtin == types.Str || isIntegerBuiltin(builtin) || isFloatBuiltin(builtin))
	}
	// printableType is the RECURSIVE print gate for one resolved concrete type:
	// a scalar builtin (bool, char, str, any integer width, any float width), or
	// — composite print slice 1 — a struct value, or — slice 2 — a tuple or
	// fixed array, or — slice 4 — a slice, or — slice 5 — a plain enum value,
	// or — slice 6 — a tagged union value, or — slice 8 — a pointer value (a
	// LEAF: address-only, never recursed into), provided every field/element/
	// variant-payload is itself printable by this same rule (slice 3: nested
	// aggregates). The recursion terminates because a field/element type is a
	// strictly nested by-value aggregate; Pebble rejects a genuinely
	// infinite-size composite at declaration/binding time, so no by-value cycle
	// can reach here (verified against a self-referential struct fixture). A
	// plain enum is a LEAF case — it has no fields to recurse into — but it is
	// still routed through this shared function (slice 5). A struct with any
	// non-struct/non-scalar field (a function value — slice 9) still rejects.
	// A field/element of a type that is not a concrete
	// known type (a generic struct's parameter-typed field, which resolves only
	// against a specific instantiation's type arguments) is not provably
	// printable at the declaration level, so it rejects too.
	var printableType func(typeID types.TypeID) bool
	// memberPrintable resolves one struct member's declared template to its
	// concrete known type and recurses; a member whose type is not a concrete
	// known type (a type parameter, an unresolved template) is not provably
	// printable.
	memberPrintable := func(memberType infer.TemplateID) bool {
		template, ok := handoff.Semantics.Template(memberType)
		if !ok || template.Kind != infer.TemplateKnown {
			return false
		}
		return printableType(template.Known)
	}
	// unionMemberPayloadPrintable resolves one tagged-union variant's payload
	// template to its concrete known type and recurses (composite print slice
	// 6). A payload-less variant — a variant declared with a void payload — is
	// trivially printable, since there is no payload value to format; a
	// payload-carrying variant is printable exactly when its payload type is,
	// by the same recursive rule. A member whose type is not a concrete known
	// type is not provably printable.
	unionMemberPayloadPrintable := func(memberType infer.TemplateID) bool {
		template, ok := handoff.Semantics.Template(memberType)
		if !ok || template.Kind != infer.TemplateKnown {
			return false
		}
		if template.Known == handoff.Semantics.Types().Builtins().Void {
			return true
		}
		return printableType(template.Known)
	}
	printableType = func(typeID types.TypeID) bool {
		if scalarPrintable(typeID) {
			return true
		}
		key, ok := handoff.Semantics.Types().Key(typeID)
		if !ok {
			return false
		}
		switch key.Kind() {
		case types.Tuple:
			elements, ok := key.Elements()
			if !ok {
				return false
			}
			for _, element := range elements {
				if !printableType(element) {
					return false
				}
			}
			return true
		case types.Array:
			_, element, ok := key.Array()
			return ok && printableType(element)
		case types.Slice:
			element, ok := key.Child()
			return ok && printableType(element)
		case types.Optional:
			payload, ok := key.Child()
			return ok && printableType(payload)
		case types.Pointer:
			// A pointer is a LEAF printable case (composite print slice 8),
			// exactly like a plain enum: printing a pointer emits only its
			// address (or the nil literal) and NEVER dereferences the pointee,
			// so a pointer type is printable REGARDLESS of what its pointee
			// type is — no key.Child() recursion here, by design. This is what
			// makes a self-referential pointer cycle trivially safe to print:
			// there is no recursion into the pointee at all, for ANY pointer.
			return true
		case types.Function:
			// A function type is a LEAF printable case (composite print slice
			// 9): printing a function value either emits its declared source
			// name when the operand directly names a top-level function, or
			// falls back to the underlying C function pointer's raw address —
			// neither branch recurses into parameter or result types, so every
			// function type is unconditionally printable.
			return true
		case types.Nominal:
		default:
			return false
		}
		decl, _, ok := key.Nominal()
		if !ok {
			return false
		}
		declaration, ok := handoff.Semantics.TypeDeclaration(decl)
		if !ok {
			return false
		}
		switch declaration.Nominal {
		case infer.NominalEnum:
			// A plain enum is a LEAF printable case (composite print slice 5):
			// it has no fields to recurse into, so merely recognizing the
			// declared kind is enough. The declaration-level distinction is the
			// same one this package's switch validation uses — a plain enum
			// is NominalEnum while a tagged union is NominalTaggedUnion, even
			// though both share the types.Nominal key kind — so a tagged union
			// is NOT admitted by this leaf case.
			return true
		case infer.NominalTaggedUnion:
			// A tagged union is printable exactly when every declared
			// variant's payload type is itself printable (composite print slice
			// 6). The checker cannot know at compile time which variant will
			// be active at runtime, so it is conservative — the same
			// reasoning slice 1 used for struct fields. A payload-less variant
			// (a void payload) is trivially printable (unionMemberPayload
			// Printable returns true for it); a payload-carrying variant's
			// payload type is checked recursively by the same printableType
			// rule, so a union any of whose variants carries a not-yet-
			// printable payload (a pointer, an optional — later slices) is
			// rejected outright.
			for _, member := range declaration.Members {
				if !unionMemberPayloadPrintable(member.Type) {
					return false
				}
			}
			return true
		case infer.NominalStruct:
			for _, member := range declaration.Members {
				if !memberPrintable(member.Type) {
					return false
				}
			}
			return true
		default:
			// A legacy untagged union (NominalUnion), an extern type, or any
			// other nominal is not a plain enum, a tagged union, or a struct,
			// so it stays unprintable.
			return false
		}
	}
	valuePrintable := func(value valueID) bool {
		resolved, ok := records.Root(value)
		if !ok || resolved.State != infer.TypeFinal {
			return false
		}
		return printableType(resolved.Type)
	}

	evalRecord = func(ctrl *controlRecord, reachable bool) []controlExit {
		if ctrl == nil {
			return nil
		}
		switch ctrl.Kind {
		case controlBlock:
			return evalRegion(ctrl.Region, reachable)
		case controlReturn:
			if len(ctrl.Values) != 0 && isVoidResult(ctrl) {
				report(ctrl, CodeMissingReturn, diagnostic.Error, "value return is forbidden for void function")
			}
			return []controlExit{{kind: exitReturn}}
		case controlBreak, controlContinue:
			if ctrl.Target == 0 || !ancestor(ctrl.Region, ctrl.Target) {
				report(ctrl, CodeInvalidTarget, diagnostic.Error, "jump has a missing or inconsistent target")
				return []controlExit{{kind: map[controlKind]controlExitKind{controlBreak: exitBreak, controlContinue: exitContinue}[ctrl.Kind], target: ctrl.Target}}
			}
			targetOwner := owner[ctrl.Target]
			if targetOwner == nil || (ctrl.Kind == controlContinue && targetOwner.Kind != controlWhile && targetOwner.Kind != controlFor && targetOwner.Kind != controlRangeLoop) || (ctrl.Kind == controlBreak && targetOwner.Kind != controlWhile && targetOwner.Kind != controlFor && targetOwner.Kind != controlRangeLoop && targetOwner.Kind != controlSwitch) {
				report(ctrl, CodeInvalidTarget, diagnostic.Error, "jump target is not an enclosing control target")
			}
			kind := exitBreak
			if ctrl.Kind == controlContinue {
				kind = exitContinue
			}
			return []controlExit{{kind: kind, target: ctrl.Target}}
		case controlIf:
			var result []controlExit
			for _, child := range ctrl.Composition {
				if arm := bySyntax[child.Arm]; arm != nil {
					for _, exit := range evalRecord(arm, reachable) {
						result = addExit(result, exit)
					}
				}
			}
			if !ctrl.ElsePresent {
				result = addExit(result, controlExit{kind: exitFallthrough})
			}
			return result
		case controlWhile, controlFor, controlRangeLoop:
			if ctrl.Kind == controlRangeLoop && ctrl.IteratorSymbol == 0 {
				report(ctrl, CodeUnboundRangeIterator, diagnostic.Error, "a range loop requires an explicit iterator name (`loop start..end : name { ... }`)")
			}
			var body []controlExit
			for _, child := range ctrl.Composition {
				if child.Role == roleBody {
					if arm := bySyntax[child.Arm]; arm != nil {
						body = evalRecord(arm, reachable)
					}
				}
			}
			result := []controlExit{}
			breakFound := false
			for _, exit := range body {
				switch {
				case exit.kind == exitContinue && exit.target == ctrl.Region:
				case exit.kind == exitBreak && exit.target == ctrl.Region:
					breakFound = true
				case exit.kind == exitFallthrough:
					// body completed normally; the loop boundary consumes
					// this and the infinite/breakFound analysis below
					// decides whether the loop construct itself can fall
					// through.
				default:
					result = addExit(result, exit)
				}
			}
			infinite := ctrl.Kind == controlFor && !ctrl.ConditionPresent
			if !infinite && ctrl.ConditionPresent && len(ctrl.Values) > 0 {
				if _, ok := records.Root(ctrl.Values[0].Value); ok {
					if syntaxRoot, found := handoff.Roots.Root(ctrl.Values[0].Value); found {
						if constant, found := records.Constant(syntaxRoot.Syntax); found && constant.State == constantKnown && constant.Value.Kind == constantBoolean && constant.Value.Boolean {
							infinite = true
						}
					}
				}
			}
			if !infinite || breakFound {
				result = addExit(result, controlExit{kind: exitFallthrough})
			}
			return result
		case controlSwitch, controlSwitchCase:
			var result []controlExit
			for _, child := range ctrl.Composition {
				if arm := bySyntax[child.Arm]; arm != nil {
					for _, exit := range evalRecord(arm, reachable) {
						if exit.kind == exitBreak && exit.target == ctrl.Region {
							result = addExit(result, controlExit{kind: exitFallthrough})
						} else {
							result = addExit(result, exit)
						}
					}
				}
			}
			if ctrl.Kind == controlSwitch && !ctrl.ElsePresent && !switchIsExhaustive(handoff, records, ctrl, bySyntax, variantBySyntax) {
				result = addExit(result, controlExit{kind: exitFallthrough})
			}
			return result
		case controlExpression:
			if ctrl.StatementForm == statementDiscard && len(ctrl.Values) != 0 {
				value, ok := records.Root(ctrl.Values[0].Value)
				void := false
				if ok && value.State == infer.TypeFinal {
					void = value.Type == handoff.Semantics.Types().Builtins().Void
				}
				if !void {
					report(ctrl, CodeStatementForm, diagnostic.Error, "expression statement discards a non-void value")
				}
			}
		case controlPrint:
			for _, value := range ctrl.Values {
				if !valuePrintable(value.Value) {
					report(ctrl, CodeStatementForm, diagnostic.Error, "print operand is not printable")
				}
			}
		}
		return []controlExit{{kind: exitFallthrough}}
	}

	evalSequence = func(sequence []*controlRecord, reachable bool) []controlExit {
		result := []controlExit{}
		canReach := reachable
		unreachableReported := false
		for _, ctrl := range sequence {
			if ctrl.Kind == controlFunction {
				continue
			}
			if !canReach {
				if warningSuppressed == 0 && !unreachableReported {
					report(ctrl, CodeUnreachable, diagnostic.Warning, "statement is unreachable")
					unreachableReported = true
				}
				warningSuppressed++
			}
			exits := evalRecord(ctrl, canReach)
			if !canReach {
				warningSuppressed--
				continue
			}
			for _, exit := range exits {
				if exit.kind != exitFallthrough {
					result = addExit(result, exit)
				}
			}
			canReach = contains(exits, controlExit{kind: exitFallthrough})
		}
		if canReach {
			result = addExit(result, controlExit{kind: exitFallthrough})
		}
		return result
	}

	evalRegion = func(region controlID, reachable bool) []controlExit {
		sequence := append([]*controlRecord(nil), byRegion[region]...)
		if first := owner[region]; first != nil && (first.Kind == controlBlock || first.Kind == controlFunction) {
			if first.Kind == controlBlock {
				for i, ctrl := range sequence {
					if ctrl == first {
						sequence = sequence[i+1:]
						break
					}
				}
			}
			if uint64(region) <= uint64(len(controls)) {
				for _, child := range controls[region-1].Children {
					if childOwner := owner[child]; childOwner != nil {
						seen := false
						for _, existing := range sequence {
							seen = seen || existing == childOwner
						}
						if !seen {
							sequence = append(sequence, childOwner)
						}
					}
				}
			}
			sort.SliceStable(sequence, func(i, j int) bool {
				return sequence[i].Header.Span.Start < sequence[j].Header.Span.Start
			})
		}
		if len(sequence) == 0 {
			return []controlExit{{kind: exitFallthrough}}
		}
		return evalSequence(sequence, reachable)
	}

	for _, root := range controls {
		if root.Parent != 0 {
			continue
		}
		function := owner[root.ID]
		if function == nil || function.Kind != controlFunction {
			continue
		}
		exits := evalRegion(root.ID, true)
		if !isVoidResult(function) && contains(exits, controlExit{kind: exitFallthrough}) {
			report(function, CodeMissingReturn, diagnostic.Error, "non-void function can fall through without returning")
		}
	}
	reporter.flush()
	return !failed
}
