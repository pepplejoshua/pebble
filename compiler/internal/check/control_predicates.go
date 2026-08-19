package check

import (
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
)

type controlKind uint8

const (
	controlFunction controlKind = iota + 1
	controlBlock
	controlReturn
	controlIf
	controlWhile
	controlRangeLoop
	controlFor
	controlSwitch
	controlSwitchCase
	controlBreak
	controlContinue
	controlDefer
	controlPrint
	controlExpression
	controlBinding
)

type statementForm uint8

const (
	statementPrint statementForm = iota + 1
	statementDiscard
	statementAssignment
	statementCall
	statementPostfixUpdate
	statementOther
)

type controlValueRole uint8

const (
	valueCondition controlValueRole = iota + 1
	valueSubject
	valueCase
	valueReturn
	valueRangeStart
	valueRangeEnd
	valueRangeIterator
	valuePrintOperand
	valueDiscarded
)

type controlValue struct {
	Role    controlValueRole
	Value   valueID
	Ordinal uint32
	Syntax  symbol.SyntaxRef
}

type structuralRole uint8

const (
	roleThen structuralRole = iota + 1
	roleElse
	roleInitializer
	roleUpdate
	roleBody
	roleCase
)

type structuralChild struct {
	Role    structuralRole
	Ordinal uint32 // zero-based; nonzero only for roleCase
	Arm     symbol.SyntaxRef
}

type controlRecord struct {
	Header                                        recordHeader
	Kind                                          controlKind
	Region, Target                                controlID
	Callable                                      callableRef
	StatementForm                                 statementForm
	Values                                        []controlValue
	Composition                                   []structuralChild
	ConditionPresent, ElsePresent, RangeInclusive bool
	PrintNewline                                  bool
	SyntheticSyntax                               bool
	// IteratorSymbol is the range loop's bound iterator variable (`loop
	// start..end : name { ... }`), zero when the loop has no bound name.
	// buildRangeLoop attaches it to the RangeLoop TIR node's own Symbol
	// field, since nothing else in typed IR records this declaration (the
	// general binding pass in ir_builder.go deliberately skips
	// bindingRangeIterator records the same way it skips bindingParameter —
	// each is meant to be attached directly by its owning node's own
	// builder instead).
	IteratorSymbol symbol.SymbolID
}

type deferRecord struct {
	Header    recordHeader
	Region    controlID
	Ordinal   uint32
	Statement symbol.SyntaxRef
}

// regionOwningControl names the closed set of control kinds that allocate their
// own lexical region. Every other kind is a leaf naming the region that contains
// it. Value suppression uses its own predicate and is unrelated to this one.
func regionOwningControl(kind controlKind) bool {
	switch kind {
	case controlFunction, controlBlock, controlIf, controlWhile, controlRangeLoop,
		controlFor, controlSwitch, controlSwitchCase:
		return true
	default:
		return false
	}
}

func validControlRecord(value controlRecord) bool {
	if value.Kind < controlFunction || value.Kind > controlBinding {
		return false
	}
	if value.StatementForm < statementPrint || value.StatementForm > statementOther {
		return false
	}
	if value.Region == 0 || value.Callable.Syntax == (symbol.SyntaxRef{}) {
		return false
	}
	if value.Target != 0 && value.Kind != controlBreak && value.Kind != controlContinue {
		return false
	}
	condition := value.Kind == controlIf || value.Kind == controlWhile || value.Kind == controlFor
	elseArm := value.Kind == controlIf || value.Kind == controlSwitch
	if value.ConditionPresent && !condition || value.ElsePresent && !elseArm {
		return false
	}
	if value.RangeInclusive && value.Kind != controlRangeLoop {
		return false
	}
	if value.PrintNewline && value.Kind != controlPrint {
		return false
	}
	for _, entry := range value.Values {
		if entry.Role < valueCondition || entry.Role > valueDiscarded || entry.Value == 0 {
			return false
		}
	}
	if !validComposition(value) {
		return false
	}
	return true
}

// allowedStructuralRoles reports the closed set of structuralRole values a
// controlKind may retain in its Composition. A kind absent from this
// function's case list must retain an empty Composition.
func allowedStructuralRoles(kind controlKind) map[structuralRole]bool {
	switch kind {
	case controlIf:
		return map[structuralRole]bool{roleThen: true, roleElse: true}
	case controlWhile:
		return map[structuralRole]bool{roleBody: true}
	case controlFor:
		return map[structuralRole]bool{roleInitializer: true, roleUpdate: true, roleBody: true}
	case controlRangeLoop:
		return map[structuralRole]bool{roleBody: true}
	case controlSwitch:
		return map[structuralRole]bool{roleCase: true, roleElse: true}
	case controlSwitchCase:
		return map[structuralRole]bool{roleBody: true}
	default:
		return nil
	}
}

// validComposition checks every closed invariant on controlRecord.Composition
// that is derivable from the record's own fields alone: role membership in
// its kind's allowed set, Ordinal zero outside roleCase, no zero Arm, no
// duplicate (Role, Ordinal) pair, contiguous ascending roleCase ordinals from
// zero, and an empty Composition for any kind outside the region-owning
// table. This does not and cannot prove an Arm is graph-owned (generation.
// addRecord's job) or that Composition matches expectedComposition (the
// freeze audit's job); it proves only internal well-formedness.
func validComposition(value controlRecord) bool {
	allowed := allowedStructuralRoles(value.Kind)
	if allowed == nil {
		return len(value.Composition) == 0
	}
	seen := make(map[structuralRole]map[uint32]bool)
	for _, entry := range value.Composition {
		if entry.Role < roleThen || entry.Role > roleCase || !allowed[entry.Role] {
			return false
		}
		if entry.Role != roleCase && entry.Ordinal != 0 {
			return false
		}
		if entry.Arm == (symbol.SyntaxRef{}) {
			return false
		}
		if seen[entry.Role] == nil {
			seen[entry.Role] = make(map[uint32]bool)
		}
		if seen[entry.Role][entry.Ordinal] {
			return false
		}
		seen[entry.Role][entry.Ordinal] = true
	}
	return validCompositionSequence(value)
}

// validCompositionSequence enforces the closed per-kind cardinality and order
// contract. Role membership alone is insufficient: a controlIf carrying two
// roleThen entries, or roleElse before roleThen, or a controlFor whose body
// precedes its initializer, all satisfy membership while contradicting the
// authored structure expectedComposition produces. Each kind's entries must
// appear in exactly the order and count listed here.
func validCompositionSequence(value controlRecord) bool {
	composition := value.Composition
	switch value.Kind {
	case controlIf:
		// roleThen first, then roleElse exactly when ElsePresent.
		if value.ElsePresent {
			return len(composition) == 2 &&
				composition[0].Role == roleThen && composition[1].Role == roleElse
		}
		return len(composition) == 1 && composition[0].Role == roleThen
	case controlWhile, controlRangeLoop, controlSwitchCase:
		return len(composition) == 1 && composition[0].Role == roleBody
	case controlFor:
		// Optional roleInitializer, optional roleUpdate, then exactly one
		// roleBody, in that order.
		index := 0
		if index < len(composition) && composition[index].Role == roleInitializer {
			index++
		}
		if index < len(composition) && composition[index].Role == roleUpdate {
			index++
		}
		return index == len(composition)-1 && composition[index].Role == roleBody
	case controlSwitch:
		// roleCase ordinals ascend from zero in authored order; exactly one
		// roleElse when ElsePresent, in its authored position; nothing else.
		next := uint32(0)
		elseCount := 0
		for index, entry := range composition {
			switch entry.Role {
			case roleCase:
				if elseCount != 0 || entry.Ordinal != next {
					return false
				}
				next++
			case roleElse:
				if elseCount != 0 || index != len(composition)-1 {
					return false
				}
				elseCount++
			default:
				return false
			}
		}
		if value.ElsePresent {
			return elseCount == 1
		}
		return elseCount == 0
	default:
		return len(composition) == 0
	}
}

// expectedComposition is the sole derivation of a region-owning statement's
// Composition. It reads only the immutable surface node and its children, so
// it returns the same value whenever it is called; it never consults
// resolution, records, regions, or traversal state. 06a.7's population calls
// it once, at retention time, and uses its result directly. 06a.8's freeze
// audit calls it again, independently, from the frozen module graph, and
// compares the result field-for-field against what was actually retained,
// proving population used it correctly and that nothing corrupted the value
// between retention and freeze, not merely that this function's own logic is
// self-consistent.
//
// It must not reuse the Missing/Error-filtering helpers that serve
// constraint generation (semanticRefs, forCondition, rangeLoopParts), because
// a damaged arm's reference must be retained rather than dropped.
func expectedComposition(ref symbol.SyntaxRef, node syntax.Node, tree *syntax.Tree) []structuralChild {
	children := node.Children()
	arm := func(id syntax.NodeID) symbol.SyntaxRef {
		return symbol.SyntaxRef{Module: ref.Module, Node: id}
	}
	switch node.Kind() {
	case syntax.IfStmt:
		var out []structuralChild
		if len(children) >= 2 {
			out = append(out, structuralChild{Role: roleThen, Arm: arm(children[1])})
		}
		if len(children) >= 3 {
			out = append(out, structuralChild{Role: roleElse, Arm: arm(children[2])})
		}
		return out
	case syntax.WhileStmt:
		if len(children) >= 2 {
			return []structuralChild{{Role: roleBody, Arm: arm(children[1])}}
		}
		return nil
	case syntax.ForStmt:
		var out []structuralChild
		if node.Data()&syntax.ForInitializerPresent != 0 && len(children) >= 1 {
			out = append(out, structuralChild{Role: roleInitializer, Arm: arm(children[0])})
		}
		if len(children) == 0 {
			return out
		}
		bodyIndex := len(children) - 1
		if node.Data()&syntax.ForUpdatePresent != 0 && bodyIndex >= 1 {
			out = append(out, structuralChild{Role: roleUpdate, Arm: arm(children[bodyIndex-1])})
		}
		return append(out, structuralChild{Role: roleBody, Arm: arm(children[bodyIndex])})
	case syntax.RangeLoopStmt:
		if len(children) == 0 {
			return nil
		}
		return []structuralChild{{Role: roleBody, Arm: arm(children[len(children)-1])}}
	case syntax.SwitchStmt:
		var out []structuralChild
		caseOrdinal := uint32(0)
		for _, id := range children {
			child, ok := tree.Node(id)
			if !ok || child.Kind() != syntax.SwitchCase {
				continue // a non-case recovery insertion names no role
			}
			if child.Token() == syntax.KwElse {
				out = append(out, structuralChild{Role: roleElse, Arm: arm(id)})
				continue
			}
			out = append(out, structuralChild{Role: roleCase, Ordinal: caseOrdinal, Arm: arm(id)})
			caseOrdinal++
		}
		return out
	case syntax.SwitchCase:
		if len(children) == 0 {
			return nil
		}
		return []structuralChild{{Role: roleBody, Arm: arm(children[len(children)-1])}}
	default:
		return nil
	}
}
