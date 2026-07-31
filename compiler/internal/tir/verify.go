package tir

import (
	"fmt"

	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

// verify runs the total verifier over the unit. It returns nil only when every
// closed invariant holds. Work and retained errors are bounded before append.
func verify(u *Unit, maxErrors uint32) error {
	if u == nil {
		return fmt.Errorf("nil unit")
	}
	if u.snapshot == nil {
		return fmt.Errorf("unit has no owning type snapshot")
	}
	regionCount := int(u.regionCount)
	v := &verifier{
		u:              u,
		maxErrors:      maxErrors,
		nodeFunction:   make([]FunctionID, len(u.nodes)),
		regionFunction: make([]FunctionID, regionCount),
		definedTemps:   make(map[TempID]NodeID),
		availableTemps: make(map[FunctionID]map[TempID]bool),
	}
	v.run()
	if len(v.errors) > 0 {
		return v.errors[0]
	}
	return nil
}

type verifier struct {
	u              *Unit
	maxErrors      uint32
	errors         []error
	nodeFunction   []FunctionID
	regionFunction []FunctionID
	definedTemps   map[TempID]NodeID
	availableTemps map[FunctionID]map[TempID]bool
}

func (v *verifier) errorf(format string, args ...any) {
	if uint32(len(v.errors)) >= v.maxErrors {
		return
	}
	v.errors = append(v.errors, fmt.Errorf(format, args...))
}

func (v *verifier) run() {
	v.verifySnapshotOwnership()
	v.computeNodeFunctions()
	for i := range v.u.nodes {
		v.verifyNode(NodeID(i + 1))
	}
	v.verifySourceMap()
	v.verifyFunctions()
	v.verifyRegions()
	v.computeRegionFunctions()
	v.verifyTemps()
	v.verifyDeclarations()
	v.verifyInstantiations()
}

func (v *verifier) verifySnapshotOwnership() {
	for i, n := range v.u.nodes {
		v.checkTypeInSnapshot("node", NodeID(i+1), n.Type)
		v.checkTypeInSnapshot("node", NodeID(i+1), n.TypeArg)
		v.checkTypeInSnapshot("node", NodeID(i+1), n.ResultType)
		v.checkTypeInSnapshot("node", NodeID(i+1), n.FunctionType)
		for j, p := range n.Parameters {
			v.checkTypeInSnapshot(fmt.Sprintf("node[%d].parameter[%d]", i, j), 0, p.Type)
		}
		for j, ta := range n.TypeArgs {
			v.checkTypeInSnapshot(fmt.Sprintf("node[%d].typearg[%d]", i, j), 0, ta)
		}
	}
	for i, g := range v.u.globals {
		v.checkTypeInSnapshot(fmt.Sprintf("global[%d]", i), 0, g.Type)
	}
	for i, r := range v.u.requirements {
		v.checkTypeInSnapshot(fmt.Sprintf("requirement[%d]", i), 0, r.Subject)
	}
	for i, in := range v.u.instantiations {
		for j, ta := range in.TypeArgs {
			v.checkTypeInSnapshot(fmt.Sprintf("instantiation[%d].typearg[%d]", i, j), 0, ta)
		}
		for j, r := range in.Requirements {
			v.checkTypeInSnapshot(fmt.Sprintf("instantiation[%d].requirement[%d]", i, j), 0, r.Subject)
		}
	}
}

func (v *verifier) checkTypeInSnapshot(ctx string, id NodeID, tid types.TypeID) {
	if tid == 0 {
		return
	}
	if !v.u.snapshot.Contains(tid) {
		if id != 0 {
			v.errorf("%s node %d references type %d not owned by snapshot", ctx, id, tid)
		} else {
			v.errorf("%s references type %d not owned by snapshot", ctx, tid)
		}
	}
}

func (v *verifier) computeNodeFunctions() {
	for _, f := range v.u.functions {
		if f.Node == 0 {
			continue
		}
		v.markNodeFunction(f.Node, f.FunctionID, make(map[NodeID]bool))
	}
}

func (v *verifier) markNodeFunction(rootID NodeID, fid FunctionID, visited map[NodeID]bool) {
	stack := []NodeID{rootID}
	for len(stack) > 0 {
		id := stack[len(stack)-1]
		stack = stack[:len(stack)-1]
		if !id.IsValid() || uint64(id) > uint64(len(v.u.nodes)) {
			continue
		}
		if visited[id] {
			continue
		}
		visited[id] = true
		if existing := v.nodeFunction[id-1]; existing != 0 && existing != fid {
			v.errorf("node %d is owned by multiple functions (%d and %d)", id, existing, fid)
			continue
		}
		v.nodeFunction[id-1] = fid
		n := v.u.nodes[id-1]
		for _, child := range n.Children {
			stack = append(stack, child)
		}
		for _, f := range n.Fields {
			stack = append(stack, f.Value)
		}
	}
}

func (v *verifier) computeRegionFunctions() {
	// First pass: establish region ownership (first claim wins) across
	// every node before any Target/DeferChain check runs. These must be
	// two genuinely separate passes over the full node list, not merged
	// into one loop — a Target reference to a region can be reached before
	// that region's true owner is, purely by node insertion order, which
	// would let the second pass silently miss a real violation if it ran
	// interleaved with the first instead of after it completes everywhere.
	for i, n := range v.u.nodes {
		if n.Region == 0 || uint64(n.Region) > uint64(v.u.regionCount) {
			continue
		}
		fid := v.nodeFunction[i]
		if fid == 0 {
			continue
		}
		ridx := n.Region - 1
		if v.regionFunction[ridx] == 0 {
			v.regionFunction[ridx] = fid
		} else if v.regionFunction[ridx] != fid {
			v.errorf("node %d (%s) region %d owned by function %d, referenced from function %d", i+1, n.Kind, n.Region, v.regionFunction[ridx], fid)
		}
	}

	// Second pass: check Target and DeferChain ownership now that region
	// ownership is fully established.
	for i, n := range v.u.nodes {
		fid := v.nodeFunction[i]
		if fid == 0 {
			continue
		}

		if n.Target != 0 && uint64(n.Target) <= uint64(v.u.regionCount) {
			ridx := n.Target - 1
			if v.regionFunction[ridx] != 0 && v.regionFunction[ridx] != fid {
				v.errorf("node %d (%s) targets region %d owned by function %d, but node belongs to function %d", i+1, n.Kind, n.Target, v.regionFunction[ridx], fid)
			}
		}

		for _, d := range n.DeferChain {
			if !d.IsValid() || uint64(d) > uint64(len(v.u.nodes)) {
				continue // already reported by expectDeferRegister
			}
			if v.u.nodes[d-1].Kind != DeferRegister {
				continue // already reported by expectDeferRegister
			}
			dfid := v.nodeFunction[d-1]
			if dfid != 0 && dfid != fid {
				v.errorf("node %d (%s) defer chain entry %d belongs to function %d, but node belongs to function %d", i+1, n.Kind, d, dfid, fid)
			}
		}
	}
}

func (v *verifier) verifyNode(id NodeID) {
	if !id.IsValid() || uint64(id) > uint64(len(v.u.nodes)) {
		v.errorf("node %d out of range", id)
		return
	}
	n := v.u.nodes[id-1]

	cat, ok := CategoryOf(n.Kind)
	if !ok {
		v.errorf("node %d has invalid kind %d", id, n.Kind)
		return
	}

	// Category/type agreement.
	switch cat {
	case CategoryValue, CategoryPlace:
		if n.Type == 0 {
			v.errorf("node %d (%s) value/place requires TypeID", id, n.Kind)
		}
	case CategoryNonvalue:
		if n.Type != 0 {
			v.errorf("node %d (%s) nonvalue must have zero TypeID", id, n.Kind)
		}
	}

	// Source/synthetic origin completeness.
	if n.Span == (source.Span{}) && n.Origin == (source.Span{}) {
		v.errorf("node %d (%s) has neither Span nor Origin", id, n.Kind)
	}
	if n.Origin != (source.Span{}) && n.SyntheticRole == "" {
		v.errorf("node %d (%s) synthetic Origin without role", id, n.Kind)
	}
	if n.SyntheticRole != "" && n.Origin == (source.Span{}) {
		v.errorf("node %d (%s) synthetic role without Origin", id, n.Kind)
	}

	// ExplicitCast only on SourceAlias.
	if n.ExplicitCast && n.Kind != SourceAlias {
		v.errorf("node %d (%s) sets ExplicitCast outside SourceAlias", id, n.Kind)
	}

	// Child ID ownership.
	for i, child := range n.Children {
		if !child.IsValid() || uint64(child) > uint64(len(v.u.nodes)) {
			v.errorf("node %d (%s) child[%d]=%d out of range", id, n.Kind, i, child)
		}
	}

	// Per-tag exact checks.
	switch n.Kind {
	case Module:
		v.allowOnly(id, n, "Symbol")
		v.requireSymbol(id, n)
	case Import:
		v.allowOnly(id, n, "TargetModule")
		if n.TargetModule == 0 {
			v.errorf("node %d Import requires TargetModule", id)
		}
	case TypeDeclaration:
		v.allowOnly(id, n, "Symbol")
		v.requireSymbol(id, n)
	case FieldDeclaration:
		v.allowOnly(id, n, "Symbol")
		v.requireSymbol(id, n)
	case VariantDeclaration:
		v.allowOnly(id, n, "Symbol")
		v.requireSymbol(id, n)
	case FunctionDeclaration:
		v.allowOnly(id, n, "Symbol", "Function", "Parameters", "ResultType", "Convention", "Variadic", "Inline", "HasBody", "Children")
		v.requireSymbol(id, n)
		v.requireFunction(id, n)
		v.requireConvention(id, n)
		if n.ResultType == 0 {
			v.errorf("node %d FunctionDeclaration requires ResultType", id)
		}
		for i, p := range n.Parameters {
			if p.Symbol == 0 {
				v.errorf("node %d FunctionDeclaration parameter[%d] missing symbol", id, i)
			}
			if p.Type == 0 {
				v.errorf("node %d FunctionDeclaration parameter[%d] missing type", id, i)
			}
		}
	case ExternDeclaration:
		v.allowOnly(id, n, "Symbol", "Function", "Parameters", "ResultType", "Convention", "Variadic", "Inline")
		v.requireSymbol(id, n)
		v.requireConvention(id, n)
		if n.HasBody {
			v.errorf("node %d ExternDeclaration must not have body", id)
		}
	case GlobalDeclaration:
		v.allowOnly(id, n, "Symbol")
		v.requireSymbol(id, n)
	case LocalDeclaration:
		v.allowOnly(id, n, "Symbol")
		v.requireSymbol(id, n)
	case ParameterDeclaration:
		v.allowOnly(id, n, "Symbol")
		v.requireSymbol(id, n)
	case TypeParameterDeclaration:
		v.allowOnly(id, n, "Symbol")
		v.requireSymbol(id, n)
	case TypeUse:
		v.allowOnly(id, n, "TypeArg")
		if n.TypeArg == 0 {
			v.errorf("node %d TypeUse requires TypeArg", id)
		}
	case Block:
		v.allowOnly(id, n, "Region", "Children")
		v.requireRegion(id, n)
		for i := range n.Children {
			v.expectChildCategory(id, n, i, CategoryNonvalue)
		}

	case Initialize:
		v.allowOnly(id, n, "Symbol", "Children")
		v.requireSymbol(id, n)
		v.expectChildCount(id, n, 1, 1)
		v.expectChildCategory(id, n, 0, CategoryValue)
	case Store:
		v.allowOnly(id, n, "Children")
		v.expectChildCount(id, n, 2, 2)
		v.expectChildCategory(id, n, 0, CategoryPlace)
		v.expectChildCategory(id, n, 1, CategoryValue)
	case CompoundStore:
		v.allowOnly(id, n, "Operator", "Children")
		v.expectChildCount(id, n, 2, 2)
		v.expectChildCategory(id, n, 0, CategoryPlace)
		v.expectChildCategory(id, n, 1, CategoryValue)
		v.requireOperator(id, n)
	case ExpressionStatement:
		v.allowOnly(id, n, "Children")
		v.expectChildCount(id, n, 1, 1)
		if len(n.Children) > 0 {
			v.expectChildCategory(id, n, 0, CategoryValue)
		}
	case Print:
		v.allowOnly(id, n, "Children")
		for i := range n.Children {
			v.expectChildCategory(id, n, i, CategoryValue)
		}
	case Return:
		v.allowOnly(id, n, "Function", "Children", "DeferChain")
		v.requireFunction(id, n)
		if n.Function != 0 && uint64(n.Function) <= uint64(len(v.u.functions)) && v.nodeFunction[id-1] != 0 {
			if v.nodeFunction[id-1] != n.Function {
				v.errorf("node %d %s declares Function %d, but is owned by function %d", id, n.Kind, n.Function, v.nodeFunction[id-1])
			}
		}
		if len(n.Children) > 1 {
			v.errorf("node %d Return has too many children", id)
		}
		if len(n.Children) == 1 {
			v.expectChildCategory(id, n, 0, CategoryValue)
		}
		for _, d := range n.DeferChain {
			v.expectDeferRegister(id, d)
		}
	case ImplicitReturn:
		v.allowOnly(id, n, "Function", "DeferChain")
		v.requireFunction(id, n)
		if n.Function != 0 && uint64(n.Function) <= uint64(len(v.u.functions)) && v.nodeFunction[id-1] != 0 {
			if v.nodeFunction[id-1] != n.Function {
				v.errorf("node %d %s declares Function %d, but is owned by function %d", id, n.Kind, n.Function, v.nodeFunction[id-1])
			}
		}
		for _, d := range n.DeferChain {
			v.expectDeferRegister(id, d)
		}
	case If:
		v.allowOnly(id, n, "Region", "HasElse", "Children")
		v.requireRegion(id, n)
		minChildren := 2
		if n.HasElse {
			minChildren = 3
		}
		v.expectChildCount(id, n, minChildren, 3)
		v.expectChildCategory(id, n, 0, CategoryValue)
		for i := 1; i < len(n.Children); i++ {
			v.expectChildCategory(id, n, i, CategoryNonvalue)
		}
	case While:
		v.allowOnly(id, n, "Region", "Children")
		v.requireRegion(id, n)
		v.expectChildCount(id, n, 2, 2)
		if len(n.Children) > 0 {
			v.expectChildCategory(id, n, 0, CategoryValue)
		}
		if len(n.Children) > 1 {
			v.expectChildCategory(id, n, 1, CategoryNonvalue)
		}
	case RangeLoop:
		v.allowOnly(id, n, "Region", "RangeInclusive", "Children")
		v.requireRegion(id, n)
		v.expectChildCount(id, n, 3, 3)
		if len(n.Children) > 0 {
			v.expectChildCategory(id, n, 0, CategoryValue)
		}
		if len(n.Children) > 1 {
			v.expectChildCategory(id, n, 1, CategoryValue)
		}
		if len(n.Children) > 2 {
			v.expectChildCategory(id, n, 2, CategoryNonvalue)
		}
	case For:
		v.allowOnly(id, n, "Region", "Children")
		v.requireRegion(id, n)
		v.expectChildCount(id, n, 1, 4)
		if len(n.Children) >= 1 {
			v.expectChildCategory(id, n, 0, CategoryNonvalue) // body
		}
	case Switch:
		v.allowOnly(id, n, "Region", "HasElse", "Children")
		v.requireRegion(id, n)
		if len(n.Children) < 1 {
			v.errorf("node %d Switch requires subject child", id)
		} else {
			v.expectChildCategory(id, n, 0, CategoryValue)
			for i := 1; i < len(n.Children); i++ {
				v.expectChildCategory(id, n, i, CategoryNonvalue)
			}
		}
	case SwitchCase:
		v.allowOnly(id, n, "Region", "CaseValue", "HasElse", "Children", "Literal")
		v.requireRegion(id, n)
		v.expectChildCount(id, n, 1, 2)
		if n.CaseValue == 0 && !n.HasElse && n.Literal == (Literal{}) {
			v.errorf("node %d SwitchCase requires CaseValue, Literal, or HasElse", id)
		}
		if n.HasElse && n.CaseValue != 0 {
			v.errorf("node %d SwitchCase cannot have both CaseValue and HasElse", id)
		}
		if n.HasElse && n.Literal != (Literal{}) {
			v.errorf("node %d SwitchCase cannot have both Literal and HasElse", id)
		}
		if n.CaseValue != 0 && n.Literal != (Literal{}) {
			v.errorf("node %d SwitchCase cannot have both CaseValue and Literal", id)
		}
	case Break, Continue:
		v.allowOnly(id, n, "Target", "DeferChain")
		v.requireTarget(id, n)
		for _, d := range n.DeferChain {
			v.expectDeferRegister(id, d)
		}
	case DeferRegister:
		v.allowOnly(id, n, "Region", "Children")
		v.requireRegion(id, n)
		v.expectChildCount(id, n, 1, 1)
		v.expectChildCategory(id, n, 0, CategoryNonvalue)

	case BoolLiteral:
		v.allowOnly(id, n, "Literal")
		if n.Literal.Kind != LiteralBool {
			v.errorf("node %d BoolLiteral has wrong literal kind %d", id, n.Literal.Kind)
		}
	case CharLiteral:
		v.allowOnly(id, n, "Literal")
		if n.Literal.Kind != LiteralChar {
			v.errorf("node %d CharLiteral has wrong literal kind %d", id, n.Literal.Kind)
		}
	case StringLiteral:
		v.allowOnly(id, n, "Literal")
		if n.Literal.Kind != LiteralString {
			v.errorf("node %d StringLiteral has wrong literal kind %d", id, n.Literal.Kind)
		}
	case IntegerLiteral:
		v.allowOnly(id, n, "Literal")
		if n.Literal.Kind != LiteralInteger {
			v.errorf("node %d IntegerLiteral has wrong literal kind %d", id, n.Literal.Kind)
		}
	case FloatLiteral:
		v.allowOnly(id, n, "Literal")
		if n.Literal.Kind != LiteralFloat {
			v.errorf("node %d FloatLiteral has wrong literal kind %d", id, n.Literal.Kind)
		}
	case NilPointer:
		v.allowOnly(id, n)
	case NoneOptional:
		v.allowOnly(id, n)
	case SomeOptional:
		v.allowOnly(id, n, "Children")
		v.expectChildCount(id, n, 1, 1)
		if len(n.Children) > 0 {
			v.expectChildCategory(id, n, 0, CategoryValue)
		}
	case TupleValue:
		v.allowOnly(id, n, "Children")
		for i := range n.Children {
			v.expectChildCategory(id, n, i, CategoryValue)
		}
	case ArrayValue:
		v.allowOnly(id, n, "Children")
		for i := range n.Children {
			v.expectChildCategory(id, n, i, CategoryValue)
		}
	case ArrayRepeat:
		v.allowOnly(id, n, "Children")
		v.expectChildCount(id, n, 2, 2)
		for i := range n.Children {
			v.expectChildCategory(id, n, i, CategoryValue)
		}
	case RecordConstruct:
		v.allowOnly(id, n, "Symbol", "Fields")
		v.requireSymbol(id, n)
		seen := make(map[symbol.SymbolID]struct{})
		for i, f := range n.Fields {
			if f.Field == 0 {
				v.errorf("node %d RecordConstruct field[%d] missing Field symbol", id, i)
			}
			if !f.Value.IsValid() || uint64(f.Value) > uint64(len(v.u.nodes)) {
				v.errorf("node %d RecordConstruct field[%d] value %d out of range", id, i, f.Value)
				continue
			}
			cat, _ := CategoryOf(v.u.nodes[f.Value-1].Kind)
			if cat != CategoryValue {
				v.errorf("node %d RecordConstruct field[%d]=%d has category %s, want value", id, i, f.Value, cat)
			}
			if _, ok := seen[f.Field]; ok {
				v.errorf("node %d RecordConstruct duplicate field %d", id, f.Field)
			} else {
				seen[f.Field] = struct{}{}
			}
		}
	case HoistedFunctionValue:
		v.allowOnly(id, n, "Symbol", "Function")
		v.requireSymbol(id, n)
		v.requireFunction(id, n)
	case SymbolValue:
		v.allowOnly(id, n, "Symbol")
		v.requireSymbol(id, n)
	case EnumVariantValue:
		v.allowOnly(id, n, "Member", "Children")
		v.requireMember(id, n)
		if len(n.Children) > 1 {
			v.errorf("node %d EnumVariantValue has too many children", id)
		}
		if len(n.Children) == 1 {
			v.expectChildCategory(id, n, 0, CategoryValue)
		}
	case ContextValue:
		v.allowOnly(id, n, "ContextAction")
		if n.ContextAction == 0 {
			v.errorf("node %d ContextValue requires ContextAction", id)
		}
		if n.ContextAction != ContextExpr {
			v.errorf("node %d ContextValue must use ContextExpr", id)
		}
	case InterpolatedString:
		v.allowOnly(id, n, "Children")
		for i := range n.Children {
			v.expectChildCategory(id, n, i, CategoryValue)
		}
	case SizeofType:
		v.allowOnly(id, n, "TypeArg")
		if n.TypeArg == 0 {
			v.errorf("node %d SizeofType requires TypeArg", id)
		}
	case PrefixValue:
		v.allowOnly(id, n, "Operator", "Children")
		v.expectChildCount(id, n, 1, 1)
		v.expectChildCategory(id, n, 0, CategoryValue)
		v.requireOperator(id, n)
	case BinaryValue:
		v.allowOnly(id, n, "Operator", "Children")
		v.expectChildCount(id, n, 2, 2)
		if len(n.Children) > 0 {
			v.expectChildCategory(id, n, 0, CategoryValue)
		}
		if len(n.Children) > 1 {
			v.expectChildCategory(id, n, 1, CategoryValue)
		}
		v.requireOperator(id, n)
	case ShortCircuitValue:
		v.allowOnly(id, n, "Operator", "Children")
		v.expectChildCount(id, n, 2, 2)
		if len(n.Children) > 0 {
			v.expectChildCategory(id, n, 0, CategoryValue)
		}
		if len(n.Children) > 1 {
			v.expectChildCategory(id, n, 1, CategoryValue)
		}
		v.requireOperator(id, n)
	case FieldValue:
		v.allowOnly(id, n, "Member", "Children")
		v.expectChildCount(id, n, 1, 1)
		v.expectChildCategory(id, n, 0, CategoryValue)
		v.requireMember(id, n)
	case TupleElementValue:
		v.allowOnly(id, n, "Ordinal", "Children")
		v.expectChildCount(id, n, 1, 1)
		v.expectChildCategory(id, n, 0, CategoryValue)
		if n.Ordinal == 0 {
			v.errorf("node %d TupleElementValue requires Ordinal", id)
		}
	case GenericFunctionValue:
		v.allowOnly(id, n, "Symbol", "GenericRef", "TypeArgs")
		v.requireSymbol(id, n)
		if n.GenericRef >= uint32(len(v.u.instantiations)) {
			v.errorf("node %d GenericFunctionValue has invalid GenericRef %d", id, n.GenericRef)
		}
	case SourceAlias:
		v.allowOnly(id, n, "ExplicitCast", "Children")
		v.expectChildCount(id, n, 1, 1)
		if len(n.Children) > 0 {
			v.expectChildCategory(id, n, 0, CategoryValue)
		}

	case StoragePlace:
		v.allowOnly(id, n, "Symbol", "Writable")
		v.requireSymbol(id, n)
	case DereferencePlace:
		v.allowOnly(id, n, "Writable", "Children")
		v.expectChildCount(id, n, 1, 1)
		v.expectChildCategory(id, n, 0, CategoryValue)
	case FieldPlace:
		v.allowOnly(id, n, "Member", "Writable", "Children")
		v.expectChildCount(id, n, 1, 1)
		v.expectChildCategory(id, n, 0, CategoryPlace)
		v.requireMember(id, n)
	case TuplePlace:
		v.allowOnly(id, n, "Ordinal", "Writable", "Children")
		v.expectChildCount(id, n, 1, 1)
		v.expectChildCategory(id, n, 0, CategoryPlace)
		if n.Ordinal == 0 {
			v.errorf("node %d TuplePlace requires Ordinal", id)
		}
	case CheckedIndexPlace:
		v.allowOnly(id, n, "Writable", "Children")
		v.expectChildCount(id, n, 2, 2)
		v.expectChildCategory(id, n, 0, CategoryPlace)
		v.expectChildCategory(id, n, 1, CategoryValue)
	case Load:
		v.allowOnly(id, n, "Children")
		v.expectChildCount(id, n, 1, 1)
		v.expectChildCategory(id, n, 0, CategoryPlace)

	case DirectCall:
		v.allowOnly(id, n, "Symbol", "Convention", "ContextAction", "TypeArgs", "Children", "FunctionType")
		v.requireSymbol(id, n)
		v.requireConvention(id, n)
		v.requireContextAction(id, n)
		if n.FunctionType == 0 {
			v.errorf("node %d DirectCall requires FunctionType", id)
		}
		for i := range n.Children {
			v.expectChildCategory(id, n, i, CategoryValue)
		}
		v.checkContextActionConvention(id, n)
	case MethodCall:
		v.allowOnly(id, n, "Symbol", "Convention", "ContextAction", "TypeArgs", "Children", "FunctionType")
		v.requireSymbol(id, n)
		v.requireConvention(id, n)
		v.requireContextAction(id, n)
		if n.FunctionType == 0 {
			v.errorf("node %d MethodCall requires FunctionType", id)
		}
		if len(n.Children) < 1 {
			v.errorf("node %d MethodCall requires at least one child (receiver)", id)
		}
		for i := range n.Children {
			v.expectChildCategory(id, n, i, CategoryValue)
		}
		v.checkContextActionConvention(id, n)
	case IndirectCall:
		v.allowOnly(id, n, "Convention", "ContextAction", "Children", "FunctionType")
		v.requireConvention(id, n)
		v.requireContextAction(id, n)
		if n.FunctionType == 0 {
			v.errorf("node %d IndirectCall requires FunctionType", id)
		}
		if len(n.Children) < 1 {
			v.errorf("node %d IndirectCall requires at least callee child", id)
		}
		for i := range n.Children {
			v.expectChildCategory(id, n, i, CategoryValue)
		}
		v.checkContextActionConvention(id, n)
	case VariantConstruct:
		v.allowOnly(id, n, "Member", "Children")
		v.requireMember(id, n)
		if len(n.Children) > 1 {
			v.errorf("node %d VariantConstruct has too many children", id)
		}
		if len(n.Children) == 1 {
			v.expectChildCategory(id, n, 0, CategoryValue)
		}

	case IntegerCast, IntegerToFloat, FloatToInteger, FloatCast, OptionalInject, EnumToInteger, OptionalIntegerToEnum, CheckedIntegerToEnum:
		v.allowOnly(id, n, "Children")
		v.expectChildCount(id, n, 1, 1)
		v.expectChildCategory(id, n, 0, CategoryValue)
	case TupleCoerce:
		v.allowOnly(id, n, "TypeArgs", "Children")
		v.expectChildCount(id, n, 2, maxChildren)
		v.expectChildCategory(id, n, 0, CategoryValue)
		for i := 1; i < len(n.Children); i++ {
			v.expectChildCategory(id, n, i, CategoryValue)
		}
	case CheckedOptionalUnwrap:
		v.allowOnly(id, n, "Children")
		v.expectChildCount(id, n, 1, 1)
		v.expectChildCategory(id, n, 0, CategoryValue)
	case CheckedIndex:
		v.allowOnly(id, n, "Children")
		v.expectChildCount(id, n, 2, 2)
		v.expectChildCategory(id, n, 0, CategoryValue)
		v.expectChildCategory(id, n, 1, CategoryValue)
	case CheckedSlice:
		v.allowOnly(id, n, "Children")
		v.expectChildCount(id, n, 1, 3)
		v.expectChildCategory(id, n, 0, CategoryValue)
		for i := 1; i < len(n.Children); i++ {
			v.expectChildCategory(id, n, i, CategoryValue)
		}
	case CheckedArithmetic, CheckedShift:
		v.allowOnly(id, n, "Operator", "Children")
		v.expectChildCount(id, n, 2, 2)
		v.expectChildCategory(id, n, 0, CategoryValue)
		v.expectChildCategory(id, n, 1, CategoryValue)
		v.requireOperator(id, n)
	case CheckedNegate:
		v.allowOnly(id, n, "Operator", "Children")
		v.expectChildCount(id, n, 1, 1)
		v.expectChildCategory(id, n, 0, CategoryValue)
		v.requireOperator(id, n)

	case TempBind:
		v.allowOnly(id, n, "Temp", "Children")
		v.requireTemp(id, n)
		if v.expectChildCount(id, n, 1, 1) {
			child := n.Children[0]
			if child.IsValid() && uint64(child) <= uint64(len(v.u.nodes)) {
				childCat, _ := CategoryOf(v.u.nodes[child-1].Kind)
				if childCat != CategoryValue && childCat != CategoryPlace {
					v.errorf("node %d TempBind child must be value or place", id)
				}
			}
		}
		if existing, ok := v.definedTemps[n.Temp]; ok {
			v.errorf("node %d TempBind redefines temp %d (first at %d)", id, n.Temp, existing)
		} else {
			v.definedTemps[n.Temp] = id
		}
	case TempRead:
		v.allowOnly(id, n, "Temp")
		v.requireTemp(id, n)
	case Sequence:
		v.allowOnly(id, n, "Children")
		if len(n.Children) < 1 {
			v.errorf("node %d Sequence requires at least one child", id)
		}
		for i := range n.Children {
			v.expectChildCategory(id, n, i, CategoryValue)
		}
	default:
		v.errorf("node %d has unhandled kind %s", id, n.Kind)
	}
}

const maxChildren = int(^uint32(0) >> 1)

func (v *verifier) allowOnly(id NodeID, n Node, fields ...string) {
	allowed := make(map[string]bool, len(fields))
	for _, f := range fields {
		allowed[f] = true
	}
	check := func(name string, set bool) {
		if set && !allowed[name] {
			v.errorf("node %d %s forbids field %s", id, n.Kind, name)
		}
	}
	check("Symbol", n.Symbol != 0)
	check("Member", n.Member != 0)
	check("Temp", n.Temp != 0)
	check("Function", n.Function != 0)
	check("Region", n.Region != 0)
	check("Target", n.Target != 0)
	check("TargetModule", n.TargetModule != 0)
	check("Ordinal", n.Ordinal != 0)
	check("TypeArg", n.TypeArg != 0)
	check("Convention", n.Convention != 0)
	check("Variadic", n.Variadic)
	check("Inline", n.Inline)
	check("HasBody", n.HasBody)
	check("HasElse", n.HasElse)
	check("RangeInclusive", n.RangeInclusive)
	check("ConditionPresent", n.ConditionPresent)
	check("Writable", n.Writable)
	check("ShortCircuit", n.ShortCircuit)
	check("ExplicitCast", n.ExplicitCast)
	check("Operator", n.Operator != 0)
	check("ContextAction", n.ContextAction != 0)
	check("GenericRef", n.GenericRef != 0)
	check("CaseValue", n.CaseValue != 0)
	check("ResultType", n.ResultType != 0)
	check("FunctionType", n.FunctionType != 0)
	check("Literal", n.Literal != (Literal{}))
	check("Children", len(n.Children) > 0)
	check("Parameters", len(n.Parameters) > 0)
	check("Fields", len(n.Fields) > 0)
	check("TypeArgs", len(n.TypeArgs) > 0)
	check("DeferChain", len(n.DeferChain) > 0)
	check("Requirements", len(n.Requirements) > 0)
}

func (v *verifier) requireSymbol(id NodeID, n Node) {
	if n.Symbol == 0 {
		v.errorf("node %d %s requires Symbol", id, n.Kind)
	}
}

func (v *verifier) requireMember(id NodeID, n Node) {
	if n.Member == 0 {
		v.errorf("node %d %s requires Member", id, n.Kind)
	}
}

func (v *verifier) requireType(id NodeID, n Node) {
	if n.Type == 0 {
		v.errorf("node %d %s requires Type", id, n.Kind)
	}
}

func (v *verifier) requireFunction(id NodeID, n Node) {
	if n.Function == 0 || uint64(n.Function) > uint64(len(v.u.functions)) {
		v.errorf("node %d %s has invalid FunctionID %d", id, n.Kind, n.Function)
	}
}

func (v *verifier) requireRegion(id NodeID, n Node) {
	if n.Region == 0 || uint64(n.Region) > uint64(v.u.regionCount) {
		v.errorf("node %d %s references invalid region %d", id, n.Kind, n.Region)
	}
}

func (v *verifier) requireTarget(id NodeID, n Node) {
	if n.Target == 0 || uint64(n.Target) > uint64(v.u.regionCount) {
		v.errorf("node %d %s references invalid target %d", id, n.Kind, n.Target)
	}
}

func (v *verifier) requireTemp(id NodeID, n Node) {
	if n.Temp == 0 || uint64(n.Temp) > uint64(v.u.tempCount) {
		v.errorf("node %d %s has invalid TempID %d", id, n.Kind, n.Temp)
	}
}

func (v *verifier) requireOperator(id NodeID, n Node) {
	if n.Operator == 0 {
		v.errorf("node %d %s requires Operator", id, n.Kind)
	}
}

func (v *verifier) requireConvention(id NodeID, n Node) {
	if n.Convention == 0 {
		v.errorf("node %d %s requires Convention", id, n.Kind)
	}
}

func (v *verifier) requireContextAction(id NodeID, n Node) {
	if n.ContextAction == 0 {
		v.errorf("node %d %s requires ContextAction", id, n.Kind)
	}
}

func (v *verifier) checkContextActionConvention(id NodeID, n Node) {
	switch n.Convention {
	case types.Pebble:
		if n.ContextAction != ContextForward {
			v.errorf("node %d %s Pebble call must use ForwardCurrentContext", id, n.Kind)
		}
	case types.C:
		if n.ContextAction != ContextNone {
			v.errorf("node %d %s C call must use NoContext", id, n.Kind)
		}
	}
}

func (v *verifier) expectChildCount(id NodeID, n Node, min, max int) bool {
	if len(n.Children) < min || len(n.Children) > max {
		v.errorf("node %d %s expects %d..%d children, got %d", id, n.Kind, min, max, len(n.Children))
		return false
	}
	return true
}

func (v *verifier) expectChildCategory(id NodeID, n Node, idx int, want nodeCategory) {
	if idx >= len(n.Children) {
		return
	}
	child := n.Children[idx]
	if !child.IsValid() || uint64(child) > uint64(len(v.u.nodes)) {
		return // already reported
	}
	got, _ := CategoryOf(v.u.nodes[child-1].Kind)
	if got != want {
		v.errorf("node %d %s child[%d]=%d has category %s, want %s", id, v.u.nodes[id-1].Kind, idx, child, got, want)
	}
}

func (v *verifier) expectDeferRegister(id NodeID, d NodeID) {
	if !d.IsValid() || uint64(d) > uint64(len(v.u.nodes)) {
		v.errorf("node %d defer chain contains invalid node %d", id, d)
		return
	}
	if v.u.nodes[d-1].Kind != DeferRegister {
		v.errorf("node %d defer chain contains non-defer %s", id, v.u.nodes[d-1].Kind)
	}
}

func (v *verifier) verifySourceMap() {
	for _, ref := range v.u.SourceRefs() {
		id := v.u.sourceMap[ref]
		if !id.IsValid() || uint64(id) > uint64(len(v.u.nodes)) {
			v.errorf("sourcemap %d:%d maps to invalid node %d", ref.Module, ref.Node, id)
			continue
		}
		n := v.u.nodes[id-1]
		if n.Syntax != ref {
			v.errorf("sourcemap %d:%d -> %d but node.Syntax is %d:%d", ref.Module, ref.Node, id, n.Syntax.Module, n.Syntax.Node)
		}
	}
	for i, n := range v.u.nodes {
		if n.Syntax != (symbol.SyntaxRef{}) {
			if mapped, ok := v.u.sourceMap[n.Syntax]; !ok || mapped != NodeID(i+1) {
				v.errorf("node %d with Syntax %d:%d is missing or mismatched in source map", i+1, n.Syntax.Module, n.Syntax.Node)
			}
		}
	}
}

func (v *verifier) verifyFunctions() {
	for _, f := range v.u.functions {
		if f.Symbol == 0 {
			v.errorf("function declaration has zero symbol")
		}
		if f.Node != 0 && (uint64(f.Node) > uint64(len(v.u.nodes)) || v.u.nodes[f.Node-1].Kind != Block) {
			v.errorf("function %d node is not a Block", f.FunctionID)
		}
		if f.FunctionID == 0 || uint64(f.FunctionID) > uint64(len(v.u.functions)) {
			v.errorf("function declaration has invalid FunctionID")
		}
	}
}

func (v *verifier) verifyRegions() {
	for i, n := range v.u.nodes {
		if n.Region != 0 && uint64(n.Region) > uint64(v.u.regionCount) {
			v.errorf("node %d references out-of-range region %d", i+1, n.Region)
		}
		if n.Target != 0 && uint64(n.Target) > uint64(v.u.regionCount) {
			v.errorf("node %d references out-of-range target %d", i+1, n.Target)
		}
	}
}

func (v *verifier) verifyTemps() {
	for i, n := range v.u.nodes {
		if n.Kind == TempBind {
			fid := v.nodeFunction[i]
			if fid == 0 {
				v.errorf("node %d TempBind is outside any function", i+1)
			}
		}
		if n.Kind == TempRead {
			if _, ok := v.definedTemps[n.Temp]; !ok {
				v.errorf("node %d TempRead uses undefined temp %d", i+1, n.Temp)
				continue
			}
			fid := v.nodeFunction[i]
			bindID := v.definedTemps[n.Temp]
			bindFid := v.nodeFunction[bindID-1]
			if fid != bindFid {
				v.errorf("node %d TempRead uses temp %d defined in function %d, not %d", i+1, n.Temp, bindFid, fid)
			}
		}
	}
	for _, f := range v.u.functions {
		if f.Node == 0 {
			continue
		}
		v.checkTempDominance(f.Node, f.FunctionID, make(map[NodeID]bool))
	}
}

func (v *verifier) checkTempDominance(rootID NodeID, fid FunctionID, visited map[NodeID]bool) {
	var stack []func()
	push := func(f func()) { stack = append(stack, f) }

	var visit func(id NodeID)
	visit = func(id NodeID) {
		if !id.IsValid() || uint64(id) > uint64(len(v.u.nodes)) {
			return
		}
		if visited[id] {
			return
		}
		visited[id] = true

		n := v.u.nodes[id-1]
		available := v.availableTemps[fid]
		if available == nil {
			available = make(map[TempID]bool)
			v.availableTemps[fid] = available
		}

		branching := n.Kind == If || n.Kind == Switch || n.Kind == While || n.Kind == For || n.Kind == RangeLoop
		var saved map[TempID]bool
		if branching {
			saved = make(map[TempID]bool)
			for t := range available {
				saved[t] = true
			}
		}

		if n.Kind == If || n.Kind == Switch {
			arms := n.Children
			if len(arms) > 0 {
				arms = arms[1:]
			}

			var dispatchArm func(postPrefix map[TempID]bool, idx int)
			dispatchArm = func(postPrefix map[TempID]bool, idx int) {
				if idx >= len(arms) {
					for t := range available {
						delete(available, t)
					}
					for t := range saved {
						available[t] = true
					}
					return
				}
				for t := range available {
					delete(available, t)
				}
				for t := range postPrefix {
					available[t] = true
				}
				child := arms[idx]
				push(func() { dispatchArm(postPrefix, idx+1) })
				push(func() { visit(child) })
			}

			startArms := func() {
				postPrefix := make(map[TempID]bool)
				for t := range available {
					postPrefix[t] = true
				}
				dispatchArm(postPrefix, 0)
			}

			if len(n.Children) > 0 {
				push(func() { startArms() })
				push(func() { visit(n.Children[0]) })
			} else {
				startArms()
			}
			return
		}

		finalize := func() {
			if branching {
				for t := range available {
					if !saved[t] {
						delete(available, t)
					}
				}
			}
			switch n.Kind {
			case TempBind:
				available[n.Temp] = true
			case TempRead:
				if !available[n.Temp] {
					v.errorf("node %d TempRead of temp %d is not dominated by its TempBind", id, n.Temp)
				}
			}
		}

		push(func() { finalize() })
		for i := len(n.Fields) - 1; i >= 0; i-- {
			val := n.Fields[i].Value
			push(func() { visit(val) })
		}
		for i := len(n.Children) - 1; i >= 0; i-- {
			child := n.Children[i]
			push(func() { visit(child) })
		}
	}

	push(func() { visit(rootID) })
	for len(stack) > 0 {
		fn := stack[len(stack)-1]
		stack = stack[:len(stack)-1]
		fn()
	}
}

func (v *verifier) verifyDeclarations() {
	for _, t := range v.u.typeDecls {
		if t.Node != 0 && (uint64(t.Node) > uint64(len(v.u.nodes)) || v.u.nodes[t.Node-1].Kind != TypeDeclaration) {
			v.errorf("type decl %d node is not TypeDeclaration", t.Symbol)
		}
	}
	for _, g := range v.u.globals {
		if g.Node != 0 && (uint64(g.Node) > uint64(len(v.u.nodes)) || v.u.nodes[g.Node-1].Kind != GlobalDeclaration) {
			v.errorf("global decl %d node is not GlobalDeclaration", g.Symbol)
		}
	}
}

func (v *verifier) verifyInstantiations() {
	for i, in := range v.u.instantiations {
		if in.Declaration == 0 {
			v.errorf("instantiation %d has zero declaration symbol", i)
		}
		for j, ta := range in.TypeArgs {
			v.checkTypeInSnapshot(fmt.Sprintf("instantiation[%d].typearg[%d]", i, j), 0, ta)
		}
	}
}
