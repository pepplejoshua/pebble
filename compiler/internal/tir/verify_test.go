package tir

import (
	"bytes"
	"strings"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/module"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

// validNode returns a minimal valid node for the given kind. It depends only
// on kind and the test snapshot. sharedTemp is used for TempBind/TempRead so
// a TempRead can reference an existing binding in complete-unit tests.
func validNode(t *testing.T, b *Builder, kind NodeKind, refs map[NodeID]struct{}, sharedTemp TempID) Node {
	t.Helper()
	snap := testSnapshot(t)
	boolType := builtinType(snap, types.Bool)
	intType := builtinType(snap, types.Int)
	voidType := builtinType(snap, types.Void)
	span := source.Span{Source: 1, Start: 0, End: 1}

	ensureChild := func(k NodeKind) NodeID {
		n := validNode(t, b, k, refs, 0)
		id, err := b.AddNode(n)
		if err != nil {
			t.Fatalf("ensureChild %s: %v", k, err)
		}
		refs[id] = struct{}{}
		return id
	}

	switch kind {
	case Module:
		return Node{Kind: kind, Span: span, Symbol: 1}
	case Import:
		return Node{Kind: kind, Span: span, TargetModule: 1}
	case TypeDeclaration:
		return Node{Kind: kind, Span: span, Symbol: 1}
	case FieldDeclaration:
		return Node{Kind: kind, Span: span, Symbol: 1}
	case VariantDeclaration:
		return Node{Kind: kind, Span: span, Symbol: 1}
	case FunctionDeclaration:
		return Node{Kind: kind, Span: span, Symbol: 1, Function: 1, Convention: types.Pebble, Parameters: []Parameter{{Symbol: 1, Type: intType}}, ResultType: voidType, HasBody: true}
	case ExternDeclaration:
		return Node{Kind: kind, Span: span, Symbol: 1, Convention: types.C}
	case GlobalDeclaration:
		return Node{Kind: kind, Span: span, Symbol: 1}
	case LocalDeclaration:
		return Node{Kind: kind, Span: span, Symbol: 1}
	case ParameterDeclaration:
		return Node{Kind: kind, Span: span, Symbol: 1}
	case TypeParameterDeclaration:
		return Node{Kind: kind, Span: span, Symbol: 1}
	case TypeUse:
		return Node{Kind: kind, Span: span, TypeArg: intType}
	case Block:
		return Node{Kind: kind, Span: span, Region: 1}
	case Initialize:
		return Node{Kind: kind, Span: span, Symbol: 1, Children: []NodeID{ensureChild(BoolLiteral)}}
	case Store:
		return Node{Kind: kind, Span: span, Children: []NodeID{ensureChild(StoragePlace), ensureChild(BoolLiteral)}}
	case CompoundStore:
		return Node{Kind: kind, Span: span, Operator: syntax.Plus, Children: []NodeID{ensureChild(StoragePlace), ensureChild(BoolLiteral)}}
	case ExpressionStatement:
		return Node{Kind: kind, Span: span, Children: []NodeID{ensureChild(BoolLiteral)}}
	case Print:
		return Node{Kind: kind, Span: span, Children: []NodeID{ensureChild(BoolLiteral)}}
	case Return:
		return Node{Kind: kind, Span: span, Function: 1, Children: []NodeID{ensureChild(BoolLiteral)}}
	case ImplicitReturn:
		return Node{Kind: kind, Span: span, Function: 1}
	case If:
		return Node{Kind: kind, Span: span, Region: 1, HasElse: true, Children: []NodeID{ensureChild(BoolLiteral), ensureChild(Block), ensureChild(Block)}}
	case While:
		return Node{Kind: kind, Span: span, Region: 1, Children: []NodeID{ensureChild(BoolLiteral), ensureChild(Block)}}
	case RangeLoop:
		return Node{Kind: kind, Span: span, Region: 1, Children: []NodeID{ensureChild(IntegerLiteral), ensureChild(IntegerLiteral), ensureChild(Block)}}
	case For:
		return Node{Kind: kind, Span: span, Region: 1, Children: []NodeID{ensureChild(Block)}}
	case Switch:
		return Node{Kind: kind, Span: span, Region: 1, Children: []NodeID{ensureChild(BoolLiteral), ensureChild(SwitchCase)}}
	case SwitchCase:
		return Node{Kind: kind, Span: span, Region: 1, CaseValue: 1, Children: []NodeID{ensureChild(Block)}}
	case Break:
		return Node{Kind: kind, Span: span, Target: 1}
	case Continue:
		return Node{Kind: kind, Span: span, Target: 1}
	case DeferRegister:
		return Node{Kind: kind, Span: span, Region: 1, Children: []NodeID{ensureChild(ExpressionStatement)}}
	case BoolLiteral:
		return Node{Kind: kind, Type: boolType, Span: span, Literal: Literal{Kind: LiteralBool, Bool: true}}
	case CharLiteral:
		return Node{Kind: kind, Type: builtinType(snap, types.Char), Span: span, Literal: Literal{Kind: LiteralChar, Char: 'a'}}
	case StringLiteral:
		return Node{Kind: kind, Type: builtinType(snap, types.Str), Span: span, Literal: Literal{Kind: LiteralString, String: "x"}}
	case IntegerLiteral:
		return Node{Kind: kind, Type: intType, Span: span, Literal: Literal{Kind: LiteralInteger, IntegerNum: "1", IntegerDen: "1"}}
	case FloatLiteral:
		return Node{Kind: kind, Type: builtinType(snap, types.F64), Span: span, Literal: Literal{Kind: LiteralFloat, Float: "1.0"}}
	case NilPointer:
		return Node{Kind: kind, Type: builtinType(snap, types.Int), Span: span}
	case NoneOptional:
		return Node{Kind: kind, Type: builtinType(snap, types.Bool), Span: span}
	case SomeOptional:
		return Node{Kind: kind, Type: boolType, Span: span, Children: []NodeID{ensureChild(BoolLiteral)}}
	case TupleValue:
		return Node{Kind: kind, Type: intType, Span: span, Children: []NodeID{ensureChild(BoolLiteral)}}
	case ArrayValue:
		return Node{Kind: kind, Type: intType, Span: span, Children: []NodeID{ensureChild(BoolLiteral)}}
	case ArrayRepeat:
		return Node{Kind: kind, Type: intType, Span: span, Children: []NodeID{ensureChild(BoolLiteral), ensureChild(IntegerLiteral)}}
	case RecordConstruct:
		return Node{Kind: kind, Type: intType, Span: span, Symbol: 1}
	case HoistedFunctionValue:
		return Node{Kind: kind, Type: intType, Span: span, Symbol: 1, Function: 1}
	case SymbolValue:
		return Node{Kind: kind, Type: intType, Span: span, Symbol: 1}
	case EnumVariantValue:
		return Node{Kind: kind, Type: intType, Span: span, Member: 1}
	case ContextValue:
		return Node{Kind: kind, Type: intType, Span: span, ContextAction: ContextExpr}
	case InterpolatedString:
		return Node{Kind: kind, Type: builtinType(snap, types.Str), Span: span, Children: []NodeID{ensureChild(BoolLiteral)}}
	case SizeofType:
		return Node{Kind: kind, Type: builtinType(snap, types.Uint), Span: span, TypeArg: intType}
	case PrefixValue:
		return Node{Kind: kind, Type: intType, Span: span, Operator: syntax.Minus, Children: []NodeID{ensureChild(IntegerLiteral)}}
	case BinaryValue:
		return Node{Kind: kind, Type: intType, Span: span, Operator: syntax.Plus, Children: []NodeID{ensureChild(IntegerLiteral), ensureChild(IntegerLiteral)}}
	case ShortCircuitValue:
		return Node{Kind: kind, Type: boolType, Span: span, Operator: syntax.LogicalAnd, Children: []NodeID{ensureChild(BoolLiteral), ensureChild(BoolLiteral)}}
	case FieldValue:
		return Node{Kind: kind, Type: intType, Span: span, Member: 1, Children: []NodeID{ensureChild(BoolLiteral)}}
	case TupleElementValue:
		return Node{Kind: kind, Type: intType, Span: span, Ordinal: 1, Children: []NodeID{ensureChild(BoolLiteral)}}
	case GenericFunctionValue:
		return Node{Kind: kind, Type: intType, Span: span, Symbol: 1, GenericRef: 0}
	case SourceAlias:
		return Node{Kind: kind, Type: intType, Span: span, Children: []NodeID{ensureChild(BoolLiteral)}}
	case StoragePlace:
		return Node{Kind: kind, Type: intType, Span: span, Symbol: 1}
	case DereferencePlace:
		return Node{Kind: kind, Type: intType, Span: span, Children: []NodeID{ensureChild(BoolLiteral)}}
	case FieldPlace:
		return Node{Kind: kind, Type: intType, Span: span, Member: 1, Children: []NodeID{ensureChild(StoragePlace)}}
	case TuplePlace:
		return Node{Kind: kind, Type: intType, Span: span, Ordinal: 1, Children: []NodeID{ensureChild(StoragePlace)}}
	case CheckedIndexPlace:
		return Node{Kind: kind, Type: intType, Span: span, Children: []NodeID{ensureChild(StoragePlace), ensureChild(IntegerLiteral)}}
	case Load:
		return Node{Kind: kind, Type: intType, Span: span, Children: []NodeID{ensureChild(StoragePlace)}}
	case DirectCall:
		return Node{Kind: kind, Type: boolType, Span: span, Symbol: 1, Convention: types.Pebble, ContextAction: ContextForward, FunctionType: boolType, Children: []NodeID{ensureChild(BoolLiteral)}}
	case IndirectCall:
		return Node{Kind: kind, Type: boolType, Span: span, Convention: types.C, ContextAction: ContextNone, FunctionType: boolType, Children: []NodeID{ensureChild(BoolLiteral)}}
	case MethodCall:
		return Node{Kind: kind, Type: boolType, Span: span, Symbol: 1, Convention: types.Pebble, ContextAction: ContextForward, FunctionType: boolType, Children: []NodeID{ensureChild(BoolLiteral)}}
	case VariantConstruct:
		return Node{Kind: kind, Type: intType, Span: span, Member: 1}
	case IntegerCast, IntegerToFloat, FloatToInteger, FloatCast, OptionalInject, EnumToInteger, OptionalIntegerToEnum, CheckedIntegerToEnum:
		return Node{Kind: kind, Type: intType, Span: span, Children: []NodeID{ensureChild(IntegerLiteral)}}
	case TupleCoerce:
		return Node{Kind: kind, Type: intType, Span: span, Children: []NodeID{ensureChild(BoolLiteral), ensureChild(IntegerLiteral)}}
	case CheckedOptionalUnwrap:
		return Node{Kind: kind, Type: intType, Span: span, Children: []NodeID{ensureChild(BoolLiteral)}}
	case CheckedIndex:
		return Node{Kind: kind, Type: intType, Span: span, Children: []NodeID{ensureChild(BoolLiteral), ensureChild(IntegerLiteral)}}
	case CheckedSlice:
		return Node{Kind: kind, Type: intType, Span: span, Children: []NodeID{ensureChild(BoolLiteral)}}
	case CheckedArithmetic, CheckedShift:
		return Node{Kind: kind, Type: intType, Span: span, Operator: syntax.Plus, Children: []NodeID{ensureChild(IntegerLiteral), ensureChild(IntegerLiteral)}}
	case TempBind:
		// Always allocate a fresh temp; sharedTemp is reserved for the TempRead
		// that references the pre-added TempBind in complete-unit tests.
		tid := mustTemp(t, b)
		return Node{Kind: kind, Span: span, Temp: tid, Children: []NodeID{ensureChild(BoolLiteral)}}
	case TempRead:
		tid := TempID(1)
		if sharedTemp != 0 {
			tid = sharedTemp
		}
		return Node{Kind: kind, Type: intType, Span: span, Temp: tid}
	case Sequence:
		return Node{Kind: kind, Type: boolType, Span: span, Children: []NodeID{ensureChild(BoolLiteral)}}
	}
	t.Fatalf("validNode missing case for %s", kind)
	return Node{}
}

// TestVerifyCompleteValid builds one unit containing a valid instance of every
// tag and checks that verification succeeds.
func TestVerifyCompleteValid(t *testing.T) {
	b := newTestBuilder(t)
	refs := make(map[NodeID]struct{})
	if err := b.AddModule(smallModule()); err != nil {
		t.Fatalf("AddModule: %v", err)
	}
	// Allocate one region and one function so nodes referencing them are valid.
	r := mustRegion(t, b)
	// Add an instantiation so GenericFunctionValue can reference it.
	if _, err := b.AddInstantiation(Instantiation{Site: ref(module.ModuleID(1), syntax.NodeID(1)), Declaration: 1, TypeArgs: []types.TypeID{1}}); err != nil {
		t.Fatalf("AddInstantiation: %v", err)
	}
	// Pre-add TempBind and TempRead inside a function body so they are valid.
	sharedTemp := mustTemp(t, b)
	bind := mustNode(t, b, Node{Kind: TempBind, Span: span(), Temp: sharedTemp, Children: []NodeID{boolLit(t, b)}})
	read := mustNode(t, b, Node{Kind: TempRead, Type: builtinType(testSnapshot(t), types.Bool), Span: span(), Temp: sharedTemp})
	exprStmt := mustNode(t, b, Node{Kind: ExpressionStatement, Span: span(), Children: []NodeID{read}})
	body := mustNode(t, b, Node{Kind: Block, Span: span(), Region: r, Children: []NodeID{bind, exprStmt}})
	if _, err := b.AddFunctionDecl(FunctionDecl{Symbol: 1, Span: source.Span{Source: 1}, Node: body}); err != nil {
		t.Fatalf("AddFunctionDecl: %v", err)
	}
	for _, kind := range NodeKinds() {
		if kind == TempBind || kind == TempRead {
			continue
		}
		n := validNode(t, b, kind, refs, sharedTemp)
		if _, err := b.AddNode(n); err != nil {
			t.Fatalf("AddNode %s: %v", kind, err)
		}
	}
	u, err := b.Build()
	if err != nil {
		t.Fatalf("Build: %v", err)
	}
	if u.NodeCount() < 81 {
		t.Fatalf("expected at least 81 nodes (one per tag), got %d", u.NodeCount())
	}
}

// TestVerifyMalformedPerTag constructs a valid node for each tag, damages one
// payload field, and asserts verification fails.
func TestVerifyMalformedPerTag(t *testing.T) {
	for _, kind := range NodeKinds() {
		t.Run(kind.String(), func(t *testing.T) {
			b := newTestBuilder(t)
			refs := make(map[NodeID]struct{})
			n := validNode(t, b, kind, refs, 0)
			n = damageNode(t, b, kind, n, refs)
			if _, err := b.AddNode(n); err != nil {
				// Limit errors before add are acceptable as a failure mode.
				return
			}
			if _, err := b.Build(); err == nil {
				t.Fatalf("expected verification failure for damaged %s", kind)
			}
		})
	}
}

// damageNode returns a copy of n with one forbidden or inconsistent field set.
func damageNode(t *testing.T, b *Builder, kind NodeKind, n Node, refs map[NodeID]struct{}) Node {
	t.Helper()
	switch kind {
	case Module:
		n.Symbol = 0
	case Import:
		n.TargetModule = 0
	case TypeDeclaration:
		n.Symbol = 0
	case FieldDeclaration:
		n.Symbol = 0
	case VariantDeclaration:
		n.Symbol = 0
	case FunctionDeclaration:
		n.Function = 0
	case ExternDeclaration:
		n.HasBody = true
	case GlobalDeclaration:
		n.Symbol = 0
	case LocalDeclaration:
		n.Symbol = 0
	case ParameterDeclaration:
		n.Symbol = 0
	case TypeParameterDeclaration:
		n.Symbol = 0
	case TypeUse:
		n.TypeArg = 0
	case Block:
		n.Region = 0
	case Initialize:
		n.Symbol = 0
	case Store:
		// Damage by adding an extra child.
		n.Children = append(n.Children, 1)
	case CompoundStore:
		n.Operator = 0
	case ExpressionStatement:
		n.Children = nil
	case Print:
		pid, err := b.AddNode(validNode(t, b, StoragePlace, refs, 0))
		if err != nil {
			t.Fatalf("damage: %v", err)
		}
		n.Children = append(n.Children, pid)
	case Return:
		n.Function = 0
	case ImplicitReturn:
		n.Function = 0
	case If:
		n.Region = 0
	case While:
		n.Children = nil
	case RangeLoop:
		n.Region = 0
	case For:
		n.Region = 0
	case Switch:
		n.Children = nil
	case SwitchCase:
		n.CaseValue = 0
	case Break, Continue:
		n.Target = 0
	case DeferRegister:
		n.Region = 0
	case BoolLiteral, CharLiteral, StringLiteral, IntegerLiteral, FloatLiteral:
		n.Literal.Kind = LiteralKind(99)
	case NilPointer, NoneOptional:
		// nonvalue field on value node: set forbidden Symbol
		n.Symbol = 1
	case SomeOptional:
		n.Children = nil
	case TupleValue, ArrayValue:
		// Add a place child to break category rule.
		pid, err := b.AddNode(validNode(t, b, StoragePlace, refs, 0))
		if err != nil {
			t.Fatalf("damage: %v", err)
		}
		n.Children = append(n.Children, pid)
	case ArrayRepeat:
		n.Children = n.Children[:1]
	case RecordConstruct:
		n.Symbol = 0
	case HoistedFunctionValue:
		n.Function = 0
	case SymbolValue:
		n.Symbol = 0
	case EnumVariantValue:
		n.Member = 0
	case ContextValue:
		n.ContextAction = 0
	case InterpolatedString:
		pid, err := b.AddNode(validNode(t, b, StoragePlace, refs, 0))
		if err != nil {
			t.Fatalf("damage: %v", err)
		}
		n.Children = append(n.Children, pid)
	case SizeofType:
		n.TypeArg = 0
	case PrefixValue:
		n.Operator = 0
	case BinaryValue:
		n.Children = n.Children[:1]
	case ShortCircuitValue:
		n.Children = n.Children[:1]
	case FieldValue:
		n.Member = 0
	case TupleElementValue:
		n.Ordinal = 0
	case GenericFunctionValue:
		n.GenericRef = 999
	case SourceAlias:
		n.Children = nil
	case StoragePlace:
		n.Symbol = 0
	case DereferencePlace:
		n.Children = nil
	case FieldPlace:
		n.Member = 0
	case TuplePlace:
		n.Ordinal = 0
	case CheckedIndexPlace:
		n.Children = n.Children[:1]
	case Load:
		n.Children = append(n.Children, 1)
	case DirectCall, MethodCall:
		n.Symbol = 0
	case IndirectCall:
		n.ContextAction = ContextExpr
	case VariantConstruct:
		n.Member = 0
	case IntegerCast, IntegerToFloat, FloatToInteger, FloatCast, OptionalInject, EnumToInteger, OptionalIntegerToEnum, CheckedIntegerToEnum:
		n.Children = nil
	case TupleCoerce:
		// Add a place child.
		pid, err := b.AddNode(validNode(t, b, StoragePlace, refs, 0))
		if err != nil {
			t.Fatalf("damage: %v", err)
		}
		n.Children = append(n.Children, pid)
	case CheckedOptionalUnwrap:
		n.Children = nil
	case CheckedIndex:
		n.Children = n.Children[:1]
	case CheckedSlice:
		n.Children = nil
	case CheckedArithmetic, CheckedShift:
		n.Operator = 0
	case TempBind:
		n.Temp = 0
	case TempRead:
		n.Temp = 0
	case Sequence:
		n.Children = nil
	}
	return n
}

// TestVerifyZeroKind rejects a node with kind zero.
func TestVerifyZeroKind(t *testing.T) {
	b := newTestBuilder(t)
	_ = mustNode(t, b, Node{Kind: 0, Span: span()})
	mustFailBuild(t, b)
}

// TestVerifyOutOfRangeKind rejects an out-of-range kind.
func TestVerifyOutOfRangeKind(t *testing.T) {
	b := newTestBuilder(t)
	_ = mustNode(t, b, Node{Kind: LastNodeKind + 1, Span: span()})
	mustFailBuild(t, b)
}

// TestVerifyTypeCategoryAgreement checks category/type rules.
func TestVerifyTypeCategoryAgreement(t *testing.T) {
	b := newTestBuilder(t)
	// Value node without type.
	_ = mustNode(t, b, Node{Kind: BoolLiteral, Span: span(), Literal: Literal{Kind: LiteralBool, Bool: true}})
	mustFailBuild(t, b)

	// Nonvalue node with type.
	b = newTestBuilder(t)
	_ = mustNode(t, b, Node{Kind: Block, Span: span(), Region: 1, Type: builtinType(testSnapshot(t), types.Bool)})
	mustFailBuild(t, b)
}

// TestVerifySourceMapCompleteness checks that nodes with Syntax are mapped.
func TestVerifySourceMapCompleteness(t *testing.T) {
	b := newTestBuilder(t)
	r := ref(module.ModuleID(1), syntax.NodeID(1))
	_ = mustNode(t, b, Node{Kind: BoolLiteral, Type: builtinType(testSnapshot(t), types.Bool), Span: span(), Syntax: r, Literal: Literal{Kind: LiteralBool, Bool: true}})
	mustFailBuild(t, b)
}

// TestVerifyFunctionRegionTarget checks invalid function/region/target IDs.
func TestVerifyFunctionRegionTarget(t *testing.T) {
	b := newTestBuilder(t)
	_ = mustNode(t, b, Node{Kind: Return, Span: span(), Function: 99})
	mustFailBuild(t, b)

	b = newTestBuilder(t)
	_ = mustNode(t, b, Node{Kind: Block, Span: span(), Region: 99})
	mustFailBuild(t, b)

	b = newTestBuilder(t)
	_ = mustNode(t, b, Node{Kind: Break, Span: span(), Target: 99})
	mustFailBuild(t, b)
}

// TestVerifyTempDominance checks single definition and dominance.
func TestVerifyTempDominance(t *testing.T) {
	b := newTestBuilder(t)
	r := mustRegion(t, b)
	t1 := mustTemp(t, b)
	val := boolLit(t, b)
	bind := mustNode(t, b, Node{Kind: TempBind, Span: span(), Temp: t1, Children: []NodeID{val}})
	read := mustNode(t, b, Node{Kind: TempRead, Type: builtinType(testSnapshot(t), types.Bool), Span: span(), Temp: t1})
	exprStmt := mustNode(t, b, Node{Kind: ExpressionStatement, Span: span(), Children: []NodeID{read}})
	body := mustNode(t, b, Node{Kind: Block, Span: span(), Region: r, Children: []NodeID{bind, exprStmt}})
	_ = mustFunction(t, b, body)
	mustBuild(t, b)
}

func TestVerifyTempUndefined(t *testing.T) {
	b := newTestBuilder(t)
	r := mustRegion(t, b)
	read := mustNode(t, b, Node{Kind: TempRead, Type: builtinType(testSnapshot(t), types.Bool), Span: span(), Temp: 1})
	body := mustNode(t, b, Node{Kind: Block, Span: span(), Region: r, Children: []NodeID{read}})
	_ = mustFunction(t, b, body)
	mustFailBuild(t, b)
}

func TestVerifyTempDoubleDefine(t *testing.T) {
	b := newTestBuilder(t)
	r := mustRegion(t, b)
	t1 := mustTemp(t, b)
	val := boolLit(t, b)
	bind1 := mustNode(t, b, Node{Kind: TempBind, Span: span(), Temp: t1, Children: []NodeID{val}})
	bind2 := mustNode(t, b, Node{Kind: TempBind, Span: span(), Temp: t1, Children: []NodeID{val}})
	body := mustNode(t, b, Node{Kind: Block, Span: span(), Region: r, Children: []NodeID{bind1, bind2}})
	_ = mustFunction(t, b, body)
	mustFailBuild(t, b)
}

func TestVerifyTempReadBeforeBind(t *testing.T) {
	b := newTestBuilder(t)
	r := mustRegion(t, b)
	t1 := mustTemp(t, b)
	val := boolLit(t, b)
	read := mustNode(t, b, Node{Kind: TempRead, Type: builtinType(testSnapshot(t), types.Bool), Span: span(), Temp: t1})
	exprStmt := mustNode(t, b, Node{Kind: ExpressionStatement, Span: span(), Children: []NodeID{read}})
	bind := mustNode(t, b, Node{Kind: TempBind, Span: span(), Temp: t1, Children: []NodeID{val}})
	body := mustNode(t, b, Node{Kind: Block, Span: span(), Region: r, Children: []NodeID{exprStmt, bind}})
	_ = mustFunction(t, b, body)
	mustFailBuild(t, b)
}

func TestVerifyTempDominanceSiblingArms(t *testing.T) {
	t.Run("if arms do not share bindings", func(t *testing.T) {
		b := newTestBuilder(t)
		rIf := mustRegion(t, b)
		rThen := mustRegion(t, b)
		rElse := mustRegion(t, b)
		rFunc := mustRegion(t, b)
		tid := mustTemp(t, b)

		bind := mustNode(t, b, Node{Kind: TempBind, Span: span(), Temp: tid, Children: []NodeID{boolLit(t, b)}})
		thenBlock := mustNode(t, b, Node{Kind: Block, Span: span(), Region: rThen, Children: []NodeID{bind}})
		read := mustNode(t, b, Node{Kind: TempRead, Type: builtinType(testSnapshot(t), types.Int), Span: span(), Temp: tid})
		readStmt := mustNode(t, b, Node{Kind: ExpressionStatement, Span: span(), Children: []NodeID{read}})
		elseBlock := mustNode(t, b, Node{Kind: Block, Span: span(), Region: rElse, Children: []NodeID{readStmt}})
		ifNode := mustNode(t, b, Node{Kind: If, Span: span(), Region: rIf, HasElse: true, Children: []NodeID{boolLit(t, b), thenBlock, elseBlock}})
		body := mustNode(t, b, Node{Kind: Block, Span: span(), Region: rFunc, Children: []NodeID{ifNode}})
		_ = mustFunction(t, b, body)

		mustFailBuild(t, b)
	})

	t.Run("switch cases do not share bindings", func(t *testing.T) {
		b := newTestBuilder(t)
		rSwitch := mustRegion(t, b)
		rCase1 := mustRegion(t, b)
		rCase2 := mustRegion(t, b)
		rFunc := mustRegion(t, b)
		tid := mustTemp(t, b)

		bind := mustNode(t, b, Node{Kind: TempBind, Span: span(), Temp: tid, Children: []NodeID{boolLit(t, b)}})
		case1Block := mustNode(t, b, Node{Kind: Block, Span: span(), Region: rCase1, Children: []NodeID{bind}})
		case1 := mustNode(t, b, Node{Kind: SwitchCase, Span: span(), Region: rCase1, CaseValue: 1, Children: []NodeID{case1Block}})
		read := mustNode(t, b, Node{Kind: TempRead, Type: builtinType(testSnapshot(t), types.Int), Span: span(), Temp: tid})
		readStmt := mustNode(t, b, Node{Kind: ExpressionStatement, Span: span(), Children: []NodeID{read}})
		case2Block := mustNode(t, b, Node{Kind: Block, Span: span(), Region: rCase2, Children: []NodeID{readStmt}})
		case2 := mustNode(t, b, Node{Kind: SwitchCase, Span: span(), Region: rCase2, CaseValue: 2, Children: []NodeID{case2Block}})
		switchNode := mustNode(t, b, Node{Kind: Switch, Span: span(), Region: rSwitch, Children: []NodeID{boolLit(t, b), case1, case2}})
		body := mustNode(t, b, Node{Kind: Block, Span: span(), Region: rFunc, Children: []NodeID{switchNode}})
		_ = mustFunction(t, b, body)

		mustFailBuild(t, b)
	})

	t.Run("binding before if is available in an arm", func(t *testing.T) {
		b := newTestBuilder(t)
		rFunc := mustRegion(t, b)
		rIf := mustRegion(t, b)
		rThen := mustRegion(t, b)
		tid := mustTemp(t, b)

		bind := mustNode(t, b, Node{Kind: TempBind, Span: span(), Temp: tid, Children: []NodeID{boolLit(t, b)}})
		read := mustNode(t, b, Node{Kind: TempRead, Type: builtinType(testSnapshot(t), types.Int), Span: span(), Temp: tid})
		readStmt := mustNode(t, b, Node{Kind: ExpressionStatement, Span: span(), Children: []NodeID{read}})
		thenBlock := mustNode(t, b, Node{Kind: Block, Span: span(), Region: rThen, Children: []NodeID{readStmt}})
		ifNode := mustNode(t, b, Node{Kind: If, Span: span(), Region: rIf, Children: []NodeID{boolLit(t, b), thenBlock}})
		body := mustNode(t, b, Node{Kind: Block, Span: span(), Region: rFunc, Children: []NodeID{bind, ifNode}})
		_ = mustFunction(t, b, body)

		mustBuild(t, b)
	})

	t.Run("binding and read in one arm remain sequential", func(t *testing.T) {
		b := newTestBuilder(t)
		rFunc := mustRegion(t, b)
		rIf := mustRegion(t, b)
		rThen := mustRegion(t, b)
		tid := mustTemp(t, b)

		bind := mustNode(t, b, Node{Kind: TempBind, Span: span(), Temp: tid, Children: []NodeID{boolLit(t, b)}})
		read := mustNode(t, b, Node{Kind: TempRead, Type: builtinType(testSnapshot(t), types.Int), Span: span(), Temp: tid})
		readStmt := mustNode(t, b, Node{Kind: ExpressionStatement, Span: span(), Children: []NodeID{read}})
		thenBlock := mustNode(t, b, Node{Kind: Block, Span: span(), Region: rThen, Children: []NodeID{bind, readStmt}})
		ifNode := mustNode(t, b, Node{Kind: If, Span: span(), Region: rIf, Children: []NodeID{boolLit(t, b), thenBlock}})
		body := mustNode(t, b, Node{Kind: Block, Span: span(), Region: rFunc, Children: []NodeID{ifNode}})
		_ = mustFunction(t, b, body)

		mustBuild(t, b)
	})
}

// TestVerifyDeferChain checks that defer chains contain DeferRegister nodes.
func TestVerifyDeferChain(t *testing.T) {
	b := newTestBuilder(t)
	r := mustRegion(t, b)
	deferStmt := mustNode(t, b, Node{Kind: ExpressionStatement, Span: span(), Children: []NodeID{boolLit(t, b)}})
	deferReg := mustNode(t, b, Node{Kind: DeferRegister, Span: span(), Region: r, Children: []NodeID{deferStmt}})
	ret := mustNode(t, b, Node{Kind: Return, Span: span(), Function: 1, DeferChain: []NodeID{deferReg}})
	body := mustNode(t, b, Node{Kind: Block, Span: span(), Region: r, Children: []NodeID{deferReg, ret}})
	_ = mustFunction(t, b, body)
	mustBuild(t, b)
}

func TestVerifyDeferChainInvalid(t *testing.T) {
	b := newTestBuilder(t)
	r := mustRegion(t, b)
	ret := mustNode(t, b, Node{Kind: Return, Span: span(), Function: 1, DeferChain: []NodeID{1}})
	body := mustNode(t, b, Node{Kind: Block, Span: span(), Region: r, Children: []NodeID{ret}})
	_ = mustFunction(t, b, body)
	mustFailBuild(t, b)
}

// TestVerifySwitchCaseLiteral checks that SwitchCase accepts a literal-constant
// case and rejects a case with both CaseValue and Literal set.
func TestVerifySwitchCaseLiteral(t *testing.T) {
	b := newTestBuilder(t)
	r := mustRegion(t, b)
	block := mustNode(t, b, Node{Kind: Block, Span: span(), Region: r})

	// Literal case with CaseValue zero — must succeed.
	_ = mustNode(t, b, Node{Kind: SwitchCase, Span: span(), Region: r, Literal: Literal{Kind: LiteralInteger, IntegerNum: "1"}, Children: []NodeID{block}})
	mustBuild(t, b)

	// Both CaseValue and Literal set — must fail.
	b = newTestBuilder(t)
	r = mustRegion(t, b)
	block = mustNode(t, b, Node{Kind: Block, Span: span(), Region: r})
	_ = mustNode(t, b, Node{Kind: SwitchCase, Span: span(), Region: r, CaseValue: 1, Literal: Literal{Kind: LiteralInteger, IntegerNum: "1"}, Children: []NodeID{block}})
	mustFailBuild(t, b)

	// Both HasElse and Literal set — must fail.
	b = newTestBuilder(t)
	r = mustRegion(t, b)
	block = mustNode(t, b, Node{Kind: Block, Span: span(), Region: r})
	_ = mustNode(t, b, Node{Kind: SwitchCase, Span: span(), Region: r, HasElse: true, Literal: Literal{Kind: LiteralInteger, IntegerNum: "1"}, Children: []NodeID{block}})
	mustFailBuild(t, b)
}

// TestVerifyContextConvention checks context action vs convention consistency.
func TestVerifyContextConvention(t *testing.T) {
	b := newTestBuilder(t)
	// Pebble direct call must forward context.
	arg := boolLit(t, b)
	call := mustNode(t, b, Node{Kind: DirectCall, Type: builtinType(testSnapshot(t), types.Bool), Span: span(), Symbol: 1, Convention: types.Pebble, ContextAction: ContextNone, Children: []NodeID{arg}})
	body := mustNode(t, b, Node{Kind: Block, Span: span(), Region: 1, Children: []NodeID{call}})
	_ = mustFunction(t, b, body)
	mustFailBuild(t, b)

	// C indirect call must have no context.
	b = newTestBuilder(t)
	callee := boolLit(t, b)
	call2 := mustNode(t, b, Node{Kind: IndirectCall, Type: builtinType(testSnapshot(t), types.Bool), Span: span(), Convention: types.C, ContextAction: ContextForward, Children: []NodeID{callee}})
	body2 := mustNode(t, b, Node{Kind: Block, Span: span(), Region: 1, Children: []NodeID{call2}})
	_ = mustFunction(t, b, body2)
	mustFailBuild(t, b)
}

// TestVerifySourceAlias checks ExplicitCast flag and rejection elsewhere.
func TestVerifySourceAlias(t *testing.T) {
	b := newTestBuilder(t)
	inner := boolLit(t, b)
	_ = mustNode(t, b, Node{Kind: SourceAlias, Type: builtinType(testSnapshot(t), types.Bool), Span: span(), ExplicitCast: true, Children: []NodeID{inner}})
	mustBuild(t, b)

	b = newTestBuilder(t)
	_ = mustNode(t, b, Node{Kind: SourceAlias, Type: builtinType(testSnapshot(t), types.Bool), Span: span(), ExplicitCast: false, Children: []NodeID{boolLit(t, b)}})
	mustBuild(t, b)

	b = newTestBuilder(t)
	_ = mustNode(t, b, Node{Kind: BoolLiteral, Type: builtinType(testSnapshot(t), types.Bool), Span: span(), Literal: Literal{Kind: LiteralBool, Bool: true}, ExplicitCast: true})
	mustFailBuild(t, b)
}

// TestVerifyLiteralPayload checks canonical literal kinds.
func TestVerifyLiteralPayload(t *testing.T) {
	b := newTestBuilder(t)
	_ = mustNode(t, b, Node{Kind: BoolLiteral, Type: builtinType(testSnapshot(t), types.Bool), Span: span(), Literal: Literal{Kind: LiteralInteger, IntegerNum: "1"}})
	mustFailBuild(t, b)
}

// TestVerifyRecoveryRejection checks that damaged/invalid inputs never publish.
func TestVerifyRecoveryRejection(t *testing.T) {
	b := newTestBuilder(t)
	// Invalid kind.
	_ = mustNode(t, b, Node{Kind: NodeKind(99), Span: span()})
	u, err := b.Build()
	if err == nil || u != nil {
		t.Fatalf("expected no unit on failure, got u=%v err=%v", u, err)
	}
}

// TestVerifyBoundedErrors checks that error retention is bounded.
func TestVerifyBoundedErrors(t *testing.T) {
	b := NewBuilder(testSnapshot(t), Config{MaxVerifyErrors: 3})
	for i := 0; i < 100; i++ {
		_ = mustNode(t, b, Node{Kind: Block, Span: span(), Region: RegionID(i) + 1})
	}
	u, err := b.Build()
	if err == nil || u != nil {
		t.Fatal("expected failure")
	}
	if !strings.Contains(err.Error(), "region") {
		t.Fatalf("unexpected error: %v", err)
	}
}

// TestVerifyTotality proves the identical exact range is covered by category
// metadata, verifier dispatch, and dumper dispatch.
func TestVerifyTotality(t *testing.T) {
	b := newTestBuilder(t)
	refs := make(map[NodeID]struct{})
	// Allocate region and function, and add a TempBind so TempRead can reference it.
	r := mustRegion(t, b)
	sharedTemp := mustTemp(t, b)
	bind := mustNode(t, b, Node{Kind: TempBind, Span: span(), Temp: sharedTemp, Children: []NodeID{boolLit(t, b)}})
	read := mustNode(t, b, Node{Kind: TempRead, Type: builtinType(testSnapshot(t), types.Bool), Span: span(), Temp: sharedTemp})
	exprStmt := mustNode(t, b, Node{Kind: ExpressionStatement, Span: span(), Children: []NodeID{read}})
	body := mustNode(t, b, Node{Kind: Block, Span: span(), Region: r, Children: []NodeID{bind, exprStmt}})
	if _, err := b.AddFunctionDecl(FunctionDecl{Symbol: 1, Span: source.Span{Source: 1}, Node: body}); err != nil {
		t.Fatalf("AddFunctionDecl: %v", err)
	}
	if _, err := b.AddInstantiation(Instantiation{Site: ref(module.ModuleID(1), syntax.NodeID(1)), Declaration: 1, TypeArgs: []types.TypeID{1}}); err != nil {
		t.Fatalf("AddInstantiation: %v", err)
	}
	for _, kind := range NodeKinds() {
		if kind == TempBind || kind == TempRead {
			continue
		}
		n := validNode(t, b, kind, refs, sharedTemp)
		if _, err := b.AddNode(n); err != nil {
			t.Fatalf("AddNode %s: %v", kind, err)
		}
	}
	u := mustBuild(t, b)

	var buf bytes.Buffer
	if err := u.Dump(&buf); err != nil {
		t.Fatalf("Dump: %v", err)
	}
	out := buf.String()

	// Every tag name appears in the dump.
	for _, kind := range NodeKinds() {
		name, _ := NodeKindName(kind)
		if !strings.Contains(out, name) {
			t.Fatalf("dump missing tag %s", name)
		}
	}
}

// TestVerifySingleFunctionRegionOwnership verifies that a normal single-function
// program with a region still builds successfully. This is a regression guard
// ensuring the cross-function checks are not too strict.
func TestVerifySingleFunctionRegionOwnership(t *testing.T) {
	b := newTestBuilder(t)
	r := mustRegion(t, b)
	exprStmt := mustNode(t, b, Node{Kind: ExpressionStatement, Span: span(), Children: []NodeID{boolLit(t, b)}})
	blk := mustNode(t, b, Node{Kind: Block, Span: span(), Region: r, Children: []NodeID{exprStmt}})
	_ = mustFunction(t, b, blk)
	mustBuild(t, b)
}

// TestVerifyCrossFunctionRegionReference checks that a node in one function
// cannot reference a Region ID introduced by a different function.
func TestVerifyCrossFunctionRegionReference(t *testing.T) {
	b := newTestBuilder(t)
	r := mustRegion(t, b)

	// Function A owns region r.
	blkA := mustNode(t, b, Node{Kind: Block, Span: span(), Region: r, Children: []NodeID{boolLit(t, b)}})
	_ = mustFunction(t, b, blkA)

	// Function B illegitimately references region r.
	exprStmt := mustNode(t, b, Node{Kind: ExpressionStatement, Span: span(), Children: []NodeID{boolLit(t, b)}})
	blkB := mustNode(t, b, Node{Kind: Block, Span: span(), Region: r, Children: []NodeID{exprStmt}})
	_ = mustFunction(t, b, blkB)

	mustFailBuild(t, b)
}

// TestVerifyCrossFunctionTargetReference checks that a Break/Continue node
// whose Target names a region owned by a different function is rejected.
func TestVerifyCrossFunctionTargetReference(t *testing.T) {
	b := newTestBuilder(t)
	r := mustRegion(t, b)

	// Function A owns region r.
	blkA := mustNode(t, b, Node{Kind: Block, Span: span(), Region: r, Children: []NodeID{boolLit(t, b)}})
	_ = mustFunction(t, b, blkA)

	// Function B has a Break targeting region r (owned by A).
	brk := mustNode(t, b, Node{Kind: Break, Span: span(), Target: r})
	blkB := mustNode(t, b, Node{Kind: Block, Span: span(), Region: r, Children: []NodeID{brk}})
	_ = mustFunction(t, b, blkB)

	mustFailBuild(t, b)
}

// TestVerifyOwnershipTypeArg checks that a TypeArg not owned by the snapshot
// is rejected.
func TestVerifyOwnershipTypeArg(t *testing.T) {
	b := newTestBuilder(t)
	_ = mustNode(t, b, Node{Kind: TypeUse, Span: span(), TypeArg: 999})
	mustFailBuild(t, b)
}

// TestVerifyOwnershipResultType checks that a ResultType not owned by the
// snapshot is rejected.
func TestVerifyOwnershipResultType(t *testing.T) {
	b := newTestBuilder(t)
	fid, err := b.AddFunctionDecl(FunctionDecl{Symbol: 1, Span: span()})
	if err != nil {
		t.Fatalf("AddFunctionDecl: %v", err)
	}
	_ = mustNode(t, b, Node{Kind: FunctionDeclaration, Span: span(), Symbol: 1, Function: fid, Convention: types.Pebble, Parameters: []Parameter{{Symbol: 1, Type: builtinType(testSnapshot(t), types.Int)}}, ResultType: 999, HasBody: true})
	mustFailBuild(t, b)
}

// TestVerifyOwnershipFunctionType checks that a FunctionType not owned by the
// snapshot is rejected.
func TestVerifyOwnershipFunctionType(t *testing.T) {
	b := newTestBuilder(t)
	_ = mustNode(t, b, Node{Kind: DirectCall, Span: span(), Type: builtinType(testSnapshot(t), types.Bool), Symbol: 1, Convention: types.Pebble, ContextAction: ContextForward, FunctionType: 999, Children: []NodeID{boolLit(t, b)}})
	mustFailBuild(t, b)
}

// TestVerifyCrossFunctionDeferChain checks that a Return whose DeferChain
// names a DeferRegister node belonging to a different function is rejected.
func TestVerifyCrossFunctionDeferChain(t *testing.T) {
	b := newTestBuilder(t)
	r := mustRegion(t, b)

	// Function A owns a DeferRegister in region r.
	deferStmt := mustNode(t, b, Node{Kind: ExpressionStatement, Span: span(), Children: []NodeID{boolLit(t, b)}})
	deferReg := mustNode(t, b, Node{Kind: DeferRegister, Span: span(), Region: r, Children: []NodeID{deferStmt}})
	blkA := mustNode(t, b, Node{Kind: Block, Span: span(), Region: r, Children: []NodeID{deferReg}})
	_ = mustFunction(t, b, blkA)

	// Function B has a Return whose DeferChain references deferReg from A.
	ret := mustNode(t, b, Node{Kind: Return, Span: span(), Function: 2, DeferChain: []NodeID{deferReg}})
	blkB := mustNode(t, b, Node{Kind: Block, Span: span(), Region: r, Children: []NodeID{ret}})
	_ = mustFunction(t, b, blkB)

	mustFailBuild(t, b)
}
