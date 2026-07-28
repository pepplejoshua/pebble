package tir

import (
	"fmt"

	"github.com/pepplejoshua/pebble/compiler/internal/module"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

// testSnapshot returns a small owned snapshot with a handful of usable TypeIDs.
func testSnapshot(t testish) *types.Snapshot {
	t.Helper()
	store, err := types.New(types.Config{})
	if err != nil {
		t.Fatalf("types.New: %v", err)
	}
	// Add a few non-builtin types for tests.
	if _, err := testPointer(store); err != nil {
		t.Fatalf("testPointer: %v", err)
	}
	if _, err := testOptional(store); err != nil {
		t.Fatalf("testOptional: %v", err)
	}
	if _, err := testTuple(store); err != nil {
		t.Fatalf("testTuple: %v", err)
	}
	snap, err := store.Snapshot()
	if err != nil {
		t.Fatalf("snapshot: %v", err)
	}
	return snap
}

func testPointer(store *types.Store) (types.TypeID, error) {
	b := store.Builtins()
	return store.Intern(types.PointerKey(b.Int))
}

func testOptional(store *types.Store) (types.TypeID, error) {
	b := store.Builtins()
	return store.Intern(types.OptionalKey(b.Int))
}

func testTuple(store *types.Store) (types.TypeID, error) {
	b := store.Builtins()
	return store.Intern(types.TupleKey([]types.TypeID{b.Int, b.Bool}))
}

func testFunction(store *types.Store) (types.TypeID, error) {
	b := store.Builtins()
	return store.Intern(types.FunctionKey(types.Pebble, []types.TypeID{b.Int}, b.Bool, false))
}

type testish interface {
	Helper()
	Fatalf(format string, args ...any)
	Errorf(format string, args ...any)
}

func newTestBuilder(t testish) *Builder {
	t.Helper()
	return NewBuilder(testSnapshot(t), Config{})
}

func simpleValue(t testish, b *Builder, kind NodeKind, typ types.TypeID, ref symbol.SyntaxRef) NodeID {
	t.Helper()
	var n Node
	switch kind {
	case BoolLiteral:
		n = Node{Kind: kind, Type: typ, Span: source.Span{Source: 1, Start: 0, End: 1}, Syntax: ref, Literal: Literal{Kind: LiteralBool, Bool: true}}
	case IntegerLiteral:
		n = Node{Kind: kind, Type: typ, Span: source.Span{Source: 1, Start: 0, End: 1}, Syntax: ref, Literal: Literal{Kind: LiteralInteger, IntegerNum: "1", IntegerDen: "1"}}
	case SymbolValue:
		n = Node{Kind: kind, Type: typ, Span: source.Span{Source: 1, Start: 0, End: 1}, Syntax: ref, Symbol: 1}
	default:
		t.Fatalf("simpleValue does not support %s", kind)
	}
	id, err := b.AddNode(n)
	if err != nil {
		t.Fatalf("AddNode %s: %v", kind, err)
	}
	if ref != (symbol.SyntaxRef{}) {
		if err := b.MapSource(ref, id); err != nil {
			t.Fatalf("MapSource: %v", err)
		}
	}
	return id
}

func mustNode(t testish, b *Builder, n Node) NodeID {
	t.Helper()
	id, err := b.AddNode(n)
	if err != nil {
		t.Fatalf("AddNode %s: %v", n.Kind, err)
	}
	return id
}

func mustRegion(t testish, b *Builder) RegionID {
	t.Helper()
	r, err := b.AddRegion()
	if err != nil {
		t.Fatalf("AddRegion: %v", err)
	}
	return r
}

func mustTemp(t testish, b *Builder) TempID {
	t.Helper()
	tid, err := b.AddTemp()
	if err != nil {
		t.Fatalf("AddTemp: %v", err)
	}
	return tid
}

func mustFunction(t testish, b *Builder, node NodeID) FunctionID {
	t.Helper()
	fid, err := b.AddFunctionDecl(FunctionDecl{Symbol: 1, Span: source.Span{Source: 1}, Node: node})
	if err != nil {
		t.Fatalf("AddFunctionDecl: %v", err)
	}
	return fid
}

func mustBuild(t testish, b *Builder) *Unit {
	t.Helper()
	u, err := b.Build()
	if err != nil {
		t.Fatalf("Build: %v", err)
	}
	return u
}

func mustFailBuild(t testish, b *Builder) {
	t.Helper()
	u, err := b.Build()
	if err == nil {
		t.Fatalf("expected Build failure, got unit with %d nodes", u.NodeCount())
	}
}

func boolLit(t testish, b *Builder) NodeID {
	t.Helper()
	return simpleValue(t, b, BoolLiteral, builtinType(testSnapshot(t), types.Bool), symbol.SyntaxRef{})
}

func intLit(t testish, b *Builder, typ types.TypeID) NodeID {
	t.Helper()
	id, err := b.AddNode(Node{
		Kind:    IntegerLiteral,
		Type:    typ,
		Span:    span(),
		Literal: Literal{Kind: LiteralInteger, IntegerNum: "1", IntegerDen: "1"},
	})
	if err != nil {
		t.Fatalf("AddNode IntegerLiteral: %v", err)
	}
	return id
}

func smallModule() ModuleDecl {
	return ModuleDecl{
		ID:     module.ModuleID(1),
		Key:    module.ModuleKey{Package: "main", Path: "main"},
		Source: source.ID(1),
		Span:   source.Span{Source: 1, Start: 0, End: 10},
		Imports: []ImportDecl{
			{Span: source.Span{Source: 1, Start: 0, End: 5}, Target: module.ModuleID(2)},
		},
		Declarations: []symbol.SymbolID{1, 2},
	}
}

func span() source.Span {
	return source.Span{Source: 1, Start: 0, End: 1}
}

func ref(m module.ModuleID, n syntax.NodeID) symbol.SyntaxRef {
	return symbol.SyntaxRef{Module: m, Node: n}
}

func builtinType(snap *types.Snapshot, k types.BuiltinKind) types.TypeID {
	b := snap.Builtins()
	switch k {
	case types.Bool:
		return b.Bool
	case types.Int:
		return b.Int
	case types.Uint:
		return b.Uint
	case types.Void:
		return b.Void
	case types.Str:
		return b.Str
	case types.Char:
		return b.Char
	case types.F64:
		return b.F64
	case types.F32:
		return b.F32
	case types.I8:
		return b.I8
	case types.I16:
		return b.I16
	case types.I32:
		return b.I32
	case types.I64:
		return b.I64
	case types.U8:
		return b.U8
	case types.U16:
		return b.U16
	case types.U32:
		return b.U32
	case types.U64:
		return b.U64
	}
	panic(fmt.Sprintf("unsupported builtin %v", k))
}
