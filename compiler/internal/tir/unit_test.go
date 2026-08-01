package tir

import (
	"bytes"
	"errors"
	"fmt"
	"io"
	"runtime"
	"runtime/debug"
	"strings"
	"sync"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/module"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

func TestUnitEmptyBuild(t *testing.T) {
	b := newTestBuilder(t)
	u := mustBuild(t, b)
	if u == nil {
		t.Fatal("nil unit")
	}
	if u.NodeCount() != 0 {
		t.Fatalf("expected 0 nodes, got %d", u.NodeCount())
	}
	if u.Snapshot() == nil {
		t.Fatal("nil snapshot")
	}
	if u.RegionCount() != 0 {
		t.Fatalf("RegionCount = %d", u.RegionCount())
	}
	if u.FunctionCount() != 0 {
		t.Fatalf("FunctionCount = %d", u.FunctionCount())
	}
	if u.TempCount() != 0 {
		t.Fatalf("TempCount = %d", u.TempCount())
	}
}

func TestUnitNilBuilder(t *testing.T) {
	var b *Builder
	if _, err := b.AddNode(Node{}); err == nil {
		t.Fatal("nil builder AddNode should fail")
	}
	if _, err := b.Build(); err == nil {
		t.Fatal("nil builder Build should fail")
	}
}

func TestUnitBuilderDoubleBuild(t *testing.T) {
	b := newTestBuilder(t)
	_ = mustBuild(t, b)
	if _, err := b.Build(); err != ErrFrozen {
		t.Fatalf("expected ErrFrozen, got %v", err)
	}
	if _, err := b.AddNode(Node{}); err != ErrFrozen {
		t.Fatalf("expected ErrFrozen, got %v", err)
	}
}

func TestUnitCallerMutation(t *testing.T) {
	b := newTestBuilder(t)
	children := []NodeID{boolLit(t, b)}
	id := mustNode(t, b, Node{Kind: TupleValue, Type: builtinType(testSnapshot(t), types.Bool), Span: span(), Children: children})
	// Mutate caller slice after adding.
	children[0] = 999
	u := mustBuild(t, b)
	got, _ := u.Node(id)
	if len(got.Children) != 1 || got.Children[0] != 1 {
		t.Fatalf("builder retained caller mutation: children=%v", got.Children)
	}
}

func TestUnitDefensiveCopies(t *testing.T) {
	b := newTestBuilder(t)
	_ = b.AddModule(smallModule())
	_ = boolLit(t, b)
	u := mustBuild(t, b)

	mods := u.Modules()
	mods[0].Declarations = append(mods[0].Declarations, 99)
	mods2 := u.Modules()
	if len(mods2[0].Declarations) != 2 {
		t.Fatalf("module accessor not defensive: %v", mods2[0].Declarations)
	}

	nodes := u.Nodes()
	firstID := NodeID(1)
	nodes[0].Children = []NodeID{99}
	got, _ := u.Node(firstID)
	if len(got.Children) != 0 {
		t.Fatalf("node accessor not defensive")
	}
}

func TestUnitDeterministicOrdering(t *testing.T) {
	b := newTestBuilder(t)
	for i := 0; i < 5; i++ {
		_ = boolLit(t, b)
	}
	u := mustBuild(t, b)
	var buf1, buf2 bytes.Buffer
	if err := u.Dump(&buf1); err != nil {
		t.Fatalf("dump1: %v", err)
	}
	if err := u.Dump(&buf2); err != nil {
		t.Fatalf("dump2: %v", err)
	}
	if !bytes.Equal(buf1.Bytes(), buf2.Bytes()) {
		t.Fatalf("dump not deterministic")
	}
}

func TestUnitConcurrentRead(t *testing.T) {
	b := newTestBuilder(t)
	for i := 0; i < 50; i++ {
		_ = boolLit(t, b)
	}
	u := mustBuild(t, b)
	var wg sync.WaitGroup
	for i := 0; i < 100; i++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			_ = u.NodeCount()
			_ = u.Nodes()
			_ = u.Modules()
			_ = u.Requirements()
			_ = u.Instantiations()
			var buf bytes.Buffer
			_ = u.Dump(&buf)
		}()
	}
	wg.Wait()
}

func TestUnitSourceMap(t *testing.T) {
	b := newTestBuilder(t)
	r := ref(module.ModuleID(1), syntax.NodeID(1))
	id := simpleValue(t, b, BoolLiteral, builtinType(testSnapshot(t), types.Bool), r)
	if err := b.MapSource(r, id); err != nil {
		t.Fatalf("MapSource: %v", err)
	}
	u := mustBuild(t, b)
	mapped, ok := u.SourceMap(r)
	if !ok || mapped != id {
		t.Fatalf("sourcemap lookup failed")
	}
	refs := u.SourceRefs()
	if len(refs) != 1 {
		t.Fatalf("expected 1 source ref, got %d", len(refs))
	}
}

func TestUnitSourceMapDuplicate(t *testing.T) {
	b := newTestBuilder(t)
	r := ref(module.ModuleID(1), syntax.NodeID(1))
	id1 := simpleValue(t, b, BoolLiteral, builtinType(testSnapshot(t), types.Bool), r)
	id2 := boolLit(t, b)
	if err := b.MapSource(r, id1); err != nil {
		t.Fatalf("MapSource first: %v", err)
	}
	if err := b.MapSource(r, id2); err == nil {
		t.Fatal("duplicate source map should fail")
	}
}

func TestUnitLimitNodes(t *testing.T) {
	b := NewBuilder(testSnapshot(t), Config{MaxIRNodes: 2})
	_ = boolLit(t, b)
	_ = boolLit(t, b)
	if _, err := b.AddNode(Node{Kind: BoolLiteral, Type: builtinType(testSnapshot(t), types.Bool), Span: span(), Literal: Literal{Kind: LiteralBool, Bool: true}}); err != ErrLimitExceeded {
		t.Fatalf("expected ErrLimitExceeded, got %v", err)
	}
}

func TestUnitLimitComponents(t *testing.T) {
	b := NewBuilder(testSnapshot(t), Config{MaxIRComponents: 2})
	_ = boolLit(t, b)
	if _, err := b.AddNode(Node{Kind: TupleValue, Type: builtinType(testSnapshot(t), types.Bool), Span: span(), Children: []NodeID{1, 2}, Literal: Literal{}}); err != ErrLimitExceeded {
		t.Fatalf("expected ErrLimitExceeded, got %v", err)
	}
}

func TestUnitLimitOverflow(t *testing.T) {
	b := NewBuilder(testSnapshot(t), Config{MaxIRComponents: 110})
	// Add 100 nodes to consume components.
	for i := 0; i < 100; i++ {
		_ = boolLit(t, b)
	}
	// Adding a node with 20 children exceeds the remaining 10 components.
	children := make([]NodeID, 20)
	for i := range children {
		children[i] = 1
	}
	if _, err := b.AddNode(Node{Kind: TupleValue, Type: builtinType(testSnapshot(t), types.Bool), Span: span(), Children: children, Literal: Literal{}}); err != ErrLimitExceeded {
		t.Fatalf("expected ErrLimitExceeded, got %v", err)
	}
}

func TestUnitModuleAndGlobal(t *testing.T) {
	b := newTestBuilder(t)
	if err := b.AddModule(smallModule()); err != nil {
		t.Fatalf("AddModule: %v", err)
	}
	if err := b.AddGlobalDecl(GlobalDecl{Symbol: 1, Span: span(), Type: builtinType(testSnapshot(t), types.Int)}); err != nil {
		t.Fatalf("AddGlobalDecl: %v", err)
	}
	u := mustBuild(t, b)
	if len(u.Modules()) != 1 {
		t.Fatalf("expected 1 module, got %d", len(u.Modules()))
	}
	if len(u.GlobalDeclarations()) != 1 {
		t.Fatalf("expected 1 global, got %d", len(u.GlobalDeclarations()))
	}
}

func TestUnitIndependentUnits(t *testing.T) {
	b1 := newTestBuilder(t)
	b2 := newTestBuilder(t)
	_ = boolLit(t, b1)
	_ = boolLit(t, b2)
	u1 := mustBuild(t, b1)
	u2 := mustBuild(t, b2)
	if u1 == u2 {
		t.Fatal("units are identical")
	}
}

func TestUnitForeignTypeID(t *testing.T) {
	// Build a unit with a non-builtin type so we can test a TypeID that is valid
	// in the first snapshot but out of range in a second snapshot.
	store1, err := types.New(types.Config{})
	if err != nil {
		t.Fatalf("types.New: %v", err)
	}
	b := store1.Builtins()
	foreignType, err := store1.Intern(types.PointerKey(b.Int))
	if err != nil {
		t.Fatalf("Intern: %v", err)
	}
	snap1, err := store1.Snapshot()
	if err != nil {
		t.Fatalf("snapshot1: %v", err)
	}
	b1 := NewBuilder(snap1, Config{})
	_ = mustNode(t, b1, Node{Kind: BoolLiteral, Type: foreignType, Span: span(), Literal: Literal{Kind: LiteralBool, Bool: true}})
	u1 := mustBuild(t, b1)

	// Create a second snapshot with only the builtins.
	store2, err := types.New(types.Config{MaxTypes: 16})
	if err != nil {
		t.Fatalf("types.New: %v", err)
	}
	snap2, err := store2.Snapshot()
	if err != nil {
		t.Fatalf("snapshot2: %v", err)
	}
	if uint64(foreignType) <= uint64(snap2.Len()) {
		t.Fatalf("foreignType %d should be out of range in snap2 (len=%d)", foreignType, snap2.Len())
	}
	b2 := NewBuilder(snap2, Config{})
	_ = mustNode(t, b2, Node{Kind: BoolLiteral, Type: foreignType, Span: span(), Literal: Literal{Kind: LiteralBool, Bool: true}})
	mustFailBuild(t, b2)
	_ = u1
}

func TestUnitFunctionAndRegion(t *testing.T) {
	b := newTestBuilder(t)
	r := mustRegion(t, b)
	body := mustNode(t, b, Node{Kind: Block, Region: r, Span: span()})
	fid := mustFunction(t, b, body)
	u := mustBuild(t, b)
	if u.RegionCount() != 1 {
		t.Fatalf("RegionCount = %d", u.RegionCount())
	}
	if u.FunctionCount() != 1 {
		t.Fatalf("FunctionCount = %d", u.FunctionCount())
	}
	f := u.FunctionDeclarations()[0]
	if f.FunctionID != fid {
		t.Fatalf("FunctionID mismatch")
	}
}

func TestUnitTempAllocation(t *testing.T) {
	b := newTestBuilder(t)
	t1 := mustTemp(t, b)
	t2 := mustTemp(t, b)
	if t1 != 1 || t2 != 2 {
		t.Fatalf("temp allocation not monotonic: %d, %d", t1, t2)
	}
	u := mustBuild(t, b)
	if u.TempCount() != 2 {
		t.Fatalf("TempCount = %d", u.TempCount())
	}
}

func TestUnitRequirementAndInstantiation(t *testing.T) {
	b := newTestBuilder(t)
	if err := b.AddRequirement(Requirement{Owner: 1, Parameter: 2, Kind: RequirementNumeric, Subject: 1}); err != nil {
		t.Fatalf("AddRequirement: %v", err)
	}
	idx, err := b.AddInstantiation(Instantiation{Site: ref(module.ModuleID(1), syntax.NodeID(1)), Declaration: 1, TypeArgs: []types.TypeID{1}})
	if err != nil {
		t.Fatalf("AddInstantiation: %v", err)
	}
	u := mustBuild(t, b)
	if len(u.Requirements()) != 1 {
		t.Fatalf("expected 1 requirement, got %d", len(u.Requirements()))
	}
	if len(u.Instantiations()) != 1 {
		t.Fatalf("expected 1 instantiation, got %d", len(u.Instantiations()))
	}
	if idx != 0 {
		t.Fatalf("instantiation index = %d", idx)
	}
	ins := u.Instantiations()
	ins[0].TypeArgs[0] = 999
	ins2 := u.Instantiations()
	if ins2[0].TypeArgs[0] == 999 {
		t.Fatal("instantiation TypeArgs not defensive")
	}
}

func TestDumpTotality(t *testing.T) {
	snap := testSnapshot(t)
	intType := builtinType(snap, types.Int)

	for _, kind := range NodeKinds() {
		t.Run(kind.String(), func(t *testing.T) {
			b := newTestBuilder(t)

			// Common infrastructure that validNode (in verify_test.go)
			// may depend on.
			r := mustRegion(t, b) // RegionID 1

			// We create the function body before adding the test node so
			// that verifyTemps dominance can reach TempBind/TempRead.
			// For most tags this body is empty (no children on Block).
			body := mustNode(t, b, Node{
				Kind: Block, Span: span(), Region: r,
			})
			mustFunction(t, b, body) // FunctionID 1
			mustTemp(t, b)           // TempID 1

			if _, err := b.AddInstantiation(Instantiation{
				Site:        ref(module.ModuleID(1), syntax.NodeID(1)),
				Declaration: 1,
				TypeArgs:    []types.TypeID{intType},
			}); err != nil {
				t.Fatalf("AddInstantiation: %v", err)
			}

			var panicked bool
			var dumpErr error

			func() {
				defer func() {
					if r := recover(); r != nil {
						panicked = true
					}
				}()

				// TempBind/TempRead must be reachable from the body
				// Block through the dominance tree.  Handle them
				// specially: chain Block -> [TempBind,
				// ExpressionStatement -> TempRead].
				switch kind {
				case TempBind:
					tid := mustTemp(t, b) // TempID 2
					tb := mustNode(t, b, Node{
						Kind: TempBind, Span: span(),
						Temp: tid, Children: []NodeID{boolLit(t, b)},
					})
					// Update body to reference TempBind.
					// We can't edit a frozen node, so replace it
					// by building a new Block. It belongs to a
					// separate function, so it needs its own
					// region, not the first function's.
					r2 := mustRegion(t, b)
					body2 := mustNode(t, b, Node{
						Kind: Block, Span: span(), Region: r2,
						Children: []NodeID{tb},
					})
					// Register a second function so dominance
					// traversal covers the new block.
					mustFunction(t, b, body2)

				case TempRead:
					tb := mustNode(t, b, Node{
						Kind: TempBind, Span: span(),
						Temp: 1, Children: []NodeID{boolLit(t, b)},
					})
					tr := mustNode(t, b, Node{
						Kind: TempRead, Type: intType,
						Span: span(), Temp: 1,
					})
					// TempRead is a value; wrap in a nonvalue
					// ExpressionStatement to nest inside Block.
					es := mustNode(t, b, Node{
						Kind: ExpressionStatement, Span: span(),
						Children: []NodeID{tr},
					})
					r2 := mustRegion(t, b)
					body2 := mustNode(t, b, Node{
						Kind: Block, Span: span(), Region: r2,
						Children: []NodeID{tb, es},
					})
					mustFunction(t, b, body2)

				default:
					refs := make(map[NodeID]struct{})
					n := validNode(t, b, kind, refs, 0)
					mustNode(t, b, n)
				}

				u, buildErr := b.Build()
				if buildErr != nil {
					t.Fatalf("Build failed for tag %s: %v", kind, buildErr)
				}
				dumpErr = u.Dump(io.Discard)
			}()

			if panicked {
				t.Fatalf("dump panicked for tag %s", kind)
			}
			if dumpErr != nil && strings.Contains(dumpErr.Error(), "unhandled kind") {
				t.Fatalf("dump reported unhandled kind for tag %s: %v", kind, dumpErr)
			}
		})
	}
}

func TestReserveFunctionDeclAssignsRealID(t *testing.T) {
	b := newTestBuilder(t)
	fid, err := b.ReserveFunctionDecl(FunctionDecl{Symbol: 7, Span: span()})
	if err != nil {
		t.Fatalf("ReserveFunctionDecl: %v", err)
	}
	if fid != 1 {
		t.Fatalf("expected first reserved FunctionID 1, got %d", fid)
	}
	if b.functions[0].FunctionID != fid {
		t.Fatalf("reserved entry FunctionID = %d, want %d", b.functions[0].FunctionID, fid)
	}
	if b.functions[0].Node != 0 {
		t.Fatalf("reserved entry Node = %d, want 0 until completed", b.functions[0].Node)
	}
	if b.functions[0].Symbol != 7 {
		t.Fatalf("reserved entry lost caller Symbol: got %d, want 7", b.functions[0].Symbol)
	}
}

func TestReserveFunctionDeclMonotonic(t *testing.T) {
	b := newTestBuilder(t)
	fid1, err := b.ReserveFunctionDecl(FunctionDecl{Symbol: 1, Span: span()})
	if err != nil {
		t.Fatalf("ReserveFunctionDecl first: %v", err)
	}
	fid2, err := b.ReserveFunctionDecl(FunctionDecl{Symbol: 2, Span: span()})
	if err != nil {
		t.Fatalf("ReserveFunctionDecl second: %v", err)
	}
	if fid1 != 1 || fid2 != 2 {
		t.Fatalf("expected monotonic FunctionIDs 1, 2, got %d, %d", fid1, fid2)
	}
	if fid1 == fid2 {
		t.Fatal("two reserves produced the same FunctionID")
	}
}

func TestCompleteFunctionDeclSetsNode(t *testing.T) {
	b := newTestBuilder(t)
	fid, err := b.ReserveFunctionDecl(FunctionDecl{Symbol: 1, Span: span()})
	if err != nil {
		t.Fatalf("ReserveFunctionDecl: %v", err)
	}
	r := mustRegion(t, b)
	body := mustNode(t, b, Node{Kind: Block, Region: r, Span: span()})
	if err := b.CompleteFunctionDecl(fid, body); err != nil {
		t.Fatalf("CompleteFunctionDecl: %v", err)
	}
	u := mustBuild(t, b)
	f := u.FunctionDeclarations()[0]
	if f.Node != body {
		t.Fatalf("completed FunctionDecl Node = %d, want %d", f.Node, body)
	}
}

func TestCompleteFunctionDeclUnreserved(t *testing.T) {
	b := newTestBuilder(t)
	if err := b.CompleteFunctionDecl(0, 1); err == nil {
		t.Fatal("completing FunctionID 0 should fail")
	} else if !errors.Is(err, ErrInvalidNode) {
		t.Fatalf("expected ErrInvalidNode, got %v", err)
	}
	fid, err := b.ReserveFunctionDecl(FunctionDecl{Symbol: 1, Span: span()})
	if err != nil {
		t.Fatalf("ReserveFunctionDecl: %v", err)
	}
	if err := b.CompleteFunctionDecl(fid+1, 1); err == nil {
		t.Fatalf("completing unreserved FunctionID %d should fail", fid+1)
	} else if !errors.Is(err, ErrInvalidNode) {
		t.Fatalf("expected ErrInvalidNode, got %v", err)
	}
}

func TestCompleteFunctionDeclDoubleCompletion(t *testing.T) {
	b := newTestBuilder(t)
	fid, err := b.ReserveFunctionDecl(FunctionDecl{Symbol: 1, Span: span()})
	if err != nil {
		t.Fatalf("ReserveFunctionDecl: %v", err)
	}
	r := mustRegion(t, b)
	body := mustNode(t, b, Node{Kind: Block, Region: r, Span: span()})
	if err := b.CompleteFunctionDecl(fid, body); err != nil {
		t.Fatalf("first CompleteFunctionDecl: %v", err)
	}
	if err := b.CompleteFunctionDecl(fid, body); err == nil {
		t.Fatal("second CompleteFunctionDecl on the same fid should fail")
	} else if !errors.Is(err, ErrInvalidNode) {
		t.Fatalf("expected ErrInvalidNode, got %v", err)
	}
}

func TestCompleteFunctionDeclInvalidNode(t *testing.T) {
	b := newTestBuilder(t)
	fid, err := b.ReserveFunctionDecl(FunctionDecl{Symbol: 1, Span: span()})
	if err != nil {
		t.Fatalf("ReserveFunctionDecl: %v", err)
	}
	if err := b.CompleteFunctionDecl(fid, 0); err == nil {
		t.Fatal("completing with node 0 should fail")
	} else if !errors.Is(err, ErrInvalidNode) {
		t.Fatalf("expected ErrInvalidNode, got %v", err)
	}
	if err := b.CompleteFunctionDecl(fid, 999); err == nil {
		t.Fatal("completing with a nonexistent node should fail")
	} else if !errors.Is(err, ErrInvalidNode) {
		t.Fatalf("expected ErrInvalidNode, got %v", err)
	}
}

func TestReserveCompleteInterleaved(t *testing.T) {
	b := newTestBuilder(t)
	fid, err := b.ReserveFunctionDecl(FunctionDecl{Symbol: 1, Span: span()})
	if err != nil {
		t.Fatalf("ReserveFunctionDecl: %v", err)
	}
	r := mustRegion(t, b)
	ret := mustNode(t, b, Node{Kind: Return, Span: span(), Function: fid, Children: []NodeID{boolLit(t, b)}})
	body := mustNode(t, b, Node{Kind: Block, Region: r, Span: span(), Children: []NodeID{ret}})
	if err := b.CompleteFunctionDecl(fid, body); err != nil {
		t.Fatalf("CompleteFunctionDecl: %v", err)
	}
	u := mustBuild(t, b)
	f := u.FunctionDeclarations()[0]
	if f.FunctionID != fid {
		t.Fatalf("FunctionID = %d, want %d", f.FunctionID, fid)
	}
	if f.Node != body {
		t.Fatalf("completed FunctionDecl Node = %d, want %d", f.Node, body)
	}
}

func TestUnitAddRegionOverflow(t *testing.T) {
	b := newTestBuilder(t)
	b.regionCount = ^uint32(0)
	rid, err := b.AddRegion()
	if err != ErrLimitExceeded {
		t.Fatalf("expected ErrLimitExceeded, got %v", err)
	}
	if rid != 0 {
		t.Fatalf("expected zero RegionID on overflow, got %d", rid)
	}
}

func TestDumpSliceFormatPreservation(t *testing.T) {
	snap := testSnapshot(t)
	boolType := builtinType(snap, types.Bool)
	intType := builtinType(snap, types.Int)

	b := NewBuilder(snap, Config{})

	// Add three leaf nodes to use as children.
	c1 := mustNode(t, b, Node{Kind: BoolLiteral, Type: boolType, Span: span(), Literal: Literal{Kind: LiteralBool, Bool: true}})
	c2 := mustNode(t, b, Node{Kind: BoolLiteral, Type: boolType, Span: span(), Literal: Literal{Kind: LiteralBool, Bool: false}})
	c3 := mustNode(t, b, Node{Kind: BoolLiteral, Type: boolType, Span: span(), Literal: Literal{Kind: LiteralBool, Bool: true}})

	// TupleValue with Children = [c1, c2, c3].
	_ = mustNode(t, b, Node{
		Kind:     TupleValue,
		Type:     boolType,
		Span:     span(),
		Children: []NodeID{c1, c2, c3},
	})

	// TupleCoerce node with TypeArgs to exercise the in-switch typeargs path.
	// Needs at least 2 children (both value-category).
	child1 := mustNode(t, b, Node{Kind: BoolLiteral, Type: boolType, Span: span(), Literal: Literal{Kind: LiteralBool, Bool: true}})
	child2 := mustNode(t, b, Node{Kind: BoolLiteral, Type: boolType, Span: span(), Literal: Literal{Kind: LiteralBool, Bool: false}})
	_ = mustNode(t, b, Node{
		Kind:     TupleCoerce,
		Type:     boolType,
		Span:     span(),
		TypeArgs: []types.TypeID{intType, boolType},
		Children: []NodeID{child1, child2},
	})

	// ExternDeclaration node with Parameters to exercise the params path.
	_ = mustNode(t, b, Node{
		Kind: ExternDeclaration, Symbol: 42, Convention: types.Pebble, Span: span(),
		Parameters: []Parameter{
			{Symbol: 10, Type: intType},
			{Symbol: 20, Type: boolType},
		},
	})

	u := mustBuild(t, b)

	var buf bytes.Buffer
	if err := u.Dump(&buf); err != nil {
		t.Fatalf("Dump: %v", err)
	}
	out := buf.String()

	// Verify children format: space-separated NodeID values inside brackets.
	// Go's %v for []NodeID (a []uint32 without String()) produces "[1 2 3]".
	childrenWant := " children=[1 2 3]"
	if !strings.Contains(out, childrenWant) {
		t.Errorf("dump missing children format %q\ngot:\n%s", childrenWant, out)
	}

	// Verify typeargs format for TupleCoerce (in-switch path).
	// Format is " typeargs=[T1 T2]" where T1, T2 are the TypeID integers.
	typeargsWant := fmt.Sprintf(" typeargs=[%d %d]", intType, boolType)
	if !strings.Contains(out, typeargsWant) {
		t.Errorf("dump missing typeargs format %q\ngot:\n%s", typeargsWant, out)
	}

	// Verify params format: "Sym:Type Sym:Type" pairs.
	paramsWant := fmt.Sprintf(" params=[%d:%d %d:%d]", 10, intType, 20, boolType)
	if !strings.Contains(out, paramsWant) {
		t.Errorf("dump missing params format %q\ngot:\n%s", paramsWant, out)
	}
}

func TestDumpBoundedAllocation(t *testing.T) {
	snap := testSnapshot(t)
	boolType := builtinType(snap, types.Bool)

	const hugeSize = 500_000
	const maxDumpBytes uint64 = 512

	// Build a Unit directly (we're in package tir) so the TupleValue is
	// the FIRST node.  This ensures Dump reaches the children=%v printf
	// before overflowing on other node output.
	//
	// Node layout:
	//   NodeID 1: TupleValue with Children = [2..500001]
	//   NodeID 2..500001: BoolLiteral leaf nodes (needed as valid children)
	children := make([]NodeID, hugeSize)
	nodes := make([]Node, 0, hugeSize+1)
	for i := range children {
		children[i] = NodeID(i + 2) // leaf IDs start at 2
		nodes = append(nodes, Node{
			Kind: BoolLiteral, Type: boolType, Span: span(),
			Literal: Literal{Kind: LiteralBool, Bool: true},
		})
	}
	// TupleValue is node 1 — the first node Dump processes.
	nodes = append([]Node{{
		Kind:     TupleValue,
		Type:     boolType,
		Span:     span(),
		Children: children,
	}}, nodes...)

	u := &Unit{
		snapshot: snap,
		nodes:    nodes,
		config:   Config{MaxDumpBytes: maxDumpBytes},
	}

	// Verify Dump returns ErrDumpOverflow.
	var buf bytes.Buffer
	err := u.Dump(&buf)
	if err != ErrDumpOverflow {
		t.Fatalf("expected ErrDumpOverflow, got %v (buf.Len=%d)", err, buf.Len())
	}

	// Measure byte allocation: run Dump multiple times with GC disabled
	// so transient allocations accumulate in the heap, then measure heap growth.
	runtime.GC() // clean slate
	var mBefore runtime.MemStats
	runtime.ReadMemStats(&mBefore)

	oldPerc := debug.SetGCPercent(-1) // disable GC
	const dumpRuns = 10
	for i := 0; i < dumpRuns; i++ {
		var b2 bytes.Buffer
		_ = u.Dump(&b2)
	}
	debug.SetGCPercent(oldPerc) // re-enable GC
	runtime.GC()

	var mAfter runtime.MemStats
	runtime.ReadMemStats(&mAfter)

	heapGrowth := mAfter.TotalAlloc - mBefore.TotalAlloc
	// With the fixed code, each Dump call allocates at most O(MaxDumpBytes)
	// worth of intermediate strings (small per-element Sprintf calls, each
	// freed quickly). With the old code, Sprintf(" children=%v", hugeSlice)
	// would allocate ~1 MB per call — proportional to the slice, not the
	// budget. Over dumpRuns calls that's ~10 MB, far exceeding our bound.
	bound := uint64(maxDumpBytes) * uint64(dumpRuns) * 10
	if raceEnabled {
		bound *= 8
	}
	if heapGrowth > bound {
		t.Errorf("Dump allocated %d bytes over %d calls (> %d bound, MaxDumpBytes=%d); allocation not bounded to budget",
			heapGrowth, dumpRuns, bound, maxDumpBytes)
	}
	t.Logf("Dump allocated %d bytes over %d calls (bound %d, MaxDumpBytes=%d, %d children)",
		heapGrowth, dumpRuns, bound, maxDumpBytes, hugeSize)
}

func TestUnitAddRegionLimit(t *testing.T) {
	b := NewBuilder(testSnapshot(t), Config{MaxIRComponents: 1})
	if err := b.AddGlobalDecl(GlobalDecl{Symbol: 1, Span: span(), Type: builtinType(testSnapshot(t), types.Int)}); err != nil {
		t.Fatalf("AddGlobalDecl: %v", err)
	}
	_, err := b.AddRegion()
	if err != ErrLimitExceeded {
		t.Fatalf("expected ErrLimitExceeded, got %v", err)
	}
	if b.regionCount != 0 {
		t.Fatalf("regionCount should remain 0 after failed AddRegion, got %d", b.regionCount)
	}
}

func TestUnitAddTempLimit(t *testing.T) {
	b := NewBuilder(testSnapshot(t), Config{MaxIRComponents: 1})
	if err := b.AddGlobalDecl(GlobalDecl{Symbol: 1, Span: span(), Type: builtinType(testSnapshot(t), types.Int)}); err != nil {
		t.Fatalf("AddGlobalDecl: %v", err)
	}
	_, err := b.AddTemp()
	if err != ErrLimitExceeded {
		t.Fatalf("expected ErrLimitExceeded, got %v", err)
	}
	if b.nextTemp != 0 {
		t.Fatalf("nextTemp should remain 0 after failed AddTemp, got %d", b.nextTemp)
	}
	if b.tempCount != 0 {
		t.Fatalf("tempCount should remain 0 after failed AddTemp, got %d", b.tempCount)
	}
}

func TestUnitMapSourceLimit(t *testing.T) {
	b := NewBuilder(testSnapshot(t), Config{MaxIRComponents: 1})
	if err := b.AddGlobalDecl(GlobalDecl{Symbol: 1, Span: span(), Type: builtinType(testSnapshot(t), types.Int)}); err != nil {
		t.Fatalf("AddGlobalDecl: %v", err)
	}
	ref := ref(module.ModuleID(1), syntax.NodeID(1))
	err := b.MapSource(ref, NodeID(1))
	if err != ErrLimitExceeded {
		t.Fatalf("expected ErrLimitExceeded, got %v", err)
	}
	if _, ok := b.sourceMap[ref]; ok {
		t.Fatal("sourceMap should not contain ref after failed MapSource")
	}
}

func TestUnitMapSourceIdempotentLimit(t *testing.T) {
	b := NewBuilder(testSnapshot(t), Config{MaxIRComponents: 1})
	ref := ref(module.ModuleID(1), syntax.NodeID(1))
	id := NodeID(1)
	if err := b.MapSource(ref, id); err != nil {
		t.Fatalf("MapSource: %v", err)
	}
	if err := b.MapSource(ref, id); err != nil {
		t.Fatalf("identical MapSource should succeed under exhausted budget, got %v", err)
	}
	if _, ok := b.sourceMap[ref]; !ok {
		t.Fatal("sourceMap should still contain ref after idempotent MapSource")
	}
}

func TestUnitMapSourceConflictNoCharge(t *testing.T) {
	b := NewBuilder(testSnapshot(t), Config{MaxIRComponents: 2})
	ref := ref(module.ModuleID(1), syntax.NodeID(1))
	id1 := NodeID(1)
	id2 := NodeID(2)
	if err := b.MapSource(ref, id1); err != nil {
		t.Fatalf("MapSource first: %v", err)
	}
	if err := b.MapSource(ref, id2); err == nil {
		t.Fatal("duplicate source map should fail")
	}
	if err := b.AddGlobalDecl(GlobalDecl{Symbol: 1, Span: span(), Type: builtinType(testSnapshot(t), types.Int)}); err != nil {
		t.Fatalf("AddGlobalDecl should succeed after conflicting MapSource, got %v", err)
	}
}

func TestUnitComponentsInUnitCounts(t *testing.T) {
	b := newTestBuilder(t)
	_ = mustRegion(t, b)
	_ = mustTemp(t, b)
	ref := ref(module.ModuleID(1), syntax.NodeID(1))
	_ = simpleValue(t, b, BoolLiteral, builtinType(testSnapshot(t), types.Bool), ref)
	u := mustBuild(t, b)

	got := componentsInUnit(u)
	// 1 node + 1 sourceMap entry + 1 region + 1 temp = 4
	if got != 4 {
		t.Fatalf("componentsInUnit = %d, want 4", got)
	}
}

// TestVerifyDeepNestingIterative demonstrates that the iterative TIR
// traversals in markNodeFunction and checkTempDominance succeed under a
// goroutine stack ceiling far too small for the equivalent recursive walk.
// The depth lives on the heap-allocated explicit stack, so lowering the
// native stack maximum with debug.SetMaxStack does not constrain it.
func TestVerifyDeepNestingIterative(t *testing.T) {
	oldMax := debug.SetMaxStack(64 * 1024)
	defer debug.SetMaxStack(oldMax)

	b := newTestBuilder(t)
	const depth = 10000

	// Innermost block holds a single leaf statement so the traversal is not
	// just a chain of empty blocks.
	leafExpr := boolLit(t, b)
	leafStmt := mustNode(t, b, Node{Kind: ExpressionStatement, Span: span(), Children: []NodeID{leafExpr}})
	inner := mustNode(t, b, Node{Kind: Block, Span: span(), Region: mustRegion(t, b), Children: []NodeID{leafStmt}})

	for i := 1; i < depth; i++ {
		inner = mustNode(t, b, Node{Kind: Block, Span: span(), Region: mustRegion(t, b), Children: []NodeID{inner}})
	}

	_ = mustFunction(t, b, inner)
	mustBuild(t, b)
}
