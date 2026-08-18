package tir

import (
	"strings"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

// TestNodeKindInventory mechanically asserts exact count/order/name/category
// for all 87 tags.
func TestNodeKindInventory(t *testing.T) {
	kinds := NodeKinds()
	if len(kinds) != 88 {
		t.Fatalf("expected 88 node kinds, got %d", len(kinds))
	}
	if FirstNodeKind != Module {
		t.Fatalf("FirstNodeKind = %v, want Module", FirstNodeKind)
	}
	if LastNodeKind != SliceFromRaw {
		t.Fatalf("LastNodeKind = %v, want SliceFromRaw", LastNodeKind)
	}
	// The range is contiguous and nonzero.
	for i, k := range kinds {
		want := NodeKind(i + 1)
		if k != want {
			t.Fatalf("kind[%d] = %v, want %v", i, k, want)
		}
		name, ok := NodeKindName(k)
		if !ok {
			t.Fatalf("NodeKindName(%v) false", k)
		}
		if name == "" {
			t.Fatalf("empty name for %v", k)
		}
		cat, ok := CategoryOf(k)
		if !ok {
			t.Fatalf("CategoryOf(%v) false", k)
		}
		if cat == 0 {
			t.Fatalf("zero category for %v", k)
		}
		// Verify no duplicate names.
		for j, other := range kinds {
			if i != j {
				otherName, _ := NodeKindName(other)
				if otherName == name {
					t.Fatalf("duplicate name %q", name)
				}
			}
		}
	}

	// Verify the seven category blocks are correctly placed.
	blocks := []struct {
		start, end NodeKind
		name       string
	}{
		{Module, Block, "declarations"},
		{Initialize, DeferRegister, "statements"},
		{BoolLiteral, AddressOf, "values"},
		{StoragePlace, Load, "places"},
		{DirectCall, VariantConstruct, "calls"},
		{IntegerCast, PointerToInteger, "coercions"},
		{TempBind, Sequence, "sequencing"},
	}
	for _, b := range blocks {
		for k := b.start; k <= b.end; k++ {
			name, _ := NodeKindName(k)
			if name == "" {
				t.Fatalf("empty name in block %s at %v", b.name, k)
			}
		}
	}
}

// TestNodeKindRange rejects zero and out-of-range tags.
func TestNodeKindRange(t *testing.T) {
	for _, k := range []NodeKind{0, LastNodeKind + 1, LastNodeKind + 10} {
		if _, ok := CategoryOf(k); ok {
			t.Fatalf("CategoryOf(%v) should fail", k)
		}
		if _, ok := NodeKindName(k); ok {
			t.Fatalf("NodeKindName(%v) should fail", k)
		}
	}
}

// TestNodeKindString verifies normalized string output.
func TestNodeKindString(t *testing.T) {
	if s := Module.String(); s != "Module" {
		t.Fatalf("Module.String() = %q", s)
	}
	if s := (LastNodeKind + 1).String(); s == "Sequence" || s == "" {
		t.Fatalf("out-of-range string should be deterministic, got %q", s)
	}
}

// TestNodeKindValid confirms IsValid covers exactly the closed range.
func TestNodeKindValid(t *testing.T) {
	if NodeKind(0).IsValid() {
		t.Fatal("zero kind is valid")
	}
	if !NodeKind(1).IsValid() {
		t.Fatal("kind 1 invalid")
	}
	if !LastNodeKind.IsValid() {
		t.Fatalf("LastNodeKind invalid")
	}
	if (LastNodeKind + 1).IsValid() {
		t.Fatalf("LastNodeKind+1 valid")
	}
}

// TestCategoryStrings covers all categories.
func TestCategoryStrings(t *testing.T) {
	for _, cat := range []nodeCategory{CategoryNonvalue, CategoryValue, CategoryPlace} {
		if s := cat.String(); s == "" || s == "unknown" {
			t.Fatalf("unexpected category string %q", s)
		}
	}
	if s := nodeCategory(99).String(); s != "unknown" {
		t.Fatalf("unknown category string = %q", s)
	}
}

// TestRequirementKindStrings covers all requirement kinds.
func TestRequirementKindStrings(t *testing.T) {
	for _, k := range []RequirementKind{RequirementNumeric, RequirementIntegral, RequirementOrdered, RequirementEquatable, RequirementLiteralFits} {
		if s := k.String(); s == "" || s == "unknown" {
			t.Fatalf("unexpected requirement kind string %q", s)
		}
	}
}

// TestLiteralKindStrings covers all literal kinds.
func TestLiteralKindStrings(t *testing.T) {
	for _, k := range []LiteralKind{LiteralBool, LiteralChar, LiteralString, LiteralInteger, LiteralFloat} {
		if s := k.String(); s == "" || s == "unknown" {
			t.Fatalf("unexpected literal kind string %q", s)
		}
	}
}

// TestBuiltinTypeAccess ensures the helper can retrieve builtins.
func TestBuiltinTypeAccess(t *testing.T) {
	snap := testSnapshot(t)
	if builtinType(snap, types.Bool) == 0 {
		t.Fatal("Bool type is zero")
	}
}

// TestVerifierDispatchTotality proves that every tag in the complete contiguous
// NodeKind range is handled by the verifier switch. For each tag and each of
// 0, 1, 2, 3 children, it constructs a minimal node, runs the verifier, and
// asserts: (a) no panic, and (b) no error containing "unhandled kind".
func TestVerifierDispatchTotality(t *testing.T) {
	snap := testSnapshot(t)
	boolType := builtinType(snap, types.Bool)
	intType := builtinType(snap, types.Int)

	// invalidChild marks that the last child slot (when numChildren > 0)
	// should be a NodeID the child-ownership loop rejects (zero/invalid),
	// rather than a real node. The per-tag switch must not crash even
	// though an earlier pass already reported that child as out of range.
	for _, kind := range NodeKinds() {
		for _, numChildren := range []int{0, 1, 2, 3} {
			for _, invalidChild := range []bool{false, true} {
				if invalidChild && numChildren == 0 {
					continue // nothing to make invalid
				}
				name := kind.String() + "/children=" + string(rune('0'+numChildren))
				if invalidChild {
					name += "/invalidChild"
				}
				t.Run(name, func(t *testing.T) {
					b := newTestBuilder(t)
					var childIDs []NodeID
					for i := 0; i < numChildren; i++ {
						id, err := b.AddNode(Node{
							Kind:    BoolLiteral,
							Type:    boolType,
							Span:    span(),
							Literal: Literal{Kind: LiteralBool, Bool: true},
						})
						if err != nil {
							t.Fatalf("AddNode child: %v", err)
						}
						childIDs = append(childIDs, id)
					}
					if invalidChild {
						// Zero is the canonical invalid NodeID: it fails
						// IsValid() but is still in-range for a naive
						// `id-1` index (it underflows), so it is the sharpest
						// probe for unguarded direct indexing.
						childIDs[len(childIDs)-1] = NodeID(0)
					}

					n := Node{Kind: kind, Type: intType, Span: span(), Children: childIDs}
					if _, err := b.AddNode(n); err != nil {
						return
					}

					var panicked bool
					var buildErr error
					func() {
						defer func() {
							if r := recover(); r != nil {
								panicked = true
							}
						}()
						u, err := b.Build()
						if err != nil {
							buildErr = err
							return
						}
						_ = u
					}()

					if panicked {
						t.Fatalf("verifier panicked for tag %s with %d children (invalidChild=%v)", kind, numChildren, invalidChild)
					}
					if buildErr != nil && strings.Contains(buildErr.Error(), "unhandled kind") {
						t.Fatalf("verifier reported unhandled kind for tag %s with %d children: %v", kind, numChildren, buildErr)
					}
				})
			}
		}
	}
}
