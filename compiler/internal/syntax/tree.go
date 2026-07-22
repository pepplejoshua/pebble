package syntax

import (
	"fmt"
	"io"
	"strings"

	"github.com/pepplejoshua/pebble/compiler/internal/source"
)

// NodeID identifies a node owned by one Tree. Zero is always invalid.
type NodeID uint32

// NodeKind identifies a surface-tree node without assigning semantic meaning.
type NodeKind uint8

const (
	Missing NodeKind = iota + 1
	Error
	Name
	Path
	Literal
	InterpolatedString
	ContextExpr
	SomeExpr
	SizeofExpr
	PrefixTerm
	PostfixExpr
	BinaryExpr
	CastExpr
	CallExpr
	BracketApply
	SliceExpr
	MemberExpr
	GroupedTerm
	TupleTerm
	ArrayExpr
	ArrayRepeatExpr
	PartialMemberExpr
	OptionalType
	SliceType
	ArrayType
	FunctionTerm
	StructType
	UnionType
	EnumType
	FieldDecl
	VariantDecl
)

var nodeKindNames = [...]string{
	Missing:            "Missing",
	Error:              "Error",
	Name:               "Name",
	Path:               "Path",
	Literal:            "Literal",
	InterpolatedString: "InterpolatedString",
	ContextExpr:        "ContextExpr",
	SomeExpr:           "SomeExpr",
	SizeofExpr:         "SizeofExpr",
	PrefixTerm:         "PrefixTerm",
	PostfixExpr:        "PostfixExpr",
	BinaryExpr:         "BinaryExpr",
	CastExpr:           "CastExpr",
	CallExpr:           "CallExpr",
	BracketApply:       "BracketApply",
	SliceExpr:          "SliceExpr",
	MemberExpr:         "MemberExpr",
	GroupedTerm:        "GroupedTerm",
	TupleTerm:          "TupleTerm",
	ArrayExpr:          "ArrayExpr",
	ArrayRepeatExpr:    "ArrayRepeatExpr",
	PartialMemberExpr:  "PartialMemberExpr",
	OptionalType:       "OptionalType",
	SliceType:          "SliceType",
	ArrayType:          "ArrayType",
	FunctionTerm:       "FunctionTerm",
	StructType:         "StructType",
	UnionType:          "UnionType",
	EnumType:           "EnumType",
	FieldDecl:          "FieldDecl",
	VariantDecl:        "VariantDecl",
}

func (k NodeKind) String() string {
	if int(k) >= len(nodeKindNames) || nodeKindNames[k] == "" {
		return "Unknown"
	}
	return nodeKindNames[k]
}

type node struct {
	kind     NodeKind
	span     source.Span
	token    TokenKind
	data     uint32
	expected string
	children []NodeID
}

// Node is an immutable copy of one surface-tree node.
type Node struct{ value node }

func (n Node) Kind() NodeKind    { return n.value.kind }
func (n Node) Span() source.Span { return n.value.span }
func (n Node) Token() TokenKind  { return n.value.token }

// Data returns compact kind-specific flags. SliceExpr uses the low two bits
// for the presence of its start and end expressions.
func (n Node) Data() uint32       { return n.value.data }
func (n Node) Expected() string   { return n.value.expected }
func (n Node) Children() []NodeID { return append([]NodeID(nil), n.value.children...) }

// Tree owns all nodes for one parsed source fragment.
type Tree struct {
	source source.ID
	root   NodeID
	nodes  []node
}

func newTree(id source.ID) *Tree { return &Tree{source: id} }

// Root returns the fragment root.
func (t *Tree) Root() NodeID { return t.root }

// Node returns an immutable node value.
func (t *Tree) Node(id NodeID) (Node, bool) {
	if id == 0 || uint64(id) > uint64(len(t.nodes)) {
		return Node{}, false
	}
	value := t.nodes[id-1]
	value.children = append([]NodeID(nil), value.children...)
	return Node{value: value}, true
}

func (t *Tree) add(kind NodeKind, span source.Span, token TokenKind, expected string, children ...NodeID) NodeID {
	return t.addData(kind, span, token, 0, expected, children...)
}

func (t *Tree) addData(kind NodeKind, span source.Span, token TokenKind, data uint32, expected string, children ...NodeID) NodeID {
	t.nodes = append(t.nodes, node{
		kind: kind, span: span, token: token, data: data, expected: expected,
		children: append([]NodeID(nil), children...),
	})
	return NodeID(len(t.nodes))
}

// Dump writes a stable, source-text-independent representation of the tree.
func (t *Tree) Dump(w io.Writer) error {
	if t.root == 0 {
		return nil
	}
	return t.dumpNode(w, t.root, 0)
}

func (t *Tree) dumpNode(w io.Writer, id NodeID, depth int) error {
	n, ok := t.Node(id)
	if !ok {
		return fmt.Errorf("invalid node ID %d", id)
	}
	indent := strings.Repeat("  ", depth)
	if _, err := fmt.Fprintf(w, "%s%s [%d,%d)", indent, n.Kind(), n.Span().Start, n.Span().End); err != nil {
		return err
	}
	if n.Token() != EOF {
		if _, err := fmt.Fprintf(w, " token=%s", n.Token()); err != nil {
			return err
		}
	}
	if n.Data() != 0 {
		if _, err := fmt.Fprintf(w, " data=%d", n.Data()); err != nil {
			return err
		}
	}
	if n.Expected() != "" {
		if _, err := fmt.Fprintf(w, " expected=%q", n.Expected()); err != nil {
			return err
		}
	}
	if _, err := io.WriteString(w, "\n"); err != nil {
		return err
	}
	for _, child := range n.Children() {
		if err := t.dumpNode(w, child, depth+1); err != nil {
			return err
		}
	}
	return nil
}

// DumpString returns the deterministic tree dump.
func (t *Tree) DumpString() string {
	var b strings.Builder
	_ = t.Dump(&b)
	return b.String()
}
