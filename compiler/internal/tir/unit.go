package tir

import (
	"errors"
	"sort"

	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

// Default limits for typed-IR construction and dumping. Zero values select
// these defaults.
const (
	DefaultMaxIRNodes      uint32 = 1 << 22
	DefaultMaxIRComponents uint64 = 1 << 24
	DefaultMaxDumpBytes    uint64 = 1 << 28
	DefaultMaxVerifyErrors uint32 = 100
)

// Config bounds one typed-IR construction. Zero values select defaults.
type Config struct {
	MaxIRNodes      uint32
	MaxIRComponents uint64
	MaxDumpBytes    uint64
	MaxVerifyErrors uint32
}

func normalizeConfig(c Config) Config {
	if c.MaxIRNodes == 0 {
		c.MaxIRNodes = DefaultMaxIRNodes
	}
	if c.MaxIRComponents == 0 {
		c.MaxIRComponents = DefaultMaxIRComponents
	}
	if c.MaxDumpBytes == 0 {
		c.MaxDumpBytes = DefaultMaxDumpBytes
	}
	if c.MaxVerifyErrors == 0 {
		c.MaxVerifyErrors = DefaultMaxVerifyErrors
	}
	return c
}

// Common construction/dump errors.
var (
	ErrLimitExceeded = errors.New("typed-IR limit exceeded")
	ErrDumpOverflow  = errors.New("typed-IR dump exceeds MaxDumpBytes")
	ErrFrozen        = errors.New("builder already frozen")
	ErrInvalidNode   = errors.New("invalid node")
)

// Unit is the immutable, verified typed-IR store. It owns no mutable AST,
// solver term, InferID, layout, backend name, or specialization body.
type Unit struct {
	snapshot       *types.Snapshot
	modules        []ModuleDecl
	typeDecls      []TypeDecl
	functions      []FunctionDecl
	globals        []GlobalDecl
	nodes          []Node
	sourceMap      map[symbol.SyntaxRef]NodeID
	requirements   []Requirement
	instantiations []Instantiation
	regionCount    uint32
	tempCount      uint32
	config         Config
}

// RegionCount returns the number of allocated regions.
func (u *Unit) RegionCount() uint32 {
	if u == nil {
		return 0
	}
	return u.regionCount
}

// FunctionCount returns the number of allocated functions.
func (u *Unit) FunctionCount() uint32 {
	if u == nil {
		return 0
	}
	return uint32(len(u.functions))
}

// TempCount returns the number of allocated temporaries.
func (u *Unit) TempCount() uint32 {
	if u == nil {
		return 0
	}
	return u.tempCount
}

// Snapshot returns the immutable type snapshot that owns every TypeID in the
// unit. The snapshot is read-only after publication.
func (u *Unit) Snapshot() *types.Snapshot {
	if u == nil {
		return nil
	}
	return u.snapshot
}

// Modules returns a defensive deep copy in dependency/module order.
func (u *Unit) Modules() []ModuleDecl {
	if u == nil {
		return nil
	}
	out := make([]ModuleDecl, len(u.modules))
	for i, m := range u.modules {
		out[i] = ModuleDecl{
			ID:           m.ID,
			Key:          m.Key,
			Source:       m.Source,
			Span:         m.Span,
			Imports:      append([]ImportDecl(nil), m.Imports...),
			Declarations: append([]symbol.SymbolID(nil), m.Declarations...),
		}
	}
	return out
}

// TypeDeclarations returns a defensive deep copy in source order.
func (u *Unit) TypeDeclarations() []TypeDecl {
	if u == nil {
		return nil
	}
	out := make([]TypeDecl, len(u.typeDecls))
	for i, d := range u.typeDecls {
		out[i] = TypeDecl{
			Symbol:  d.Symbol,
			Span:    d.Span,
			Members: append([]symbol.SymbolID(nil), d.Members...),
			Node:    d.Node,
		}
	}
	return out
}

// FunctionDeclarations returns a defensive deep copy in source order.
func (u *Unit) FunctionDeclarations() []FunctionDecl {
	if u == nil {
		return nil
	}
	out := make([]FunctionDecl, len(u.functions))
	copy(out, u.functions)
	return out
}

// GlobalDeclarations returns a defensive deep copy in source order.
func (u *Unit) GlobalDeclarations() []GlobalDecl {
	if u == nil {
		return nil
	}
	out := make([]GlobalDecl, len(u.globals))
	copy(out, u.globals)
	return out
}

// Requirements returns a defensive deep copy of normalized requirements.
func (u *Unit) Requirements() []Requirement {
	if u == nil {
		return nil
	}
	out := make([]Requirement, len(u.requirements))
	copy(out, u.requirements)
	return out
}

// Instantiations returns a defensive deep copy; nested TypeArgs and
// Requirements are also copied.
func (u *Unit) Instantiations() []Instantiation {
	if u == nil {
		return nil
	}
	out := make([]Instantiation, len(u.instantiations))
	for i, in := range u.instantiations {
		out[i] = Instantiation{
			Site:         in.Site,
			Declaration:  in.Declaration,
			TypeArgs:     append([]types.TypeID(nil), in.TypeArgs...),
			Requirements: append([]Requirement(nil), in.Requirements...),
		}
	}
	return out
}

// Node returns a defensive copy of one node, or false for an invalid ID.
func (u *Unit) Node(id NodeID) (Node, bool) {
	if u == nil || !id.IsValid() || uint64(id) > uint64(len(u.nodes)) {
		return Node{}, false
	}
	return cloneNode(u.nodes[id-1]), true
}

// Nodes returns the complete ordered node store as defensive copies.
func (u *Unit) Nodes() []Node {
	if u == nil {
		return nil
	}
	out := make([]Node, len(u.nodes))
	for i, n := range u.nodes {
		out[i] = cloneNode(n)
	}
	return out
}

// SourceMap looks up the runtime node for a surface reference.
func (u *Unit) SourceMap(ref symbol.SyntaxRef) (NodeID, bool) {
	if u == nil || u.sourceMap == nil {
		return 0, false
	}
	id, ok := u.sourceMap[ref]
	return id, ok
}

// SourceRefs returns all mapped surface references in deterministic order.
func (u *Unit) SourceRefs() []symbol.SyntaxRef {
	if u == nil || len(u.sourceMap) == 0 {
		return nil
	}
	refs := make([]symbol.SyntaxRef, 0, len(u.sourceMap))
	for ref := range u.sourceMap {
		refs = append(refs, ref)
	}
	sort.Slice(refs, func(i, j int) bool {
		if refs[i].Module != refs[j].Module {
			return refs[i].Module < refs[j].Module
		}
		return refs[i].Node < refs[j].Node
	})
	return refs
}

// NodeCount returns the number of nodes in the store.
func (u *Unit) NodeCount() int {
	if u == nil {
		return 0
	}
	return len(u.nodes)
}

func cloneNode(n Node) Node {
	n.Children = append([]NodeID(nil), n.Children...)
	n.Parameters = append([]Parameter(nil), n.Parameters...)
	n.Fields = append([]FieldInit(nil), n.Fields...)
	n.Parts = append([]InterpolationPart(nil), n.Parts...)
	n.TypeArgs = append([]types.TypeID(nil), n.TypeArgs...)
	n.DeferChain = append([]NodeID(nil), n.DeferChain...)
	n.Requirements = append([]Requirement(nil), n.Requirements...)
	return n
}
