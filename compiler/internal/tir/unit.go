package tir

import (
	"errors"
	"fmt"
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

// Builder constructs and verifies one Unit. It is single-owner: after Build
// returns, the builder is frozen and the Unit is immutable.
type Builder struct {
	config    Config
	snapshot  *types.Snapshot
	modules   []ModuleDecl
	typeDecls []TypeDecl
	functions []FunctionDecl
	globals   []GlobalDecl
	nodes     []Node
	sourceMap map[symbol.SyntaxRef]NodeID

	requirements   []Requirement
	instantiations []Instantiation

	components  uint64
	frozen      bool
	nextTemp    TempID
	tempCount   uint32
	regionCount uint32
}

// NewBuilder creates a builder bound to one immutable type snapshot.
func NewBuilder(snapshot *types.Snapshot, config Config) *Builder {
	return &Builder{
		config:    normalizeConfig(config),
		snapshot:  snapshot,
		sourceMap: make(map[symbol.SyntaxRef]NodeID),
	}
}

func (b *Builder) checkFrozen() error {
	if b == nil {
		return errors.New("nil builder")
	}
	if b.frozen {
		return ErrFrozen
	}
	return nil
}

func canAdd(used, add, limit uint64) bool {
	if add > limit {
		return false
	}
	return used <= limit-add
}

// AddModule appends an immutable module container. Imports and declarations
// are copied before retention.
func (b *Builder) AddModule(m ModuleDecl) error {
	if err := b.checkFrozen(); err != nil {
		return err
	}
	add := uint64(1) + uint64(len(m.Imports)) + uint64(len(m.Declarations))
	if !canAdd(b.components, add, b.config.MaxIRComponents) {
		return ErrLimitExceeded
	}
	b.components += add
	b.modules = append(b.modules, ModuleDecl{
		ID:           m.ID,
		Key:          m.Key,
		Source:       m.Source,
		Span:         m.Span,
		Imports:      append([]ImportDecl(nil), m.Imports...),
		Declarations: append([]symbol.SymbolID(nil), m.Declarations...),
	})
	return nil
}

// AddTypeDecl appends a type declaration container.
func (b *Builder) AddTypeDecl(t TypeDecl) error {
	if err := b.checkFrozen(); err != nil {
		return err
	}
	add := uint64(1) + uint64(len(t.Members))
	if !canAdd(b.components, add, b.config.MaxIRComponents) {
		return ErrLimitExceeded
	}
	b.components += add
	b.typeDecls = append(b.typeDecls, TypeDecl{
		Symbol:  t.Symbol,
		Span:    t.Span,
		Members: append([]symbol.SymbolID(nil), t.Members...),
		Node:    t.Node,
	})
	return nil
}

// AddFunctionDecl appends a function declaration container and assigns its
// monotonic FunctionID. The returned FunctionID is stable for use in nodes.
func (b *Builder) AddFunctionDecl(f FunctionDecl) (FunctionID, error) {
	if err := b.checkFrozen(); err != nil {
		return 0, err
	}
	if !canAdd(b.components, 1, b.config.MaxIRComponents) {
		return 0, ErrLimitExceeded
	}
	b.components++
	fid := FunctionID(len(b.functions) + 1)
	f.FunctionID = fid
	b.functions = append(b.functions, f)
	return fid, nil
}

// ReserveFunctionDecl allocates a real, final FunctionID for a function
// declaration before its body is built, mirroring AddRegion's identity-first
// allocation. The declaration's Node is initially zero (matching the existing
// meaning "no body" -- see AddFunctionDecl) and must be completed with
// CompleteFunctionDecl once the body's node exists. Every field of f except
// FunctionID and Node should already be set by the caller; both are
// overwritten by this call and by the later CompleteFunctionDecl.
func (b *Builder) ReserveFunctionDecl(f FunctionDecl) (FunctionID, error) {
	if err := b.checkFrozen(); err != nil {
		return 0, err
	}
	if !canAdd(b.components, 1, b.config.MaxIRComponents) {
		return 0, ErrLimitExceeded
	}
	b.components++
	fid := FunctionID(len(b.functions) + 1)
	f.FunctionID = fid
	f.Node = 0
	b.functions = append(b.functions, f)
	return fid, nil
}

// CompleteFunctionDecl attaches the built body node to a previously reserved
// function declaration. Calling it with a fid that was never reserved, or
// completing the same fid twice, is an error -- this API exists precisely
// because nothing else (the verifier included) catches a forgotten or
// duplicated completion.
func (b *Builder) CompleteFunctionDecl(fid FunctionID, node NodeID) error {
	if err := b.checkFrozen(); err != nil {
		return err
	}
	if !fid.IsValid() || uint64(fid) > uint64(len(b.functions)) {
		return fmt.Errorf("%w: FunctionID %d was never reserved", ErrInvalidNode, fid)
	}
	index := int(fid) - 1
	if b.functions[index].Node != 0 {
		return fmt.Errorf("%w: FunctionID %d was already completed", ErrInvalidNode, fid)
	}
	if !node.IsValid() || uint64(node) > uint64(len(b.nodes)) {
		return fmt.Errorf("%w: node %d is not a valid built node", ErrInvalidNode, node)
	}
	b.functions[index].Node = node
	return nil
}

// AddRegion allocates a new lexical RegionID.
func (b *Builder) AddRegion() (RegionID, error) {
	if err := b.checkFrozen(); err != nil {
		return 0, err
	}
	if b.regionCount+1 < b.regionCount {
		return 0, ErrLimitExceeded
	}
	if !canAdd(b.components, 1, b.config.MaxIRComponents) {
		return 0, ErrLimitExceeded
	}
	b.components++
	b.regionCount++
	return RegionID(b.regionCount), nil
}

// AddTemp allocates a new TempID. Temps are monotonic within the builder and are
// verified to have a single binding per function.
func (b *Builder) AddTemp() (TempID, error) {
	if err := b.checkFrozen(); err != nil {
		return 0, err
	}
	if b.nextTemp+1 < b.nextTemp {
		return 0, ErrLimitExceeded
	}
	if !canAdd(b.components, 1, b.config.MaxIRComponents) {
		return 0, ErrLimitExceeded
	}
	b.components++
	b.nextTemp++
	b.tempCount++
	return b.nextTemp, nil
}

// AddGlobalDecl appends a global declaration container.
func (b *Builder) AddGlobalDecl(g GlobalDecl) error {
	if err := b.checkFrozen(); err != nil {
		return err
	}
	if !canAdd(b.components, 1, b.config.MaxIRComponents) {
		return ErrLimitExceeded
	}
	b.components++
	b.globals = append(b.globals, g)
	return nil
}

// AddRequirement appends one normalized requirement.
func (b *Builder) AddRequirement(r Requirement) error {
	if err := b.checkFrozen(); err != nil {
		return err
	}
	if !canAdd(b.components, 1, b.config.MaxIRComponents) {
		return ErrLimitExceeded
	}
	b.components++
	b.requirements = append(b.requirements, r)
	return nil
}

// AddInstantiation appends one generic instantiation reference and returns
// its index for use in GenericFunctionValue nodes.
func (b *Builder) AddInstantiation(i Instantiation) (uint32, error) {
	if err := b.checkFrozen(); err != nil {
		return 0, err
	}
	add := uint64(1) + uint64(len(i.TypeArgs)) + uint64(len(i.Requirements))
	if !canAdd(b.components, add, b.config.MaxIRComponents) {
		return 0, ErrLimitExceeded
	}
	b.components += add
	idx := uint32(len(b.instantiations))
	b.instantiations = append(b.instantiations, Instantiation{
		Site:         i.Site,
		Declaration:  i.Declaration,
		TypeArgs:     append([]types.TypeID(nil), i.TypeArgs...),
		Requirements: append([]Requirement(nil), i.Requirements...),
	})
	return idx, nil
}

// AddNode appends one node and returns its allocated NodeID. The node is
// copied before retention; caller mutations after the call do not affect the
// builder.
func (b *Builder) AddNode(n Node) (NodeID, error) {
	if err := b.checkFrozen(); err != nil {
		return 0, err
	}
	if uint32(len(b.nodes))+1 > b.config.MaxIRNodes {
		return 0, ErrLimitExceeded
	}
	add := uint64(1) +
		uint64(len(n.Children)) +
		uint64(len(n.Parameters)) +
		uint64(len(n.Fields)) +
		uint64(len(n.Parts)) +
		uint64(len(n.TypeArgs)) +
		uint64(len(n.DeferChain)) +
		uint64(len(n.Requirements))
	if !canAdd(b.components, add, b.config.MaxIRComponents) {
		return 0, ErrLimitExceeded
	}
	id := NodeID(len(b.nodes) + 1)
	b.components += add
	b.nodes = append(b.nodes, cloneNode(n))
	return id, nil
}

// MapSource records that a surface reference maps to the given node. It is
// used for source-map completeness verification.
func (b *Builder) MapSource(ref symbol.SyntaxRef, id NodeID) error {
	if err := b.checkFrozen(); err != nil {
		return err
	}
	if !id.IsValid() {
		return ErrInvalidNode
	}
	if existing, ok := b.sourceMap[ref]; ok {
		if existing != id {
			return errors.New("duplicate source map entry")
		}
		return nil
	}
	if !canAdd(b.components, 1, b.config.MaxIRComponents) {
		return ErrLimitExceeded
	}
	b.sourceMap[ref] = id
	b.components++
	return nil
}

// Build verifies the accumulated store and, on success, publishes an immutable
// Unit. On failure it returns nil and discards no partial result from the
// builder, but the builder remains unfrozen so callers may inspect or repair.
func (b *Builder) Build() (*Unit, error) {
	if err := b.checkFrozen(); err != nil {
		return nil, err
	}
	u := &Unit{
		snapshot:       b.snapshot,
		modules:        append([]ModuleDecl(nil), b.modules...),
		typeDecls:      append([]TypeDecl(nil), b.typeDecls...),
		functions:      append([]FunctionDecl(nil), b.functions...),
		globals:        append([]GlobalDecl(nil), b.globals...),
		nodes:          append([]Node(nil), b.nodes...),
		sourceMap:      make(map[symbol.SyntaxRef]NodeID, len(b.sourceMap)),
		requirements:   append([]Requirement(nil), b.requirements...),
		instantiations: make([]Instantiation, len(b.instantiations)),
		regionCount:    b.regionCount,
		tempCount:      b.tempCount,
		config:         b.config,
	}
	for ref, id := range b.sourceMap {
		u.sourceMap[ref] = id
	}
	for i, in := range b.instantiations {
		u.instantiations[i] = Instantiation{
			Site:         in.Site,
			Declaration:  in.Declaration,
			TypeArgs:     append([]types.TypeID(nil), in.TypeArgs...),
			Requirements: append([]Requirement(nil), in.Requirements...),
		}
	}
	if err := verify(u, b.config.MaxVerifyErrors); err != nil {
		return nil, err
	}
	b.frozen = true
	return u, nil
}
