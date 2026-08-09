package tir

import (
	"errors"
	"fmt"

	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

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

// RefreshSnapshot replaces the builder's owning type snapshot with a fresh view
// of the type store. Types interned after construction — for example the
// concrete substitutions created while building generic specializations — are
// not owned by the original snapshot, so the unit's owning snapshot must be
// retaken once all type interning is complete and before Build verifies it.
func (b *Builder) RefreshSnapshot(snapshot *types.Snapshot) error {
	if err := b.checkFrozen(); err != nil {
		return err
	}
	if snapshot == nil {
		return errors.New("nil type snapshot")
	}
	b.snapshot = snapshot
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
		Symbol:      t.Symbol,
		Span:        t.Span,
		Members:     append([]symbol.SymbolID(nil), t.Members...),
		MemberTypes: append([]types.TypeID(nil), t.MemberTypes...),
		Node:        t.Node,
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

// SetGlobalInitializer attaches the initializer value node to the global
// declaration identified by symbol. The checker records a `var` global's
// initializer only after the declaration container was added (the initializer
// value node cannot be built during buildDeclarations, since the expression
// index it needs is populated in a later step), so the container's Initializer
// field is filled in place through this method before the builder freezes.
func (b *Builder) SetGlobalInitializer(symbol symbol.SymbolID, node NodeID) error {
	if err := b.checkFrozen(); err != nil {
		return err
	}
	for i := range b.globals {
		if b.globals[i].Symbol == symbol {
			b.globals[i].Initializer = node
			return nil
		}
	}
	return fmt.Errorf("no global declaration recorded for symbol %d", symbol)
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
		runtime:        b.config.Runtime,
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
