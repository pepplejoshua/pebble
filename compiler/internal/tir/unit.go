package tir

import (
	"errors"
	"fmt"
	"io"
	"sort"

	"github.com/pepplejoshua/pebble/compiler/internal/source"
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

// AddRegion allocates a new lexical RegionID.
func (b *Builder) AddRegion() (RegionID, error) {
	if err := b.checkFrozen(); err != nil {
		return 0, err
	}
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
	if existing, ok := b.sourceMap[ref]; ok && existing != id {
		return errors.New("duplicate source map entry")
	}
	b.sourceMap[ref] = id
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

// Dump writes a deterministic normalized representation of the unit to w,
// bounded by MaxDumpBytes. It returns ErrDumpOverflow if the bound is reached.
func (u *Unit) Dump(w io.Writer) error {
	if u == nil {
		_, err := io.WriteString(w, "unit nil\n")
		return err
	}
	d := newDumper(w, u.config.MaxDumpBytes)
	return d.dumpUnit(u)
}

// countingWriter wraps an io.Writer and tracks bytes written against a limit.
type countingWriter struct {
	w     io.Writer
	limit uint64
	used  uint64
}

func newCountingWriter(w io.Writer, limit uint64) *countingWriter {
	return &countingWriter{w: w, limit: limit}
}

func (c *countingWriter) Write(p []byte) (int, error) {
	if c == nil || c.w == nil {
		return 0, errors.New("nil writer")
	}
	// Check before writing to avoid cutting UTF-8.
	if uint64(len(p)) > c.limit-c.used {
		return 0, ErrDumpOverflow
	}
	n, err := c.w.Write(p)
	c.used += uint64(n)
	return n, err
}

func (c *countingWriter) printf(format string, args ...any) error {
	// We could stream byte-by-byte, but for bounded dumping we preformat and
	// check the whole line/component. This keeps UTF-8 intact.
	s := fmt.Sprintf(format, args...)
	if uint64(len(s)) > c.limit-c.used {
		return ErrDumpOverflow
	}
	_, err := c.Write([]byte(s))
	return err
}

type dumper struct {
	cw *countingWriter
}

func newDumper(w io.Writer, limit uint64) *dumper {
	return &dumper{cw: newCountingWriter(w, limit)}
}

func (d *dumper) dumpUnit(u *Unit) error {
	if err := d.cw.printf("unit nodes=%d components=%d snapshot=%d regions=%d functions=%d temps=%d\n",
		len(u.nodes), componentsInUnit(u), u.snapshot.Len(), u.regionCount, len(u.functions), u.tempCount); err != nil {
		return err
	}

	for _, m := range u.modules {
		if err := d.dumpModule(m); err != nil {
			return err
		}
	}
	for _, t := range u.typeDecls {
		if err := d.cw.printf("typedecl symbol=%d node=%d members=%v span=%s\n",
			t.Symbol, t.Node, t.Members, spanString(t.Span)); err != nil {
			return err
		}
	}
	for _, f := range u.functions {
		if err := d.cw.printf("funcdecl symbol=%d fid=%d node=%d span=%s\n",
			f.Symbol, f.FunctionID, f.Node, spanString(f.Span)); err != nil {
			return err
		}
	}
	for _, g := range u.globals {
		if err := d.cw.printf("global symbol=%d type=%d node=%d span=%s\n",
			g.Symbol, g.Type, g.Node, spanString(g.Span)); err != nil {
			return err
		}
	}
	for i, n := range u.nodes {
		if err := d.dumpNode(NodeID(i+1), n, u); err != nil {
			return err
		}
	}
	refs := u.SourceRefs()
	for _, ref := range refs {
		if err := d.cw.printf("sourcemap %d:%d -> %d\n", ref.Module, ref.Node, u.sourceMap[ref]); err != nil {
			return err
		}
	}
	for i, r := range u.requirements {
		if err := d.dumpRequirement(uint32(i), r); err != nil {
			return err
		}
	}
	for i, in := range u.instantiations {
		if err := d.dumpInstantiation(uint32(i), in); err != nil {
			return err
		}
	}
	return nil
}

func (d *dumper) dumpModule(m ModuleDecl) error {
	if err := d.cw.printf("module id=%d package=%q path=%q source=%d span=%s\n",
		m.ID, string(m.Key.Package), string(m.Key.Path), m.Source, spanString(m.Span)); err != nil {
		return err
	}
	for _, imp := range m.Imports {
		if err := d.cw.printf("  import target=%d span=%s\n", imp.Target, spanString(imp.Span)); err != nil {
			return err
		}
	}
	for _, decl := range m.Declarations {
		if err := d.cw.printf("  decl symbol=%d\n", decl); err != nil {
			return err
		}
	}
	return nil
}

func (d *dumper) dumpNode(id NodeID, n Node, u *Unit) error {
	name := n.Kind.String()
	span := spanString(n.Span)
	if n.Origin != (source.Span{}) {
		span = "origin=" + spanString(n.Origin)
		if n.SyntheticRole != "" {
			span += " role=" + n.SyntheticRole
		}
	}
	if err := d.cw.printf("node %d %s type=%d span=%s syntax=%d:%d",
		id, name, n.Type, span, n.Syntax.Module, n.Syntax.Node); err != nil {
		return err
	}

	switch n.Kind {
	case Module:
		if err := d.cw.printf(" symbol=%d", n.Symbol); err != nil {
			return err
		}
	case Import:
		if err := d.cw.printf(" target=%d", n.Symbol); err != nil {
			return err
		}
	case TypeDeclaration:
		if err := d.cw.printf(" symbol=%d", n.Symbol); err != nil {
			return err
		}
	case FieldDeclaration, VariantDeclaration, ParameterDeclaration, TypeParameterDeclaration:
		if err := d.cw.printf(" symbol=%d", n.Symbol); err != nil {
			return err
		}
	case FunctionDeclaration, ExternDeclaration:
		if err := d.cw.printf(" symbol=%d function=%d convention=%d variadic=%t inline=%t hasBody=%t",
			n.Symbol, n.Function, n.Convention, n.Variadic, n.Inline, n.HasBody); err != nil {
			return err
		}
	case GlobalDeclaration, LocalDeclaration:
		if err := d.cw.printf(" symbol=%d", n.Symbol); err != nil {
			return err
		}
	case TypeUse:
		if err := d.cw.printf(" typearg=%d", n.TypeArg); err != nil {
			return err
		}
	case Block:
		if err := d.cw.printf(" region=%d", n.Region); err != nil {
			return err
		}
	case Initialize:
		if err := d.cw.printf(" symbol=%d", n.Symbol); err != nil {
			return err
		}
	case CompoundStore:
		if err := d.cw.printf(" operator=%d", n.Operator); err != nil {
			return err
		}
	case Return, ImplicitReturn:
		if err := d.cw.printf(" function=%d defer=%v", n.Function, n.DeferChain); err != nil {
			return err
		}
	case If, While, RangeLoop, For, Switch, SwitchCase:
		if err := d.cw.printf(" region=%d hasElse=%t rangeInclusive=%t case=%d",
			n.Region, n.HasElse, n.RangeInclusive, n.CaseValue); err != nil {
			return err
		}
	case Break, Continue:
		if err := d.cw.printf(" target=%d defer=%v", n.Target, n.DeferChain); err != nil {
			return err
		}
	case DeferRegister:
		if err := d.cw.printf(" region=%d", n.Region); err != nil {
			return err
		}
	case BoolLiteral:
		if err := d.cw.printf(" bool=%t", n.Literal.Bool); err != nil {
			return err
		}
	case CharLiteral:
		if err := d.cw.printf(" char=%q", n.Literal.Char); err != nil {
			return err
		}
	case StringLiteral:
		if err := d.cw.printf(" string=%q", n.Literal.String); err != nil {
			return err
		}
	case IntegerLiteral:
		if err := d.cw.printf(" int=%s/%s", n.Literal.IntegerNum, n.Literal.IntegerDen); err != nil {
			return err
		}
	case FloatLiteral:
		if err := d.cw.printf(" float=%q", n.Literal.Float); err != nil {
			return err
		}
	case RecordConstruct:
		if err := d.cw.printf(" symbol=%d fields=%d", n.Symbol, len(n.Fields)); err != nil {
			return err
		}
	case HoistedFunctionValue:
		if err := d.cw.printf(" symbol=%d function=%d", n.Symbol, n.Function); err != nil {
			return err
		}
	case SymbolValue:
		if err := d.cw.printf(" symbol=%d", n.Symbol); err != nil {
			return err
		}
	case EnumVariantValue:
		if err := d.cw.printf(" member=%d", n.Member); err != nil {
			return err
		}
	case SizeofType:
		if err := d.cw.printf(" typearg=%d", n.TypeArg); err != nil {
			return err
		}
	case PrefixValue, BinaryValue, CheckedArithmetic, CheckedShift:
		if err := d.cw.printf(" operator=%d", n.Operator); err != nil {
			return err
		}
	case ShortCircuitValue:
		if err := d.cw.printf(" operator=%d", n.Operator); err != nil {
			return err
		}
	case FieldValue, FieldPlace:
		if err := d.cw.printf(" member=%d", n.Member); err != nil {
			return err
		}
	case TupleElementValue, TuplePlace:
		if err := d.cw.printf(" ordinal=%d", n.Ordinal); err != nil {
			return err
		}
	case GenericFunctionValue:
		if err := d.cw.printf(" symbol=%d generic=%d", n.Symbol, n.GenericRef); err != nil {
			return err
		}
	case SourceAlias:
		if err := d.cw.printf(" explicitCast=%t", n.ExplicitCast); err != nil {
			return err
		}
	case StoragePlace:
		if err := d.cw.printf(" symbol=%d writable=%t", n.Symbol, n.Writable); err != nil {
			return err
		}
	case DereferencePlace, CheckedIndexPlace:
		// payload printed via children
	case TupleCoerce:
		if err := d.cw.printf(" typeargs=%v", n.TypeArgs); err != nil {
			return err
		}
	case TempBind:
		if err := d.cw.printf(" temp=%d", n.Temp); err != nil {
			return err
		}
	case TempRead:
		if err := d.cw.printf(" temp=%d", n.Temp); err != nil {
			return err
		}
	case DirectCall, IndirectCall, MethodCall, VariantConstruct:
		if err := d.cw.printf(" convention=%d context=%s symbol=%d member=%d",
			n.Convention, n.ContextAction.String(), n.Symbol, n.Member); err != nil {
			return err
		}
	}

	if len(n.Children) > 0 {
		if err := d.cw.printf(" children=%v", n.Children); err != nil {
			return err
		}
	}
	if len(n.Parameters) > 0 {
		params := make([]string, len(n.Parameters))
		for i, p := range n.Parameters {
			params[i] = fmt.Sprintf("%d:%d", p.Symbol, p.Type)
		}
		if err := d.cw.printf(" params=%v", params); err != nil {
			return err
		}
	}
	if len(n.TypeArgs) > 0 && n.Kind != TupleCoerce {
		if err := d.cw.printf(" typeargs=%v", n.TypeArgs); err != nil {
			return err
		}
	}
	if n.Writable && isPlaceKind(n.Kind) {
		if err := d.cw.printf(" writable=%t", n.Writable); err != nil {
			return err
		}
	}

	return d.cw.printf("\n")
}

func (d *dumper) dumpRequirement(i uint32, r Requirement) error {
	return d.cw.printf("requirement %d owner=%d parameter=%d kind=%s subject=%d origin=%d:%d operator=%d lit=%s num=%q den=%q\n",
		i, r.Owner, r.Parameter, r.Kind.String(), r.Subject, r.Origin.Module, r.Origin.Node,
		r.Operator, r.LiteralKind.String(), r.Numerator, r.Denominator)
}

func (d *dumper) dumpInstantiation(i uint32, in Instantiation) error {
	if err := d.cw.printf("instantiation %d site=%d:%d declaration=%d typeargs=%v reqs=%d\n",
		i, in.Site.Module, in.Site.Node, in.Declaration, in.TypeArgs, len(in.Requirements)); err != nil {
		return err
	}
	return nil
}

func spanString(s source.Span) string {
	return fmt.Sprintf("%d[%d,%d)", s.Source, s.Start, s.End)
}

func isPlaceKind(k NodeKind) bool {
	switch k {
	case StoragePlace, DereferencePlace, FieldPlace, TuplePlace, CheckedIndexPlace:
		return true
	}
	return false
}

func componentsInUnit(u *Unit) uint64 {
	if u == nil {
		return 0
	}
	var c uint64
	for _, m := range u.modules {
		c += 1 + uint64(len(m.Imports)) + uint64(len(m.Declarations))
	}
	c += uint64(len(u.typeDecls))
	for _, t := range u.typeDecls {
		c += uint64(len(t.Members))
	}
	c += uint64(len(u.functions))
	c += uint64(len(u.globals))
	c += uint64(len(u.requirements))
	c += uint64(len(u.instantiations))
	for _, in := range u.instantiations {
		c += uint64(len(in.TypeArgs)) + uint64(len(in.Requirements))
	}
	for _, n := range u.nodes {
		c += 1 +
			uint64(len(n.Children)) +
			uint64(len(n.Parameters)) +
			uint64(len(n.Fields)) +
			uint64(len(n.TypeArgs)) +
			uint64(len(n.DeferChain)) +
			uint64(len(n.Requirements))
	}
	return c
}
