package infer

import (
	"sort"
	"sync"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/module"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

type ArrayLengthState uint8

const (
	ArrayLengthKnown ArrayLengthState = iota + 1
	ArrayLengthError
	ArrayLengthUnavailable
)

type ArrayLengthResult struct {
	State ArrayLengthState
	Value uint64
}

type ArrayLengthEvaluator interface {
	ArrayLength(symbol.SyntaxRef) ArrayLengthResult
}

type LiteralTarget struct{ WordBits uint8 }

type ProgramInputs struct {
	Graph         *module.Graph
	Sources       *source.FileSet
	Resolution    *symbol.Result
	Types         *types.Store
	ArrayLengths  ArrayLengthEvaluator
	LiteralTarget LiteralTarget
}

type DeclarationState uint8

const (
	DeclarationReady DeclarationState = iota + 1
	DeclarationError
)

type TypeDeclarationForm uint8

const (
	DeclarationNominal TypeDeclarationForm = iota + 1
	DeclarationAlias
)

type NominalKind uint8

const (
	NominalStruct NominalKind = iota + 1
	NominalUnion
	NominalTaggedUnion
	NominalEnum
	NominalExtern
)

type TemplateKind uint8

const (
	TemplateKnown TemplateKind = iota + 1
	TemplateParameter
	TemplatePointer
	TemplateArray
	TemplateSlice
	TemplateTuple
	TemplateOptional
	TemplateFunction
	TemplateNominal
)

type TypeTemplate struct {
	ID          TemplateID
	Kind        TemplateKind
	Known       types.TypeID
	Parameter   symbol.SymbolID
	Declaration symbol.SymbolID
	Length      uint64
	Convention  types.CallingConvention
	Variadic    bool
	Children    []TemplateID
}

type MemberDescriptor struct {
	Symbol symbol.SymbolID
	Type   TemplateID
}

type TypeDeclaration struct {
	Symbol     symbol.SymbolID
	State      DeclarationState
	Form       TypeDeclarationForm
	Nominal    NominalKind
	Parameters []symbol.SymbolID
	Concrete   types.TypeID
	Template   TemplateID
	Members    []MemberDescriptor
}

type Signature struct {
	Symbol     symbol.SymbolID
	State      DeclarationState
	Parameters []symbol.SymbolID
	TypeParams []symbol.SymbolID
	Inputs     []TemplateID
	Result     TemplateID
	Convention types.CallingConvention
	Variadic   bool
}

// RuntimeTypes contains the two compiler-owned nominal identities prepared
// for phase 6. It is returned by value from a frozen Program.
type RuntimeTypes struct {
	Allocator types.TypeID
	Context   types.TypeID
}

type Program struct {
	storeMu      *sync.Mutex
	identity     *programToken
	inputs       ProgramInputs
	config       Config
	valid        bool
	templates    []TypeTemplate
	declarations map[symbol.SymbolID]TypeDeclaration
	signatures   map[symbol.SymbolID]Signature
	typeParams   map[symbol.SymbolID]types.TypeID
	owners       map[symbol.SymbolID][]symbol.SymbolID
	modules      map[module.ModuleID]module.Module
	aliasState   map[symbol.SymbolID]uint8
	aliasStack   []symbol.SymbolID
	reporter     *reporter
	runtimeTypes RuntimeTypes
	runtimeReady bool
}

// 05a deliberately exposes a single-owner Store. All 05b access that may run
// from independent sessions is serialized through the prepared Program.
func (p *Program) typeKey(id types.TypeID) (types.TypeKey, bool) {
	if p.storeMu == nil {
		return p.inputs.Types.Key(id)
	}
	p.storeMu.Lock()
	defer p.storeMu.Unlock()
	return p.inputs.Types.Key(id)
}

func (p *Program) internType(key types.TypeKey) (types.TypeID, error) {
	if p.storeMu == nil {
		return p.inputs.Types.Intern(key)
	}
	p.storeMu.Lock()
	defer p.storeMu.Unlock()
	return p.inputs.Types.Intern(key)
}

func (p *Program) builtins() types.Builtins {
	if p.storeMu == nil {
		return p.inputs.Types.Builtins()
	}
	p.storeMu.Lock()
	defer p.storeMu.Unlock()
	return p.inputs.Types.Builtins()
}

func (p *Program) storeLength() uint32 {
	if p == nil || p.inputs.Types == nil {
		return 0
	}
	if p.storeMu == nil {
		return p.inputs.Types.Len()
	}
	p.storeMu.Lock()
	defer p.storeMu.Unlock()
	return p.inputs.Types.Len()
}

func (p *Program) ensureIdentity() *programToken {
	if p == nil {
		return nil
	}
	if p.storeMu == nil {
		if p.identity == nil {
			p.identity = &programToken{}
		}
		return p.identity
	}
	p.storeMu.Lock()
	defer p.storeMu.Unlock()
	if p.identity == nil {
		p.identity = &programToken{}
	}
	return p.identity
}

func (p *Program) TypeDeclaration(id symbol.SymbolID) (TypeDeclaration, bool) {
	if p == nil {
		return TypeDeclaration{}, false
	}
	v, ok := p.declarations[id]
	return cloneDeclaration(v), ok
}

func (p *Program) TypeDeclarations() []TypeDeclaration {
	if p == nil {
		return nil
	}
	ids := make([]int, 0, len(p.declarations))
	for id := range p.declarations {
		ids = append(ids, int(id))
	}
	sort.Ints(ids)
	out := make([]TypeDeclaration, 0, len(ids))
	for _, id := range ids {
		out = append(out, cloneDeclaration(p.declarations[symbol.SymbolID(id)]))
	}
	return out
}

func (p *Program) Signature(id symbol.SymbolID) (Signature, bool) {
	if p == nil {
		return Signature{}, false
	}
	v, ok := p.signatures[id]
	return cloneSignature(v), ok
}

func (p *Program) Signatures() []Signature {
	if p == nil {
		return nil
	}
	ids := make([]int, 0, len(p.signatures))
	for id := range p.signatures {
		ids = append(ids, int(id))
	}
	sort.Ints(ids)
	out := make([]Signature, 0, len(ids))
	for _, id := range ids {
		out = append(out, cloneSignature(p.signatures[symbol.SymbolID(id)]))
	}
	return out
}

func (p *Program) Template(id TemplateID) (TypeTemplate, bool) {
	if p == nil || id == 0 || uint64(id) > uint64(len(p.templates)) {
		return TypeTemplate{}, false
	}
	v := p.templates[id-1]
	v.Children = append([]TemplateID(nil), v.Children...)
	return v, true
}

func (p *Program) RuntimeTypes() (RuntimeTypes, bool) {
	if p == nil || !p.runtimeReady {
		return RuntimeTypes{}, false
	}
	return p.runtimeTypes, true
}

func (p *Program) TypeParameter(id symbol.SymbolID) (types.TypeID, bool) {
	if p == nil || id == 0 {
		return 0, false
	}
	v, ok := p.typeParams[id]
	return v, ok
}

func cloneDeclaration(v TypeDeclaration) TypeDeclaration {
	v.Parameters = append([]symbol.SymbolID(nil), v.Parameters...)
	v.Members = append([]MemberDescriptor(nil), v.Members...)
	return v
}
func cloneSignature(v Signature) Signature {
	v.Parameters = append([]symbol.SymbolID(nil), v.Parameters...)
	v.TypeParams = append([]symbol.SymbolID(nil), v.TypeParams...)
	v.Inputs = append([]TemplateID(nil), v.Inputs...)
	return v
}

func (p *Program) addTemplate(v TypeTemplate) TemplateID {
	v.ID = TemplateID(len(p.templates) + 1)
	v.Children = append([]TemplateID(nil), v.Children...)
	p.templates = append(p.templates, v)
	return v.ID
}

func Prepare(inputs ProgramInputs, diagnostics *diagnostic.DiagnosticSet, config Config) *Program {
	config = normalizeConfig(config)
	p := &Program{
		storeMu: &sync.Mutex{}, identity: &programToken{}, inputs: inputs, config: config, valid: true,
		declarations: make(map[symbol.SymbolID]TypeDeclaration),
		signatures:   make(map[symbol.SymbolID]Signature),
		typeParams:   make(map[symbol.SymbolID]types.TypeID),
		owners:       make(map[symbol.SymbolID][]symbol.SymbolID),
		modules:      make(map[module.ModuleID]module.Module),
		aliasState:   make(map[symbol.SymbolID]uint8),
		reporter:     newReporter(diagnostics, config.MaxDiagnostics),
	}
	if inputs.Graph == nil || inputs.Sources == nil || inputs.Resolution == nil || inputs.Resolution.Symbols == nil || inputs.Types == nil {
		p.valid = false
		p.reporter.error(CodeResourceLimit, "inference preparation requires graph, sources, resolution, and type store", Origin{})
		p.reporter.flush()
		return p
	}
	if inputs.LiteralTarget.WordBits != 32 && inputs.LiteralTarget.WordBits != 64 {
		p.valid = false
		p.reporter.error(CodeResourceLimit, "literal target word width must be 32 or 64", Origin{})
	}
	for _, m := range inputs.Graph.Modules() {
		p.modules[m.ID] = m
	}
	p.prepareRuntimePrelude()
	p.prepareDeclarations()
	p.prepareSignatures()
	p.reporter.flush()
	return p
}
