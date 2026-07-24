package infer

import (
	"fmt"

	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

func (p *Program) prepareRuntimePrelude() {
	allocatorSymbol, allocatorOK := p.runtimeSymbol(symbol.RuntimeAllocator)
	contextSymbol, contextOK := p.runtimeSymbol(symbol.RuntimeContext)
	if !allocatorOK || !contextOK {
		p.runtimeFailure("runtime prelude type identities are unavailable")
		return
	}
	allocatorMembers, ok := p.runtimeMembers(allocatorSymbol, []string{"ptr", "alloc", "realloc", "free"})
	if !ok {
		p.runtimeFailure("Allocator runtime member identities are damaged")
		return
	}
	contextMembers, ok := p.runtimeMembers(contextSymbol, []string{"default_allocator"})
	if !ok {
		p.runtimeFailure("Context runtime member identities are damaged")
		return
	}

	allocator, err := p.inputs.Types.Intern(types.NominalKey(allocatorSymbol, nil))
	if err != nil {
		p.runtimeFailure(fmt.Sprintf("cannot intern Allocator runtime type: %v", err))
		return
	}
	context, err := p.inputs.Types.Intern(types.NominalKey(contextSymbol, nil))
	if err != nil {
		p.runtimeFailure(fmt.Sprintf("cannot intern Context runtime type: %v", err))
		return
	}
	builtins := p.inputs.Types.Builtins()
	voidPointer, err := p.inputs.Types.Intern(types.PointerKey(builtins.Void))
	if err != nil {
		p.runtimeFailure(fmt.Sprintf("cannot intern runtime void pointer: %v", err))
		return
	}
	alloc, err := p.inputs.Types.Intern(types.FunctionKey(types.Pebble, []types.TypeID{voidPointer, builtins.Uint}, voidPointer, false))
	if err != nil {
		p.runtimeFailure(fmt.Sprintf("cannot intern Allocator.alloc type: %v", err))
		return
	}
	realloc, err := p.inputs.Types.Intern(types.FunctionKey(types.Pebble, []types.TypeID{voidPointer, voidPointer, builtins.Uint}, voidPointer, false))
	if err != nil {
		p.runtimeFailure(fmt.Sprintf("cannot intern Allocator.realloc type: %v", err))
		return
	}
	free, err := p.inputs.Types.Intern(types.FunctionKey(types.Pebble, []types.TypeID{voidPointer, voidPointer}, builtins.Void, false))
	if err != nil {
		p.runtimeFailure(fmt.Sprintf("cannot intern Allocator.free type: %v", err))
		return
	}

	allocatorTemplate := p.knownTemplate(allocator)
	contextTemplate := p.knownTemplate(context)
	p.declarations[allocatorSymbol] = TypeDeclaration{
		Symbol: allocatorSymbol, State: DeclarationReady, Form: DeclarationNominal,
		Nominal: NominalStruct, Concrete: allocator, Template: allocatorTemplate,
		Members: p.runtimeMemberDescriptors(allocatorMembers, []types.TypeID{voidPointer, alloc, realloc, free}),
	}
	p.declarations[contextSymbol] = TypeDeclaration{
		Symbol: contextSymbol, State: DeclarationReady, Form: DeclarationNominal,
		Nominal: NominalStruct, Concrete: context, Template: contextTemplate,
		Members: p.runtimeMemberDescriptors(contextMembers, []types.TypeID{allocator}),
	}
	p.runtimeTypes = RuntimeTypes{Allocator: allocator, Context: context}
	p.runtimeReady = true
}

func (p *Program) runtimeSymbol(kind symbol.RuntimeType) (symbol.SymbolID, bool) {
	id, ok := p.inputs.Resolution.Runtime(kind)
	if !ok {
		return 0, false
	}
	value, ok := p.inputs.Resolution.Symbols.Symbol(id)
	if !ok || value.Error || value.Kind != symbol.SymbolRuntimeType || value.Runtime != kind || value.Declaration != (symbol.SyntaxRef{}) {
		return 0, false
	}
	if kind == symbol.RuntimeAllocator && value.Name != "Allocator" {
		return 0, false
	}
	if kind == symbol.RuntimeContext && value.Name != "" {
		return 0, false
	}
	return id, true
}

func (p *Program) runtimeMembers(owner symbol.SymbolID, names []string) ([]symbol.SymbolID, bool) {
	ids := p.inputs.Resolution.Members(owner)
	if len(ids) != len(names) {
		return nil, false
	}
	for index, id := range ids {
		value, ok := p.inputs.Resolution.Symbols.Symbol(id)
		if !ok || value.Error || value.Kind != symbol.SymbolField || value.Name != names[index] || value.Containing != owner || value.Declaration != (symbol.SyntaxRef{}) {
			return nil, false
		}
	}
	return ids, true
}

func (p *Program) runtimeMemberDescriptors(symbols []symbol.SymbolID, typeIDs []types.TypeID) []MemberDescriptor {
	result := make([]MemberDescriptor, len(symbols))
	for index := range symbols {
		result[index] = MemberDescriptor{Symbol: symbols[index], Type: p.knownTemplate(typeIDs[index])}
	}
	return result
}

func (p *Program) runtimeFailure(message string) {
	p.valid = false
	p.runtimeReady = false
	p.runtimeTypes = RuntimeTypes{}
	p.reporter.error(CodeResourceLimit, message, Origin{Role: "runtime prelude"})
}
