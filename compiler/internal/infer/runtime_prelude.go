package infer

import (
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
)

// recordRuntimeTypes marks the prelude module's ordinary parsed Allocator and
// Context struct declarations as "the" runtime identities. Since the
// Allocator/Context cutover both types flow through the ordinary
// declaration-preparation machinery exactly like any other .peb-declared
// struct — prepareDeclarations interns their nominal identities and resolves
// their member descriptors. This step runs after preparation and only records
// which prepared nominal identities are the Allocator and Context so the
// checker and backend keep threading the implicit context value. When the
// identities are unavailable (a graph without a prelude, or a prelude whose
// declarations failed to intern), the marker is simply left empty.
func (p *Program) recordRuntimeTypes() {
	allocator, ok := p.runtimeSymbolType(symbol.RuntimeAllocator)
	context, contextOK := p.runtimeSymbolType(symbol.RuntimeContext)
	if !ok || !contextOK {
		return
	}
	allocatorDeclaration := p.declarations[allocator]
	contextDeclaration := p.declarations[context]
	if allocatorDeclaration.State != DeclarationReady || allocatorDeclaration.Concrete == 0 ||
		contextDeclaration.State != DeclarationReady || contextDeclaration.Concrete == 0 {
		return
	}
	p.runtimeTypes = RuntimeTypes{Allocator: allocatorDeclaration.Concrete, Context: contextDeclaration.Concrete}
	p.runtimeReady = true
}

// runtimeSymbolType resolves one runtime identity to its parsed prelude
// declaration symbol.
func (p *Program) runtimeSymbolType(kind symbol.RuntimeType) (symbol.SymbolID, bool) {
	id, ok := p.inputs.Resolution.Runtime(kind)
	if !ok {
		return 0, false
	}
	value, ok := p.inputs.Resolution.Symbols.Symbol(id)
	if !ok || value.Error || value.Kind != symbol.SymbolType {
		return 0, false
	}
	return id, true
}
