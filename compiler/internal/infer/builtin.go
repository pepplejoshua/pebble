package infer

import (
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

// prepareBuiltinSignatures registers the fixed signatures of the
// compiler-owned builtin functions the resolver places in the prelude. Unlike
// authored callables these have no syntax declaration to derive a signature
// from, so their signatures are registered here directly: the wrapping u64
// arithmetic builtins each take two u64 inputs and return u64, carrying the
// Pebble calling convention like any other language-level callable. The
// backend lowers a call to one of these symbols to the corresponding
// runtime helper rather than a pebble_fn_<symbolID> helper, which is why they
// never need a declaration in the typed-IR unit.
func (p *Program) prepareBuiltinSignatures() {
	builtins := p.builtins()
	u64 := p.knownTemplate(builtins.U64)
	for _, function := range []symbol.BuiltinFunction{
		symbol.BuiltinWrappingMulU64,
		symbol.BuiltinWrappingAddU64,
	} {
		id, ok := p.inputs.Resolution.BuiltinFunction(function)
		if !ok {
			p.valid = false
			p.reporter.error(CodeResourceLimit, "builtin function identity is unavailable", Origin{Role: "builtin prelude"})
			continue
		}
		p.signatures[id] = Signature{
			Symbol:     id,
			State:      DeclarationReady,
			Inputs:     []TemplateID{u64, u64},
			Result:     u64,
			Convention: types.Pebble,
		}
		// A builtin signature has no type parameters, so its owner-table entry
		// is the empty list; recording it keeps the semantic snapshot's owner
		// table consistent with the signature table.
		p.owners[id] = []symbol.SymbolID(nil)
	}
}
