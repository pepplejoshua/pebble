package check

import (
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

type callableRef struct {
	Symbol symbol.SymbolID
	Syntax symbol.SyntaxRef
}

type contextFlowKind uint8

const (
	contextExpression contextFlowKind = iota + 1
	contextForward
	contextNone
	contextIndirect
)

type contextFlowRecord struct {
	Header  recordHeader
	Kind    contextFlowKind
	Caller  callableRef
	Callee  valueID
	Context types.TypeID
}

func (w *walker) handleContext(ref symbol.SyntaxRef, node syntax.Node, ctx walkContext) {
	var runtime infer.RuntimeTypes
	ok := false
	if w.runtimeTypes != nil {
		runtime, ok = w.runtimeTypes()
	}
	origin := w.origin(ref, node, "runtime context", ctx.typeOwner, ctx.genericOwner)
	term := w.session.Error(origin)
	suppressed := !ok || runtime.Context == 0
	if !suppressed {
		term = w.session.Known(runtime.Context)
	}
	value, published := w.publishSyntax(ref, term, origin)
	if !published {
		suppressed = true
	}
	header := w.header(ref, ctx.genericOwner, suppressed)
	record := contextFlowRecord{
		Header: header, Kind: contextExpression,
		Caller: ctx.callable, Context: runtime.Context,
	}
	_ = value
	w.generation.addRecord(retainedRecord{Header: header, ContextFlow: &record})
}
