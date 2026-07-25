package infer

import (
	"fmt"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
)

const (
	CodeInvalidType        diagnostic.Code = "T0501"
	CodeAliasCycle         diagnostic.Code = "T0502"
	CodeAnonymousAggregate diagnostic.Code = "T0503"
	CodeEmptyTuple         diagnostic.Code = "T0504"
	CodeUnification        diagnostic.Code = "T0505"
	CodeOccursCheck        diagnostic.Code = "T0506"
	CodeCapability         diagnostic.Code = "T0507"
	CodeLiteral            diagnostic.Code = "T0508"
	CodeAmbiguous          diagnostic.Code = "T0509"
	CodeUnresolved         diagnostic.Code = "T0510"
	CodeDamagedInput       diagnostic.Code = "T0511"
	CodeResourceLimit      diagnostic.Code = "T0512"
)

type reporter struct {
	set      *diagnostic.DiagnosticSet
	max      uint32
	items    []diagnostic.Diagnostic
	overflow bool
	emitted  uint32
	onFatal  func()
	isFatal  func() bool
}

func newReporter(set *diagnostic.DiagnosticSet, max uint32) *reporter {
	if set == nil {
		set = diagnostic.NewDiagnosticSet()
	}
	return &reporter{set: set, max: max}
}

func (r *reporter) add(item diagnostic.Diagnostic) {
	if r == nil {
		return
	}
	if r.emitted+uint32(len(r.items)) < r.max {
		r.items = append(r.items, item)
		return
	}
	r.overflow = true
	r.markFatal()
	r.materializeOverflow()
}

func (r *reporter) error(code diagnostic.Code, message string, origin Origin, related ...Origin) {
	if r == nil {
		return
	}
	if code == CodeResourceLimit {
		r.markFatal()
	}
	item := diagnostic.Diagnostic{
		Severity: diagnostic.Error,
		Code:     code,
		Message:  message,
		Primary:  diagnostic.Label{Span: origin.Span, Message: origin.Role},
	}
	for _, value := range related {
		item.Related = append(item.Related, diagnostic.Label{Span: value.Span, Message: value.Role})
	}
	r.add(item)
}

func (r *reporter) markFatal() {
	if r != nil && r.onFatal != nil {
		r.onFatal()
	}
}

func (r *reporter) sessionFatal() bool {
	return r != nil && r.isFatal != nil && r.isFatal()
}

func (r *reporter) materializeOverflow() {
	if r == nil || !r.overflow || len(r.items) == 0 {
		return
	}
	last := len(r.items) - 1
	r.items[last] = diagnostic.Diagnostic{
		Severity: diagnostic.Error,
		Code:     CodeResourceLimit,
		Message:  fmt.Sprintf("inference diagnostic limit of %d reached", r.max),
		Primary:  r.items[last].Primary,
	}
}

func (r *reporter) flush() {
	if r == nil {
		return
	}
	r.materializeOverflow()
	for _, item := range r.items {
		r.set.Add(item)
	}
	r.emitted += uint32(len(r.items))
	r.items = nil
	r.overflow = false
}

func zeroOrigin(span source.Span) Origin { return Origin{Span: span} }
