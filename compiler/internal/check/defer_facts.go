package check

import (
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
)

// deferOrdinal is the zero-based registration order of the next defer in one
// lexical region. Defers are registered on entry in authored order, so a
// per-region counter is exactly record allocation order without rescanning the
// arena.
// It advances only when the record is actually retained, so a refused append
// leaves no gap in the region's registration order.
func (w *walker) deferOrdinal(region controlID) uint32 {
	if w.deferOrdinals == nil {
		return 0
	}
	return w.deferOrdinals[region]
}

// prepareDefer registers one deferred statement with its current lexical region
// and then visits that statement exactly once, here at registration. It is never
// revisited on an exit edge; defer edge expansion belongs to 06b, and a deferred
// return, break, continue, or nested defer is 06b's C0613.
func (w *walker) prepareDefer(ref symbol.SyntaxRef, node syntax.Node, ctx walkContext, tree *syntax.Tree, items []walkItem) []walkItem {
	region := ctx.control.region
	if region == 0 || w.session == nil || w.session.Fatal() {
		return items
	}
	statement := symbol.SyntaxRef{}
	if semantic := semanticRefs(ref.Module, node, tree); len(semantic) != 0 {
		statement = semantic[0]
	}
	if statement == (symbol.SyntaxRef{}) {
		return items
	}
	w.retainControl(ref, ctx, controlEmission{
		kind: controlDefer, form: statementOther, region: region,
	})
	header := w.header(ref, ctx.genericOwner, false)
	record := deferRecord{
		Header: header, Region: region, Ordinal: w.deferOrdinal(region), Statement: statement,
	}
	if _, retained := w.addRecord(retainedRecord{Header: header, Controls: []controlID{region}, Defer: &record}); retained && w.deferOrdinals != nil {
		w.deferOrdinals[region]++
	}
	return items
}
