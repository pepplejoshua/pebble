package module

import (
	"fmt"
	"strings"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
)

func (b *builder) validateCyclesAndOrder() {
	const (
		unvisited uint8 = iota
		visiting
		visited
	)
	states := make([]uint8, len(b.graph.modules)+1)
	stackModules := make([]ModuleID, 0, len(b.graph.modules))
	stackEdges := make([]ImportEdge, 0, len(b.graph.modules))
	order := make([]ModuleID, 0, len(b.graph.modules))

	var visit func(ModuleID)
	visit = func(id ModuleID) {
		states[id] = visiting
		stackModules = append(stackModules, id)
		for _, edge := range b.graph.modules[id-1].Imports {
			switch states[edge.Target] {
			case unvisited:
				stackEdges = append(stackEdges, edge)
				visit(edge.Target)
				stackEdges = stackEdges[:len(stackEdges)-1]
			case visiting:
				start := 0
				for i, stacked := range stackModules {
					if stacked == edge.Target {
						start = i
						break
					}
				}
				chain := append([]ImportEdge(nil), stackEdges[start:]...)
				chain = append(chain, edge)
				b.reportCycle(chain)
			}
		}
		stackModules = stackModules[:len(stackModules)-1]
		states[id] = visited
		order = append(order, id)
	}

	for id := ModuleID(1); int(id) <= len(b.graph.modules); id++ {
		if states[id] == unvisited {
			visit(id)
		}
	}
	b.graph.dependency = order
}

// buildReverseIndex inverts every authored import edge so each target knows
// which modules import it directly. The result is cached on the graph in
// module ID order and never mutated, matching the graph's immutability.
func (b *builder) buildReverseIndex() {
	b.graph.reverse = make(map[ModuleID][]ModuleID, len(b.graph.modules))
	for id := ModuleID(1); int(id) <= len(b.graph.modules); id++ {
		for _, edge := range b.graph.modules[id-1].Imports {
			b.graph.reverse[edge.Target] = append(b.graph.reverse[edge.Target], id)
		}
	}
}

func (b *builder) reportCycle(chain []ImportEdge) {
	if len(chain) == 0 || b.moduleErrors >= b.maxDiagnostics {
		return
	}
	related := make([]diagnostic.Label, 0, len(chain)-1)
	names := make([]string, 0, len(chain)+1)
	for i, edge := range chain {
		if i == 0 {
			names = append(names, edge.Qualifier)
		}
		names = append(names, edge.Spelling)
		if i < len(chain)-1 {
			related = append(related, diagnostic.Label{Span: edge.Span, Message: fmt.Sprintf("imports %q", edge.Spelling)})
		}
	}
	closing := chain[len(chain)-1]
	b.diagnostics.Add(diagnostic.Diagnostic{
		Severity: diagnostic.Error,
		Code:     CodeModuleCycle,
		Message:  "module import cycle",
		Primary:  diagnostic.Label{Span: closing.Span, Message: "cycle closes here"},
		Related:  related,
		Notes:    []string{"import chain: " + strings.Join(names, " -> ")},
	})
	b.moduleErrors++
}
