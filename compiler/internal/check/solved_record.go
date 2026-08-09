package check

import (
	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
)

// solvedRecords is an immutable, already-resolved index used by 06b.2 onward.
// It maps every active value root to its solved infer.TypeResult and every
// frozen constant to its constantResult. Inactive guarded roots are excluded;
// failed resolution states such as TypeError or constantError are stored as-is.
type solvedRecords struct {
	roots     map[valueID]infer.TypeResult
	constants map[symbol.SyntaxRef]constantResult
}

// Root returns the resolved infer.TypeResult for an active root value, if any.
func (s *solvedRecords) Root(value valueID) (infer.TypeResult, bool) {
	if s == nil {
		return infer.TypeResult{}, false
	}
	result, ok := s.roots[value]
	return result, ok
}

// Constant returns the resolved constantResult for a syntax site, if any.
func (s *solvedRecords) Constant(ref symbol.SyntaxRef) (constantResult, bool) {
	if s == nil {
		return constantResult{}, false
	}
	result, ok := s.constants[ref]
	return result.clone(), ok
}

// resolvedRootType resolves a value's solved type directly through the handoff,
// mirroring resolveRecords' guarded-root handling. It recovers information that
// walk-time facts could not pin down because the involved types were still
// solver variables then (for example, a generic union receiver's nominal
// declaration, which only exists once solving materializes the template).
func resolvedRootType(handoff *solveHandoff, value valueID) (infer.TypeResult, bool) {
	if handoff == nil || handoff.Solution == nil {
		return infer.TypeResult{}, false
	}
	root, ok := handoff.Roots.Root(value)
	if !ok {
		return infer.TypeResult{}, false
	}
	if root.Alternative.Guarded {
		selected, ok := handoff.Solution.Selection(root.Alternative.Choice)
		if !ok || selected != root.Alternative.Index {
			return infer.TypeResult{}, false
		}
	}
	switch root.Kind {
	case rootSyntax:
		return handoff.Solution.SyntaxType(root.Syntax)
	case rootSymbol:
		return handoff.Solution.SymbolType(root.Symbol)
	case rootSlot:
		return handoff.Solution.Slot(root.Slot)
	case rootInstantiation:
		inst, ok := handoff.Solution.Instantiation(root.Syntax)
		if !ok || root.Parameter >= uint32(len(inst.Arguments)) {
			return infer.TypeResult{}, false
		}
		return inst.Arguments[root.Parameter], true
	case rootMethod:
		method, ok := handoff.Solution.Method(root.Syntax)
		if !ok || root.Parameter >= uint32(len(method.Arguments)) {
			return infer.TypeResult{}, false
		}
		return method.Arguments[root.Parameter], true
	}
	return infer.TypeResult{}, false
}

// resolveRecords consumes handoff.Roots and handoff.Constants exactly once each,
// audits them, resolves active roots through handoff.Solution, and builds a local
// lookup arena.
//
// Preconditions: auditHandoff has already succeeded for the handoff, so
// handoff.Semantics and handoff.Solution are non-nil and matching.
//
// On any of the C0619 failure modes listed in the spec, resolveRecords emits one
// bounded C0619 diagnostic and returns (nil, false). Per the conservative reading
// of the "Complete when" wording, a single bad root or constant fails the whole
// arena rather than leaving a partial index.
func resolveRecords(handoff *solveHandoff, diagnostics *diagnostic.DiagnosticSet, config Config) (*solvedRecords, bool) {
	reporter := newValidationReporter(diagnostics, config.MaxDiagnostics)
	fail := func(message string) bool {
		reporter.add(diagnostic.Diagnostic{
			Severity: diagnostic.Error,
			Code:     CodeGeneration,
			Message:  message,
		})
		reporter.flush()
		return false
	}

	roots := handoff.Roots.All()
	constants := handoff.Constants.All()

	s := &solvedRecords{
		roots:     make(map[valueID]infer.TypeResult, len(roots)),
		constants: make(map[symbol.SyntaxRef]constantResult, len(constants)),
	}

	for _, rv := range roots {
		if rv.Root.Alternative.Guarded {
			selected, ok := handoff.Solution.Selection(rv.Root.Alternative.Choice)
			if !ok || selected != rv.Root.Alternative.Index {
				// Inactive guarded roots are silently excluded.
				continue
			}
		}

		if !rv.Value.valid() {
			return nil, fail("root references an invalid value")
		}

		if _, exists := s.roots[rv.Value]; exists {
			return nil, fail("duplicate root value")
		}

		// Validate the root's kind-specific identity.
		switch rv.Root.Kind {
		case rootSyntax, rootInstantiation, rootMethod:
			if rv.Root.Syntax == (symbol.SyntaxRef{}) {
				return nil, fail("root has an invalid syntax reference")
			}
		case rootSymbol:
			if rv.Root.Symbol == 0 {
				return nil, fail("root has an invalid symbol reference")
			}
		case rootSlot:
			if rv.Root.Slot == (infer.SlotID{}) {
				return nil, fail("root has an invalid slot reference")
			}
		default:
			return nil, fail("root has an invalid kind")
		}

		var result infer.TypeResult
		var ok bool
		switch rv.Root.Kind {
		case rootSyntax:
			result, ok = handoff.Solution.SyntaxType(rv.Root.Syntax)
		case rootSymbol:
			result, ok = handoff.Solution.SymbolType(rv.Root.Symbol)
		case rootInstantiation:
			inst, instOK := handoff.Solution.Instantiation(rv.Root.Syntax)
			if !instOK {
				ok = false
				break
			}
			if rv.Root.Parameter >= uint32(len(inst.Arguments)) {
				return nil, fail("root instantiation parameter is out of range")
			}
			result, ok = inst.Arguments[rv.Root.Parameter], true
		case rootMethod:
			method, methodOK := handoff.Solution.Method(rv.Root.Syntax)
			if !methodOK {
				ok = false
				break
			}
			if rv.Root.Parameter >= uint32(len(method.Arguments)) {
				return nil, fail("root method parameter is out of range")
			}
			result, ok = method.Arguments[rv.Root.Parameter], true
		case rootSlot:
			result, ok = handoff.Solution.Slot(rv.Root.Slot)
		}

		if !ok {
			return nil, fail("root resolution failed")
		}

		s.roots[rv.Value] = result
	}

	for _, c := range constants {
		if _, exists := s.constants[c.Syntax]; exists {
			return nil, fail("duplicate constant syntax")
		}
		s.constants[c.Syntax] = c.Result
	}

	return s, true
}
