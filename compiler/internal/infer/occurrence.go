package infer

import (
	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
)

func cloneConflict(value *inferenceConflict) *inferenceConflict {
	if value == nil {
		return nil
	}
	out := *value
	out.related = append([]Origin(nil), value.related...)
	return &out
}

func cloneTypeOccurrenceMemo(values map[resolveKey]occurrenceResult) map[resolveKey]occurrenceResult {
	out := make(map[resolveKey]occurrenceResult, len(values))
	for key, value := range values {
		value.shape = cloneShape(value.shape)
		value.conflict = cloneConflict(value.conflict)
		out[key] = value
	}
	return out
}

func cloneValueOccurrenceMemo(values map[symbol.SyntaxRef]*inferenceConflict) map[symbol.SyntaxRef]*inferenceConflict {
	out := make(map[symbol.SyntaxRef]*inferenceConflict, len(values))
	for key, value := range values {
		out[key] = cloneConflict(value)
	}
	return out
}

func (s *Session) applyTypeOccurrence(value Constraint) (bool, bool) {
	key := resolveKey{ref: value.ref, owner: value.owner}
	result, ok := s.typeOccurrenceMemo[key]
	if !ok {
		result = s.resolveTypeOccurrence(value.ref, value.owner, value.origin)
		s.typeOccurrenceMemo[key] = result
	}
	if result.conflict != nil {
		return false, s.conflict(result.conflict.code, result.conflict.message, result.conflict.origin, result.conflict.related...)
	}
	return s.constrainShape(value.a, result.shape, value.origin)
}

func (s *Session) resolveTypeOccurrence(ref symbol.SyntaxRef, owner symbol.SymbolID, origin Origin) occurrenceResult {
	local := newReporter(diagnostic.NewDiagnosticSet(), s.config.MaxDiagnostics)
	scratch := s.program.resolutionScratch(local)
	scratch.deferMaterialization = true
	template := scratch.resolveTemplate(ref, owner, false, 0)
	if template == 0 {
		return occurrenceResult{conflict: occurrenceConflict(local, origin)}
	}
	shape, ok := s.occurrenceTemplateShape(scratch, template, owner, 0)
	if !ok {
		return occurrenceResult{conflict: &inferenceConflict{code: CodeInvalidType, message: "type occurrence did not resolve to a concrete or rigid semantic type", origin: origin}}
	}
	return occurrenceResult{shape: shape}
}

func occurrenceConflict(local *reporter, origin Origin) *inferenceConflict {
	if local == nil || len(local.items) == 0 {
		return &inferenceConflict{code: CodeInvalidType, message: "syntax occurrence cannot be interpreted as a type", origin: origin}
	}
	selected := local.items[0]
	for _, item := range local.items[1:] {
		if item.Code == CodeResourceLimit && selected.Code != CodeResourceLimit {
			selected = item
			break
		}
	}
	conflictOrigin := origin
	conflictOrigin.Span = selected.Primary.Span
	conflictOrigin.Role = selected.Primary.Message
	related := make([]Origin, len(selected.Related))
	for index, label := range selected.Related {
		related[index] = Origin{Span: label.Span, Role: label.Message}
	}
	return &inferenceConflict{code: selected.Code, message: selected.Message, origin: conflictOrigin, related: related}
}

func (s *Session) occurrenceTemplateShape(program *Program, id TemplateID, owner symbol.SymbolID, depth uint32) (Shape, bool) {
	if depth >= s.config.MaxTypeSyntaxDepth {
		return Shape{}, false
	}
	value, ok := program.Template(id)
	if !ok {
		return Shape{}, false
	}
	if value.Kind == TemplateKnown {
		return Leaf(Term{owner: s.token, kind: termKnown, known: value.Known}), true
	}
	if value.Kind == TemplateParameter {
		typeID, ok := s.program.typeParams[value.Parameter]
		if !ok || !containsSymbol(s.program.ownerParameters(owner), value.Parameter) {
			return Shape{}, false
		}
		return Leaf(Term{owner: s.token, kind: termKnown, known: typeID}), true
	}
	children := make([]Shape, len(value.Children))
	for index, child := range value.Children {
		children[index], ok = s.occurrenceTemplateShape(program, child, owner, depth+1)
		if !ok {
			return Shape{}, false
		}
	}
	switch value.Kind {
	case TemplatePointer:
		if len(children) == 1 {
			return PointerShape(children[0]), true
		}
	case TemplateArray:
		if len(children) == 1 {
			return ArrayShape(value.Length, children[0]), true
		}
	case TemplateSlice:
		if len(children) == 1 {
			return SliceShape(children[0]), true
		}
	case TemplateTuple:
		if len(children) != 0 {
			return TupleShape(children), true
		}
	case TemplateOptional:
		if len(children) == 1 {
			return OptionalShape(children[0]), true
		}
	case TemplateFunction:
		if len(children) != 0 {
			return FunctionShape(value.Convention, children[:len(children)-1], children[len(children)-1], value.Variadic), true
		}
	case TemplateNominal:
		if value.Declaration != 0 {
			return NominalShape(value.Declaration, children), true
		}
	}
	return Shape{}, false
}

func (s *Session) applyValueOccurrence(value Constraint) bool {
	if conflict, ok := s.valueOccurrenceMemo[value.ref]; ok {
		if conflict == nil {
			return true
		}
		return s.conflict(conflict.code, conflict.message, conflict.origin, conflict.related...)
	}
	conflict := s.valueOccurrenceConflict(value.ref, value.origin, 0)
	s.valueOccurrenceMemo[value.ref] = conflict
	if conflict == nil {
		return true
	}
	return s.conflict(conflict.code, conflict.message, conflict.origin, conflict.related...)
}

func (s *Session) valueOccurrenceConflict(ref symbol.SyntaxRef, origin Origin, depth uint32) *inferenceConflict {
	if depth >= s.config.MaxTypeSyntaxDepth {
		return &inferenceConflict{code: CodeResourceLimit, message: "value-occurrence depth limit exceeded", origin: origin}
	}
	node, tree, ok := s.program.node(ref)
	if !ok {
		return &inferenceConflict{code: CodeResourceLimit, message: "value occurrence is missing from its immutable tree", origin: origin}
	}
	switch node.Kind() {
	case syntax.Name, syntax.Path:
		lookup := ref
		if node.Kind() == syntax.Path {
			children := semanticNodeIDs(tree, node.Children())
			if len(children) == 0 {
				return &inferenceConflict{code: CodeDamagedInput, message: "value path has damaged resolution evidence", origin: origin}
			}
			lookup.Node = children[len(children)-1]
		}
		resolution, ok := s.program.inputs.Resolution.Reference(lookup)
		if !ok || resolution.State != symbol.ResolutionResolved || resolution.Symbol == 0 {
			return &inferenceConflict{code: CodeDamagedInput, message: "value occurrence lacks immutable 04b resolution evidence", origin: origin}
		}
		selected, ok := s.program.inputs.Resolution.Symbols.Symbol(resolution.Symbol)
		if !ok || selected.Error {
			return &inferenceConflict{code: CodeDamagedInput, message: "value occurrence has damaged symbol evidence", origin: origin}
		}
		switch selected.Kind {
		case symbol.SymbolFunction, symbol.SymbolBinding, symbol.SymbolParameter, symbol.SymbolLoopBinding, symbol.SymbolField, symbol.SymbolVariant, symbol.SymbolMethod, symbol.SymbolExternFunction, symbol.SymbolExternBinding:
			return nil
		default:
			return &inferenceConflict{code: CodeInvalidType, message: "syntax occurrence resolves to a type-only or wrong-category symbol", origin: origin}
		}
	case syntax.GroupedTerm:
		children := semanticNodeIDs(tree, node.Children())
		if len(children) != 1 {
			return &inferenceConflict{code: CodeInvalidType, message: "grouped occurrence cannot be interpreted as a runtime value", origin: origin}
		}
		return s.valueOccurrenceConflict(symbol.SyntaxRef{Module: ref.Module, Node: children[0]}, origin, depth+1)
	case syntax.Literal, syntax.InterpolatedString, syntax.ContextExpr, syntax.SomeExpr, syntax.SizeofExpr, syntax.PrefixTerm, syntax.PostfixExpr, syntax.BinaryExpr, syntax.CastExpr, syntax.CallExpr, syntax.SliceExpr, syntax.MemberExpr, syntax.TupleTerm, syntax.ArrayExpr, syntax.ArrayRepeatExpr, syntax.RecordExpr, syntax.PartialMemberExpr:
		return nil
	case syntax.BracketApply:
		mode, ok := s.program.inputs.Resolution.Bracket(ref)
		if !ok {
			return &inferenceConflict{code: CodeDamagedInput, message: "bracket occurrence lacks immutable 04b evidence", origin: origin}
		}
		if mode == symbol.BracketTypeNames {
			return &inferenceConflict{code: CodeInvalidType, message: "type application cannot be interpreted as a runtime value", origin: origin}
		}
		return nil
	case syntax.FunctionTerm:
		if node.Data()&syntax.FunctionBodyPresent != 0 {
			return nil
		}
		fallthrough
	case syntax.OptionalType, syntax.SliceType, syntax.ArrayType, syntax.StructType, syntax.UnionType, syntax.EnumType:
		return &inferenceConflict{code: CodeInvalidType, message: "type syntax cannot be interpreted as a runtime value", origin: origin}
	case syntax.Missing, syntax.Error:
		return &inferenceConflict{code: CodeDamagedInput, message: "damaged syntax cannot be interpreted as a runtime value", origin: origin}
	default:
		return &inferenceConflict{code: CodeInvalidType, message: "syntax occurrence is not a runtime value", origin: origin}
	}
}
