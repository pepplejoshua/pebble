package check

import (
	"fmt"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
)

type requirementKind uint8

const (
	requirementNumeric requirementKind = iota + 1
	requirementIntegral
	requirementOrdered
	requirementEquatable
	requirementLiteralFits
	requirementUnsupportedField
	requirementUnsupportedMethod
	requirementUnsupportedIndex
	requirementUnsupportedSlice
	requirementUnsupportedCall
	requirementUnsupportedConversion
	requirementUnsupportedLayout
	requirementUnsupportedPrint
	requirementUnsupportedConstruction
)

type requirementRecord struct {
	Header   recordHeader
	Kind     requirementKind
	Subject  valueID
	Operator syntax.TokenKind
}

func (w *walker) retainRequirement(header recordHeader, kind requirementKind, subject valueID) {
	if w.session == nil || w.session.Fatal() {
		return
	}
	if header.Owner == 0 || subject == 0 {
		return
	}
	if w.activeBranch != nil {
		w.activeBranch.requirements++
	} else if !w.generation.addGenericRequirement() {
		return
	}
	record := requirementRecord{Header: header, Kind: kind, Subject: subject}
	w.addRecord(retainedRecord{Header: header, Requirement: &record})
}

type genericApplication struct {
	site          symbol.SyntaxRef
	generic       symbol.SymbolID
	parameters    []symbol.SymbolID
	arguments     []typedValue
	substitutions []infer.Substitution
}

func (w *walker) prepareGeneric(site symbol.SyntaxRef, generic symbol.SymbolID, explicit []symbol.SyntaxRef, ctx walkContext) (genericApplication, bool) {
	a := genericApplication{site: site, generic: generic}
	if signature, ok := w.program.Signature(generic); ok && signature.State == infer.DeclarationReady {
		a.parameters = signature.TypeParams
	} else if declaration, ok := w.program.TypeDeclaration(generic); ok && declaration.State == infer.DeclarationReady {
		a.parameters = declaration.Parameters
	} else {
		w.generation.report("generic application has no prepared declaration", spanForRef(w.generation.inputs, site))
		return a, false
	}
	if len(explicit) > len(a.parameters) {
		w.generation.report("generic application has too many explicit arguments", spanForRef(w.generation.inputs, site))
		return a, false
	}
	for i, parameter := range a.parameters {
		origin := w.originForRef(site, fmt.Sprintf("generic argument %d", i+1), ctx.typeOwner, ctx.genericOwner)
		value, ok := w.newSlotValue(w.session.Variable(origin), origin)
		if !ok {
			return a, false
		}
		a.arguments = append(a.arguments, value)
		a.substitutions = append(a.substitutions, infer.Substitution{Parameter: parameter, Argument: value.Term})
	}
	for i, ref := range explicit {
		if w.activeBranch != nil {
			w.addConstraint(infer.TypeOccurrence(ref, ctx.typeOwner, a.arguments[i].Term, w.originForRef(ref, "explicit generic argument", ctx.typeOwner, ctx.genericOwner)))
			w.retainTypeUse(ref, ctx.genericOwner, a.arguments[i].ID, typeUseExplicitArgument, false)
		} else {
			value := w.resolveTypeUse(ref, ctx.typeOwner, ctx.genericOwner, fmt.Sprintf("explicit generic argument %d", i+1), typeUseExplicitArgument)
			w.addConstraint(infer.Equal(a.arguments[i].Term, value.Term, w.originForRef(ref, "explicit generic argument", ctx.typeOwner, ctx.genericOwner)))
		}
	}
	terms := make([]infer.Term, len(a.arguments))
	for i := range a.arguments {
		terms[i] = a.arguments[i].Term
	}
	w.publishInstantiation(site, generic, terms)
	w.mirrorTypeInstantiation(site, generic, explicit, a.parameters)
	return a, true
}

func (w *walker) mirrorTypeInstantiation(site symbol.SyntaxRef, generic symbol.SymbolID, explicit []symbol.SyntaxRef, parameters []symbol.SymbolID) {
	runtime := w.activeBranch
	if runtime == nil || runtime.typeSibling == nil || len(explicit) == 0 {
		return
	}
	declaration, ok := w.program.TypeDeclaration(generic)
	if !ok || declaration.State != infer.DeclarationReady || len(parameters) != len(declaration.Parameters) {
		return
	}
	sibling := runtime.typeSibling
	terms := make([]infer.Term, len(parameters))
	for index := range terms {
		origin := w.originForRef(site, fmt.Sprintf("guarded type application argument %d", index+1), runtime.typeOwner, runtime.genericOwner)
		terms[index] = w.session.Variable(origin)
		value := w.newValue(terms[index], origin)
		w.queueRootFor(sibling, value)
		if index < len(explicit) {
			sibling.constraints = append(sibling.constraints, infer.TypeOccurrence(explicit[index], runtime.typeOwner, terms[index], w.originForRef(explicit[index], "guarded nested type argument", runtime.typeOwner, runtime.genericOwner)))
			header := w.header(explicit[index], runtime.genericOwner, false)
			typeUse := typeUseRecord{Header: header, Kind: typeUseExplicitArgument, Type: value.ID}
			sibling.records = append(sibling.records, pendingBranchRecord{local: recordID(len(sibling.records) + 1), value: retainedRecord{Header: header, TypeUse: &typeUse}})
		}
	}
	sibling.instantiations = append(sibling.instantiations, branchInstantiation{site: site, generic: generic, arguments: terms})
}

func (w *walker) instantiate(template infer.TemplateID, substitutions []infer.Substitution, origin infer.Origin) typedValue {
	term := w.session.Variable(origin)
	w.addConstraint(infer.Instantiate(template, substitutions, term, origin))
	value, _ := w.newSlotValue(term, origin)
	return value
}

func (w *walker) instantiateSignature(signature infer.Signature, a genericApplication, origin infer.Origin) (typedValue, []typedValue, typedValue) {
	parameters := make([]typedValue, len(signature.Inputs))
	shapes := make([]infer.Shape, len(parameters))
	for i, template := range signature.Inputs {
		child := origin
		child.Role = fmt.Sprintf("parameter %d", i+1)
		parameters[i] = w.instantiate(template, a.substitutions, child)
		shapes[i] = infer.Leaf(parameters[i].Term)
	}
	resultOrigin := origin
	resultOrigin.Role = "result"
	result := w.instantiate(signature.Result, a.substitutions, resultOrigin)
	term := w.session.Variable(origin)
	w.addConstraint(infer.ConstrainShape(term, infer.FunctionShape(signature.Convention, shapes, infer.Leaf(result.Term), signature.Variadic), origin))
	callable, _ := w.newSlotValue(term, origin)
	return callable, parameters, result
}
