package infer

import (
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
)

type Origin struct {
	Syntax       symbol.SyntaxRef
	Span         source.Span
	Role         string
	Symbol       symbol.SymbolID
	GenericOwner symbol.SymbolID
}

type Substitution struct {
	Parameter symbol.SymbolID
	Argument  Term
}

type CallableArgument struct {
	Source      Term
	Destination Term
}

type Alternative struct {
	Label       string
	Constraints []Constraint
}

type constraintKind uint8

const (
	constraintEqual constraintKind = iota + 1
	constraintNumeric
	constraintIntegral
	constraintOrdered
	constraintHasField
	constraintSelectMethod
	constraintCallable
	constraintIndexable
	constraintSliceable
	constraintLiteralFits
	constraintShape
	constraintInstantiate
	constraintTypeOccurrence
	constraintValueOccurrence
	constraintOneOf
)

// Constraint is a closed, constructor-created algebraic fact.
type Constraint struct {
	kind          constraintKind
	a, b, c       Term
	name          string
	site          symbol.SyntaxRef
	shape         Shape
	template      TemplateID
	owner         symbol.SymbolID
	ref           symbol.SyntaxRef
	substitutions []Substitution
	explicit      []Term
	arguments     []CallableArgument
	alternatives  []Alternative
	origin        Origin
}

func Equal(a, b Term, origin Origin) Constraint {
	return Constraint{kind: constraintEqual, a: a, b: b, origin: origin}
}
func Numeric(term Term, origin Origin) Constraint {
	return Constraint{kind: constraintNumeric, a: term, origin: origin}
}
func Integral(term Term, origin Origin) Constraint {
	return Constraint{kind: constraintIntegral, a: term, origin: origin}
}
func Ordered(term Term, origin Origin) Constraint {
	return Constraint{kind: constraintOrdered, a: term, origin: origin}
}
func HasField(receiver Term, name string, field Term, origin Origin) Constraint {
	return Constraint{kind: constraintHasField, a: receiver, b: field, name: name, origin: origin}
}
func SelectMethod(receiver Term, name string, callable Term, explicit []Term, site symbol.SyntaxRef, origin Origin) Constraint {
	return Constraint{kind: constraintSelectMethod, a: receiver, b: callable, name: name, site: site, explicit: append([]Term(nil), explicit...), origin: origin}
}
func Callable(callee Term, arguments []CallableArgument, result Term, origin Origin) Constraint {
	return Constraint{kind: constraintCallable, a: callee, b: result, arguments: append([]CallableArgument(nil), arguments...), origin: origin}
}
func Indexable(receiver, result Term, origin Origin) Constraint {
	return Constraint{kind: constraintIndexable, a: receiver, b: result, origin: origin}
}
func Sliceable(receiver, result Term, origin Origin) Constraint {
	return Constraint{kind: constraintSliceable, a: receiver, b: result, origin: origin}
}
func LiteralFits(literal, candidate Term, origin Origin) Constraint {
	return Constraint{kind: constraintLiteralFits, a: literal, b: candidate, origin: origin}
}
func ConstrainShape(subject Term, shape Shape, origin Origin) Constraint {
	return Constraint{kind: constraintShape, a: subject, shape: cloneShape(shape), origin: origin}
}
func Instantiate(template TemplateID, substitutions []Substitution, subject Term, origin Origin) Constraint {
	copySub := append([]Substitution(nil), substitutions...)
	return Constraint{kind: constraintInstantiate, a: subject, template: template, substitutions: copySub, origin: origin}
}
func TypeOccurrence(ref symbol.SyntaxRef, owner symbol.SymbolID, subject Term, origin Origin) Constraint {
	return Constraint{kind: constraintTypeOccurrence, ref: ref, owner: owner, a: subject, origin: origin}
}
func ValueOccurrence(ref symbol.SyntaxRef, origin Origin) Constraint {
	return Constraint{kind: constraintValueOccurrence, ref: ref, origin: origin}
}
func OneOf(alternatives []Alternative, origin Origin) Constraint {
	copyAlternatives := make([]Alternative, len(alternatives))
	for i, alt := range alternatives {
		copyAlternatives[i] = Alternative{Label: alt.Label, Constraints: cloneConstraints(alt.Constraints)}
	}
	return Constraint{kind: constraintOneOf, alternatives: copyAlternatives, origin: origin}
}

func cloneConstraint(value Constraint) Constraint {
	value.shape = cloneShape(value.shape)
	value.substitutions = append([]Substitution(nil), value.substitutions...)
	value.explicit = append([]Term(nil), value.explicit...)
	value.arguments = append([]CallableArgument(nil), value.arguments...)
	if len(value.alternatives) != 0 {
		alts := make([]Alternative, len(value.alternatives))
		for i, alt := range value.alternatives {
			alts[i] = Alternative{Label: alt.Label, Constraints: cloneConstraints(alt.Constraints)}
		}
		value.alternatives = alts
	}
	return value
}
func cloneConstraints(values []Constraint) []Constraint {
	out := make([]Constraint, len(values))
	for i := range values {
		out[i] = cloneConstraint(values[i])
	}
	return out
}
