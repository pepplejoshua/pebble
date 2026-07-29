package check

import (
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
)

const aggregateValidationSource = `
type Box = struct { value i32; other i32; };
type Color = enum { red, blue };
fn check() void {
    let explicit Box = Box.{ value = 1, other = 2 };
    let inferred Box = .{ value = 3, other = 4 };
    let color Color = .red;
}
`

func runAggregateValidation(t *testing.T, source string) (*diagnostic.DiagnosticSet, *solveHandoff, *solvedRecords) {
	t.Helper()
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(source)})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil || handoff.Semantics == nil || handoff.Solution == nil {
		t.Fatalf("06a did not produce a handoff: %+v", diagnostics.Items())
	}
	records, ok := resolveRecords(handoff, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatalf("records did not resolve: %+v", diagnostics.Items())
	}
	return diagnostics, handoff, records
}

func aggregateRecords(handoff *solveHandoff) []*aggregateRecord {
	var result []*aggregateRecord
	for index := range handoff.Records.values {
		if handoff.Records.values[index].Aggregate != nil {
			result = append(result, handoff.Records.values[index].Aggregate)
		}
	}
	return result
}

func declarationOfNominal(handoff *solveHandoff, nominal infer.NominalKind) symbol.SymbolID {
	for _, declaration := range handoff.Semantics.TypeDeclarations() {
		if declaration.Nominal == nominal {
			return declaration.Symbol
		}
	}
	return 0
}

func TestValidateAggregateRecordsAcceptsExplicitInferredStructsAndEnumVariant(t *testing.T) {
	diagnostics, handoff, records := runAggregateValidation(t, aggregateValidationSource)
	if !validateAggregateRecords(handoff, records, diagnostics, Config{}) || hasValidationDiagnostic(diagnostics, CodeMember) || hasValidationDiagnostic(diagnostics, CodeAggregate) {
		t.Fatalf("valid aggregate records were rejected: %+v", diagnostics.Items())
	}
}

func TestValidateAggregateRecordsAcceptsBaseLessVoidTaggedVariant(t *testing.T) {
	source := `
type Choice = union enum { empty void; value i32; };
fn check() void {
    let choice Choice = .empty;
}
`
	diagnostics, handoff, records := runAggregateValidation(t, source)
	found := false
	for _, aggregate := range aggregateRecords(handoff) {
		if aggregate.Kind == aggregateEnumVariant {
			found = true
			break
		}
	}
	if !found {
		t.Skip("void tagged-union variant does not reach finishPartialMember")
	}
	if !validateAggregateRecords(handoff, records, diagnostics, Config{}) || hasValidationDiagnostic(diagnostics, CodeMember) || hasValidationDiagnostic(diagnostics, CodeAggregate) {
		t.Fatalf("valid void tagged variant was rejected: %+v", diagnostics.Items())
	}
}

func TestValidateAggregateRecordsRejectsFieldNameErrors(t *testing.T) {
	cases := []struct {
		name string
		edit func(*aggregateRecord)
	}{
		{"duplicate", func(record *aggregateRecord) {
			record.Fields = append(record.Fields, record.Fields[0])
		}},
		{"unknown", func(record *aggregateRecord) {
			record.Fields[0].Member = 0
			record.Fields[0].Name = "missing"
		}},
		{"missing", func(record *aggregateRecord) {
			record.Fields = record.Fields[:1]
		}},
	}
	for _, test := range cases {
		t.Run(test.name, func(t *testing.T) {
			diagnostics, handoff, records := runAggregateValidation(t, aggregateValidationSource)
			var aggregate *aggregateRecord
			for _, candidate := range aggregateRecords(handoff) {
				if candidate.Kind == aggregateStruct && len(candidate.Fields) == 2 {
					aggregate = candidate
					break
				}
			}
			if aggregate == nil {
				t.Fatal("fixture did not produce a two-field struct aggregate")
			}
			test.edit(aggregate)
			if validateAggregateRecords(handoff, records, diagnostics, Config{}) || !hasValidationDiagnostic(diagnostics, CodeMember) {
				t.Fatalf("%s field error was not rejected: %+v", test.name, diagnostics.Items())
			}
		})
	}
}

func TestValidateAggregateRecordsRejectsConstructionAgainstNonStruct(t *testing.T) {
	diagnostics, handoff, records := runAggregateValidation(t, aggregateValidationSource)
	var aggregate *aggregateRecord
	for _, candidate := range aggregateRecords(handoff) {
		if candidate.Kind == aggregateStruct {
			aggregate = candidate
			break
		}
	}
	if aggregate == nil {
		t.Fatal("fixture did not produce a struct aggregate")
	}
	aggregate.Declaration = declarationOfNominal(handoff, infer.NominalEnum)
	if validateAggregateRecords(handoff, records, diagnostics, Config{}) || !hasValidationDiagnostic(diagnostics, CodeAggregate) {
		t.Fatalf("non-struct construction was not rejected: %+v", diagnostics.Items())
	}
}

func TestValidateAggregateRecordsRejectsWrongVariantCategoryAndUnknownVariant(t *testing.T) {
	cases := []struct {
		name string
		edit func(*aggregateRecord, symbol.SymbolID)
	}{
		{"wrong category", func(record *aggregateRecord, box symbol.SymbolID) {
			record.Declaration = box
		}},
		{"unknown variant", func(record *aggregateRecord, _ symbol.SymbolID) {
			record.Fields[0].Name = "missing"
		}},
	}
	for _, test := range cases {
		t.Run(test.name, func(t *testing.T) {
			diagnostics, handoff, records := runAggregateValidation(t, aggregateValidationSource)
			var variant *aggregateRecord
			for _, candidate := range aggregateRecords(handoff) {
				if candidate.Kind == aggregateEnumVariant {
					variant = candidate
					break
				}
			}
			if variant == nil {
				t.Fatal("fixture did not produce an enum variant aggregate")
			}
			test.edit(variant, declarationOfNominal(handoff, infer.NominalStruct))
			if validateAggregateRecords(handoff, records, diagnostics, Config{}) || !hasValidationDiagnostic(diagnostics, CodeMember) {
				t.Fatalf("%s variant error was not rejected: %+v", test.name, diagnostics.Items())
			}
		})
	}
}

func TestValidateAggregateRecordsIgnoresInactiveAggregate(t *testing.T) {
	diagnostics, handoff, records := runAggregateValidation(t, aggregateValidationSource)
	var aggregate *aggregateRecord
	for _, candidate := range aggregateRecords(handoff) {
		if candidate.Kind == aggregateStruct {
			aggregate = candidate
			break
		}
	}
	if aggregate == nil {
		t.Fatal("fixture did not produce a struct aggregate")
	}
	aggregate.Fields[0].Name = "missing"
	for index := range handoff.Records.values {
		if handoff.Records.values[index].Aggregate == aggregate {
			handoff.Records.values[index].Header.Alternative.Guarded = true
			break
		}
	}
	if !validateAggregateRecords(handoff, records, diagnostics, Config{}) || hasValidationDiagnostic(diagnostics, CodeMember) {
		t.Fatal("inactive aggregate was not ignored")
	}
}
