package check

import (
	"bytes"
	"os"
	"path/filepath"
	"reflect"
	"sort"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/module"
	"github.com/pepplejoshua/pebble/compiler/internal/tir"
)

const validationDeterminismRuns = 50

type validationDeterminismCase struct {
	name       string
	path       string
	entryCheck bool
}

// TestValidationDeterminism drives one real fixture from each validation
// diagnostic family through the public checker entry point. The repeated runs
// happen in one process so map iteration is exercised without relying on
// process-level map seed controls.
func TestValidationDeterminism(t *testing.T) {
	cases := []validationDeterminismCase{
		{name: "conversion", path: "../../../tests/check/validation/invalid/C0601/conversion_forbidden.peb"},
		{name: "operator", path: "../../../tests/check/validation/invalid/C0603/operator_unsigned_negate.peb"},
		{name: "member", path: "../../../tests/check/validation/invalid/C0605/aggregate_unknown_field.peb"},
		{name: "call", path: "../../../tests/check/validation/invalid/C0604/call_arity_mismatch.peb"},
		{name: "capture", path: "../../../tests/check/validation/invalid/C0617/callable_capturing_anonymous.peb"},
		{name: "generic-anonymous", path: "../../../tests/check/validation/invalid/C0608/callable_generic_anonymous.peb"},
		{name: "index", path: "../../../tests/check/validation/invalid/C0609/index_out_of_range.peb"},
		{name: "place", path: "../../../tests/check/validation/invalid/C0606/address_of_let_field.peb"},
		{name: "unsupported-generic", path: "../../../tests/check/validation/invalid/C0610/generic_unsupported_field.peb"},
		{name: "binding-initializer", path: "../../../tests/check/validation/invalid/C0602/global_missing_initializer.peb"},
		{name: "nonconstant-global", path: "../../../tests/check/validation/invalid/C0616/global_nonconstant.peb"},
		{name: "aggregate", path: "../../../tests/check/validation/invalid/C0615/sizeof_void.peb"},
		{name: "missing-return", path: "../../../tests/check/validation/invalid/C0607/missing_return.peb"},
		{name: "invalid-target", path: "../../../tests/check/validation/invalid/C0611/switch_duplicate.peb"},
		{name: "unreachable", path: "../../../tests/check/validation/invalid/C0618/unreachable_after_return.peb"},
		{name: "defer", path: "../../../tests/check/validation/invalid/C0613/defer_return.peb"},
		{name: "statement", path: "../../../tests/check/validation/invalid/C0612/discard_nonvoid.peb"},
		{name: "entry", path: "../../../tests/check/validation/invalid/C0620/entry_method.peb", entryCheck: true},
	}

	for _, testCase := range cases {
		t.Run(testCase.name, func(t *testing.T) {
			contents, err := os.ReadFile(testCase.path)
			if err != nil {
				t.Fatal(err)
			}

			var firstSuccessful bool
			var firstDiagnostics []diagnostic.Diagnostic
			var firstDump []byte
			var firstIRNil bool
			for run := 0; run < validationDeterminismRuns; run++ {
				inputs, diagnostics := factInputs(t, checkProvider{"main.peb": contents})
				config := Config{}
				if testCase.entryCheck {
					config.Entry = EntryPoint{Mode: EntryRequired, Symbol: entrySymbol(t, inputs, "entry")}
				}
				result := Check(inputs, diagnostics, config)
				items := diagnostics.Items()
				ir := result.IR()

				if run == 0 {
					firstSuccessful = result.Successful()
					firstDiagnostics = append([]diagnostic.Diagnostic(nil), items...)
					firstIRNil = ir == nil
					if ir != nil {
						firstDump = dumpValidationIR(t, ir)
					}
					continue
				}
				if result.Successful() != firstSuccessful {
					t.Fatalf("run %d: Successful() = %v, want %v", run, result.Successful(), firstSuccessful)
				}
				if !reflect.DeepEqual(items, firstDiagnostics) {
					t.Fatalf("run %d: diagnostics differ\n got: %+v\nwant: %+v", run, items, firstDiagnostics)
				}
				if (ir == nil) != firstIRNil {
					t.Fatalf("run %d: IR nilness differs: got %v, want %v", run, ir == nil, firstIRNil)
				}
				if ir != nil && !bytes.Equal(dumpValidationIR(t, ir), firstDump) {
					t.Fatalf("run %d: IR dump differs", run)
				}
			}
		})
	}
}

// TestValidationMultimoduleDeterminism loads each case's complete file set as
// source. The flat filename convention mirrors module.Build's canonical paths
// and the existing facts multimodule fixtures.
func TestValidationDeterminismMultimodule(t *testing.T) {
	dirs, err := filepath.Glob("../../../tests/check/validation/valid/multimodule/*")
	if err != nil || len(dirs) == 0 {
		t.Fatalf("multimodule fixture glob failed: %v", err)
	}
	sort.Strings(dirs)

	for _, dir := range dirs {
		dir := dir
		t.Run(filepath.Base(dir), func(t *testing.T) {
			provider := loadValidationMultimodule(t, dir)
			var firstSuccessful bool
			var firstDiagnostics []diagnostic.Diagnostic
			var firstDump []byte
			var firstIRNil bool
			for run := 0; run < validationDeterminismRuns; run++ {
				inputs, diagnostics := factInputs(t, provider)
				result := Check(inputs, diagnostics, Config{})
				items := diagnostics.Items()
				ir := result.IR()
				if run == 0 {
					firstSuccessful = result.Successful()
					firstDiagnostics = append([]diagnostic.Diagnostic(nil), items...)
					firstIRNil = ir == nil
					if ir != nil {
						firstDump = dumpValidationIR(t, ir)
					}
					continue
				}
				if result.Successful() != firstSuccessful || !reflect.DeepEqual(items, firstDiagnostics) {
					t.Fatalf("run %d: result or diagnostics differ", run)
				}
				if (ir == nil) != firstIRNil {
					t.Fatalf("run %d: IR nilness differs", run)
				}
				if ir != nil && !bytes.Equal(dumpValidationIR(t, ir), firstDump) {
					t.Fatalf("run %d: IR dump differs", run)
				}
			}
		})
	}
}

func loadValidationMultimodule(t *testing.T, dir string) checkProvider {
	t.Helper()
	paths, err := filepath.Glob(filepath.Join(dir, "*.peb"))
	if err != nil || len(paths) == 0 {
		t.Fatalf("fixture file glob %s: %v", dir, err)
	}
	sort.Strings(paths)
	provider := make(checkProvider, len(paths))
	for _, path := range paths {
		contents, err := os.ReadFile(path)
		if err != nil {
			t.Fatal(err)
		}
		provider[module.CanonicalPath(filepath.Base(path))] = contents
	}
	return provider
}

func dumpValidationIR(t *testing.T, unit *tir.Unit) []byte {
	t.Helper()
	var dump bytes.Buffer
	if err := unit.Dump(&dump); err != nil {
		t.Fatal(err)
	}
	return append([]byte(nil), dump.Bytes()...)
}
