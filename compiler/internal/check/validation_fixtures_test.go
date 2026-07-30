package check

import (
	"os"
	"path/filepath"
	"sort"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
)

func TestValidationFixtures(t *testing.T) {
	t.Run("Conversion", func(t *testing.T) {
		validPaths := validationFixturePaths(t, "../../../tests/check/validation/valid/conversion_*.peb")
		invalidPaths := validationFixturePaths(t, "../../../tests/check/validation/invalid/C0601/conversion_*.peb")

		for _, path := range validPaths {
			path := path
			t.Run(filepath.Base(path), func(t *testing.T) {
				diagnostics, handoff, records := runValidationFixture(t, path)
				if !validateCompatibilityRecords(handoff, records, diagnostics, Config{}) || hasValidationDiagnostic(diagnostics, CodeConversion) {
					t.Fatalf("valid conversion fixture was rejected: %+v", diagnostics.Items())
				}
			})
		}

		for _, path := range invalidPaths {
			path := path
			t.Run(filepath.Base(path), func(t *testing.T) {
				diagnostics, handoff, records := runValidationFixture(t, path)
				valid := validateCompatibilityRecords(handoff, records, diagnostics, Config{})
				if valid || !hasValidationDiagnostic(diagnostics, CodeConversion) {
					t.Fatalf("invalid conversion was not rejected: valid=%v diagnostics=%+v", valid, diagnostics.Items())
				}
			})
		}
	})

	t.Run("Operator", func(t *testing.T) {
		validPaths := validationFixturePaths(t, "../../../tests/check/validation/valid/operator_*.peb")
		invalidPaths := validationFixturePaths(t, "../../../tests/check/validation/invalid/C0603/*.peb")

		for _, path := range validPaths {
			path := path
			t.Run(filepath.Base(path), func(t *testing.T) {
				diagnostics, handoff, records := runValidationFixture(t, path)
				arithmeticOK := validateArithmeticOperators(handoff, records, diagnostics, Config{})
				booleanOK := validateBooleanOperators(handoff, records, diagnostics, Config{})
				if !arithmeticOK || !booleanOK || hasValidationDiagnostic(diagnostics, CodeOperator) {
					t.Fatalf("valid operator fixture was rejected: %+v", diagnostics.Items())
				}
			})
		}

		for _, path := range invalidPaths {
			path := path
			t.Run(filepath.Base(path), func(t *testing.T) {
				diagnostics, handoff, records := runValidationFixture(t, path)
				arithmeticOK := validateArithmeticOperators(handoff, records, diagnostics, Config{})
				booleanOK := validateBooleanOperators(handoff, records, diagnostics, Config{})
				if arithmeticOK && booleanOK || !hasValidationDiagnostic(diagnostics, CodeOperator) {
					t.Fatalf("invalid operator was not rejected: arithmetic=%v boolean=%v diagnostics=%+v", arithmeticOK, booleanOK, diagnostics.Items())
				}
			})
		}
	})

	t.Run("Aggregate", func(t *testing.T) {
		validPaths := validationFixturePaths(t, "../../../tests/check/validation/valid/aggregate_*.peb")
		conversionPaths := validationFixturePaths(t, "../../../tests/check/validation/invalid/C0601/field_*.peb")
		memberPaths := validationFixturePaths(t, "../../../tests/check/validation/invalid/C0605/*.peb")

		for _, path := range validPaths {
			path := path
			t.Run(filepath.Base(path), func(t *testing.T) {
				diagnostics, handoff, records := runValidationFixture(t, path)
				aggregateOK := validateAggregateRecords(handoff, records, diagnostics, Config{})
				compatibilityOK := validateCompatibilityRecords(handoff, records, diagnostics, Config{})
				if !aggregateOK || !compatibilityOK || hasValidationDiagnostic(diagnostics, CodeMember) || hasValidationDiagnostic(diagnostics, CodeAggregate) || hasValidationDiagnostic(diagnostics, CodeConversion) {
					t.Fatalf("valid aggregate fixture was rejected: aggregate=%v compatibility=%v diagnostics=%+v", aggregateOK, compatibilityOK, diagnostics.Items())
				}
			})
		}

		for _, path := range conversionPaths {
			path := path
			t.Run(filepath.Base(path), func(t *testing.T) {
				diagnostics, handoff, records := runValidationFixture(t, path)
				if validateCompatibilityRecords(handoff, records, diagnostics, Config{}) || !hasValidationDiagnostic(diagnostics, CodeConversion) {
					t.Fatalf("invalid aggregate field conversion was not rejected: diagnostics=%+v", diagnostics.Items())
				}
			})
		}

		for _, path := range memberPaths {
			path := path
			t.Run(filepath.Base(path), func(t *testing.T) {
				diagnostics, handoff, records := runValidationFixture(t, path)
				aggregateOK := validateAggregateRecords(handoff, records, diagnostics, Config{})
				memberOK := validateMemberRecords(handoff, records, diagnostics, Config{})
				if aggregateOK && memberOK || !hasValidationDiagnostic(diagnostics, CodeMember) {
					t.Fatalf("invalid aggregate member was not rejected: aggregate=%v member=%v diagnostics=%+v", aggregateOK, memberOK, diagnostics.Items())
				}
			})
		}
	})

	t.Run("Call", func(t *testing.T) {
		validPaths := validationFixturePaths(t, "../../../tests/check/validation/valid/call_*.peb")
		conversionPaths := validationFixturePaths(t, "../../../tests/check/validation/invalid/C0601/call_*.peb")
		callPaths := validationFixturePaths(t, "../../../tests/check/validation/invalid/C0604/*.peb")

		for _, path := range validPaths {
			path := path
			t.Run(filepath.Base(path), func(t *testing.T) {
				diagnostics, handoff, records := runValidationFixture(t, path)
				callOK := validateCallRecords(handoff, records, diagnostics, Config{})
				compatibilityOK := validateCompatibilityRecords(handoff, records, diagnostics, Config{})
				if !callOK || !compatibilityOK || hasValidationDiagnostic(diagnostics, CodeCall) || hasValidationDiagnostic(diagnostics, CodeConversion) {
					t.Fatalf("valid call fixture was rejected: call=%v compatibility=%v diagnostics=%+v", callOK, compatibilityOK, diagnostics.Items())
				}
			})
		}

		for _, path := range conversionPaths {
			path := path
			t.Run(filepath.Base(path), func(t *testing.T) {
				diagnostics, handoff, records := runValidationFixture(t, path)
				if validateCompatibilityRecords(handoff, records, diagnostics, Config{}) || !hasValidationDiagnostic(diagnostics, CodeConversion) {
					t.Fatalf("invalid call argument conversion was not rejected: diagnostics=%+v", diagnostics.Items())
				}
			})
		}

		for _, path := range callPaths {
			path := path
			t.Run(filepath.Base(path), func(t *testing.T) {
				diagnostics, handoff, records := runValidationFixture(t, path)
				if validateCallRecords(handoff, records, diagnostics, Config{}) || !hasValidationDiagnostic(diagnostics, CodeCall) {
					t.Fatalf("invalid call was not rejected: diagnostics=%+v", diagnostics.Items())
				}
			})
		}
	})

	t.Run("Callable", func(t *testing.T) {
		validPaths := validationFixturePaths(t, "../../../tests/check/validation/valid/callable_*.peb")
		capturePaths := validationFixturePaths(t, "../../../tests/check/validation/invalid/C0617/*.peb")
		genericPaths := validationFixturePaths(t, "../../../tests/check/validation/invalid/C0608/*.peb")

		for _, path := range validPaths {
			path := path
			t.Run(filepath.Base(path), func(t *testing.T) {
				diagnostics, handoff, records := runValidationFixture(t, path)
				if !validateCallableRecords(handoff, records, diagnostics, Config{}) || hasValidationDiagnostic(diagnostics, CodeCaptureViolation) || hasValidationDiagnostic(diagnostics, CodeGenericAnonymous) {
					t.Fatalf("valid callable fixture was rejected: diagnostics=%+v", diagnostics.Items())
				}
			})
		}

		for _, path := range capturePaths {
			path := path
			t.Run(filepath.Base(path), func(t *testing.T) {
				diagnostics, handoff, records := runValidationFixture(t, path)
				if validateCallableRecords(handoff, records, diagnostics, Config{}) || !hasValidationDiagnostic(diagnostics, CodeCaptureViolation) {
					t.Fatalf("capturing anonymous callable was not rejected: diagnostics=%+v", diagnostics.Items())
				}
			})
		}

		for _, path := range genericPaths {
			path := path
			t.Run(filepath.Base(path), func(t *testing.T) {
				diagnostics, handoff, records := runValidationFixture(t, path)
				if validateCallableRecords(handoff, records, diagnostics, Config{}) || !hasValidationDiagnostic(diagnostics, CodeGenericAnonymous) {
					t.Fatalf("generic anonymous callable was not rejected: diagnostics=%+v", diagnostics.Items())
				}
			})
		}
	})

	t.Run("Index", func(t *testing.T) {
		validPaths := validationFixturePaths(t, "../../../tests/check/validation/valid/index_*.peb")
		invalidPaths := validationFixturePaths(t, "../../../tests/check/validation/invalid/C0609/*.peb")

		for _, path := range validPaths {
			path := path
			t.Run(filepath.Base(path), func(t *testing.T) {
				diagnostics, handoff, records := runValidationFixture(t, path)
				if !validateIndexRecords(handoff, records, diagnostics, Config{}) || hasValidationDiagnostic(diagnostics, CodeIndexBound) {
					t.Fatalf("valid index fixture was rejected: %+v", diagnostics.Items())
				}
			})
		}

		for _, path := range invalidPaths {
			path := path
			t.Run(filepath.Base(path), func(t *testing.T) {
				diagnostics, handoff, records := runValidationFixture(t, path)
				valid := validateIndexRecords(handoff, records, diagnostics, Config{})
				if valid || !hasValidationDiagnostic(diagnostics, CodeIndexBound) {
					t.Fatalf("invalid index was not rejected: valid=%v diagnostics=%+v", valid, diagnostics.Items())
				}
			})
		}
	})

	t.Run("Place", func(t *testing.T) {
		validPaths := validationFixturePaths(t, "../../../tests/check/validation/valid/place_*.peb")
		assignmentMismatchPaths := validationFixturePaths(t, "../../../tests/check/validation/invalid/C0601/assignment_*.peb")
		invalidPaths := validationFixturePaths(t, "../../../tests/check/validation/invalid/C0606/*.peb")

		for _, path := range validPaths {
			path := path
			t.Run(filepath.Base(path), func(t *testing.T) {
				diagnostics, handoff, records := runValidationFixture(t, path)
				placeOK := validatePlaceRecords(handoff, records, diagnostics, Config{})
				assignmentOK := validateAssignmentRecords(handoff, records, diagnostics, Config{})
				compatibilityOK := validateCompatibilityRecords(handoff, records, diagnostics, Config{})
				if !placeOK || !assignmentOK || !compatibilityOK || hasValidationDiagnostic(diagnostics, CodePlace) || hasValidationDiagnostic(diagnostics, CodeConversion) {
					t.Fatalf("valid place fixture was rejected: place=%v assignment=%v compatibility=%v diagnostics=%+v", placeOK, assignmentOK, compatibilityOK, diagnostics.Items())
				}
			})
		}

		for _, path := range assignmentMismatchPaths {
			path := path
			t.Run(filepath.Base(path), func(t *testing.T) {
				diagnostics, handoff, records := runValidationFixture(t, path)
				if validateCompatibilityRecords(handoff, records, diagnostics, Config{}) || !hasValidationDiagnostic(diagnostics, CodeConversion) {
					t.Fatalf("invalid assignment type mismatch was not rejected: diagnostics=%+v", diagnostics.Items())
				}
			})
		}

		for _, path := range invalidPaths {
			path := path
			t.Run(filepath.Base(path), func(t *testing.T) {
				diagnostics, handoff, records := runValidationFixture(t, path)
				placeOK := validatePlaceRecords(handoff, records, diagnostics, Config{})
				assignmentOK := validateAssignmentRecords(handoff, records, diagnostics, Config{})
				if placeOK && assignmentOK || !hasValidationDiagnostic(diagnostics, CodePlace) {
					t.Fatalf("invalid place fixture was not rejected: place=%v assignment=%v diagnostics=%+v", placeOK, assignmentOK, diagnostics.Items())
				}
			})
		}
	})

	t.Run("Generic", func(t *testing.T) {
		validPaths := validationFixturePaths(t, "../../../tests/check/validation/valid/generic_*.peb")
		invalidPaths := validationFixturePaths(t, "../../../tests/check/validation/invalid/C0610/*.peb")

		for _, path := range validPaths {
			path := path
			t.Run(filepath.Base(path), func(t *testing.T) {
				contents, err := os.ReadFile(path)
				if err != nil {
					t.Fatal(err)
				}
				diagnostics := diagnostic.NewDiagnosticSet()
				inputs, _ := factInputs(t, checkProvider{"main.peb": contents})
				handoff := run06a(inputs, diagnostics, Config{})
				if handoff == nil {
					t.Fatalf("06a did not produce a handoff for %s", path)
				}
				result := run06b(handoff, diagnostics, Config{})
				if !result.Successful() || hasValidationDiagnostic(diagnostics, CodeUnsupportedGeneric) {
					t.Fatalf("valid generic fixture was rejected: result=%v diagnostics=%+v", result.Successful(), diagnostics.Items())
				}
			})
		}

		for _, path := range invalidPaths {
			path := path
			t.Run(filepath.Base(path), func(t *testing.T) {
				contents, err := os.ReadFile(path)
				if err != nil {
					t.Fatal(err)
				}
				diagnostics := diagnostic.NewDiagnosticSet()
				inputs, _ := factInputs(t, checkProvider{"main.peb": contents})
				handoff := run06a(inputs, diagnostics, Config{})
				if handoff == nil {
					t.Fatalf("06a did not produce a handoff for %s", path)
				}
				result := run06b(handoff, diagnostics, Config{})
				if result.Successful() || !hasValidationDiagnostic(diagnostics, CodeUnsupportedGeneric) {
					t.Fatalf("invalid generic fixture was not rejected: result=%v diagnostics=%+v", result.Successful(), diagnostics.Items())
				}
			})
		}
	})

	t.Run("Global", func(t *testing.T) {
		validPaths := validationFixturePaths(t, "../../../tests/check/validation/valid/global_*.peb")
		c0602Paths := validationFixturePaths(t, "../../../tests/check/validation/invalid/C0602/*.peb")
		c0616Paths := validationFixturePaths(t, "../../../tests/check/validation/invalid/C0616/*.peb")

		for _, path := range validPaths {
			path := path
			t.Run(filepath.Base(path), func(t *testing.T) {
				diagnostics, handoff, records := runValidationFixture(t, path)
				if !validateBindings(handoff, records, diagnostics, Config{}) || hasValidationDiagnostic(diagnostics, CodeBindingInitializer) || hasValidationDiagnostic(diagnostics, CodeNonconstantGlobal) {
					t.Fatalf("valid global fixture was rejected: %+v", diagnostics.Items())
				}
			})
		}

		for _, path := range c0602Paths {
			path := path
			t.Run(filepath.Base(path), func(t *testing.T) {
				diagnostics, handoff, records := runValidationFixture(t, path)
				if validateBindings(handoff, records, diagnostics, Config{}) || !hasValidationDiagnostic(diagnostics, CodeBindingInitializer) {
					t.Fatalf("invalid C0602 fixture was not rejected: %+v", diagnostics.Items())
				}
			})
		}

		for _, path := range c0616Paths {
			path := path
			t.Run(filepath.Base(path), func(t *testing.T) {
				diagnostics, handoff, records := runValidationFixture(t, path)
				if validateBindings(handoff, records, diagnostics, Config{}) || !hasValidationDiagnostic(diagnostics, CodeNonconstantGlobal) {
					t.Fatalf("invalid C0616 fixture was not rejected: %+v", diagnostics.Items())
				}
			})
		}
	})

	t.Run("Sizeof", func(t *testing.T) {
		validPaths := validationFixturePaths(t, "../../../tests/check/validation/valid/sizeof_*.peb")
		invalidPaths := validationFixturePaths(t, "../../../tests/check/validation/invalid/C0615/*.peb")

		for _, path := range validPaths {
			path := path
			t.Run(filepath.Base(path), func(t *testing.T) {
				diagnostics, handoff, records := runValidationFixture(t, path)
				if !validateSizeof(handoff, records, diagnostics, Config{}) || hasValidationDiagnostic(diagnostics, CodeAggregate) {
					t.Fatalf("valid sizeof fixture was rejected: %+v", diagnostics.Items())
				}
			})
		}

		for _, path := range invalidPaths {
			path := path
			t.Run(filepath.Base(path), func(t *testing.T) {
				diagnostics, handoff, records := runValidationFixture(t, path)
				sizeofOK := validateSizeof(handoff, records, diagnostics, Config{})
				aggregateOK := validateAggregateRecords(handoff, records, diagnostics, Config{})
				if (sizeofOK && aggregateOK) || !hasValidationDiagnostic(diagnostics, CodeAggregate) {
					t.Fatalf("invalid C0615 fixture was not rejected: sizeof=%v aggregate=%v diagnostics=%+v", sizeofOK, aggregateOK, diagnostics.Items())
				}
			})
		}
	})

	t.Run("Entry", func(t *testing.T) {
		validPaths := validationFixturePaths(t, "../../../tests/check/validation/valid/entry_*.peb")
		invalidPaths := validationFixturePaths(t, "../../../tests/check/validation/invalid/C0620/*.peb")

		for _, path := range validPaths {
			path := path
			t.Run(filepath.Base(path), func(t *testing.T) {
				contents, err := os.ReadFile(path)
				if err != nil {
					t.Fatal(err)
				}
				diagnostics := diagnostic.NewDiagnosticSet()
				inputs, _ := factInputs(t, checkProvider{"main.peb": contents})
				id := entrySymbol(t, inputs, "entry")
				handoff := run06a(inputs, diagnostics, Config{})
				if handoff == nil {
					t.Fatalf("06a did not produce a handoff for %s", path)
				}
				result := run06b(handoff, diagnostics, Config{Entry: EntryPoint{Mode: EntryRequired, Symbol: id}})
				if !result.Successful() || hasValidationDiagnostic(diagnostics, CodeEntryPoint) {
					t.Fatalf("valid entry fixture was rejected: result=%v diagnostics=%+v", result.Successful(), diagnostics.Items())
				}
			})
		}

		for _, path := range invalidPaths {
			path := path
			t.Run(filepath.Base(path), func(t *testing.T) {
				contents, err := os.ReadFile(path)
				if err != nil {
					t.Fatal(err)
				}
				diagnostics := diagnostic.NewDiagnosticSet()
				inputs, _ := factInputs(t, checkProvider{"main.peb": contents})
				id := entrySymbol(t, inputs, "entry")
				handoff := run06a(inputs, diagnostics, Config{})
				if handoff == nil {
					t.Fatalf("06a did not produce a handoff for %s", path)
				}
				result := run06b(handoff, diagnostics, Config{Entry: EntryPoint{Mode: EntryRequired, Symbol: id}})
				if result.Successful() || !hasValidationDiagnostic(diagnostics, CodeEntryPoint) {
					t.Fatalf("invalid entry fixture was not rejected: result=%v diagnostics=%+v", result.Successful(), diagnostics.Items())
				}
			})
		}
	})
}

func validationFixturePaths(t *testing.T, pattern string) []string {
	t.Helper()
	paths, err := filepath.Glob(pattern)
	if err != nil || len(paths) == 0 {
		t.Fatalf("glob %s: %v", pattern, err)
	}
	sort.Strings(paths)
	return paths
}

func runValidationFixture(t *testing.T, path string) (*diagnostic.DiagnosticSet, *solveHandoff, *solvedRecords) {
	t.Helper()
	contents, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": contents})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil || handoff.Semantics == nil || handoff.Solution == nil {
		t.Fatalf("06a did not produce a handoff for %s: %+v", path, diagnostics.Items())
	}
	records, ok := resolveRecords(handoff, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatalf("records did not resolve for %s: %+v", path, diagnostics.Items())
	}
	return diagnostics, handoff, records
}

func hasValidationDiagnostic(set *diagnostic.DiagnosticSet, code diagnostic.Code) bool {
	for _, item := range set.Items() {
		if item.Code == code {
			return true
		}
	}
	return false
}
