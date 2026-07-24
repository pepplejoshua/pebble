package check

import (
	"fmt"
	"os"
	"path/filepath"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
)

func loadConstantFixture(t *testing.T, path string) (Inputs, *diagnostic.DiagnosticSet) {
	t.Helper()
	contents, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	return constantInputs(t, checkProvider{"main.peb": contents})
}

func evaluateFixtureConstants(t *testing.T, evaluator *constantEvaluator) (known, failed int) {
	t.Helper()
	for _, candidate := range evaluator.inputs.Resolution.Symbols.All() {
		initializer, ok, _ := evaluator.bindingInitializer(candidate.ID)
		if !ok {
			continue
		}
		switch evaluator.evaluate(initializer).State {
		case constantKnown:
			known++
		case constantError:
			failed++
		}
	}
	for _, ref := range constantArrayLengths(evaluator.inputs) {
		switch evaluator.ArrayLength(ref).State {
		case infer.ArrayLengthKnown:
			known++
		case infer.ArrayLengthError:
			failed++
		}
	}
	return known, failed
}

func TestConstantRepositoryFixtures(t *testing.T) {
	tests := []struct {
		pattern      string
		wantKnown    bool
		wantFailures bool
	}{
		{pattern: "../../../tests/check/facts/valid/constant_*.peb", wantKnown: true},
		{pattern: "../../../tests/check/facts/invalid/C0614/constant_*.peb", wantFailures: true},
		{pattern: "../../../tests/check/facts/recovery/constant_*.peb", wantKnown: true, wantFailures: true},
	}
	for _, test := range tests {
		paths, err := filepath.Glob(test.pattern)
		if err != nil {
			t.Fatal(err)
		}
		if len(paths) == 0 {
			t.Fatalf("no fixtures match %s", test.pattern)
		}
		for _, path := range paths {
			t.Run(fmt.Sprintf("%s/%s", filepath.Base(filepath.Dir(path)), filepath.Base(path)), func(t *testing.T) {
				inputs, diagnostics := loadConstantFixture(t, path)
				evaluator := newConstantEvaluator(inputs, diagnostics, Config{})
				known, failed := evaluateFixtureConstants(t, evaluator)
				if test.wantKnown && known == 0 {
					t.Fatalf("known results = %d", known)
				}
				if test.wantFailures && failed == 0 {
					t.Fatalf("failed results = %d", failed)
				}
				for _, item := range diagnostics.Items() {
					if item.Code != CodeInvalidConstant {
						t.Fatalf("unexpected diagnostic: %+v", item)
					}
				}
			})
		}
	}
}
