package check

import (
	"os"
	"path/filepath"
	"sort"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/module"
)

// TestFactHandoffRepositoryFixtures exercises the freeze/solve/handoff pipeline
// on multi-file programs loaded from disk and recovery test cases.
func TestFactHandoffRepositoryFixtures(t *testing.T) {
	// Test multimodule cases: each subdirectory contains one complete program.
	multimodulePath := "../../../tests/check/facts/valid/multimodule/*"
	multimoduleDirs, err := filepath.Glob(multimodulePath)
	if err != nil || len(multimoduleDirs) == 0 {
		t.Fatalf("glob %s: %v", multimodulePath, err)
	}
	sort.Strings(multimoduleDirs)

	for _, dir := range multimoduleDirs {
		caseInfo, err := os.Stat(dir)
		if err != nil || !caseInfo.IsDir() {
			t.Logf("skipping non-directory: %s", dir)
			continue
		}

		caseName := filepath.Base(dir)
		t.Run("multimodule_"+caseName, func(t *testing.T) {
			// Load all .peb files in this directory into a checkProvider.
			pattern := filepath.Join(dir, "*.peb")
			paths, err := filepath.Glob(pattern)
			if err != nil || len(paths) == 0 {
				t.Fatalf("glob %s: %v", pattern, err)
			}
			sort.Strings(paths)

			provider := make(checkProvider)
			for _, path := range paths {
				contents, err := os.ReadFile(path)
				if err != nil {
					t.Fatal(err)
				}
				// Use the filename (not the full path) as the canonical key.
				fileName := filepath.Base(path)
				canonPath := module.CanonicalPath(fileName)
				provider[canonPath] = contents
			}

			// Run through the full pipeline.
			inputs, diagnostics := factInputs(t, provider)
			handoff := run06a(inputs, diagnostics, Config{})

			if handoff == nil {
				t.Fatal("run06a returned nil")
			}

			// For multimodule valid cases, we expect successful generation.
			// The key property: Semantics must be non-nil and modules must be loaded.
			if handoff.Semantics == nil {
				t.Fatalf("Semantics is nil; diagnostics: %+v", diagnostics.Items())
			}

			// Assert the frozenCompilation has the expected structure. Both
			// fixture cases are exactly two files (main.peb plus one sibling),
			// so this checks the real module count, not just "at least one" -
			// a regression that silently collapsed the two files into one
			// module must fail this test, not pass it by accident.
			if len(paths) != 2 {
				t.Fatalf("fixture directory %s has %d .peb files, expected exactly 2", dir, len(paths))
			}
			if len(handoff.Compilation.Modules) != 2 {
				t.Fatalf("Compilation.Modules has %d entries, want exactly 2 (real cross-file loading collapsed?): %+v",
					len(handoff.Compilation.Modules), handoff.Compilation.Modules)
			}
			if handoff.Compilation.Root == 0 {
				t.Fatal("Compilation.Root is zero")
			}

			// Assert DependencyOrder matches module count.
			if len(handoff.Compilation.DependencyOrder) != len(handoff.Compilation.Modules) {
				t.Fatalf("DependencyOrder length %d != module count %d",
					len(handoff.Compilation.DependencyOrder), len(handoff.Compilation.Modules))
			}

			// All modules should be present in dependency order.
			modulesByID := make(map[module.ModuleID]bool)
			for _, m := range handoff.Compilation.Modules {
				modulesByID[m.ID] = true
			}
			for _, depID := range handoff.Compilation.DependencyOrder {
				if !modulesByID[depID] {
					t.Fatalf("DependencyOrder contains non-existent module %d", depID)
				}
			}

			// The root module (main.peb) must import exactly the other module,
			// and DependencyOrder must place the imported module before the
			// root, proving dependencies-before-importers actually held across
			// two real files, not just that both happened to load.
			var rootModule *frozenModule
			for index := range handoff.Compilation.Modules {
				if handoff.Compilation.Modules[index].ID == handoff.Compilation.Root {
					rootModule = &handoff.Compilation.Modules[index]
					break
				}
			}
			if rootModule == nil {
				t.Fatalf("root module %d not found in Modules", handoff.Compilation.Root)
			}
			if len(rootModule.Imports) != 1 {
				t.Fatalf("root module has %d imports, want exactly 1: %+v", len(rootModule.Imports), rootModule.Imports)
			}
			importedID := rootModule.Imports[0].Target
			if importedID == 0 || importedID == handoff.Compilation.Root {
				t.Fatalf("root module's import target is invalid: %d", importedID)
			}
			if !modulesByID[importedID] {
				t.Fatalf("root module imports module %d, which is not in Modules", importedID)
			}
			rootIndex, importedIndex := -1, -1
			for index, depID := range handoff.Compilation.DependencyOrder {
				if depID == handoff.Compilation.Root {
					rootIndex = index
				}
				if depID == importedID {
					importedIndex = index
				}
			}
			if importedIndex == -1 || rootIndex == -1 || importedIndex >= rootIndex {
				t.Fatalf("DependencyOrder %v does not place imported module %d before root %d",
					handoff.Compilation.DependencyOrder, importedID, handoff.Compilation.Root)
			}
		})
	}

	// Test recovery cases: single-file .peb programs with known behaviors.
	recoveryPath := "../../../tests/check/facts/recovery/handoff_*.peb"
	recoveryPaths, err := filepath.Glob(recoveryPath)
	if err != nil || len(recoveryPaths) == 0 {
		t.Fatalf("glob %s: %v", recoveryPath, err)
	}
	sort.Strings(recoveryPaths)

	// Define expected behavior per recovery file.
	recoveryExpectations := map[string]struct {
		wantGenerationErrors bool
		wantSemantics       bool
		wantSolutionSuccess bool
		wantDiagnostics     bool
	}{
		"handoff_type_conflict.peb": {
			wantGenerationErrors: true,  // Type conflict -> GenerationHadErrors
			wantSemantics:        true,  // Semantics still populated
			wantSolutionSuccess:  false, // Solution should fail
			wantDiagnostics:      true,  // Has conflict errors
		},
		"handoff_empty_module.peb": {
			wantGenerationErrors: false, // Empty module is valid
			wantSemantics:        true,
			wantSolutionSuccess:  true,
			wantDiagnostics:      false, // No errors
		},
		"handoff_multi_function.peb": {
			wantGenerationErrors: false, // Complex but valid
			wantSemantics:        true,
			wantSolutionSuccess:  true,
			wantDiagnostics:      false, // No errors
		},
	}

	for _, path := range recoveryPaths {
		fileName := filepath.Base(path)
		expect, ok := recoveryExpectations[fileName]
		if !ok {
			// Skip recovery files not in our expectations map.
			t.Logf("skipping recovery fixture without explicit expectations: %s", fileName)
			continue
		}

		t.Run("recovery_"+fileName, func(t *testing.T) {
			contents, err := os.ReadFile(path)
			if err != nil {
				t.Fatal(err)
			}

			inputs, diagnostics := factInputs(t, checkProvider{"main.peb": contents})
			handoff := run06a(inputs, diagnostics, Config{})

			if handoff == nil {
				t.Fatal("run06a returned nil")
			}

			// Check GenerationHadErrors.
			if handoff.GenerationHadErrors != expect.wantGenerationErrors {
				t.Fatalf("GenerationHadErrors=%v, want %v; diagnostics=%+v",
					handoff.GenerationHadErrors, expect.wantGenerationErrors, diagnostics.Items())
			}

			// Check Semantics is populated.
			if (handoff.Semantics != nil) != expect.wantSemantics {
				t.Fatalf("Semantics != nil is %v, want %v", handoff.Semantics != nil, expect.wantSemantics)
			}

			// Check Solution success.
			if handoff.Solution != nil && handoff.Solution.Successful() != expect.wantSolutionSuccess {
				t.Fatalf("Solution.Successful()=%v, want %v", handoff.Solution.Successful(), expect.wantSolutionSuccess)
			}

			// Check diagnostic presence.
			hasDiagnostics := diagnostics.HasErrors()
			if hasDiagnostics != expect.wantDiagnostics {
				t.Fatalf("HasErrors()=%v, want %v; items=%+v", hasDiagnostics, expect.wantDiagnostics, diagnostics.Items())
			}

			// Assert no generation inconsistency codes.
			for _, item := range diagnostics.Items() {
				if item.Code == CodeGeneration {
					t.Fatalf("fixture produced a generation inconsistency: %+v", item)
				}
			}
		})
	}
}
