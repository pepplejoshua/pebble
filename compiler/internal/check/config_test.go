package check

import "testing"

func TestNormalizeConfig_06bDefaults(t *testing.T) {
	c := normalizeConfig(Config{})
	if c.MaxValidationSteps != DefaultMaxValidationSteps {
		t.Errorf("MaxValidationSteps: got %d, want %d", c.MaxValidationSteps, DefaultMaxValidationSteps)
	}
	if c.MaxIRNodes != DefaultMaxIRNodes {
		t.Errorf("MaxIRNodes: got %d, want %d", c.MaxIRNodes, DefaultMaxIRNodes)
	}
	if c.MaxIRComponents != DefaultMaxIRComponents {
		t.Errorf("MaxIRComponents: got %d, want %d", c.MaxIRComponents, DefaultMaxIRComponents)
	}
	if c.MaxFlowStates != DefaultMaxFlowStates {
		t.Errorf("MaxFlowStates: got %d, want %d", c.MaxFlowStates, DefaultMaxFlowStates)
	}
	if c.MaxDeferEdges != DefaultMaxDeferEdges {
		t.Errorf("MaxDeferEdges: got %d, want %d", c.MaxDeferEdges, DefaultMaxDeferEdges)
	}
	if c.MaxDumpBytes != DefaultMaxDumpBytes {
		t.Errorf("MaxDumpBytes: got %d, want %d", c.MaxDumpBytes, DefaultMaxDumpBytes)
	}
}

func TestNormalizeConfig_06bPreservesNonzero(t *testing.T) {
	c := normalizeConfig(Config{
		MaxValidationSteps: 42,
		MaxIRNodes:         99,
		MaxIRComponents:    7,
		MaxFlowStates:      13,
		MaxDeferEdges:      55,
		MaxDumpBytes:       888,
	})
	if c.MaxValidationSteps != 42 {
		t.Errorf("MaxValidationSteps: got %d, want 42", c.MaxValidationSteps)
	}
	if c.MaxIRNodes != 99 {
		t.Errorf("MaxIRNodes: got %d, want 99", c.MaxIRNodes)
	}
	if c.MaxIRComponents != 7 {
		t.Errorf("MaxIRComponents: got %d, want 7", c.MaxIRComponents)
	}
	if c.MaxFlowStates != 13 {
		t.Errorf("MaxFlowStates: got %d, want 13", c.MaxFlowStates)
	}
	if c.MaxDeferEdges != 55 {
		t.Errorf("MaxDeferEdges: got %d, want 55", c.MaxDeferEdges)
	}
	if c.MaxDumpBytes != 888 {
		t.Errorf("MaxDumpBytes: got %d, want 888", c.MaxDumpBytes)
	}
}

func TestNormalizeConfig_EntryPointZeroValue(t *testing.T) {
	c := normalizeConfig(Config{})
	if c.Entry.Mode != 0 {
		t.Errorf("Entry.Mode: got %d, want 0", c.Entry.Mode)
	}
	if c.Entry.Symbol != 0 {
		t.Errorf("Entry.Symbol: got %d, want 0", c.Entry.Symbol)
	}
}
