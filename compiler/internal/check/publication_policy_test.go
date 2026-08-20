package check

import "testing"

func TestCanContinueWithPartial(t *testing.T) {
	cases := []struct {
		name    string
		handoff *solveHandoff
		config  Config
		want    bool
	}{
		{name: "nil", config: Config{}, want: false},
		{name: "hard failure without opt in", handoff: &solveHandoff{GenerationFailed: true}, want: false},
		{name: "hard failure with opt in", handoff: &solveHandoff{GenerationFailed: true, GenerationHadErrors: true}, config: Config{AllowPartialOnRecoveredErrors: true}, want: false},
		{name: "clean default", handoff: &solveHandoff{}, config: Config{}, want: true},
		{name: "diagnostic default", handoff: &solveHandoff{GenerationHadErrors: true}, config: Config{}, want: false},
		{name: "diagnostic opt in", handoff: &solveHandoff{GenerationHadErrors: true}, config: Config{AllowPartialOnRecoveredErrors: true}, want: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			if got := canContinueWithPartial(tc.handoff, tc.config); got != tc.want {
				t.Fatalf("canContinueWithPartial() = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestRecoveredSyntaxCanPublishOnlyWithOptIn(t *testing.T) {
	source := []byte(`
type Unit = struct {};
fn healthy(value Unit) Unit => value;
fn
`)

	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": source})
	if !diagnostics.HasErrors() {
		t.Fatal("fixture should produce a recovered syntax diagnostic")
	}
	withoutOptIn := Check(inputs, diagnostics, Config{})
	if withoutOptIn.IR() != nil {
		t.Fatal("default configuration must not publish IR after a diagnostic")
	}

	partialInputs, partialDiagnostics := factInputs(t, checkProvider{"main.peb": source})
	withOptIn := Check(partialInputs, partialDiagnostics, Config{AllowPartialOnRecoveredErrors: true})
	if withOptIn.Solution() == nil {
		t.Fatal("partial configuration should preserve the completed solution")
	}
	if withOptIn.IR() == nil {
		t.Fatal("completed recovered-syntax fixture should publish IR with opt-in")
	}
}

func TestPartialPublicationStillRejectsValidationFailure(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn conflict() void {
    let x u8 = 1;
    let y i32 = 2;
    let z i32 = x + y;
}
`)})

	result := Check(inputs, diagnostics, Config{AllowPartialOnRecoveredErrors: true})
	if result.IR() != nil {
		t.Fatal("partial publication must not hide a genuine validation failure")
	}
}
