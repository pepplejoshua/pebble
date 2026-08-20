package check

// canContinueWithPartial reports whether a completed handoff may be consumed
// by generation-dependent validation and IR construction. Diagnostics from a
// completed generation are bypassable only with the explicit opt-in, and only
// when every existing error is a lexer/parser recovery diagnostic
// (RecoverableDiagnosticsOnly) rather than a name-resolution or checker
// error — the latter mean the program is genuinely semantically broken, and
// no opt-in may paper over that. A hard generation failure is never
// bypassable regardless of diagnostic kind.
func canContinueWithPartial(handoff *solveHandoff, config Config) bool {
	if handoff == nil || handoff.GenerationFailed {
		return false
	}
	if !handoff.GenerationHadErrors {
		return true
	}
	return config.AllowPartialOnRecoveredErrors && handoff.RecoverableDiagnosticsOnly
}
