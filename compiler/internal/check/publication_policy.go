package check

// canContinueWithPartial reports whether a completed handoff may be consumed
// by generation-dependent validation and IR construction. Diagnostics from a
// completed generation are bypassable only with the explicit opt-in; a hard
// generation failure is never bypassable.
func canContinueWithPartial(handoff *solveHandoff, config Config) bool {
	if handoff == nil || handoff.GenerationFailed {
		return false
	}
	return !handoff.GenerationHadErrors || config.AllowPartialOnRecoveredErrors
}
