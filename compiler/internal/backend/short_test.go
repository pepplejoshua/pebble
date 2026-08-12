package backend

import "testing"

// requireCIntegration keeps real C compilation and process execution out of
// Go's standard short-test mode. Emit-only tests still run in short mode.
func requireCIntegration(t *testing.T) {
	t.Helper()
	if testing.Short() {
		t.Skip("skipping C compile-and-run integration test in short mode")
	}
}
