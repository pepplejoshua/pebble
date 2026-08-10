// Package prelude is the source of truth for the compiler's implicit runtime
// prelude: the Allocator and Context type declarations every Pebble-convention
// program sees without an explicit import. The .peb source here is embedded
// into any binary that imports this package, so the prelude resolves with no
// filesystem discovery at runtime and travels with the compiled compiler.
package prelude

import "embed"

//go:embed runtime.peb
var FS embed.FS
