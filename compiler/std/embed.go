// Package std is the source of truth for the Pebble standard library. The
// .peb sources here are embedded into any binary that imports this package, so
// `import "std:..."` resolves with no filesystem dependency at runtime.
package std

import "embed"

//go:embed *.peb mem
var FS embed.FS
