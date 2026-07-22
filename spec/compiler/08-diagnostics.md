# Diagnostics

## Structured diagnostic model

Diagnostics are data until the driver renders them:

```go
type Diagnostic struct {
    Severity Severity
    Code     Code
    Message  string
    Primary  Label
    Related  []Label
    Notes    []string
    Help     []string
}
```

A label contains a span and short message. The renderer obtains source lines
from the source database. Semantic phases do not print directly.

## Required behavior

- Multiple source files may contribute labels to one diagnostic.
- Ordering is deterministic by phase, source, span, and emission order.
- Error counts are derived from stored diagnostics and are never reset to probe
  whether a suboperation failed.
- A phase can checkpoint the diagnostic list length when it needs local success
  information.
- Color is a rendering option; tests use a stable color-free format.
- Human and machine-readable renderers consume the same diagnostic values.

## Recovery

Parser recovery produces explicit missing/error syntax nodes. Semantic recovery
uses `Error` symbols and types. Once an expression contains an error type,
dependent rules avoid repeating the same mismatch unless they add genuinely
new information.

## Parser direction

The rewritten parser's richer reporter and recovery work should be preserved as
a behavior to characterize. The Go rewrite should carry over recovery cases,
not the C diagnostic ownership model.

## Golden form

Diagnostic golden files contain stable, color-free rendered diagnostics. Paths
are normalized relative to the test case. Tests may additionally compare a
structured JSON form when exact label relationships matter.
