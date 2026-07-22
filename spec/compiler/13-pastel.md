# Pastel Formatting Contract

Pastel is the compiler's small, deterministic terminal-formatting package. It
turns trusted markup into either ANSI text or plain text. It does not own
diagnostics, terminal detection, output policy, or user-facing messages.

## Package boundary

**Required:**

- the package lives at `compiler/internal/pastel`;
- it imports only the Go standard library;
- it must not import `diagnostic`, `source`, `syntax`, or any other compiler
  package;
- compiler renderers may depend on Pastel, but Pastel never depends on them;
- it has no package-global mutable state and never writes directly to stdout or
  stderr.

This dependency direction keeps terminal presentation replaceable and keeps
color out of compiler semantics and tests.

## Public API

The first implementation exposes this surface:

```go
package pastel

import "io"

type Mode uint8

const (
	Plain Mode = iota
	ANSI
)

func Format(input string, mode Mode) (string, error)
func Write(w io.Writer, input string, mode Mode) error

type SyntaxError struct {
	Offset  int
	Message string
}

func (e *SyntaxError) Error() string
```

`Plain` is the zero value so tests, redirected output, and accidental default
construction remain color-free. `Write` must produce no partial output when
markup validation fails; it may implement that guarantee by calling `Format`
before writing.

Pastel does not inspect environment variables, file descriptors, `TERM`, or
whether a writer is a terminal. The driver chooses the mode.

## Markup

A formatted region has this shape:

```text
*[option, option]text[/]
```

Regions may nest. Closing `[/]` restores the complete parent style.

Supported style options are:

| Meaning | Short forms |
| --- | --- |
| bold | `*`, `b` |
| underline | `_`, `u` |
| italic | `/`, `i` |
| strikethrough | `~`, `s` |
| dim | `d` |
| reverse | `r` |

Supported color names are `black`, `red`, `green`, `yellow`, `blue`,
`magenta`, `cyan`, and `white`.

- `red` selects the bright foreground variant and is shorthand for `l_red`;
- `l_red` explicitly selects the bright foreground variant;
- `d_red` selects the dark/standard foreground variant;
- `red:blue` selects foreground and background;
- `:blue` selects only the background;
- light/dark prefixes apply independently on either side of `:`.

Options are comma-separated and surrounding ASCII whitespace is ignored.
Option and color names are case-sensitive. The initial port has no markup
escape syntax.

## Validation

**Required:** malformed markup returns `*SyntaxError`; it must never panic,
silently discard text, or emit partial output. The offset is a zero-based byte
offset into the input.

Errors include:

- an unterminated `*[...]` opener;
- an unmatched `[/]` closer;
- an unclosed formatted region at end of input;
- an empty, unknown, or malformed option;
- an unknown color;
- nesting deeper than 64 regions.

The parser preserves arbitrary valid UTF-8 text byte-for-byte. Markers and
options are ASCII. Invalid UTF-8 is not Pastel's concern because Go strings may
contain arbitrary bytes; non-marker bytes pass through unchanged.

## Rendering

In `Plain` mode, markup is removed and text content is preserved exactly. No
ANSI bytes are emitted.

In `ANSI` mode:

- styles use standard SGR escape sequences;
- closing a region resets SGR state and reapplies every active parent style;
- plain input with no formatted region receives no gratuitous reset sequence;
- output ends with the terminal's default style whenever any style was
  activated;
- repeated calls are independent and deterministic.

Pastel makes no promise that its ANSI byte sequence is minimal. It promises
that the visible style and final reset state are correct.

## Trust boundary

Only compiler-owned format templates are Pastel markup. Source text, paths,
symbol names, diagnostic messages, help text, and toolchain output are
untrusted plain text and must never be passed through `Format` as part of a
larger markup string.

The diagnostic renderer must format trusted fragments such as `error[E0001]`
separately, then write dynamic text directly. This prevents a Pebble program
containing `*[red]` or `[/]` from changing terminal styles or breaking
diagnostic rendering.

## Tests and completion criteria

Pastel is infrastructure rather than Pebble language behavior, so focused Go
unit tests are appropriate. Tests must cover:

- every style and color form in both modes;
- nested restoration;
- Unicode text preservation;
- all validation failures and their byte offsets;
- no partial writes on syntax errors;
- short-writing and failing `io.Writer` implementations;
- deterministic repeated calls;
- absence of ANSI bytes in `Plain` mode.

The port is complete when `go test ./...` passes from `compiler`, its public API
matches this document, and no compiler package is imported by Pastel.
