# Task: fix print's %c formatting for a multi-byte Unicode char value

## The bug

`print`'s char-operand handling always emits a single C `%c` format
specifier, which can only ever output ONE byte after C's integer
promotion — so a `char` value outside ASCII range (any Unicode scalar
requiring more than 1 UTF-8 byte) prints as garbage instead of the
correct multi-byte UTF-8 sequence.

Reproduce standalone:

```
fn main() int {
    print 'é';
    return 0;
}
```

Prints a replacement/garbage character instead of `é` (U+00E9, a
2-byte UTF-8 sequence: `0xC3 0xA9`). Confirmed this is unrelated to
`String`/`std/string.peb` — a bare char literal touching no `String`
code at all reproduces it, so this is purely `print`'s own
char-formatting.

## Location

`compiler/internal/backend/emit.go`, function `buildPrint`, the
`case kind == types.Char:` branch (search for `case kind ==
types.Char:`) — currently:

```go
case kind == types.Char:
    // A char operand prints as the single character its int32_t C
    // value encodes; the value is built under the char grammar.
    formatParts = append(formatParts, `"%c"`)
    if isSliceIndex {
        sliceIndexPre, arg, err = buildSliceIndexValue(unit, snapshot, fileSet, operandID, child, scope, width, false)
    } else {
        arg, err = buildCharOperand(unit, snapshot, fileSet, operandID, scope, width)
    }
```

## Design constraint — read the whole function first

`buildPrint`'s own top comment documents that a print statement with
MULTIPLE operands emits exactly ONE combined `printf` call, not one
call per operand (`print a, b, c;` → one `printf("<fmt for
a><fmt for b><fmt for c>\n", a, b, c);`). A char operand's value is a
RUNTIME expression (not always a compile-time constant), so encoding
it to UTF-8 bytes must happen at RUNTIME in the emitted C — and since
it needs to be usable as a single combined-printf argument (a `%s`
-style C string), the encoded bytes need to live in a small buffer
BEFORE the printf call runs.

`buildPrint` ALREADY has a mechanism for exactly this shape: it
returns a `(pre, expr, error)` triple (search for the `sliceIndexPre`/
`preParts` handling in this same function, added for the recent
slice-indexing fix) — a leading pre-statement declared before the
final combined `printf(...)` line. Follow this SAME pattern: encode
the char operand into a small stack buffer via a NEW leading statement
(a call to a new runtime helper, most likely — see below), then use
`%s` with that buffer as the printf argument instead of `%c` with the
raw int32 value.

## The encoding algorithm (reference, do not reinvent)

The exact UTF-8 encoding algorithm was just implemented in pure Pebble
arithmetic in `compiler/std/string.peb`'s `push_char` method (added
earlier this session for the `String`/`char` redesign — read it for
the exact bit-manipulation reference, though you'll be porting the
SAME algorithm to C runtime code here, not reusing the Pebble version
directly, since this needs to run inside the backend's emitted C):

- `cp < 0x80`: 1 byte, the byte itself.
- `cp < 0x800`: 2 bytes, `[0xC0 | (cp >> 6), 0x80 | (cp & 0x3F)]`.
- `cp < 0x10000`: 3 bytes, `[0xE0 | (cp >> 12), 0x80 | ((cp >> 6) &
  0x3F), 0x80 | (cp & 0x3F)]`.
- else: 4 bytes, `[0xF0 | (cp >> 18), 0x80 | ((cp >> 12) & 0x3F), 0x80
  | ((cp >> 6) & 0x3F), 0x80 | (cp & 0x3F)]`.

## Investigation and design needed

1. Decide where the runtime encoding function lives. The most likely
   correct approach: a new small runtime C function in `runtime/src/`
   (check `runtime/src/str.c` for the existing UTF-8 decode
   implementation as a model for style/conventions — mirror its
   structure) — something like `void pebble_rt_utf8_encode(int32_t cp,
   char *out)` that writes the encoded bytes AND a NUL terminator into
   a caller-provided buffer (the caller/emitted-C provides a
   fixed-size local buffer, e.g. `char buf[5];`, big enough for 4 UTF-8
   bytes + NUL). Declare it in `runtime/include/pebble_rt.h` following
   the existing declaration conventions there.
2. In `buildPrint`'s char case, emit a leading pre-statement declaring
   a small stack buffer and calling this new helper to fill it (using
   a uniquely-named temp, following this file's existing temp-naming
   conventions — e.g. something like `pebble_char_buf_<nodeID>`, mirror
   how `buildSliceIndexValue`'s `pebble_slice_index_<id>` naming
   works), then use `%s` with that buffer (not `%c` with the raw
   value) as the printf argument.
3. Thread this pre-statement through the SAME `sliceIndexPre`/
   `preParts` mechanism already in `buildPrint` — a char operand's own
   pre-statement should compose correctly alongside a slice-index
   operand's pre-statement if a single print statement somehow has
   both (unlikely in practice, but the mechanism should not assume
   only one kind of pre exists).
4. Confirm this also correctly handles the `isSliceIndex` branch (a
   char read from `buildSliceIndexValue`, i.e. `print foo()[i]` where
   the element type is `char`) — that value ALSO needs the same UTF-8
   -encode-then-%s treatment, not just the `buildCharOperand` path.

## Do NOT

- Do not change the ONE-combined-printf-per-print-statement invariant
  for any OTHER operand type (bool/str/int/float) — only the char case
  changes.
- Do not touch `buildCharOperand` itself, `buildSliceIndexValue`, or
  any other char-VALUE builder — this fix is scoped to how `buildPrint`
  FORMATS a char value for output, not how char values are built
  elsewhere (a char used in a comparison, stored in a variable, etc.
  is unaffected and should stay exactly as-is).
- Do not attempt to fix `str`'s own printing (`case kind ==
  types.Str:`) — that already correctly handles multi-byte content
  (it prints the str's raw UTF-8 bytes directly via `%s`, already
  correct) and needs no change.
- Do not attempt the `Allocator` record-construction gap, the `C0604`
  signature-preparation gap, or the shift-width gap — separate,
  unrelated, out of scope.

## Tests

Add end-to-end tests in `compiler/internal/backend/emit_test.go`
(follow the existing pattern for a print-capture test — search for
`emitAndRunCapture` usage on a print statement, for the harness style
that captures and asserts on stdout). At minimum:
- `print 'é';` (or another 2-byte character) — capture stdout, assert
  the captured bytes are the CORRECT 2-byte UTF-8 sequence followed by
  the trailing newline `print` always appends (compare against the
  Go string literal `"é\n"`, which Go source already encodes as the
  correct UTF-8 bytes, so a literal Go string comparison is a clean,
  correct assertion).
- A 3-byte and/or 4-byte character too, for full algorithm coverage
  (e.g. `'€'` for 3 bytes, an emoji character for 4 bytes).
- Confirm plain ASCII char printing (`print 'A';`) still works
  correctly and unchanged (a regression guard — the existing ASCII
  print tests should already cover this, but add one if none exist in
  a form that would catch a regression from this specific change).
- A multi-operand print mixing a char with another type (e.g. `print
  'é', 42;`) to confirm the combined-printf-with-pre-statement
  mechanism composes correctly.

## Acceptance criteria

- `go build ./...` and `go vet ./...` clean.
- `gofmt -l .` empty.
- New test(s) pass, end-to-end (real `cc` compile + execution),
  asserting the CORRECT UTF-8 bytes were printed (not just that the
  program exited 0).
- Full `go test ./... -count=1` (from the `compiler/` dir) stays green.
- If you added a new runtime C file/function, confirm the runtime's
  own build/test setup (check `runtime/test/` for existing smoke
  tests) still passes, if applicable.
- Do NOT commit. Leave changes in the working tree for review.

## Scratch files

If you need scratch/fixture `.peb` or C files while investigating,
write them inside the repo's own working tree (not `/tmp`), and delete
them before finishing.
