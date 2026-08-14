# 13 — v1 parity gaps

**Purpose.** This file is the working area for exactly one active defect.
Proposal 14 is the full parity ledger and the open backlog. Closed work does
not stay in this file.

## Workflow

1. Pick one open item from proposal 14's fifth-pass table.
2. Reproduce it with the current compiler.
3. Record its minimal source, exact failure, root cause, and scope below.
4. Dispatch one small implementation slice through Orc.
5. Review the diff, remove scratch work, run `gofmt`/`go vet` plus the
   package(s) actually touched, and do a causation check. **Do not run the
   full backend/repo test suite per item** — it's a periodic checkpoint
   (roughly every 5 items), not a per-item gate; use `-parallel 12` when a
   checkpoint run does happen, to reduce contention-driven flaky loop/break
   failures.
6. Commit and push the verified fix. Update proposal 14 with the closing
   commit, then clear the active defect from this file.
7. Do not start another implementation worker until the current worker has
   finished and the worktree is clean.

## Dispatch rules

- Use `orc run --claude` when dispatching as Claude (this assistant); use the
  attribution flag matching whichever assistant is actually dispatching.
- Alternate `opencode-go/deepseek-v4-flash` and `vercel/alibaba/qwen3.7-flash`
  per dispatch for routine/mechanical tasks.
- `opencode-go/mimo-v2.5` and `opencode-go/kimi-k2.7-code` are banned.
- Use `openai/gpt-5.6-luna` only after a real failure from the primary
  rotation, unless the user explicitly selects Luna.
- Never use `openai/gpt-5.6-sol`.
- Do not use `openai/gpt-5.6-terra` without user approval.
- Resume a stalled session (`orc run --session <id>`) rather than
  re-dispatching fresh. **`orc delete`/`--clean`/`--delete` is permanently
  banned for any session, any status** — never run it. If a "failed" session's
  independently-verified diff is actually correct, run `orc complete <id>`
  instead; this only corrects the status field and destroys no data.
- Keep implementation tasks sequential. Parallel workers are permitted only
  for read-only work when the user explicitly requests them.
- Before an implementation dispatch, require a clean worktree and no active
  Orc or OpenCode worker for this repository.
- After a dispatch, inspect the real diff. Check for scratch files, debug
  output, scope growth, and stale tests. Do not trust the worker summary.

## Active defect

*(empty — F5-06 (interpolation of a `char` value part) closed in
`170ff96`. The scalar is UTF-8-encoded via the existing
`pebble_rt_char_to_utf8`, reused from the char-print path. A real
NUL-char (Unicode scalar 0) regression surfaced during verification:
the write pass's first draft reused the int/float cases' `strlen`-
based copy, which returns 0 for a buffer whose first byte is `0x00`,
silently dropping the character and desyncing every later part's
offset — fixed by encoding directly into the destination and using
the encoder's own return value as the byte count, not `strlen`. Two
follow-up dispatches were needed: one to fix the NUL bug itself (a
session that stalled mid-edit left a genuinely broken, uncompilable
`#if 0` with no `#endif` in `str.c` — caught by grep, not trusted from
the "completed" report, fixed with a second tiny dispatch), and one to
rescope 3 Go test cases that asserted on `print` OUTPUT of a
NUL-containing string — `print`'s C emission
(`fprintf(..., "%s", ...)`) truncates at any embedded NUL for ANY
`str` value, confirmed independent of interpolation or char work
entirely (`print "x\0y";` already truncates on plain `HEAD`); rescoped
those 3 cases to assert on `.len` materialization instead (which IS
correct), and logged the print-NUL-truncation limitation as its own
new backlog row (F5-06b) rather than trying to fix it here. Full
`internal/backend` checkpoint hit the same known rotating loop/while
flakiness at `-parallel 12` (exit -1, unrelated tests); confirmed
flaky by isolation rerun, clean at `-parallel 4`. Causation-checked
via file-copy swap against HEAD.

Picking up F5-07 next (interpolation of an enum value part — same
"want bool, char, an integer type, a float type, or a str type"
rejection, confirmed live with a plain enum:
`type Color = enum { red, green, blue }; ... print `color={c}`;`
fails; V1 formats the variant name. A bare (non-interpolated)
`print c;` of an enum ALREADY works today — `buildEnumPrintValueCalls`
(`statements.go`) emits a runtime C switch over the enum's
discriminant, each case `fprintf`-ing a STATIC string
(`"TypeName.variantName"`), with a defensive default case for an
invalid discriminant. Interpolation needs the SAME switch shape but
producing a `PebbleStr` VALUE (assigned to a temp via a pre-statement
switch) instead of directly calling `fprintf` — each case assigns the
temp from a `{ .data = (const uint8_t *)"...", .len = N }` PebbleStr
literal (the same literal-construction shape `buildStrLiteralValue`
already uses for a bare string literal), then the temp is referenced
as a normal `{ PEBBLE_STR_PART_STR, .str_value = <temp> }` entry —
reusing the already-existing str-part machinery from F5-05, not
inventing new runtime plumbing. This is architecturally different
from bool/int/float/char/str (all of which build a single value
expression); enum needs a pre-statement switch block, closer in shape
to how `buildScalarPrintParts`' char case already threads a
pre-statement buffer. Scope to plain (non-union) enums only — tagged
unions are a separate, more complex follow-up (payload recursion),
matching how the print matrix split plain-enum (composite print slice
5) from tagged-union (slice 6) work previously.)*
