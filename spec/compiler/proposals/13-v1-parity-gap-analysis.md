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

*(empty — F5-07 (interpolation of a plain-enum value part) closed in
`cd5e3c6`. A plain enum's formatted representation depends on a
runtime tag comparison across N static strings, so it can't become a
single inline `PebbleStrPart` entry; `buildEnumInterpolationSwitch`
emits a pre-statement C switch assigning a temp `PebbleStr` per
variant (reusing F5-05's `PEBBLE_STR_PART_STR` machinery — no new
runtime code), and reuses the existing enum-print naming helpers
(`enumSourceName`/`variantSourceName`/`enumVariantName`) so an
interpolated enum renders identically to the same enum passed straight
to `print`. `buildInterpolatedStringParts`'s signature grew a `[]string`
pre-statements return, threaded through all 3 call sites (local decl,
general expression, and both of `buildPrint`'s combined/sequential
paths). A tagged union (payload-carrying enum) is explicitly rejected
with a clear error, confirmed by test — not silently mishandled.
Also fixed a real collection gap found during this work:
`collectEnumTypesWalk` only followed `node.Children`, missing an enum
referenced only via an interpolation's `node.Parts` (e.g.
`` `pick={Color.green}` ``), leaving its typedef/variant constants
uncollected — the same Parts-not-Children shape `collectDirectCalls`
already closes for helper calls used as interpolated values.

Picking up F5-08 next (interpolation of a struct value part — same
rejection, confirmed live with a plain struct:
`type Point = struct { x int; y int; }; ... print `point={p}`;`
fails; V1 recursively formats the value. A bare (non-interpolated)
`print p;` of a struct ALREADY works today, producing
`Point{ x: 1, y: 2 }` — `buildStructPrintValueCalls` (`statements.go`)
recurses field-by-field through `buildPrintValueCalls`, so a scalar
field produces its own `buildScalarPrintParts` call and a nested
struct/tuple/array field recurses further, joined by static punctuation
text (`"Point{ "`, `": "`, `", "`, `" }"`). Interpolation needs the
same field-by-field formatting but producing ONE materialized
`PebbleStr` (or a sequence of `PebbleStrPart` entries feeding a single
`pebble_rt_str_from_parts` call) instead of a sequence of `fprintf`
calls — closer in shape to F5-07's pre-statement-switch pattern than
to a scalar builder, but with N field parts instead of N enum-variant
cases, and each field's part built via whichever existing scalar/str/
char/enum builder already applies to that field's own type (reusing
the exact same dispatch this task's own F5-05/F5-06/F5-07 slices just
built into `buildInterpolatedStringParts`, not reinventing it).
SCOPE PER THE LEDGER'S OWN GUIDANCE: non-nested structs only first
(every field a scalar/str/char/plain-enum type, no struct/tuple/array
field) — keep nested-field recursion as a separate, harder follow-up
if it turns out not to fall out naturally from reusing the dispatch.)*
