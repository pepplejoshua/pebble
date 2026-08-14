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

*(empty — F5-05 (interpolation of a `str` value part) closed in
`a785060`. A `str` needs no formatting/snprintf at all: it already
carries `.data`/`.len`, so interpolating it is a direct memcpy of its
own bytes, the same as the existing literal-text part. Added
`PEBBLE_STR_PART_STR` to `PebbleStrPartKind`/`PebbleStrPart`, a
matching case in `pebble_rt_str_from_parts`, and widened
`buildInterpolatedStringParts` plus `buildPrint`'s combined and
sequential paths to build the value via `buildStrOperand` (the same
helper F5-03/F5-04 relied on). New runtime smoke coverage (empty str,
str+text, multiple strs, str mixed with bool/int/float) and Go
end-to-end tests across local-init, call-argument, return, comparison,
reassignment, and print positions — all independently reviewed file by
file before verification. Dispatch's own report was a terse,
early-stalled "failed"/`provider_stalled` message despite substantial
real, correct work (359 lines across 6 files) — not trusted at face
value, verified fully as always. Full `internal/backend` checkpoint
hit the known rotating loop-test flakiness at `-parallel 12`
(unrelated range-loop width-matrix tests, exit -1); confirmed flaky by
isolation rerun (all pass standalone), clean at `-parallel 4` per the
established fallback. Causation-checked via file-copy swap against
HEAD (original repro fails with the exact pre-fix error; passes again
once restored).

Picking up F5-06 next (interpolation of a `char` value part — same
"want bool, an integer type, or a float type" rejection, confirmed
live with `var c char = 'x'; print `hello {c}`;`. V1 formats the
character. A reusable building block already exists:
`pebble_rt_char_to_utf8(int32_t scalar, uint8_t out[5])` in
`runtime/src/str.c`/`pebble_rt.h` — already used by the "char-to-UTF-8
encoding" smoke test — encodes a Unicode scalar into up to 4 UTF-8
bytes plus a length. Unlike the int/float parts, this needs a small
fixed scratch buffer (5 bytes) per part, similar in shape to those
cases but calling this existing encoder instead of `snprintf`.
Include both ASCII and multi-byte Unicode (e.g. `'é'`, matching Phase
3 #52's non-ASCII precedent) in the proof.)*
