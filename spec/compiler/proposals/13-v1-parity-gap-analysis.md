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

*(empty — F5-01 (generic tagged-union method switch subject) closed in
`73245a0`. Root cause was checker-side: `buildMethodCall`'s specialization
trigger required `methodSymbol.Generic`, true only for a method declaring
its own type parameters (`map[U]`), so a method that only INHERITS type
parameters from its containing generic type (`is_ok`, `unwrap_or`) never
got a concrete specialization — `self` kept the unspecialized template
TypeID at every call site, confirmed via `tirdump`. Fixed by triggering
specialization on non-empty `signature.TypeParams` alone. Broke
`std/result.peb` end to end before the fix; `is_ok`/`unwrap_or` now
verified compiling and running correctly. A genuinely separate,
pre-existing bug was found during investigation and deliberately NOT
fixed: two DIFFERENT instantiations of the same generic tagged union
(`Result[int,str]` + `Result[bool,str]` both live at once) emit
duplicate C enumerators — now its own tracker 14 row, F5-01b. Picking up
F5-02 next (generic untagged-union field specialization — a generic
untagged union instantiated with a scalar payload reaches Emit with the
field type still recorded as a type parameter; per the audit's own note,
do not combine with F5-01 unless one root cause is proven — check
current state for staleness first, per the established pattern).)*
