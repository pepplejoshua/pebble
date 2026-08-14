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

*(empty — F5-02 (generic untagged-union field specialization) closed in
`eee586e`. Root cause mirrored F5-01's shape but was independent, as the
audit anticipated: `resolveStructInfo` recovers a generic instantiation's
concrete type arguments and substitutes each field's type via
`structSubstitutions`/`snapshot.Substitute` before it reaches C-type
naming; `resolveUntaggedUnionInfo` (Phase 3 #51) never did this, reading
each member's type directly from `typeDecl.MemberTypes` with no
substitution, so a generic union's field reached Emit still carrying the
raw type-parameter symbol. Fixed by mirroring `resolveStructInfo`'s
logic exactly (same helper, no new machinery). Two follow-ups were
needed during verification: a first implementation pass deleted the
existing non-generic rejection test coverage without replacing it
(caught during review, restored via a small dispatch), and the
supervisor's own full-suite run caught a genuine test-authoring bug (a
u64 test asserting 999 as a process exit code, which truncates to a
single byte — 999 mod 256 = 231 — not an implementation bug, fixed by
switching to the file's established in-program-comparison pattern).
Also reconfirmed (memory updated): `-parallel 12` full-suite runs can
still show rotating, unrelated loop-test contention flakiness (always
exit -1, never the same test twice); `-parallel 4` gave a clean run.
Picking up F5-03/F5-04 next (`str` reassignment from another local and
from a call — `buildStoreCore`'s `str` branch only accepts
`StringLiteral`/`InterpolatedString` sources; V1 handles both cases as
ordinary string-view assignment — check current state for staleness
first, per the established pattern).)*
