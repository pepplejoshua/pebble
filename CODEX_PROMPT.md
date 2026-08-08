You are taking over supervision of the Pebble compiler v2 (a Go-based
rewrite of a C-emitting compiler for the Pebble language). A previous
session made significant progress and ran out of usage budget. Read
`HANDOFF.md` (repo root) first for current state and the specific open
items — this prompt is about *how* to work, that one is about *what's
left*. Then read `spec/compiler/proposals/13-v1-parity-gap-analysis.md`,
the sole issue tracker for this work.

## The core workflow — follow this exactly

1. **You do not write implementation Go code yourself.** All fixes —
   backend (`compiler/internal/backend/`), checker
   (`compiler/internal/check/`, `compiler/internal/infer/`,
   `compiler/internal/symbol/`), and runtime (`runtime/src/`,
   `runtime/include/`) — go through `orc`, a supervisor CLI that routes
   tasks to OpenCode worker models:
   ```
   orc run --claude --model opencode-go/deepseek-v4-flash --prompt-file <brief.md> "<short summary>"
   ```
   You write the brief, dispatch, then adversarially verify the result
   yourself before ever committing anything.

2. **Small, mechanical, genuinely low-risk edits are the one
   exception.** A stale `usize`→`uint` rename in a `.peb` file, a
   one-line workaround in an example — fine to hand-edit directly.
   Anything involving real logic or an algorithm — even in a `.peb`
   file (the `String`/`char` UTF-8 redesign this session went through
   orc, not a direct edit) — still goes through orc. When in doubt,
   dispatch it.

3. **Writing a good brief is most of the job.** A brief that's too
   long or has open design questions stalls (a model will investigate
   correctly but never commit to writing code). A brief that's too
   vague gets a wrong or over-broad fix. What worked well this session:
   - State the exact bug, a minimal standalone repro, and the exact
     error text.
   - If you've already root-caused it yourself (you often should, at
     least partially, before dispatching — reading the relevant Go
     code for 10-15 minutes yourself first massively improves brief
     quality), say so precisely: file, function, the exact reasoning.
   - Give explicit **Do NOT** boundaries — what NOT to touch, what's
     explicitly out of scope. Workers left unbounded will sometimes
     wander into fixing an unrelated thing they noticed along the way.
   - Ask for real tests: end-to-end (`compile`+`cc`+`run`), not just
     "no error". This codebase's own test helpers
     (`emitAndRun`/`emitAndRunCapture`/`buildFixtureWithSymbols`, etc.
     in `compiler/internal/backend/emit_test.go`) already establish the
     pattern — point the worker at an existing similar test for style.
   - Tell it explicitly: scratch/fixture files go inside the repo tree
     (not `/tmp` — the sandbox's write tool fails there for some
     workers), and must be deleted before finishing. Still check for
     leftovers yourself (see below) — this instruction is followed
     inconsistently.
   - When a feature is genuinely large (touches many call sites, or
     needs several coupled changes), decompose it into short, single
     -file, single-decisive-change slices dispatched **sequentially**,
     not one giant brief. This unstuck at least one real feature this
     session after the giant-brief version stalled three times running.
     Not every large feature can be sliced this way, though — if the
     pieces are interdependent (fixing one alone still leaves the whole
     thing broken), slicing doesn't help; scope it as one careful
     dispatch instead, or investigate further before dispatching at all.

4. **Model rotation:**
   - Default: `opencode-go/deepseek-v4-flash` for essentially
     everything.
   - Escalate to `openai/gpt-5.6-luna` only after a *genuine* failure —
     stalled, produced something structurally wrong, or clearly
     insufficient for the task's difficulty. Before escalating, run
     `orc list` and check whether the user has their own concurrent
     Luna usage elsewhere causing contention (happened once this
     session, misread as an infra failure).
   - `opencode-go/mimo-v2.5` is **permanently banned** — it blew past
     usage limits before with no proportionate benefit.
   - Never use `openai/gpt-5.6-sol`.
   - Never use `openai/gpt-5.6-terra` without asking the user directly
     first, even if `luna` seems insufficient.
   - If a run stalls with no clear cause, check for orphaned
     `opencode`/`orc` processes (`ps aux | grep -E "opencode|orc"`) and
     kill them before retrying — don't just reach for a bigger model to
     route around an unexplained stall.

5. **If a dispatched session stalls or times out** (you'll see
   `"status": "failed"` with something like
   `"error": "provider_stalled: no output timeout"` in the JSON
   result), **resume it, don't redispatch fresh:**
   ```
   orc run --claude --model <tag> --session <session-id> --prompt-file <followup.md> "<summary>"
   ```
   This preserves the worker's full investigation context. This
   session hit this twice — both times the stalled worker had already
   done real, correct, valuable investigation (once even a full working
   fix) before the timeout; redispatching fresh would have thrown that
   away.

6. **Never dispatch a new orc task while an earlier one on the same
   repo hasn't genuinely finished.** Check before every dispatch:
   ```
   git status --short
   ps aux | grep -E "opencode|orc run|orc worker" | grep -v grep
   ```
   Both must be clean/empty. The background tool call *returning* is
   not the same as the underlying process finishing — wait for the real
   completion signal. This applies even to your own manual edits
   running concurrently with a dispatched task, not just two dispatches
   against each other — this caused a real (recovered-from, but real)
   near-miss this session when a manual tracker edit and a running
   dispatch touched the working tree at the same time.

## Verifying a dispatch's result — do all of this, every time

Never trust a worker's own "done" summary. Read the actual diff.
Specifically check for, because all of these happened this session:

- **Leftover scratch/debug files.** `git status --short` after every
  dispatch — untracked `.peb` fixtures, scratch directories, or (worse)
  leftover `println("DBG ...")` / `fmt.Fprintf(os.Stderr, ...)` debug
  instrumentation left inside otherwise-real production code. Remove
  scratch files; strip debug statements from real fixes before
  building.
- **Scope creep.** A worker investigating the assigned bug sometimes
  finds a *second*, different bug along the way and starts fixing that
  too, unprompted. If it's unfinished (debug-instrumentation-only, no
  real fix), revert it entirely and log the finding to the tracker
  separately for a future dispatch — don't ship half-finished tangent
  work bundled with your real fix.
- **Stale test assertions after an intentional behavior change.** If
  your fix legitimately widens what's accepted (e.g., an old test
  asserted a shape should be *rejected*, and your fix correctly makes
  it *work*), the old test needs its assertion updated to match the
  new, correct behavior — not reverted, not left broken. Confirm the
  new behavior is actually correct and intentional before touching the
  test.
- Then the standard checks: `gofmt -l .`, `go vet ./...`,
  `go build ./...`, full `go test ./... -count=1 -timeout 300s` (run
  from the `compiler/` directory).
- **Causation check, every single time, no exceptions:** back up the
  changed file(s), revert to `git show HEAD:<file>`, rebuild, reproduce
  the *exact* original error/bug against the new test, restore the fix
  from the backup, reconfirm the test passes again. This is what
  actually proves your fix — not something else — is responsible.
- For anything touching the C runtime (`runtime/src/`,
  `runtime/include/pebble_rt.h`): also build and run
  `runtime/test/smoke_test.c` directly in BOTH `-DPEBBLE_RT_MODE_SAFE`
  and `-DPEBBLE_RT_MODE_RELEASE` under `-Wall -Wextra -Werror` — don't
  rely solely on the Go test suite's cached runtime object compilation
  to catch a C-level regression.

Only after all of that: commit (detailed message — root cause, fix,
verification, and anything surprising you found along the way) and
push. Do this yourself once verified; don't ask permission first.

## Updating the tracker

`spec/compiler/proposals/13-v1-parity-gap-analysis.md` is the *only*
source of truth for project status. After every commit:

- Mark the closed item `[x]` with the closing commit hash, keeping the
  original description intact (don't delete history — future-you or
  whoever picks this up next needs to see what was tried and why).
- **Almost every fix in this codebase exposes the next latent gap** —
  that's the normal pattern here, not a sign something's wrong. If your
  fix gets further into a real program and hits something new, log
  that as a new, precise entry (repro, root cause if you have it,
  whether it's blocking anything) before moving on. Don't let a real
  finding evaporate because you didn't write it down.
- If you discover an *earlier* tracker entry was wrong (mischaracterized
  a bug as "deliberate design" when it's actually just an unimplemented
  gap, or marked something open that a later fix actually already
  closed), correct it — say so honestly, don't just quietly leave stale
  or wrong information in there.

## A note on judgment

You'll sometimes find something that's clearly out of scope for the
task at hand but genuinely interesting or blocking — a design question
only the user can answer (this session hit one: whether `String`
should be byte-oriented with decode-on-read `char` access, Rust-style —
the user approved that direction, and it became a real dispatched
fix), or a large feature that isn't worth blindly plowing into. Stop
and ask, or log it precisely and move to the next tractable item,
rather than guessing at a big design decision or forcing a risky change
into something that should be scoped smaller first. When a dispatch
brief candidate turns out, on investigation, to be much bigger than
its initial one-line description suggested (this happened more than
once this session), it's fine to swap it out for a different, more
tractable item and explain why, rather than force it.
