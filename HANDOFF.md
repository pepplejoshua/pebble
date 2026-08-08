# Handoff — Pebble compiler v2 supervision (2026-08-07)

Written because the current session is out of usage budget. Picking up
supervisor should read this, then `spec/compiler/proposals/13-v1-parity-gap-analysis.md`
(the sole issue tracker for this work — keep it current: new findings
get a precise entry immediately, closed items get `[x]` + the closing
commit hash, never silently removed).

## Standing workflow (read before doing anything)

- **Never write implementation Go code directly.** All fixes go
  through `orc` (a supervisor CLI routing to OpenCode worker models —
  `orc run --claude --model <tag> --prompt-file <brief.md> "<summary>"`).
  You decompose work into a tight, narrow markdown brief per task,
  dispatch, then adversarially verify the diff yourself before
  committing: full diff read, `gofmt -l .`, `go vet ./...`,
  `go build ./...`, full `go test ./... -count=1 -timeout 300s` (from
  `compiler/`), AND a causation check — revert the changed file(s) to
  `git show HEAD:<file>`, reproduce the original bug/error exactly,
  restore the fix, reconfirm it's gone. Only then commit.
- Small, mechanical, low-risk `.peb` file edits (a stale `usize`→`uint`
  rename, a one-line workaround) are fine to hand-edit directly rather
  than dispatch. Anything involving real algorithm/logic — even in a
  `.peb` file (see the `String` redesign below) — still goes through
  orc.
- **Never dispatch a new orc task while an earlier one on the same
  repo hasn't genuinely finished** (check `ps aux | grep opencode` —
  the background tool call returning is NOT the same as the process
  finishing; wait for the real completion notification). This has
  caused real file corruption before, including once when the
  concurrent writer was my own manual edit, not another orc task.
- Model rotation: `opencode-go/deepseek-v4-flash` for routine dispatch.
  Escalate to `openai/gpt-5.6-luna` only after a genuine failure
  (stalled, wrong result), and check `orc list` first for contention
  from the user's own concurrent Luna usage elsewhere. `mimo` is
  permanently banned (blew usage limits before). Never use
  `gpt-5.6-sol`; never use `gpt-5.6-terra` without asking the user
  first.
- **If a dispatched session stalls** (provider timeout, or diagnosed
  correctly but wrote no/incomplete code): resume it via
  `orc run --claude --model <tag> --session <id> --prompt-file <followup.md> "<summary>"`,
  don't redispatch fresh — it keeps full context.
- **Always independently re-verify a dispatch's own "done" claim.**
  Workers sometimes: leave debug `println`/scratch files behind (check
  `git status --short` for untracked files every time, delete before
  committing); assert stale error-message text in a test that should
  have been updated for an intentional behavior change; wander into
  investigating an out-of-scope tangent they found along the way and
  leave half-finished debug instrumentation in otherwise-real fix
  files (happened once this session — reverted the tangent, kept the
  real fix). Read every line of every diff before trusting it.
- Once a fix is verified correct: commit and push yourself, don't ask
  first. Then update the tracker in the same pattern — mark the closed
  item `[x]` with the commit hash, and if the fix exposed a new latent
  gap (extremely common in this codebase — almost every fix this
  session revealed the next layer), log that precisely too before
  moving on.

## Where things stand

The compiler can now compile and run real, substantial Pebble
programs, including full file I/O (`std/io.peb`'s `FILE`-based API is
completely wired up now) and a byte-oriented `String` type with
correct UTF-8 decode-on-read semantics. `examples/read_file.peb` runs
completely correctly end-to-end — that was the single longest-running
thread of this session and it's now fully closed, including the
data-corruption bug that motivated the `String` redesign.

Roughly 20+ real backend/checker/runtime fixes landed this session (see
`git log --oneline` for the full list — every commit has a detailed
message with root cause, fix, and verification). The methodology above
was applied to every one of them without exception.

## Open items (as of `393a544`)

Check `spec/compiler/proposals/13-v1-parity-gap-analysis.md` directly
for the authoritative, up-to-date list — it may have changed since this
was written. As of now:

### 1. `print`'s `%c` can't render a multi-byte Unicode character

**Ready to dispatch — the brief already exists at
`spec/compiler/orc-briefs/print_multibyte_char.md`** (copy its
contents into a `--prompt-file` temp path when dispatching, or point
orc at it directly if that's supported). Confirmed pre-existing,
`String`-independent (a
bare `print 'é';` reproduces it). `buildPrint`'s char case always emits
a single C `%c` specifier, which can only output one byte. The fix
needs a new runtime UTF-8-encode helper (mirroring the algorithm just
added to `std/string.peb`'s `push_char`, but implemented in C this
time) and threading a leading pre-statement (the same mechanism
`buildPrint` already uses for the recent slice-indexing fix) to build
a small encoded buffer before the combined `printf` call. Read the
brief file for full detail before dispatching.

### 2. `Allocator.{ ... }` record construction always fails (`N0001`)

**Ready to dispatch — brief exists at
`spec/compiler/orc-briefs/allocator_record.md`**. This is the most
precisely root-caused open item: a single
missing guard in `internal/symbol/visit.go`'s `resolveRecord` (the
sibling function `resolveMember`, used for member READS, already has
the exact fix pattern — skip early syntax-level validation for a
`SymbolRuntimeType` owner, defer to the later type-driven phase).
Confirmed via a minimal standalone repro; blocks
`compiler/std/mem/arena.peb`'s entire purpose (building a custom
`Allocator`). Should be one of the faster fixes in this list.

### 3. Qualified static-method calls (`TypeName.method(...)`) unsupported

Large (checker: `call_facts.go`+`member_facts.go`,
`call_validation.go`+`record.go`, IR: `ir_builder_calls.go`, plus an
`infer` constraint — ~8 files across 3 layers). NOT currently blocking
anything (a top-level generic-helper-function workaround already
covers every real use in the tree). Root-caused precisely in the
tracker (search "Qualified static-method calls"). Structurally can't
be sliced into an independently-testable first step the way other
large features here were (its three rejection points are
interdependent — fixing one alone still leaves it fully rejected), so
it needs either one larger combined dispatch or a genuinely careful
slicing design before dispatching. Low priority — do this last, if at
all, unless something in the tree starts needing it.

### 4. Self-referential generic union-variant narrowing (`std/result.peb`)

Pre-existing, already deeply investigated (see tracker, search
"self-referential GENERIC"). `std/result.peb`'s own `is_ok`/
`unwrap_or`/`map`/`set_error` methods can't read `self.Ok`/`self.Err`
because the flow-sensitive narrowing implemented earlier this session
doesn't cover a self-referential generic receiver (`self Result[T,
E]`). Confirmed reachable via a completely ordinary `import
"std:result"` now (the `C0604` fix removed what was masking it).
NOT currently blocking anything in the tree (nothing depends on these
specific methods working). A partial investigation trail exists from
an earlier stalled dispatch — search the tracker for
"`aggregateTaggedVariant`" and "`Declaration=0`" for what was found:
the record for `.Ok` in a generic method has no declaration to
re-derive the variant from, because the receiver type is a solver
variable at authored-traversal time. Not scoped for dispatch yet —
would need real investigation before a brief can be written.

## A caution for whoever picks this up

This session found that almost every fix exposes the next latent gap
— that's normal for this codebase at its current stage, not a sign
something's wrong. Don't be surprised if items 1 and 2 above, once
fixed, reveal something else. Keep applying the same methodology:
root-cause precisely, dispatch narrowly, verify adversarially, log
honestly.
