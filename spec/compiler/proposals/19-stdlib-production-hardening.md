# 19 — standard-library production hardening (toward `pjson`)

**Status:** in progress, started 2026-08-15.

**Purpose.** This is the working tracker for hardening `compiler/std/*.peb`
into something a real application can depend on — starting with `pjson`, a
`jsmn`-style JSON parser and CLI, not built in this document's scope. The
gaps here were found by an external review (attributed below as "Sol") and
independently spot-verified against the actual source before any slice
started. This follows the same two-purpose split as `13`/`14`: this file is
the live slice log (what's done, what's next); it is not a general v1-parity
tracker — that's `13`/`14`, and unrelated compiler-parity defects found along
the way get logged there instead, not fixed inline here (see "Scope
boundaries" below).

## Provenance and deviations from the source brief

The initiating brief (external review, referred to here as "Sol") is
reproduced in full in `19a-stdlib-hardening-source-brief.md` for reference.
Its technical findings were independently verified against the current
source before trusting them (three spot-checks, all confirmed real: `Vec.eq`
returns `true` on a mismatch — internal test premise `!=` was written `==`
with a leftover `// Wait, should be != for inequality` comment already in
the file; `Vec.reverse` computes `self.len - 1` before checking for an empty
vector, underflowing the unsigned `len`; `mem::delete_slice` clears `.data`
but leaves the stale `.len`, so a deleted slice still reports its old
length). Two explicit deviations from the brief, per direct instruction:

- **Push discipline**: the brief says "do not push unless told." This
  project's standing rule (confirmed directly, 2026-08-15) is the opposite —
  commit and push once a slice is independently verified, same as every
  other item in `13`/`14`. Followed here.
- **`GOCACHE` override**: the brief specifies
  `GOCACHE=/tmp/pebble-sonnet-gocache` for verification commands. This
  project has not used a custom `GOCACHE` override this session — the
  default Go build cache has been used throughout with no issues. Not
  adopted; plain `go build`/`go test` is used for verification, matching
  every other item in this session's history. (A stale, unrelated custom
  cache directory — `/tmp/pebble-fifth-gocache`, 140M, orphaned from an
  earlier, unrelated pass — was found and removed while reviewing this.)

## Workflow

Same discipline as `13`/`14`, restated here for a self-contained tracker:

1. Work one slice at a time, in order. Do not start the next slice until the
   current one is committed and pushed.
2. Dispatch each slice through Orc (model rotation, banned-model list, and
   session-resume rules are identical to `13`'s — see that file's "Dispatch
   rules" section, not repeated here).
3. Never trust a worker's own "completed"/"failed" report. Read the actual
   diff. Build, vet, gofmt, and run the targeted tests yourself.
4. Causation-check every defect fix: keep the regression test, temporarily
   restore the pre-fix production file (via an isolated `git worktree`, not
   `git stash` — a worker's own stash use collided with nothing this time,
   but stash is still avoided for causation checks per standing practice),
   confirm the test fails against the old code, restore the fix, confirm it
   passes.
5. One focused commit per completed slice; commit message states root
   cause, fix, tests, and causation proof.
6. If a slice's own verification surfaces an unrelated defect, log it in
   `14` (or here, if it's stdlib-scoped but genuinely out of this task's
   slice list) rather than fixing it inline.

## Test harness

- Pebble test programs: `tests/stdlib/*.peb`.
- Go integration runner: `compiler/internal/backend/stdlib_integration_test.go`.
- Tests the real files under `compiler/std/` directly (no stub/replacement
  modules) — matches the existing `buildStdMemFixture`/`buildStdFixture`
  pattern in `internal/backend/emit_test.go`, which already reads real
  `compiler/std/*.peb` files off disk rather than embedding copies.
- Groups many assertions per emitted executable (target: 3-5 executables
  total for the whole stdlib suite, not one-cc-invocation-per-assertion).
- Skips under `go test -short` (the C-compile-and-link step is the slow
  part); `cd compiler && go test -short -count=1 ./... -parallel 16` must
  stay fast.
- Every emitted-and-run test has a timeout, so a probe-loop or reverse
  regression can't hang the suite.
- Full production-module smoke coverage (real import + construction +
  representative operation + cleanup, at minimum) is required across all of:
  `mem`, `mem/arena`, `string`, `vec`, `hash`, `hmap`, `set`, `result`,
  `func`, `math`, `io`, `libc` — modules actually touched by a slice need
  deeper boundary-condition tests; untouched modules need the baseline
  smoke pass only.

## Scope boundaries (do not do these here)

- Implement `pjson` itself.
- Reorganize or rename existing stdlib modules.
- Design borrowing, lifetimes, automatic destruction, Rc/Arc.
- Add implicit allocation to `str`, or `str + str`.
- Rewrite all of `std:io`, `std:libc`, or the allocator system.
- Fix unrelated compiler-parity defects (log to `13`/`14` instead).
- `.gitignore` generated files/caches, or leave scratch `.peb`/C/binary/debug
  output in the tree.
- Copy the Go module for a causation check (use an isolated worktree).

## Slice log

*(empty — Slice 1 (test harness + `Vec` correctness) closed in `fca5d45`.
Two real bugs in `compiler/std/vec.peb`: `Vec.eq` returned `true` on the
first mismatching element (`if a != b { return true; }` — backwards, with
a leftover `// Wait, should be != for inequality` comment already sitting
next to it); `Vec.reverse` computed `self.len - 1` before checking for an
empty vector, underflowing the unsigned `len` and corrupting every index
the reverse loop then touched. Both fixed with the minimal change (invert
the comparison; add an early return for `len == 0`). Also built the real
end-to-end stdlib integration harness this whole initiative depends on
(`compiler/internal/backend/stdlib_integration_test.go` +
`tests/stdlib/*.peb`), resolving real `std:` imports through the same
embedded provider `pebc` itself uses, compiling and linking against the
real runtime, running under a bounded timeout, skipping under `go test
-short`. 23 assertions covering `Vec.eq`, `Vec.reverse`, and every other
mutation method. Causation-checked via an isolated `git worktree` (not
`git stash`) — the new test run against the pre-fix `vec.peb` reproduces
both bugs exactly (the three eq-mismatch cases report `FAIL`; the reverse
regression panics with a checked-index "out of bounds" before it can
corrupt further). One dispatch-process note, not a code defect: the
worker that produced this slice also made an unauthorized change to the
repo-root `Makefile` (renaming the unrelated legacy V1 `pebc` build
target to `pebcv1`) and drafted a `pjson/SPEC.md` — both explicitly out
of this task's scope, reverted/removed before verification. The actual
requested deliverables (the harness and the two bug fixes) were correct
and used as-is.

Slice 2 (`mem::delete_slice` stale `.len` after clearing `.data`) closed in
`102d866`. `delete_slice` cleared `.data` but never reset `.len`; added
`s.len = 0;` alongside the existing `s.data = nil;`. New
`tests/stdlib/mem_test.peb` covers: delete of a populated slice (the actual
regression — asserts `.len` specifically, since the original bug already
correctly cleared `.data`), delete of an allocated-but-empty slice, delete
of an already-nil-backed slice (no crash), and confirms normal
allocation/copy/cleanup elsewhere in `mem.peb` are unaffected.
Causation-checked via an isolated `git worktree` — the pre-fix code fails
exactly at `delete_populated_post_len`. This dispatch respected scope
correctly (only the three files asked for were touched; the previous
slice's Makefile/`pjson` stray-scope note does not apply here), though it
did leave a stray `git worktree` from its own causation check that the
supervisor removed during verification — flagged only because the "remove
the temporary worktree when done" instruction was in the brief; not a
correctness issue.

Slice 3 (`str_byte_at` — new checked builtin, language decision already
approved 2026-08-15, see "Planned slice order" above) is next.)*

### Planned slice order

1. Test harness bootstrap + `Vec.eq`/`Vec.reverse` correctness.
2. `mem::delete_slice` leaves a stale `.len` after clearing `.data`.
3. **Language decision, approved 2026-08-15**: add `str_byte_at(value str,
   byte_index uint) u8` as a new checked builtin (real checker/backend/
   runtime work, not a plain `extern` function) — `str` currently has no
   safe raw-byte read at all: `str[i]` always does an O(n) UTF-8 scalar
   decode (confirmed via `runtime/src/str.c`'s `pebble_rt_utf8_decode_one`),
   and `.data` is deliberately not exposed on `str` in the checker's
   member-validation gate (`compiler/internal/check/member_validation.go`,
   only `.len` is valid for `str`; `slice` gets both `.len` and `.data`).
   Read-only; no mutable `[]u8` view exposed. Mirrors the existing `slice
   ptr, count` builtin's implementation shape (checker-recognized, backend
   lowers to a new runtime helper, compiler auto-injects the real
   `PebbleSourceLoc` at the call site — a plain `extern fn` cannot do this,
   and every other checked operation in the language panics with a real
   source location, so this shouldn't be the one exception). SAFE mode:
   panics on `byte_index >= value.len`, real source location. RELEASE mode:
   unchecked `data[byte_index]`. Ordinary call syntax (unlike `slice`,
   there's no parsing ambiguity here forcing keyword syntax).
4. Repair `String`/`hash_str` byte-vs-scalar correctness using the new
   builtin (`push_str`, `starts_with`, `ends_with`, `find`, `insert`,
   `hash_str`); add `String.push_byte`/`push_bytes`.
5. Reliable byte I/O: `open_checked`, `read_all_into`, `read_line_into`,
   `write_all` (`Result`-returning, output-parameter style; keep existing
   compatibility APIs where in-tree callers still use them).
6. Bounded `HashMap`/`Set` probing (explicit probe-limit, no unbounded
   `while true`).
7. Final smoke-coverage pass across every remaining production module.
