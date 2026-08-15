# 19a — source brief (external review, "Sol")

Reproduced verbatim for reference. See `19-stdlib-production-hardening.md`
for the live tracker, deviations from this brief, and actual slice
progress/status — this file is not updated as work proceeds.

---

You are improving Pebble's production standard library. Pebble is a native,
C-emitting language. The current compiler is the Go compiler under `compiler/`.

Repository:
  /Users/iwarilama/Desktop/Code/pebble

Read these files first:

- compiler/std/*.peb
- compiler/std/mem/arena.peb
- spec/compiler/10-c-backend-and-runtime.md
- spec/compiler/12-testing.md
- spec/compiler/proposals/14-v2-v1-checker-backend-parity-audit.md
- compiler/internal/backend/emit_test.go
- compiler/internal/backend/integration_test.go

Inspect the live Git status and recent history. Do not assume that an older
commit hash is still current. Preserve all unrelated changes.

## Goal

Make the standard library reliable enough to support a real JSON parser and
CLI named `pjson`. Add a real end-to-end standard-library test harness.

Do not implement this as one large patch. Work through the slices below in
order. Finish and verify one slice before you start the next one. Do not stop
after analysis unless a stated language decision blocks you.

## File placement

Put Pebble standard-library test programs under:

  /Users/iwarilama/Desktop/Code/pebble/tests/stdlib/

Put the Go integration runner under:

  /Users/iwarilama/Desktop/Code/pebble/compiler/internal/backend/stdlib_integration_test.go

You can adjust the Go filename if an existing standard-library runner is a
better fit. Tell me before you choose a materially different location.

## Test-harness requirements

- Test the real files under `compiler/std/`. Do not use small replacement
  modules or copied standard-library stubs.
- Compile, link with cc, and run real Pebble programs.
- Check exact exit status and exact output where output matters.
- Group many assertions into each Pebble test executable. Do not invoke cc once
  for every small assertion.
- Target three to five emitted executables for the complete standard-library
  integration suite.
- Make the C integration suite skip under `go test -short`.
- Keep the fast suite fast:
    cd compiler
    go test -short -count=1 ./... -parallel 16
- Use the existing runtime-object and compiler-test helpers where possible.
- Give each failure a unique message or exit code.
- Add a timeout for programs that could hang.
- The harness must eventually compile and run behavior or smoke coverage for
  every production module:
    mem
    mem/arena
    string
    vec
    hash
    hmap
    set
    result
    func
    math
    io
    libc
- The modules changed in this task need detailed boundary tests. Unchanged
  modules need at least real import, construction, representative operation,
  and cleanup coverage.
- If smoke coverage finds an unrelated defect, report it separately. Do not
  add an unplanned fix to the current slice.

## Slice 1: test harness and Vec correctness

Confirmed defects in `compiler/std/vec.peb`:

1. `Vec.eq` returns true when it finds unequal elements.
2. `Vec.reverse` calculates `self.len - 1` before it checks for an empty vector.
   Because `len` is unsigned, an empty vector underflows.

Fix both defects.

Required Vec tests:

- Two empty vectors are equal.
- Equal one-element and multi-element vectors are equal.
- A mismatch at the first, middle, or final element returns false.
- Different lengths return false.
- Reversing an empty vector is safe.
- Reversing one element changes nothing.
- Reverse odd-length and even-length vectors.
- Reverse twice restores the original order.
- Test push, pop, insert, remove, swap_remove, clear, resize, truncate,
  as_slice, and delete as part of the Vec behavior suite.
- Run the executable under a strict timeout so the empty reverse regression
  cannot hang.

## Slice 2: memory cleanup state

Confirmed defect in `compiler/std/mem.peb`:

`delete_slice` clears the data pointer but leaves the old slice length.

After deletion, the slice must have:

- data == nil
- len == 0

Required tests:

- Delete a populated slice.
- Delete a zero-length slice.
- Confirm no stale length remains.
- Confirm normal allocation, copy, alignment, and cleanup behavior.
- Do not add ownership or automatic destruction semantics in this slice.

## Slice 3: raw byte access decision

Several current APIs use `str.len`, which is a byte count, but then use
`str[index]`, which reads a Unicode scalar. This corrupts non-ASCII text and can
also use an invalid scalar index.

Affected code includes:

- String.push_str
- String.starts_with
- String.ends_with
- String.find
- String.insert
- hash.hash_str

First determine whether the current language already has a safe way to read one
raw byte from a `str` at a byte offset.

If no such path exists, stop before implementing this slice and report one
small design proposal. The preferred proposal is:

  str_byte_at(value str, byte_index uint) u8

It must be a read-only operation. Do not expose a mutable `[]u8` view into
`str`. The operation can be a builtin backed by a runtime helper, similar to
the existing named wrapping-arithmetic builtins.

Report:

- The exact compiler, backend, runtime, and header files that would change.
- Checked-mode bounds behavior.
- Release-mode behavior.
- The tests that prove ASCII, two-byte, three-byte, four-byte, embedded-NUL,
  empty, and out-of-range behavior.

Do not implement this language/runtime addition until I approve it.

## Slice 4: String and hash byte correctness

After the raw-byte operation is approved and implemented, repair the affected
standard-library APIs.

Add these String operations:

  push_byte(self *String, value u8)
  push_bytes(self *String, values []u8)

Requirements:

- `push_byte` appends exactly one raw byte.
- `push_bytes` preserves all bytes, including NUL and malformed UTF-8.
- `push_str` copies the exact UTF-8 bytes of a `str`.
- `starts_with`, `ends_with`, `find`, and `insert` compare or copy raw bytes.
- Do not use scalar indexing in a loop bounded by a byte length.
- Preserve the existing byte-oriented String design.
- Do not add implicit allocation to plain `str`.
- Do not add `str + str`.

Repair `hash_str`:

- Use `s.len`, not `strlen`.
- Hash the exact raw bytes.
- Embedded NUL bytes must take part in the hash.
- ASCII and UTF-8 input must match `hash_bytes` over the same bytes.

Required String and hash tests:

- ASCII.
- Empty string.
- Embedded NUL.
- `é` as a two-byte sequence.
- `€` as a three-byte sequence.
- An astral scalar such as `😀` as a four-byte sequence.
- Mixed ASCII and Unicode.
- push_str, push_byte, and push_bytes.
- Prefix, suffix, find, insert, remove, substr, clear, and equality.
- Byte length and exact byte contents.
- hash_str equals hash_bytes for equivalent data.
- Hashing does not panic from normal u64 wrapping arithmetic.
- Delete resets String state and is safe for an empty String.

If `hash_combine` uses checked arithmetic where modulo-2^64 behavior is
required, repair it with the existing explicit wrapping u64 builtins. Add
boundary tests. Do not change normal checked arithmetic semantics.

## Slice 5: reliable byte I/O

The current I/O API cannot write a dynamic `String` or `[]u8`. It also uses an
empty String for both an empty file and a read error.

Add checked APIs without immediately deleting the old compatibility APIs:

  type IOError = i32

  open_checked(path str, mode str) Result[*FILE, IOError]
  read_all_into(file *FILE, output *string::String)
      Result[uint, IOError]
  read_line_into(file *FILE, output *string::String)
      Result[bool, IOError]
  write_all(file *FILE, data []u8)
      Result[uint, IOError]

Use output parameters for String results. Do not require
`Result[String, IOError]` if that aggregate payload is not fully supported by
the compiler.

Required semantics:

- `open_checked` captures `errno` when fopen fails.
- Never call ferror with a null FILE pointer.
- `read_all_into` returns Ok(0) for a valid empty file.
- It returns Err(error_code) for a read or seek error.
- It returns the exact byte count on success.
- `write_all` continues after a partial write until all bytes are written or a
  real error occurs.
- `write_all` preserves Unicode bytes and embedded NUL bytes.
- `read_line_into` reads raw u8 bytes. It must not read one byte into a char and
  encode that value again.
- `read_line_into` returns Ok(false) only for clean EOF before a new line.
- An empty line returns Ok(true) with an empty String.
- Remove the line terminator from the returned value.
- Make stdin, stdout, and stderr available through `std:io` in a clear,
  testable form.
- Keep existing I/O functions temporarily if in-tree callers still use them.
  Implement them as compatibility wrappers where practical.
- Update all affected in-tree examples and documentation.
- Do not redesign path handling, directories, or the complete libc surface.

Required I/O tests:

- Read an empty file successfully.
- Read a non-empty ASCII file.
- Read exact UTF-8 bytes.
- Read bytes containing NUL.
- Distinguish a missing-file error from an empty file.
- Read an empty line, a final line without a newline, and clean EOF.
- Write a dynamic String through `as_bytes`.
- Write embedded NUL and verify the file byte for byte.
- Test stdout and stderr capture where possible.
- Close and flush success cases.
- Use a temporary directory supplied by the Go test. Do not leave files in the
  repository.

## Slice 6: bounded HashMap and Set probing

The HashMap and Set probe loops use `while true` without an explicit probe
limit. Small tables can become full. Missing-key operations must not loop
forever.

Requirements:

- No lookup, remove, or insertion probe can examine more than `cap` slots
  without taking a defined action.
- A missing lookup returns none or false after a full probe.
- Remove returns false after a full probe.
- Insert uses an available tombstone after a full probe.
- If there is no free or tombstone slot, grow and retry.
- Preserve replacement behavior for an existing key.
- Do not add ownership, key destruction, or iterator APIs.
- Keep HashMap and Set behavior consistent.

Required tests for both containers:

- Capacity 0.
- Explicit capacities 1, 2, and 3.
- Constant-hash collision chains.
- Fill the table.
- Missing lookup in a full table.
- Remove every item, producing tombstones.
- Missing lookup with tombstones.
- Reinsert through a tombstone.
- Replace an existing HashMap value.
- Growth and rehash preserve all items.
- Clear preserves capacity and removes logical contents.
- Every test must complete under a strict timeout.

## Final standard-library smoke slice

After all focused fixes are complete, add the remaining smoke coverage needed
for every production standard-library module. Do not redesign these modules.
If a smoke test exposes a new problem, report it with:

- Minimal Pebble reproduction.
- Exact diagnostic, emitted-C failure, crash, wrong output, or timeout.
- Probable source file and function.
- Whether it blocks `pjson`.
- A proposed small future slice.

## Scope boundaries

Do not:

- Implement `pjson` in this task.
- Reorganize or rename the existing standard-library modules.
- Design borrowing, lifetimes, automatic destruction, Rc, or Arc.
- Add implicit allocation to `str`.
- Rewrite all of `std:io`, `std:libc`, or the allocator system.
- Fix unrelated compiler parity defects.
- Hide generated files or caches with `.gitignore`.
- Leave scratch Pebble files, C output, binaries, or debug prints in the tree.
- copy the Go module to perform causation tests.

## Verification for every slice

Read the actual diff before accepting the slice.

Run from `compiler/`, with an external Go cache:

  GOCACHE=/tmp/pebble-sonnet-gocache go test -short -count=1 ./... -parallel 16
  GOCACHE=/tmp/pebble-sonnet-gocache go vet ./...
  GOCACHE=/tmp/pebble-sonnet-gocache go build ./...
  GOCACHE=/tmp/pebble-sonnet-gocache go test ./... -count=1 -timeout 300s -parallel 16

Also run:

  gofmt -l .
  git diff --check
  git status --short

If runtime C files or `runtime/include/pebble_rt.h` change, build and run
`runtime/test/smoke_test.c` in both:

- PEBBLE_RT_MODE_SAFE
- PEBBLE_RT_MODE_RELEASE

Use `-Wall -Wextra -Werror`.

Perform a causation check for each defect:

1. Keep the new regression test present.
2. Stash only the production fix files.
3. Rebuild and show that the test reproduces the original failure.
4. Reapply the stash.
5. Rebuild and show that the same test passes.
6. Confirm that the stash and working tree contain no lost or duplicate work.

Do not copy the module for this test.

Monitor `/tmp/pebble-sonnet-gocache`. Clear it if it grows beyond 250 MB.

## Git discipline (superseded — see 19's own workflow section)

- Make one focused commit per completed slice.
- Each commit message must state the root cause, fix, tests, and causation proof.
- Do not combine unrelated findings with a completed fix.
- Do not push unless I separately tell you to push.
- After each slice, report the commit hash, files changed, exact tests run,
  cache size, and any new finding.
