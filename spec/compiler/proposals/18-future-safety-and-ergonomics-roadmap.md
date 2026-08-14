# 18 — future safety and ergonomics roadmap

**This document is a wishlist, not a plan.** It exists to hold future
upgrades worth wanting for Pebble — things that would make it more fun to
write and closer to Rust-safe to use — before they're scoped, decided, or
scheduled. Nothing here is committed. An idea moves OUT of this file the
moment real work starts on it: it gets its own numbered proposal (decided
semantics, touch points, a real plan) and is removed from here, or reduced
to a one-line pointer at the promoted doc.

Do not treat anything below as a spec. Do not implement anything here
without first writing the kind of decision document `16-pointer-arithmetic.md`
or `11-raw-pointers-and-unsafe-ops.md` are — this file is upstream of that,
not a substitute for it.

## Static analysis

The compiler currently has essentially none of this. V1 didn't have it
either, so nothing here is a v1-parity gap — it would all be new ground for
Pebble specifically.

- **Escape analysis for stack-local addresses.** Parked 2026-08-13 (see
  `14-v2-v1-checker-backend-parity-audit.md`'s "Escape analysis for a
  stack-local's address" row) — `fn mk() *T { var x T = ...; return &x; }`
  compiles today to a real dangling pointer, caught only by the C
  compiler's `-Wreturn-stack-address` under `-Werror`, not by the checker.
  Beyond closing that specific hole, this is the prerequisite for:
  - Proving a value never escapes its function, so it can be
    stack-allocated instead of going through the Allocator — a real
    performance win with zero source-level change.
  - Making a safe `&local` return possible in the general case, once
    paired with lifetime tracking, instead of rejecting the pattern
    outright or trusting the programmer.
  - Better closure-capture decisions (stack vs. heap capture) and
    downstream optimizations (dead-store elimination, inlining).
- **Move semantics / use-after-move checking.** Given Pebble's explicit
  Allocator model (no GC), this is probably the highest-leverage safety
  analysis after escape analysis — it's what actually prevents
  double-free and use-after-free, the two failure modes an explicit
  allocator makes easy to hit.
- **Borrow / aliasing checking.** The full "one mutable xor many
  immutable references" rule, Rust's central safety mechanism. Escape
  analysis is a prerequisite for this being sound at all.
- **Definite-assignment analysis.** Flow-sensitive checking that a
  variable is genuinely initialized on every path before it's read
  (not just "has an initializer at declaration").
- **Exhaustiveness checking on switch/tagged-union matches.** Confirm
  how much of this already exists before scoping — may be partially
  covered already.
- **Static bounds/overflow analysis.** Narrow the cases where the
  existing runtime-checked arithmetic and indexing can be PROVEN safe
  at compile time and the runtime check elided, without weakening the
  runtime check where it can't be proven.
- **Unused variable/import/dead-code detection.** Cheap relative to the
  above, high day-to-day ergonomic value — "fun to write" more than
  "rust-safe," but worth having on the same list since it's the same
  kind of static-analysis investment.
- **Data race / aliasing analysis for mutable references**, if/when
  Pebble ever gets a threading story. Not urgent without concurrency,
  but the same aliasing-analysis foundation as borrow checking would
  make this cheaper to add later.

## Decided, not yet scheduled

Unlike the static-analysis section above (open ideas, no decision
made), these have an accepted design decision already — they're just
not pressing enough to schedule as active work right now.

- **C-ABI variadic extern support.** Decided 2026-08-13 (see
  `14-v2-v1-checker-backend-parity-audit.md`'s "C variadic extern
  call" row): support a real C-ABI bare `...` (untyped, no name) as
  the final parameter of an `extern fn` declaration, with NO interop
  type checking on the variadic call-site tail — pure unsafe
  passthrough, the same "unsafe, Pebble-land gets its own real
  checked alternative" treatment as untagged unions. Confirmed nothing
  in `std/` or the example programs needs this today, so it stays
  parked rather than scheduled. A first implementation attempt was
  dispatched and then abandoned mid-run once this "not needed right
  now" call was made — do not resume or trust any leftover
  working-tree state from that attempt; start fresh from the decision
  above when this is picked up.

## Notes for whoever picks one of these up

- Read `11-raw-pointers-and-unsafe-ops.md` and `open-language-decisions.md`
  first — several of these interact directly with already-decided
  pointer/unsafe semantics and must not silently relitigate them.
- Escape analysis and move/borrow checking are listed in the order
  they'd likely need to be BUILT in (escape analysis underpins the
  other two), not necessarily the order of user-facing priority — that
  priority call belongs to whoever decides to schedule one of these.
- Every item here is intentionally under-specified. The first real step
  on any of them is the same shape as `16-pointer-arithmetic.md`'s own
  investigation: reproduce the current gap for real, decide semantics,
  find the exact checker/backend touch points, THEN write the
  implementation plan.
