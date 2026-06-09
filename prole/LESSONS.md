# Prole Lessons And Rejected Directions

This file records things we try, reject, or learn while building Prole. Keep it
practical. The point is to avoid losing context and repeating mistakes.

## Decisions

### Keep Prole separate from `pebc`

Prole should be importable by other C projects and eventually callable from
Pebble itself through extern bindings. This rules out direct dependencies on
Pebble ASTs, checker internals, compiler options, or `pebc` arena state.

### Use malloc/free for early diagnostics

The first Prole diagnostics layer uses normal heap allocation. This keeps the
library independent while the API settles.

This may change later if diagnostics need allocator injection, but it should not
block the VM/runtime allocator design.

### Add allocator hooks before VM state

Prole now has a generic allocator interface plus malloc-backed and arena-backed
implementations. Bytecode/module ownership should use allocator hooks. VM state,
debug state, and runtime heap ownership should follow the same pattern when they
are added.

### Add disassembler before assembler

Bytecode needs to be inspectable before the VM and assembler are complete. The
disassembler also lets us design a hand-writable assembly syntax by looking at
real output.

### Do not add actor runtime first

The actor/Erlang-like runtime is a later goal. First Prole needs boring
bytecode, a step-able VM, function calls, control flow, and useful debugging.

## Things To Watch

### Do not mix target and output mode

`--target=prole` should select the backend. `--dump-bc` should control output.
Avoid adding a separate `TARGET_BYTECODE` unless it has a distinct execution
meaning.

### Avoid a second type checker

The Prole backend should consume checked AST from `pebc`. Revalidating semantic
rules in Prole generation will duplicate checker behavior and create drift.

### Keep assembler diagnostics parser-quality

If the assembler becomes fun to hand-write, its parser needs the same care as
`parser2`: spans, accumulated diagnostics, local recovery, dynamic lists, and
invalid tests.
