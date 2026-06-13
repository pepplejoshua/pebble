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

### Use tracking allocator for ownership tests

Prole has a tracking allocator that wraps another allocator and records
outstanding blocks. Smoke tests should use it so module/VM ownership bugs show
up immediately. The tracking allocator uses normal heap allocation for its own
bookkeeping and is intended for tests/debug builds, not normal production VM
execution.

### Infer register count

Function arity stays explicit in assembly headers, but register count should be
inferred from operands. Params occupy callee registers `r0..rN`, calls copy
contiguous caller arg registers into those param registers, and call return
values are written to the caller destination register from the call instruction.

### Keep operand order destination-first

Opcode encoding and assembly syntax should use destination-first order where an
instruction writes somewhere. `store.local` originally used `a = src reg, b =
local index`, which was inconsistent with the rest of the design. It now uses
`a = dst local index, b = src reg`, matching assembly syntax:
`store.local local0, r1`.

### Defer register reuse

Start with monotonic temporary register allocation. It is less optimal but much
easier to validate while the bytecode format and VM semantics are still moving.
Later, add expression-local temp release or a cheap liveness/compaction pass and
measure register-count drops per function. This could make a useful write-up
because the before/after numbers should be concrete.

### Add disassembler before assembler

Bytecode needs to be inspectable before the VM and assembler are complete. The
disassembler also lets us design a hand-writable assembly syntax by looking at
real output.

### Validate before run or dump

Validation belongs in Prole, not only in `pebc`, because every bytecode producer
should follow the same contract. The compiler backend, assembler, tests, and VM
runner should all validate modules before relying on their structure.

### Start VM execution small, then add frames

The first VM execution slice used the module/function bytecode model but only
ran the entry function. Direct calls were added after the basic `step()` loop by
moving VM state to a frame stack. Calls copy contiguous caller arg registers into
callee `r0..rN`; `ret` pops the callee and writes the value to the caller's call
destination register.

### Add execution in narrow opcode groups

The VM should grow by small opcode groups that can be verified in `make smoke`.
Constants/print/returns came first, then i64 arithmetic/comparisons, then direct
calls, then locals and jumps. Native calls should follow as a separate focused
slice.

### Labels are assembly syntax, not VM state

The VM executes numeric instruction offsets for `jump` and `jif`. Dot-prefixed
labels like `.done` and `.loop` belong to future assembler input and prettier
disassembly. Handwritten labels should be function-local and do not need to use
the generated `.L0` naming style.

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
