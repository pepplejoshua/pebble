# C Backend and Runtime ABI

## Backend input

The C backend consumes lowered, fully typed IR. It does not perform name
resolution, infer types, insert semantic conversions, or discover generic
instances while printing.

The lowering specification must eventually define:

- expression evaluation order;
- temporary creation and lifetime;
- defer expansion on every exit edge;
- loop and switch lowering;
- aggregate construction;
- optional and tagged-union operations;
- bounds and unwrap checks;
- Pebble-to-C calling convention adaptation.

Exact-width integers use `<stdint.h>` types in generated C. Pebble `int` and
`uint` use the target-native signed and unsigned word representations, such as
`intptr_t` and `uintptr_t`; they do not inherit the implementation-defined width
of C `int`.

## Runtime ABI

Compiler and runtime share a versioned ABI rather than undocumented emitted
snippets. The ABI specification must define:

- context and allocator layout;
- string, slice, optional, enum, and tagged-union representation;
- entry-point adapter and argument representation;
- allocation hooks;
- assertion, bounds-failure, unwrap-failure, and panic behavior;
- initialization and shutdown;
- safe, release, and freestanding configurations;
- C calling convention interoperation;
- runtime ABI version.

**Proposed layout:**

```text
runtime/include/pebble_rt.h
runtime/src/context.c
runtime/src/memory.c
runtime/src/panic.c
runtime/src/platform_<target>.c
```

Generated C includes the stable header and links the selected runtime. A
freestanding program supplies or selects the minimal implementation required by
its used features.

## Compiler lowering versus runtime

`defer`, most aggregate construction, implicit coercions, and generic
specialization belong to compiler lowering. Shared executable mechanisms such
as panic reporting, default allocation, and platform initialization belong in
the runtime.

## Current embedded behavior to inventory

The C backend currently emits context and allocator definitions, allocation
wrappers, assertion support, a C `main`, argument conversion, optional/slice
representations, and safety checks directly into generated output. Each item
must be accepted into the ABI, redesigned, or removed before backend
reimplementation.
