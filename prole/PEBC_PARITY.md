# pebc To Prole Parity

`pebc` is the Pebble compiler. This file tracks what Prole must support before
`pebc` can target it with useful programs.

The current C backend is the reference for language behavior. Prole does not
need to copy the C backend's implementation, but it does need to represent the
same checked Pebble semantics where possible.

## Integration Principle

The Prole backend should start after parsing, imports, type checking,
monomorphization, and entry-point verification.

```text
source -> parser -> checker -> checked AST -> Prole bytecode generator
```

Prole should not know about Pebble AST nodes, checker state, compiler options,
or module tables. The adapter belongs in `pebc`, not in `prole/`.

Expected adapter location:

```text
src/prole_gen.h
src/prole_gen.c
```

The adapter may reject unsupported checked AST/type features with normal `pebc`
diagnostics while Prole parity is incomplete.

## Current Prole Baseline

Implemented today:

- Bytecode module/function/native containers.
- Function arity and inferred register count.
- Locals.
- Constants for `i64` and `bool`.
- `i64` arithmetic and comparisons.
- Direct bytecode function calls.
- Frame stack execution.
- Numeric-offset `jump` and `jif`.
- `print`, `ret`, and `ret.void`.
- Public bytecode validation.
- Disassembly.
- Allocator hooks and tracking allocator.

Not implemented yet:

- Native calls.
- Strings.
- Floats.
- Heap values.
- Globals.
- Structs, tuples, arrays, slices, enums, unions, optionals, pointers, opaque
  values.
- Source/debug metadata.
- Assembler.
- A `pebc` bytecode generator.

## Reference C Backend Capabilities

The C backend currently supports a large language surface:

- Modules/imports and module-qualified names.
- Global variables and constants.
- Normal functions, anonymous functions, methods, associated functions, and
  monomorphized generic functions.
- Pebble and C calling conventions.
- Extern functions, extern variables, extern constants, extern opaque types,
  and `extern "lib" { ... }` blocks.
- Integers, sized integers, floats, bool, char, string, nil, void, and none.
- Pointers, arrays, slices, structs, tuples, enums, unions, tagged unions,
  optionals, function values, and opaque types.
- Arithmetic, comparison, equality, bitwise, logical, address-of, dereference,
  casts, indexing, slicing, member access, calls, `some`, force unwrap, postfix
  increment, and postfix decrement.
- `if`, `while`, `loop`, `for`, `switch`, `break`, `continue`, `return`, block,
  assignment, expression statements, print, and defer.
- Bounds checks and tagged-union access checks.
- Entry point lowering for `main(argc, argv)` and `main(argv []str)`.
- A generated Pebble runtime context with allocator hooks.

## Calling Conventions

Pebble currently has two function conventions:

```text
CALL_CONV_C
CALL_CONV_PEBBLE
```

`CALL_CONV_PEBBLE` is not only a C codegen trick. The type system creates a real
builtin `__pebble_context` struct type and uses the convention to mean "context
is explicitly passed as the first ABI argument". This is similar to Odin's
context model.

Today the builtin context shape is:

```text
__pebble_context
  default_allocator: Allocator

Allocator
  ptr: *void
  alloc: fn *void, usize -> *void     # Pebble convention
  realloc: fn *void, *void, usize -> *void
  free: fn *void, *void -> void
```

The C backend emits Pebble-convention functions with an explicit first C
parameter:

```c
__user_function(__pebble_context context, ...)
```

Calls to Pebble-convention functions pass `context` as the first argument.
Functions with C convention cannot use the `context` expression and currently
cannot call Pebble-convention functions directly.

Prole should preserve this ABI model. Once Prole supports pointers, structs, and
function values, `pebc` can materialize a Prole context value whose layout
matches the Pebble context type. The VM can own the actual runtime state, but the
Pebble program should still see and pass an explicit context value according to
its function type.

Initial recommendation:

- Treat Pebble convention as an explicit first-argument ABI in Prole bytecode.
- Reserve `r0` for context in Pebble-convention functions, with user parameters
  shifted after it.
- Let C-convention functions omit context, matching the current checker rules.
- Add a VM-created context value before running a Pebble-convention entry point.
- Keep the context layout aligned with the builtin `__pebble_context` and
  `Allocator` types.

## Externs And Native Calls

The C backend handles externs by emitting C declarations and relying on the C
compiler/linker:

```c
extern int puts(const char *);
```

Prole cannot get this behavior for free. It needs a host/native bridge.

Initial Prole model:

```text
call.native rdst, native_name_or_index, first_arg, arg_count
```

The embedder registers native functions before execution:

```c
prole_vm_register_native(vm, "puts", puts_adapter, user_data);
```

This maps Pebble extern functions to registered native functions.

### Extern Function Parity

Needed:

- Native registry in `ProleVm` or loaded module state.
- Native signatures in bytecode metadata.
- Type-aware argument marshalling from `ProleValue` to native adapter inputs.
- Type-aware return marshalling from native adapter output to `ProleValue`.
- Validation that `call.native` target and arity are valid.

Initial scope:

- Host-registered native adapters only.
- No automatic `dlopen`, `dlsym`, or arbitrary C ABI calls.

Later scope:

- Optional dynamic-library loading.
- Optional `libffi`-style ABI bridge if the project really needs direct C ABI
  calls without handwritten adapters.

### Extern Type Parity

C extern types become opaque Pebble types. Prole should represent them as opaque
handles.

Needed:

- `opaque`/`handle` value kind.
- Pointer/handle safety rules.
- Native adapters allowed to create, consume, and compare opaque handles.

Initial scope:

- Opaque handles are VM values with no field/member access.
- Dereferencing opaque values remains invalid, matching checker rules.

### Extern Var/Const Parity

The C backend emits references to external symbols. Prole needs a different
model.

Options:

- Native getter/setter functions generated by `pebc`.
- Host-registered global symbols.
- Module native slots with load/store instructions.

Initial recommendation:

- Compile extern constants/variables to generated native getter/setter calls.
- Add direct native global slots only after the native API stabilizes.

## Type And Value Parity

Prole currently has only a tiny value set. Full `pebc` parity needs a stable VM
value model.

Suggested staged order:

1. Existing `void`, `i64`, `bool`.
2. `string`.
3. `f64`, then `f32` if needed separately.
4. Sized integers and `char`.
5. Opaque handles and pointers.
6. Arrays and slices.
7. Structs and tuples.
8. Optionals.
9. Enums.
10. Unions and tagged unions.
11. Function values/closures.

Do not add all of these at once. Each type should come with bytecode ops,
validation rules, disassembler syntax, smoke coverage, and a lowering path from
checked Pebble AST.

## Control Flow Parity

Current Prole jumps can represent basic `if` and `while` lowering.

Needed for wider parity:

- Label-aware assembler/disassembler output.
- Lowering rules for `if`, `while`, `loop`, `for`, and `switch`.
- Break/continue target stacks in `pebc`'s Prole generator.
- Defer lowering before `return`, `break`, and `continue`.

Important: labels are assembly/debug syntax. VM instructions should continue to
store numeric instruction offsets.

## Runtime Checks

The C backend emits runtime checks for:

- Bounds checks.
- Slice pointer validity checks in some paths.
- Tagged-union access checks.
- Optional force unwrap checks should be explicit when added.

Prole should make runtime traps debuggable:

```text
trap kind
message
function
instruction offset
source span, when available
```

These traps should be visible through the VM/debug API and any future TUI.

## Globals And Constants

The C backend emits global storage and global initializers.

Prole needs:

- Module-level global slots.
- Global type metadata.
- `load.global` and `store.global` or equivalent instructions.
- Constant data section for literal constants.
- Initialization order for globals with non-trivial initializers.

Initial scope:

- Constants/literals first.
- Mutable globals later.

## Entry Points

The C backend has special handling for `main`:

- `main() -> int`
- `main(argv []str) -> int`
- `main(argc int, argv *str) -> int`

Non-main entry points must be `() -> void` with C convention.

Prole module entry currently points at a function. For `pebc` integration, the
generator should preserve the checked entry function and build any needed argv
setup outside the core VM, likely in the command-line runner or embedding API.

Needed:

- Module entry metadata.
- Optional startup argument injection.
- Clear difference between running a Prole module as a process and embedding it
  inside another host.

## Debug Metadata

Prole should carry optional metadata from `pebc`:

- Instruction offset to source span.
- Function index to Pebble function/module name.
- Local index to source local name.
- Register index to optional compiler-generated display name.
- Native index to extern declaration/source span.

This metadata is not required for execution, but it is required for useful
stepping, traps, disassembly, and future UI work.

## Minimal pebc Integration Slice

The first useful integration should deliberately support a small subset:

- One compiled module flattened into one Prole module.
- Functions with `i64`, `bool`, and `void` signatures.
- Local variables.
- Integer/bool literals.
- Arithmetic/comparison expressions.
- `if` and `while`.
- Direct function calls.
- `print` for supported types.
- `return` and `ret.void`.
- No externs except maybe one manually registered smoke native.

Everything else should report a clear unsupported-feature diagnostic from the
Prole generator.

## Near-Term Checklist

- Implement `call.native` and a native registry.
- Add native-call validation and smoke coverage.
- Add debug metadata structs without requiring all producers to fill them.
- Add strings.
- Add a first `src/prole_gen.c` that lowers a tiny checked subset.
- Add `--target=prole --dump-bc` or equivalent compiler path.
- Add unsupported-feature diagnostics for every unhandled AST/type kind.
- Add CLI runner support for validating, disassembling, and running generated
  bytecode.

## Deferred Checklist

- Dynamic library loading for `extern "lib"`.
- Direct C ABI bridge.
- Mutable globals.
- Heap object model.
- Struct/tuple/array/slice layout.
- Optional/enum/union/tagged-union layout.
- Function values and closures.
- Defer lowering.
- Full switch lowering.
- Register reuse/liveness optimization.
- Assembler round-trip tests.
- REPL and TUI/debugger.
