# Pebble Compiler

## 🚧 Missing Language Features

### Statements
- [ ] **For loops** - If desired, later
  - `for i in 0..10 { }`
  - Or C-style: `for (i = 0; i < 10; i++) { }`

- [ ] **Switch/match statements** - If desired

- [ ] **Defer statements** - If desired (Go-style)

### Type System Enhancements
- [ ] **Const evaluation** - Evaluate constant expressions
  - `const SIZE = 10 * 10;`
  - Currently only supports literals

- [ ] **Generic types** - Future (complex)

### File Input and Processing
- [ ] **Command-line argument parsing**
  - Accept source file path: `./peb file.peb`
  - Multiple files: `./peb main.peb lib.peb`
  - Flags: `--output`, `--verbose`, `--help`, etc.

- [ ] **File reading**
  - Read source file into memory
  - Handle file not found errors
  - Handle read errors gracefully
  - Support stdin: `./peb -` or `cat file.peb | ./peb`

- [ ] **Error reporting with file context**
  - Show filename in error messages
  - Display source line with error
  - Point to exact column with `^`
  - Support multiple files in error messages

- [ ] **Output file generation**
  - Write generated C to output file
  - Default: `input.peb` → `output.c`
  - Custom: `./peb input.peb -o custom.c`
  - Only output the files: `./peb input.peb -o`
  - Option to write to stdout for piping

### Basic C FFI (Foreign Function Interface)
- [ ] **Parse `extern` declarations**
  - `extern fn c_function(params) return_type;`
  - Mark symbols as external (don't codegen body)

- [ ] **Codegen for extern functions**
  - Emit C declarations (or `#include` directives)
  - No type conversions - direct passthrough

- [ ] **Type restrictions for extern**
  - Allow: `int`, `float`, `bool`, `void`, `*T`
  - Disallow (for now): `str`, `[]T`, tuples, structs
  - Error if invalid type used in extern signature

- [ ] **Test with standard C library**
  - `extern fn printf(fmt *char) int;`
  - `extern fn malloc(size uint) *void;`
  - `extern fn strlen(s *char) int;`
  - Verify generated C compiles and links


## 🔨 Code Generation (Next Major Phase)

### Pass 5: C Code Generation
- [ ] **Setup**
  - Create `codegen.c/h`
  - Implement C AST builder (or direct string generation)

- [ ] **Type translation**
  - Map Pebble types → C types
  - Generate struct definitions
  - Generate typedefs

- [ ] **Expression codegen**
  - Literals
  - Variables
  - Binary/unary operations
  - Function calls
  - Array indexing
  - Member access
  - Tuple access → struct field access

- [ ] **Statement codegen**
  - Variable declarations
  - Assignments
  - If/while/for
  - Return
  - Blocks

- [ ] **Function codegen**
  - Function prototypes
  - Function definitions
  - Parameter handling

- [ ] **Self-referential types**
  - Handle circular struct definitions properly
  - Forward declare structs when needed

### Build System Integration
- [ ] **Compilation pipeline**
  - Pebble → C translation
  - Invoke C compiler (gcc/clang)
  - Handle intermediate files
  - Link object files

- [ ] **Runtime library** (if needed)
  - String operations
  - Array/slice operations
  - Memory management helpers

## 🧪 Testing & Quality

### More Tests
- [ ] **Struct tests**
  - Struct type resolution
  - Struct literals
  - Field access
  - Nested structs

- [ ] **Array/slice tests**
  - Bounds checking (compile-time if possible)
  - Multi-dimensional arrays

- [ ] **Error message quality**
  - Add more context to error messages
  - Suggest fixes where possible
  - Show source code snippets

- [ ] **Integration tests**
  - Full programs that compile and run
  - Test against expected output

### Code Quality
- [ ] **Refactoring**
  - Clean up any messy code
  - Improve naming consistency
  - Add comments to complex logic

- [ ] **Performance**
  - Profile compilation speed
  - Optimize hot paths if needed

- [ ] **Memory safety**
  - Ensure no leaks (or document arena usage)
  - Verify bounds checking

## 📚 Documentation

- [ ] **Language specification**
  - Formal grammar
  - Type system rules
  - Semantics documentation

- [ ] **User guide**
  - Getting started
  - Language tour
  - Examples

- [ ] **Implementation notes**
  - Architecture overview
  - Adding new features guide
  - Compiler internals

## 🎯 Nice-to-Have Features

### Language Features
- [ ] String interpolation
- [ ] Multiple return values (or just use tuples)
- [ ] Destructuring assignments
- [ ] Pattern matching
- [ ] Enums/sum types
- [ ] Interfaces/traits
- [ ] Closures/lambdas

### Tooling
- [ ] REPL
- [ ] Package manager
- [ ] Standard library
- [ ] Formatter
- [ ] LSP server (for editor support)
- [ ] Debugger integration

### Optimizations
- [ ] Constant folding
- [ ] Dead code elimination
- [ ] Inline small functions
- [ ] Tail call optimization

## 🚀 Immediate Next Steps

**Recommended order:**
1. ✅ Struct types (most important missing feature)
2. ✅ Struct literals and field access
3. ✅ Function type parsing
4. ✅ Basic codegen (simple programs first)
5. ✅ Expand codegen to handle all features
6. ✅ Integration testing with real C compilation
