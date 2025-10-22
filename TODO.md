# Pebble Compiler

## 🚧 Missing Language Features

### Ideas from discussions with Caleb
- extern can take a lib name. the compiler cli can take -L (lib location) and -l (lib name)
- `if let` to safely work with pointers.
- When a variable is declared without init, auto init with {0} in the c code.
- In range loop, allow the user specify the binding name for their loop variable
- Parser errors trigger a lot of segfaults
- Allow multiple variable declarations:
```go
fn main() void {
    var f int, i int;
}
```
- Improve `print` to take multiple arguments. Remove default '\n' at the end of `print`.
- Maybe add `println` with default `print`.
- Add bounds checks for index access on arrays and slices.
- Module system would be great. Call modules "pebbles".
- Support destructing for tuples, arrays and structs. Start simple and allow deeply nesting / mixing them.
- When destructuring array / slice, the "rest" of the array / slice, can be a slice itself:
```go
var arr [5]int = [1,2,3,4,5];
var [a, b, ...rest] = arr; // 1, 2, [3,4,5]
```
- Consider UFCS instead of method syntax, since it just works:
```go
fn mult(a int, b int) int => a * b;

fn main() int {
  var a = 21;
  print a.mult(2);
  return 0;
}
```
- Get inline function-typed stuff working:
```go
fn add(a int, b int) int {
  return a + b;
}

fn take_function(f fn (int, int) int, a int, b int) int => f(a, b);

fn main() int {
    let a = add;
    print take_function(a, 10, 20);
    return 0;
}
```
compiles to:
```c
typedef int (*func_int_int_ret_int)(int, int);

int add(int a, int b);
int main();

int add(int a, int b) {
    return a + b;
}

int take_function(func_int_int_ret_int f, int a, int b) {
  return f(a, b);
}

int main() {
    const func_int_int_ret_int a = add;
    printf("%d\n", take_function(a, 10, 20));
    return 0;
}
```
- `const` type modifier

### Statements
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
