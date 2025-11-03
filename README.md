# pebble
A statically-typed systems programming language built from scratch in C using a multi-pass compiler architecture.

Pebble compiles to C, providing low-level control with modern language features including modules, optionals, slices, defer statements, and a unique context-based allocator system.

## Quick Start

### Building
```sh
make          # Build the peb compiler
make clean    # Clean build artifacts
```

### Usage
```sh
# Compile a Pebble source file
./peb program.peb

# Compile with options
./peb program.peb -o myapp --warnings

# Generate C without compiling
./peb program.peb --generate-only --keep-c-file

# Compile in freestanding mode (no stdlib)
./peb program.peb --freestanding

# Link with libraries
./peb program.peb -l pthread -L /usr/local/lib -I /usr/local/include
```

## Language Features

### Basic Syntax

**Functions:**
```go
// Regular function
fn add(a int, b int) int {
    return a + b;
}

// Expression function (auto-returns)
fn square(x int) int => x * x

// Variadic function
fn sum(...values []int) int {
    // Implementation
}
```

**Variables:**
```go
let name = "hello";    // Immutable
var count = 42;        // Mutable
let pi = 3.14;         // Type inference
var x i32 = 100;       // Explicit type
```

**Control Flow:**
```go
fn max(a int, b int) int {
    if a > b {
        return a;
    } else {
        return b;
    }
}

fn factorial(n int) int {
    var result = 1;
    while n > 1 {
        result = result * n;
        n = n - 1;
    }
    return result;
}

// Range loop (exclusive)
fn print_numbers() void {
    loop 0..10 {
        print iter;  // iter is implicit loop variable
    }
}

// Range loop (inclusive)
fn sum_range(start int, end int) int {
    var total = 0;
    loop start..=end {
        total = total + iter;
    }
    return total;
}

// Range loop with custom iterator name
fn print_with_custom_var() void {
    loop 0..5 : i {
        print i;
    }
}

// For loop
fn print_range(start int, end int) void {
    for var i = start; i < end; i++ {
        print i;
    }
}

// While loop with break/continue
fn search(arr []int, target int) bool {
    var i = 0;
    while i < arr.len {
        if arr[i] == target {
            return true;
        }
        i++;
    }
    return false;
}
```

### Types

#### Primitive Types
- `int`, `i8`, `i16`, `i32`, `i64`, `isize` - Signed integers
- `u8`, `u16`, `u32`, `u64`, `usize` - Unsigned integers
- `float` - 64-bit floating point
- `bool` - Boolean values (`true`/`false`)
- `char` - Single character
- `str` - String literals (null-terminated C strings)
- `void` - No return value

#### Composite Types

**Pointers:**
```go
var x int = 42;
var ptr *int = &x;      // Address-of
var value int = *ptr;   // Dereference
```

**Arrays:**
```go
var arr [5]int = [1, 2, 3, 4, 5];
var repeated [10]int = [0; 10];  // [0, 0, 0, ...]
var elem int = arr[2];
```

**Slices:**
```go
var numbers []int;
var subset = arr[1:4];   // Slice from index 1 to 3
var prefix = arr[:3];    // First 3 elements
var suffix = arr[2:];    // From index 2 to end
```

**Structs:**
```go
type Point = struct {
    x float;
    y float;
};

var p Point = Point.{ x: 10.0, y: 20.0 };
var px = p.x;
```

**Tuples:**
```go
var pair (int, str) = (42, "hello");
var triple (int, int, int) = (1, 2, 3);
```

**Enums:**
```go
type Color = enum {
    RED,
    GREEN,
    BLUE
};

var output_col = Color.RED;
```

**Optionals:**
```go
var maybe ?int = some 42;
var nothing ?int = none;

// Force unwrap (runtime error if none)
var value int = maybe!;
```

**Function Types:**
```go
type BinaryOp = fn(int, int) int;

fn apply(op BinaryOp, a int, b int) int {
    return op(a, b);
}
```

### Pattern Matching

**Switch Statements:**
```go
fn classify(n int) str {
    switch n {
        case 0: return "zero";
        case 1: return "one";
        case 2: return "two";
        else: return "many";
    }
}
```

### Resource Management

**Defer Statement:**
```go
fn read_file(path str) void {
    var file = io::open(path, io::MODE_READ);
    defer io::close(file);  // Runs at end of scope

    // Use file...
    // close() automatically called even if early return
}
```

Deferred statements execute in LIFO order (Last In, First Out):
```go
defer print "third";
defer print "second";
defer print "first";
// Prints: first, second, third
```

### Type Casting & Operators

**Explicit Casting:**
```go
var x int = 42;
var y float = x as float;
var ptr *void = &x as *void;
```

**Sizeof:**
```go
var size = sizeof int;
var custom_size = sizeof Point;
```

**Bitwise Operators:**
```go
var a = 0b1010;
var b = 0b1100;
var and = a & b;    // Bitwise AND
var or = a | b;     // Bitwise OR
var xor = a ^ b;    // Bitwise XOR
var not = ~a;       // Bitwise NOT
var shl = a << 2;   // Left shift
var shr = a >> 1;   // Right shift
```

**Increment/Decrement:**
```go
var i = 0;
i++;      // Postfix increment
i--;      // Postfix decrement
```

### Module System

**Importing Modules:**
```go
// Import from standard library
import "std:io";
import "std:mem";
import "std:string";

// Import from relative path
import "utils/math";
import "data/structures";

fn main() void {
    var s = string::new();
    string::push_str(s, "Hello");
    print string::to_str(s);
    string::delete(s);
}
```

### C Interoperability

**Extern Declarations:**
```go
// Single extern function
extern fn strlen(s str) usize;

// Extern block
extern {
    fn malloc(size usize) *void;
    fn free(ptr *void) void;
    fn memcpy(dst *void, src *void, n usize) *void;
}

// Extern with library name
extern "libm.so" {
    fn sqrt(x float) float;
    fn pow(base float, exp float) float;
}

// Extern opaque types
extern {
    type FILE;
    type pthread_t;
}

// Extern variables and constants
extern {
    var errno i32;
    let stdin *FILE;
}
```

**Calling Conventions:**
```go
// C calling convention (for FFI)
fn "c" callback(value int) void {
    print value;
}

// Pebble calling convention (default, receives context)
fn allocate_data(size usize) *void {
    // `context` is implicitly available
    return context.default_allocator.alloc(
        context.default_allocator.ptr,
        size
    );
}
```

### The Context System

Pebble functions use a unique calling convention that passes a `context` parameter containing an allocator:

```go
fn use_allocator() void {
    // context.default_allocator is automatically available
    var data = context.default_allocator.alloc(
        context.default_allocator.ptr,
        1024
    );

    // Use data...

    context.default_allocator.free(
        context.default_allocator.ptr,
        data
    );
}
```

In C, this looks like:
```c
typedef struct {
  Allocator default_allocator;
} __pebble_context;

typedef struct Allocator {
  void *ptr;
  void *(*alloc)(__pebble_context, void *, size_t);
  void (*free)(__pebble_context, void *, void *);
} Allocator;
```

The context contains:
- `default_allocator` - An allocator with function pointers for `alloc` and `free`

This enables:
- Custom memory allocation strategies
- Dependency injection of resources
- Freestanding/embedded environments (empty context)

## Standard Library

### std:mem
Memory allocation utilities:
```go
import "std:mem";

var ptr = mem::new(1024);           // Allocate 1024 bytes
defer mem::delete(ptr);             // Free memory

var aligned = mem::align_up(size, 16);  // Align to 16 bytes
```

### std:string
Growable string type:
```go
import "std:string";

var s = string::new();
defer string::delete(s);

string::push_char(s, 'H');
string::push_str(s, "ello");
print string::to_str(s);
```

### std:io
File I/O operations:
```go
import "std:io";

// File operations
var file = io::open("data.txt", io::MODE_READ);
defer io::close(file);

var content = io::read_all(file);
var line = io::read_line(file);

// Path utilities
if io::exists("config.json") {
    print "Config found";
}

var is_directory = io::is_dir("/tmp");

// Directory operations
io::makedir("output");
io::delete("old.txt");
io::rename_path("old.txt", "new.txt");
```

## Compiler Options

### Basic Options
```sh
-v, --verbose              Enable verbose output
-w, --warnings             Enable C compiler warnings
--keep-c                   Keep generated C file (default)
--no-keep-c                Remove generated C file after compilation
--generate-only            Only generate C source without compiling
--compiler <compiler>      Specify C compiler (autodetects gcc/clang/cc)
-o <name>                  Output executable name (default: output)
-c <name>                  Output C file name (default: output.c)
```

### Library & Include Options
```sh
-l <library>               Link library (e.g., -l pthread)
-L <path>                  Add library search path
-I <path>                  Add include search path
--header <name>            Include local header in source
--sys-header <name>        Include system header in source
```

### Module Paths
```sh
--std-path <path>          Location of std library (default: alongside compiler)
```

### Freestanding & Library Options
```sh
--freestanding             Generate freestanding code (no standard library)
--entry-point <symbol>     Entry point of your code (default: main)
--no-main                  No entry point. Compiles to object file only
--shared                   Compile as a shared library
```

### Release Modes
```sh
--debug                    Compile in debug mode
--release-small            Compile for a smaller binary
--release-safe             Compile with runtime safety features
--release                  Compile with standard release compiler flags
```

### Examples
```sh
# Compile with pthread
./peb server.peb -l pthread -o server

# Generate shared library
./peb mylib.peb --shared -o libmylib

# Freestanding for embedded
./peb kernel.peb --freestanding --entry-point kernel_main

# Custom C compiler with includes
./peb app.peb --compiler clang -I ./include -L ./lib -l mylib

# Keep C file for debugging
./peb program.peb --keep-c --generate-only

# Object file without entry point
./peb module.peb --no-main -o module.o
```

## Compiler Architecture

Pebble uses a **5-pass compilation strategy**:

1. **Pass 1**: Lexical analysis + Parsing → AST
2. **Pass 2**: Global symbol collection → Symbol table
3. **Pass 3**: Type resolution and canonicalization → Type system
4. **Pass 4**: Type checking and function body validation → Verified AST
5. **Pass 5**: Entry point verification and C code generation → Executable

This approach enables:
- **Forward references**: Declare types/functions in any order
- **Type safety**: Comprehensive compile-time checking
- **Clear errors**: Precise location information for all diagnostics
- **Multi-module**: Automatic dependency resolution
- **Incremental development**: Test each phase independently

### Code Generation

Pebble compiles to C as an intermediate representation:
1. Generates type-safe C code with proper struct declarations
2. Handles type dependencies with topological sorting
3. Emits context plumbing for Pebble calling convention
4. Invokes C compiler (clang/gcc/cc) for final compilation

## Language Design Goals

- **Simplicity**: Clean syntax inspired by Go, Rust, and C
- **Safety**: Static typing with optional types and compile-time checks
- **Control**: Direct memory management with modern conveniences (defer, context)
- **Interoperability**: Seamless C FFI for systems programming
- **Modularity**: First-class module system with standard library
- **Flexibility**: Multiple build modes from freestanding to full stdlib

## Implementation Notes

- **Memory Management**: Arena allocator for compiler, user code manages memory explicitly
- **Calling Conventions**: Pebble convention injects context, C convention for FFI
- **Type System**: Structural typing with nominal type aliases
- **Error Handling**: Optionals and explicit error codes (no exceptions)
- **Compilation Speed**: Fast single-pass C generation after type checking

## Project Status

Pebble is a personal project for learning compiler construction in C. The compiler is functional and supports:
- ✅ Complete lexer and parser
- ✅ Multi-module compilation
- ✅ Full type system with inference
- ✅ C code generation
- ✅ Standard library (mem, string, io)
- ✅ Extern FFI system
- ✅ Context-based allocator convention
- ✅ Defer statements
- ✅ Pattern matching (switch/case)
