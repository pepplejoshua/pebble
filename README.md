# pebble
A statically-typed programming language built from scratch in C using a 4-pass compiler architecture.
I enjoy writing compilers, and I am trying to use C more, so pebble will let me do that.

## Language Features

### Syntax Examples

**Functions:**
```go
// Regular function
fn add(a int, b int) int {
    return a + b;
}

// Expression function (auto-returns)
fn square(x int) int => x * x
```

**Variables:**
```go
let name = "hello";    // Immutable
var count = 42;        // Mutable
let pi = 3.14;         // Type inference
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
```

### Built-in Types
- `int` - 64-bit integers
- `float` - 64-bit floating point
- `bool` - Boolean values (`true`/`false`)
- `str` - String literals
- `void` - No return value

## Building
```sh
make          # Build the peb compiler
make clean    # Clean build artifacts
```

## Usage
```sh
# Test the foundation systems
./peb --test

# Test the lexer
./peb --test-lexer

# Compile a Pebble source file (when parser is ready)
./peb program.peb
```

## Compiler Architecture
Pebble uses a **4-pass compilation strategy**:

1. **Pass 1**: Lexical analysis + Syntax analysis → AST
2. **Pass 2**: Collect global declarations → Symbol table
3. **Pass 3**: Type resolution and checking → Typed AST
4. **Pass 4**: Function body validation → Verified program

This approach enables:
- Clear separation of concerns
- Excellent error messages with location info
- Forward reference support
- Incremental testing and debugging

## Language Design Goals

- **Simplicity**: Clean, readable syntax inspired by modern languages
- **Safety**: Static typing with comprehensive error checking
- **Performance**: Zero-cost abstractions and efficient compilation
- **Clarity**: Explicit over implicit, minimal syntax ambiguity
