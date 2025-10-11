# Advanced Language Features - Implementation Notes

## 1. Nested Functions and Types with Hoisting

### Current Limitation
C restricts functions and types to top-level only. Nested declarations are not allowed.

### Goal
Support nested functions and type declarations while compiling to C.

```go
fn outer(x int) int {
    fn inner(y int) int {
        return x + y;  // Closure: accesses outer's x
    }

    type Point struct {
        a int;
        b int;
    };

    let p Point = {a = 1, b = 2};
    return inner(p.a);
}
```

### Implementation Strategy

**Hoisting with Parameter Injection:**
- Transform nested functions into top-level functions
- Pass captured variables as hidden parameters
- Maintain closure semantics through explicit passing

```go
// After transformation:
fn outer_inner(captured_x int, y int) int {
    return captured_x + y;
}

type outer_Point = struct {
    a int;
    b int;
};

fn outer(x int) int {
    // Point hoisted to top-level as outer_Point
    let p outer_Point = {a = 1, b = 2};
    return outer_inner(x, p.a);  // Pass x explicitly
}
```

**Type Name Mangling:**
- Hoist local types to top-level with mangled names
- Prefix with enclosing function name to avoid collisions

**Challenges:**
- Mutable captured variables need pointer passing
- Multi-level nesting requires careful capture analysis
- Name mangling must avoid collisions

## 2. First-Class Function Types and Struct Declarations

### Function Types Everywhere
Allow function types in any type position for expressive callbacks and function values.

```go
// Function type in parameters
fn map(arr []int, func (int) str) []str {
    var result []str;
    // ... implementation
    return result;
}

// Function type in variables
let transform fn(int) int = double;
let callback fn() void = printDone;

// Function type in return position
fn makeAdder(x int) fn(int) int {
    // Returns a function
}
```

### Struct as Dedicated Declaration
Structs use dedicated `struct` keyword, not `type` expressions. Prevents inline anonymous structs.

```go
// Struct declaration (not a type alias)
struct Point {
    x int;
    y int;
}

// Usage
let p Point = {x = 1, y = 2};

// Not allowed - no inline structs
let bad struct { x int } = {x = 1};  // Error
```

### Implementation Notes
- Parse `AST_TYPE_FUNCTION` nodes in any type expression context
- Add `AST_DECL_STRUCT` separate from `AST_DECL_TYPE`
- Reject `AST_TYPE_STRUCT` in type resolution (only named structs allowed)
- Function types resolve to `TYPE_FUNCTION` in type system
