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

## 2. Anonymous Type Code Generation

### Challenge
Pebble allows inline/anonymous type expressions, but C requires all types to be declared before use.

```pebble
// Valid Pebble - inline types
var point (int, int) = (10, 20);
var data struct { x: int; y: str; };
fn process(items [](int, str)) { }
```

```c
// Invalid C - can't use anonymous types directly
struct { int x; char* y; } data;  // ❌ Error
```

### Solution: Multi-Pass Codegen with Content-Based Naming

**Pass 1: Type Collection**
- Walk entire AST
- Collect all anonymous types that need typedef declarations
- Generate deterministic names based on type structure
- Deduplicate identical structures

**Pass 2: C Code Generation**
1. Emit all typedef declarations
2. Emit named type declarations
3. Emit function prototypes
4. Emit function definitions

### Naming Scheme

Anonymous types are named by their structural content, ensuring identical types share the same name.

**Primitives (no typedef needed):**
```c
int, float, bool, char* (for str), void
```

**Pointers (no typedef, direct C syntax):**
```c
int*
char**
tuple_int_int*
```

**Tuples (typedef with content-based name):**
```pebble
(int, str)          → tuple_int_str
(int, (str, bool))  → tuple_int_tuple_str_bool
(*int, str)         → tuple_ptr_int_str
```

Generated C:
```c
typedef struct {
    int _0;
    char* _1;
} tuple_int_str;
```

**Anonymous Structs (typedef with field names):**
```pebble
struct { x: int; y: str }  → anon_struct_x_int_y_str
```

Generated C:
```c
typedef struct {
    int x;
    char* y;
} anon_struct_x_int_y_str;
```

**Slices (always typedef, always struct):**
```pebble
[]int               → slice_int
[]tuple_int_str     → slice_tuple_int_str
[]*int              → slice_ptr_int
```

Generated C:
```c
typedef struct {
    int* data;
    size_t len;
    size_t cap;
} slice_int;
```

**Arrays:**
- Simple arrays: Use direct C syntax
- Complex element types: Generate typedef

```pebble
[10]int             → int[10]           // Direct
[5](int, str)       → array_tuple_int_str_5[5]  // Typedef
```

Generated C for complex case:
```c
typedef tuple_int_str array_tuple_int_str_5[5];
```

### Implementation Details

**Type Name Generation Algorithm:**
```c
char* type_to_c_typename(Type* type) {
    switch (type->kind) {
        case TYPE_INT:    return "int";
        case TYPE_FLOAT:  return "float";
        case TYPE_BOOL:   return "bool";
        case TYPE_STRING: return "str";
        case TYPE_VOID:   return "void";

        case TYPE_POINTER:
            return sprintf("ptr_%s", type_to_c_typename(type->base));

        case TYPE_TUPLE:
            // "tuple_<type1>_<type2>_..."
            char* name = "tuple";
            for (each element type) {
                name = sprintf("%s_%s", name, type_to_c_typename(element));
            }
            return name;

        case TYPE_SLICE:
            return sprintf("slice_%s", type_to_c_typename(type->element));

        case TYPE_ARRAY:
            return sprintf("array_%s_%zu",
                          type_to_c_typename(type->element),
                          type->size);

        case TYPE_STRUCT:
            if (named) return type->name;
            // "anon_struct_<field1>_<type1>_..."
            char* name = "anon_struct";
            for (each field) {
                name = sprintf("%s_%s_%s", name, field.name,
                              type_to_c_typename(field.type));
            }
            return name;
    }
}
```

**Deduplication:**
- Content-based names naturally deduplicate
- `(int, str)` used 10 times → single `tuple_int_str` typedef
- Track emitted typedefs to avoid duplicates

**Output Structure:**
```c
// 1. Standard includes
#include <stddef.h>
#include <stdbool.h>

// 2. Forward declarations (for recursive types)
typedef struct tuple_int_ptr_tuple_int_ptr_tuple_int_ptr_... tuple_int_ptr_...;

// 3. Anonymous type typedefs
typedef struct { int _0; char* _1; } tuple_int_str;
typedef struct { int x; char* y; } anon_struct_x_int_y_str;
typedef struct { int* data; size_t len; size_t cap; } slice_int;

// 4. Named type declarations
typedef tuple_int_str Point;
typedef struct Node Node;
struct Node { int value; Node* next; };

// 5. Function prototypes
int main(void);
tuple_int_str get_point(void);

// 6. Function definitions
tuple_int_str get_point(void) {
    tuple_int_str result = {10, "hello"};
    return result;
}
```

### Edge Cases

**Self-Referential Anonymous Types:**
```pebble
// Not allowed - anonymous types can't reference themselves
var x (int, *???)  // What would ??? be?

// Solution: Must use named types
type Node = (int, *Node);  // ✓ OK
```

**Very Long Names:**
```pebble
type Complex = ([10](int, (str, bool)), *((int, int), str));
// Generates: array_tuple_int_tuple_str_bool_10, tuple_tuple_int_int_str, etc.
```

Name truncation not needed - C identifiers can be very long, and these are internal.

**Recursive Type Dependencies:**
```pebble
type A = (B, int);
type B = (A, str);  // Circular!
```

Already caught by semantic analysis (circular dependency error).

### Benefits

1. **Automatic deduplication** - Identical structures share typedefs
2. **Deterministic output** - Same input always generates same C code
3. **Readable names** - Type structure visible in name
4. **No collisions** - Content-based naming prevents conflicts
5. **C compiler friendly** - Clean, standard C with typedefs
```
