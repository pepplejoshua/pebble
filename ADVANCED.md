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


## 3. Module System

### Goal
Support multi-file projects with explicit imports and qualified access to avoid name collisions.

```pebble
// math.peb
fn add(a int, b int) int {
    return a + b;
}

fn multiply(a int, b int) int {
    return a * b;
}

type Point = (int, int);

// main.peb
use "math";

fn main() int {
    let p math.Point = (10, 20);
    return math.add(p.0, p.1);
}
```

### Design Principles

**Simplicity First:**
- File-based imports (no complex package system)
- Qualified access prevents name collisions
- Everything public by default (visibility added later)
- Mutual dependencies supported naturally

**Build on Existing 4-Pass System:**
The current compilation passes already support this design:
1. Parse all files (including imports)
2. Collect globals from all modules
3. Resolve types across modules
4. Check function bodies with cross-module references

### Syntax

**Import Declaration:**
```pebble
use "path/to/module";  // Looks for path/to/module.peb
use "math";            // Looks for math.peb
use "lib/utils";       // Looks for lib/utils.peb
```

- Auto-appends `.peb` extension
- Path relative to current file (initially)
- Module name = filename (last component of path)

**Qualified Access:**
```pebble
use "math";

fn main() {
    math.add(1, 2);          // Function call
    let p math.Point;        // Type reference
    math.CONSTANT;           // Constant reference
}
```

**Multiple Imports:**
```pebble
use "math";
use "graphics";
use "lib/utils";

fn main() {
    math.add(1, 2);
    graphics.draw_line(0, 0, 10, 10);
    utils.log("Hello");
}
```

### Implementation Strategy

**Phase 1: Parsing and Module Loading**

1. Start with entry file (e.g., `main.peb`)
2. Parse and collect `use` declarations
3. Recursively load and parse imported files
4. Build dependency graph
5. Detect import cycles (error if found)

```c
typedef struct Module {
    char* path;              // "math.peb"
    char* name;              // "math" (for qualified access)
    AstNode* ast;            // Parsed AST
    Scope* scope;            // Module's global scope
    Module** imports;        // Imported modules
    size_t import_count;
} Module;
```

**Phase 2: Symbol Collection (Pass 2 Extended)**

For each module:
1. Create module-level scope
2. Collect all global declarations into module scope
3. Track module name for qualified access

```c
// In symbol table:
Symbol {
    char* name;           // "add"
    Module* module;       // Which module it's from
    char* qualified_name; // "math.add"
    ...
}
```

**Phase 3: Cross-Module Name Resolution**

When resolving `math.add`:
1. Look up module named "math" in current file's imports
2. Look up symbol "add" in math module's scope
3. Return the symbol

```c
Symbol* resolve_qualified_name(const char* module, const char* name) {
    Module* mod = find_imported_module(current_module, module);
    if (!mod) {
        error("module '%s' not imported", module);
    }

    Symbol* sym = scope_lookup(mod->scope, name);
    if (!sym) {
        error("'%s' not found in module '%s'", name, module);
    }

    return sym;
}
```

**Phase 4: Code Generation**

All modules compile into a single C translation unit:

```c
// Generated C from multiple modules:

// Forward declarations (all modules)
int math_add(int, int);
typedef struct { int _0; int _1; } math_Point;
int main(void);

// Definitions (all modules)
int math_add(int a, int b) {
    return a + b;
}

int main(void) {
    math_Point p = {10, 20};
    return math_add(p._0, p._1);
}
```

Name mangling: `module_name` (e.g., `math_add`, `math_Point`)

### Mutual Dependencies

Mutual dependencies work naturally with the 4-pass system:

```pebble
// a.peb
use "b";
type A = (int, *b.B);

fn process_a() b.B {
    // ...
}

// b.peb
use "a";
type B = (str, *a.A);

fn process_b() a.A {
    // ...
}
```

**Why it works:**
1. **Pass 1:** Parse both files, build AST
2. **Pass 2:** Collect `A` and `B` as symbols (types not resolved yet)
3. **Pass 3:** Resolve `A = (int, *b.B)`
   - Lookup `b.B` in module b's scope
   - It exists (collected in Pass 2), return placeholder
   - Pointers allow forward references ✓
4. **Pass 4:** Check function bodies

Circular type dependencies are still caught:
```pebble
// a.peb
use "b";
type A = b.B;  // Direct alias

// b.peb
use "a";
type B = a.A;  // Direct alias

// Error: circular type dependency!
```

### Path Resolution

**Initial implementation (simple):**
- Paths relative to current file
- `use "math"` in `/project/src/main.peb` → looks for `/project/src/math.peb`
- `use "lib/utils"` → looks for `/project/src/lib/utils.peb`

**Future enhancements:**
- Search paths: `-I/include/path`
- Standard library location
- Absolute paths: `use "/abs/path/module"`
- Current directory: `use "./sibling"`
- Parent directory: `use "../other"`

### Module Name Extraction

From path, extract module name:

```
use "math"           → module name: "math"
use "lib/utils"      → module name: "utils"
use "a/b/c/helper"   → module name: "helper"
```

Algorithm:
1. Take last path component
2. Strip `.peb` extension if present
3. Use as module name for qualified access

### Import Cycle Detection

```pebble
// a.peb
use "b";

// b.peb
use "c";

// c.peb
use "a";  // Cycle: a → b → c → a
```

**Detection:**
- Track "currently loading" modules
- If we try to load a module already in this set, it's a cycle
- Report error with full cycle path

```
error: circular import detected
  a.peb imports b.peb
  b.peb imports c.peb
  c.peb imports a.peb
```

### Future Enhancements

**Visibility Control (later):**
```pebble
// math.peb
pub fn add(a int, b int) int { ... }     // Public
fn helper(x int) int { ... }             // Private

// main.peb
use "math";
math.add(1, 2);      // ✓ OK
math.helper(5);      // ✗ Error: 'helper' is private
```

**Import Aliasing (later):**
```pebble
use "very/long/path/math" as m;

fn main() {
    m.add(1, 2);  // Shorter!
}
```

**Selective Imports (later):**
```pebble
from "math" use add, multiply;  // Import specific items

fn main() {
    add(1, 2);  // No qualification needed
}
```

**Package System (much later):**
- Package manifests
- Versioning
- Dependency management
- Central repository

### Testing Strategy

**Test 1: Basic import**
```pebble
// helper.peb
fn double(x int) int { return x * 2; }

// main.peb
use "helper";
fn main() int { return helper.double(21); }
```

**Test 2: Mutual dependency**
```pebble
// a.peb
use "b";
type A = (int, *b.B);

// b.peb
use "a";
type B = (str, *a.A);

// main.peb
use "a";
use "b";
fn main() {
    var x a.A;
    var y b.B;
}
```

**Test 3: Import cycle detection**
```pebble
// Should error
// a.peb: use "b";
// b.peb: use "a";
```

**Test 4: Multiple imports**
```pebble
use "math";
use "graphics";
use "utils";

fn main() {
    math.add(1, 2);
    graphics.draw(10, 20);
    utils.log("test");
}
```

### Benefits

1. **Scales naturally** - No global namespace pollution
2. **Clear dependencies** - Explicit `use` statements
3. **No surprises** - Qualified access makes source clear
4. **Future-proof** - Can add visibility, aliasing, packages later
5. **Works with existing compiler** - Minimal changes to 4-pass system
