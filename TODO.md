# Pebble Compiler

## 🚧 Missing Language Features

### Ideas from discussions with Caleb
- extern can take a lib name. the compiler cli can take -L (lib location) and -l (lib name)
- `if let` to safely work with pointers.
- Add bitwise operators
- In range loop, allow the user specify the binding name for their loop variable
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

# Const Type Modifier Feature Specification

## Purpose
`const` is a type qualifier that prevents writes to memory while allowing reads. It applies only to reference types (slices, pointers) where memory access occurs.

## Syntax
```go
[]const T    // Slice of const elements
*const T     // Pointer to const data
```

## Semantics

### Slice Elements
`[]const T` allows reading elements but prevents writes to the underlying array memory.

```go
var arr []const int = [1, 2, 3];
print arr[0];     // ✓ Read allowed
arr[0] = 99;      // ✗ Error: cannot assign to const element

let data []const char = "hello";
print data[0];    // ✓ Read 'h'
data[0] = 'H';    // ✗ Error: cannot modify const element
```

### Pointers
`*const T` allows dereferencing to read but prevents writes through the pointer.

```go
var x int = 42;
var p *const int = &x;
print *p;         // ✓ Read allowed
*p = 99;          // ✗ Error: cannot assign through const pointer
p = &y;           // ✓ Reassigning pointer itself is allowed
```

### Type Propagation
Operations on const types yield const results.

```go
var arr []const int = [1, 2, 3];
var sub = arr[0..2];  // sub is []const int (const propagates)
sub[0] = 5;           // ✗ Error: sub's elements are const
```

### String Literals
String literals are `[]const char`, preventing modification of string data.

```go
type str = []const char;

var s str = "hello";
print s[0];       // ✓ Read 'h'
s[0] = 'H';       // ✗ Error: string elements are const

var sub = s[0..3]; // sub is []const char
```

**Codegen:** String literals emit as static global arrays:
```c
static const char _str_1[] = "hello";
// Usage wraps in slice struct
```

### Subtyping (Future)
Eventually support safe widening: `T → const T`

```go
fn read_only(data []const int) { ... }

var arr []int = [1, 2, 3];
read_only(arr);   // Future: []int → []const int (safe widening)
```

Narrowing `const T → T` remains disallowed (unsafe).

## What We Don't Support

### No const on Primitives in Variables
`const` on primitive variables is meaningless since variables are reassigned, not mutated:

```go
var x const int = 5;  // ✗ Disallow: const on primitive variable is meaningless
x = 10;               // This is reassignment, not mutation

// Use 'let' for immutable binding:
let x int = 5;        // ✓ Correct way to prevent reassignment
```

### No const Slice Itself
`const []T` syntax is confusing and redundant:

```go
const []int   // ✗ Disallow: unclear if slice or elements are const
[]const int   // ✓ Use this: clearly elements are const
```

### No const Pointer Itself
`const *T` (const pointer, mutable data) overlaps with `let`:

```go
const *int ptr;  // ✗ Disallow: use 'let' for immutable binding
let ptr *int;    // ✓ Pointer binding cannot be reassigned
```

### No Struct Fields (For Now)
Const on struct fields is deferred:

```go
type Point = struct {
    x const int,  // ✗ Not supported yet
    y int
};
```

### No Const Widening Initially
Implicit conversion `T → const T` is deferred:

```go
var arr []int = [1, 2, 3];
var readonly []const int = arr;  // ✗ Not supported initially (requires cast)
```

## Implementation Summary

**Type Representation:**
- Add `is_const` flag to Type
- For `[]const T`: set `element_type->is_const = true`
- For `*const T`: set `base_type->is_const = true`

**Canonical Names:**
- Include const in canonicalization
- `[]const int` → `"slice_const_int"`
- `[]int` → `"slice_int"`

**Type Checking:**
- Block writes through const references (indexing, dereferencing)
- Allow reads
- Propagate const through operations (slicing, etc.)

**Codegen:**
- Emit `const` in C types
- `[]const char` → `const char *`
- `*const int` → `const int *`
- String literals → `static const char[]` arrays

## Examples

**File reading returns immutable buffer:**
```go
fn read_file(path str) []const char {
    // ... read file ...
    return buffer;  // Caller can read but not modify
}

var contents = read_file("data.txt");
print contents[0];        // ✓ Read
contents[0] = 'X';        // ✗ Cannot modify const data
```

**Pointer to const for safe APIs:**
```go
fn compute(data *const int, len usize) int {
    var sum = 0;
    for i = 0; i < len; i = i + 1 {
        sum = sum + *(data + i);  // ✓ Read through const pointer
    }
    // *(data + 0) = 0;           // ✗ Would error: can't write
    return sum;
}
```

**Working with strings:**
```go
fn count_vowels(s str) int {  // str is []const char
    var count = 0;
    for i = 0; i < len(s); i = i + 1 {
        var c = s[i];             // ✓ Read char
        if is_vowel(c) {
            count = count + 1;
        }
    }
    return count;
}
```

### Statements
- [ ] **Switch/match statements** - If desired

- [ ] **Defer statements** - If desired (Go-style)

### Type System Enhancements
- [ ] **Const evaluation** - Evaluate constant expressions
  - `const SIZE = 10 * 10;`
  - Currently only supports literals

- [ ] **Generic types** - Future (complex)

### File Input and Processing
- [ ] **Error reporting with file context**
  - Show filename in error messages
  - Display source line with error
  - Point to exact column with `^`
  - Support multiple files in error messages

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
