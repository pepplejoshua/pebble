# Compiler Implementation Guide - First 4 Passes

## Project Setup

Create these core files:
- `lexer.c/h` - Tokenization
- `parser.c/h` - AST construction
- `ast.h` - AST node definitions
- `symbol.c/h` - Symbol table implementation
- `type.c/h` - Type system
- `checker.c/h` - Semantic analysis (passes 2-4)
- `main.c` - Entry point

## Data Structures to Define

### AST Nodes
Create an AST node structure with a tagged union. You need node types for:
- Declarations: function, variable, constant, type
- Statements: return, if, while, block, expression statement, assignment
- Expressions: literals, identifiers, binary ops, unary ops, function calls, indexing
- Type expressions: named types, pointers, arrays, structs

Each node should store source location (file, line, column) for error messages.

### Symbol Structure
A symbol needs:
- Name (string)
- Kind (enum: function, variable, constant, type)
- Pointer to declaration AST node
- Type pointer (NULL initially, filled during pass 3)
- For functions: pointer to their local scope
- For constants: whether evaluated, and the constant value
- For variables: whether global, and storage info for later

### Type Structure
A type needs:
- Kind (enum: int, float, bool, string, void, pointer, array, slice, struct, function)
- For compound types: base type pointer
- For arrays: size
- For structs: arrays of field names and field types
- For functions: array of parameter types, return type
- Optional name (for user-defined types)

### Scope Structure
A scope needs:
- Pointer to parent scope (NULL for global)
- Hash map from names to symbol pointers
- Array of child scopes (for nested scopes)

## Type System Initialization

Before pass 1, initialize your type system:
1. Create built-in type objects (int, float, bool, string, void)
2. Create a global type table (hash map)
3. Register built-in types in the type table
4. Create the global scope
5. Set current scope to global

## Pass 1: Lexer and Parser

### Lexer Details
- Read source character by character
- Skip whitespace and comments
- Recognize keywords, identifiers, literals, operators, punctuation
- Return tokens with: type, lexeme, location, and literal value (for numbers/strings)
- Handle escape sequences in strings
- Support your chosen numeric literal formats

### Parser Details
- Implement recursive descent parsing
- Build AST nodes as you parse
- For precedence, either use precedence climbing or separate functions per precedence level
- Store source location in every AST node
- Don't validate semantics yet - just build the tree
- Handle syntax errors gracefully with clear messages

Grammar suggestions for simplicity:
- Require semicolons for statements (easier to parse)
- C-like syntax for familiarity
- Explicit type annotations (skip type inference initially)
- No operator overloading yet

## Pass 2: Collect Globals

### Implementation approach:
1. Walk the top-level declarations from your parsed AST
2. For each declaration, extract just the name and kind
3. Check if the name already exists in global scope
4. If duplicate, report error with both locations
5. Create a symbol with: name, kind, AST node pointer, NULL type
6. Add symbol to global scope

### What NOT to do:
- Don't look at function bodies
- Don't resolve any types
- Don't check if types exist
- Don't evaluate constants
- Don't process anything nested

### Error handling:
- Duplicate global names are errors
- Store the first declaration location in the symbol so you can report "already declared at line X"

## Pass 3: Check Globals

This is the biggest pass. Break it into sub-passes:

### Sub-pass 3a: Check Type Declarations
Walk all symbols that are types:
1. Look at the AST node for this type declaration
2. Recursively resolve all type expressions in the declaration
3. For named types, look them up in the type table
4. For struct types, resolve each field's type
5. Create the complete type object
6. Store it in symbol->type
7. Add to type table under the type's name

Handle forward references:
- For pointers to types, you can reference types not fully defined yet
- For value fields in structs, you need the complete type
- Detect cycles: struct A { b: B }; struct B { a: A } - this is invalid
- Struct A { b: *B }; struct B { a: *A } - this is valid (pointers break the cycle)

### Sub-pass 3b: Check Constants
Walk all symbols that are constants:
1. Look at the initializer expression
2. If it's a literal, infer the type
3. If explicit type given, resolve it
4. Validate initializer matches the type
5. Store the type in symbol->type
6. Evaluate the constant value if it's a compile-time literal
7. Mark as evaluated

For now, only support literal constants. Constant folding (expressions like `2 + 3`) can come later.

### Sub-pass 3c: Check Global Variables
Walk all symbols that are global variables:
1. Resolve their declared type
2. If there's an initializer, type-check it (but this requires expression checking)
3. For now, you can skip initializer checking or only allow literals
4. Store type in symbol->type

### Sub-pass 3d: Check Function Signatures
Walk all symbols that are functions:
1. Resolve each parameter's type
2. Resolve the return type
3. Create a function type object containing parameter types and return type
4. Store in symbol->type

Then, create the function's scope:
1. Create a new scope with global as parent
2. For each parameter, create a symbol in this scope
3. Set parameter symbol's type
4. Store this scope in function symbol->scope

### Type Resolution Algorithm
When resolving a type expression:
- Named type: look up in type table, error if not found
- Pointer type: resolve base type, create pointer type
- Array type: resolve element type, store size, create array type
- Struct type: resolve all field types, create struct type
- Function type: resolve all param types and return type, create function type

Cache compound types to avoid duplicates - `*int` should be the same type object everywhere.

## Pass 4: Check Function Bodies

Walk all function symbols:
1. Get the function's scope from symbol->scope
2. Push this scope (it already has parameters)
3. Get the function's return type from symbol->type
4. Type-check the function body
5. Verify all paths return the correct type (if not void)
6. Pop the scope
7. Mark function as checked

### Statement Checking
For each statement type:

**Variable declaration:**
- Resolve the declared type
- If there's an initializer, check it
- Verify initializer type matches declared type
- Create symbol in current scope with the type
- Check for shadowing if desired (warning or error)

**Return statement:**
- Check the expression
- Verify type matches current function's return type

**If statement:**
- Check condition expression
- Verify condition is boolean
- Check then-branch
- Check else-branch if present
- For now, don't worry about return path analysis

**While loop:**
- Check condition, verify boolean
- Check body

**Assignment:**
- Check left-hand side (must be a variable)
- Check right-hand side expression
- Verify types match

**Expression statement:**
- Just check the expression, ignore result

**Block:**
- Create new scope as child of current
- Push it
- Check all statements
- Pop it

### Expression Checking
For each expression, return its type:

**Literals:**
- Int literal → int type
- Float literal → float type
- String literal → string type
- Bool literal → bool type

**Identifier:**
- Look up in current scope (searches up parent chain)
- If not found, error
- Return symbol's type

**Binary operations:**
- Check left operand
- Check right operand
- Verify operation is valid for these types
- For arithmetic (+, -, *, /): both must be numeric, result is numeric
- For comparison (<, >, <=, >=): both must be numeric, result is bool
- For equality (==, !=): types must match, result is bool
- For logical (&&, ||): both must be bool, result is bool
- Return the result type

**Unary operations:**
- Check operand
- Verify operation is valid
- Negation (-): operand must be numeric, result is same type
- Logical not (!): operand must be bool, result is bool

**Function call:**
- Look up function name in scope
- Verify it's a function symbol
- Check argument count matches parameter count
- Check each argument expression
- Verify each argument type matches corresponding parameter type
- Return function's return type

**Array/slice indexing:**
- Check array expression
- Verify it's an array or slice type
- Check index expression
- Verify index is integer
- Return element type of array/slice

**Member access (for structs):**
- Check object expression
- Verify it's a struct type
- Look up field name in struct
- Verify field exists
- Return field's type

### Scope Management
Keep a global "current scope" pointer. Operations:
- `scope_push(scope)`: set current to scope
- `scope_pop()`: set current to current->parent
- `symbol_lookup(name)`: search current, then parent, then parent's parent, etc.
- `symbol_declare(name, ...)`: add to current scope only

### Error Messages
Make them helpful:
- Include file, line, column
- Show the actual code line
- Point with ^ to the problem
- Explain what's wrong
- Suggest fixes if possible

Examples:
```
error: use of undeclared variable 'x'
  --> test.lang:5:12
   |
 5 |     return x + 1;
   |            ^
   |
help: did you mean 'y'?

error: type mismatch in binary operation
  --> test.lang:8:15
   |
 8 |     var z = x + "hello";
   |               ^^^^^^^^^
   |
expected: int + int
got: int + string
```

## Testing Strategy

Test each pass incrementally:

**After Pass 1:**
- Parse simple programs, print AST
- Verify structure is correct
- Test error recovery

**After Pass 2:**
- Print symbol table after collection
- Verify all globals are found
- Test duplicate name detection

**After Pass 3:**
- Print symbol table with types
- Verify all types are resolved
- Test forward references
- Test invalid type references

**After Pass 4:**
- Test simple function bodies
- Test variable scoping
- Test type errors in expressions
- Test function calls

Start with tiny programs:
```
fn main() int {
    return 42;
}
```

Then grow:
```
fn add(a int, b int) int {
    return a + b;
}

fn main() int => add(5, 10)
```

Then add complexity:
```
type Point = struct {
    x: int;
    y: int;
};

fn distance(p Point) int {
    return p.x + p.y;  // simplified
}
```

## Common Implementation Pitfalls

1. **Forgetting to check for NULL:** Always verify symbol_lookup didn't return NULL
2. **Not copying strings:** If you store strings from AST, duplicate them - AST might be freed
3. **Circular type dependencies:** Detect infinite recursion in type resolution
4. **Scope leaks:** Always pop what you push
5. **Error cascading:** After an error, try to keep checking - one error shouldn't cause 50 more
6. **Location tracking:** Store it everywhere, you'll need it for errors
7. **Memory management:** Be consistent - decide if you're using arena allocation, manual free, or garbage collection

## Nice-to-Have Features

Once basics work:
- Better error recovery (don't stop at first error)
- Type inference for local variables
- Constant expression evaluation
- Return path analysis (ensure all paths return)
- Unused variable warnings
- Better built-in types (i32, i64, f32, f64)
- String interning for performance

## Debugging Tips

Add debug printing:
- Print symbol table after each pass
- Print AST in a readable format
- Add `-v` flag for verbose output
- Print what you're checking as you check it

Use assertions:
- Assert types are non-NULL when expected
- Assert scopes are properly nested
- Assert symbol kinds match expectations

Build incrementally:
- Get integers and addition working first
- Add more operations gradually
- Add features one at a time
- Always have a working version to fall back to
