# Open Decisions

These are intentionally unresolved. Resolve them with examples and tests before
depending on them in the rewrite.

## Highest priority

1. Which implicit conversions exist between concrete numeric types?
2. Are all module cycles rejected initially?
3. Do types and values occupy one namespace or separate namespaces?
4. Are generic requirements inferred only, explicitly expressible, or both?
5. What source-level behavior is guaranteed for overflow in each release mode?
6. What are the ownership and lifetime rules for slices and strings?

## Syntax and parser

- Retention of `print`, C-style `for`, `Type.{...}`, `some`, and `none`

## Semantics

- Shadowing rules
- Pointer arithmetic and pointer comparison
- Enum conversion rules
- Semantic representation and C ABI of `char`
- Untagged-union safety model
- Function compatibility across calling conventions
- Constant-expression language and evaluation rules
- Global initialization ordering
- Closure capture semantics

## Backend and runtime

- Supported C dialect and minimum toolchain versions
- ABI stability promise
- Runtime linkage model
- Panic and safety-check behavior
- Freestanding runtime requirements
- Target data-model assumptions

## Documentation method

For each decision:

1. add minimal positive and negative Pebble examples;
2. state the rule without referring to implementation order;
3. record its typed-IR consequence;
4. record its runtime or ABI consequence if any;
5. add the examples to the source test suite.
