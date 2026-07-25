# Open Decisions

These are intentionally unresolved. Resolve them with examples and tests before
depending on them in the rewrite.

## Highest priority

1. Are generic requirements inferred only, explicitly expressible, or both?
2. What source-level behavior is guaranteed for overflow in each release mode?
3. What are the ownership and lifetime rules for slices and strings?

## Syntax and parser

- Long-term retention of `print`, C-style `for`, range `loop`, `some`, and
  `none` after the compatibility parser is complete

## Semantics

- Pointer arithmetic and pointer comparison
- Enum conversion rules
- Semantic representation and C ABI of `char`
- Untagged-union safety model
- Function compatibility across calling conventions
- Closure capture semantics

## Backend and runtime

- Supported C dialect and minimum toolchain versions
- ABI stability promise
- Runtime linkage model
- Panic and safety-check behavior
- Freestanding runtime requirements
- Target data-model assumptions

## Resolved

These were listed as open and have since been decided by the phase
documents. They are recorded here so they are not reopened by mistake.

- **Implicit conversions between concrete numeric types: none.** Every
  distinct concrete numeric pair requires an explicit `as` cast.
  `06b-validation-and-typed-ir.md` states it directly: "There is no implicit
  conversion between distinct concrete numeric types. `int`, `uint`, every
  exact-width integer, `f32`, and `f64` are distinct."
- **Constant-expression language and evaluation rules: specified.**
  `06a-semantic-fact-generation.md` defines the accepted constant grammar
  and its exclusions in "Constant evaluator and `ArrayLengthEvaluator`".
  Slice `06a.2` implements it.
- **Global initialization ordering: moot by construction.** Every non-extern
  global `let`/`var` requires a constant initializer accepted by the `06a`
  constant language, so all globals are determined at compile time and there
  is no runtime ordering to specify. This becomes a live decision again only
  if non-constant global initializers are ever accepted.

## Documentation method

For each decision:

1. add minimal positive and negative Pebble examples;
2. state the rule without referring to implementation order;
3. record its typed-IR consequence;
4. record its runtime or ABI consequence if any;
5. add the examples to the source test suite.
