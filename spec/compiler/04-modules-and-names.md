# Modules, Names, and Scopes

## Module identity

A module is identified by a canonical source path plus its compilation package
identity. A short basename is display information and must not be the unique
identity.

Loading produces an explicit directed graph:

```text
ModuleID -> imported ModuleIDs
```

Graph construction records the import span for every edge. Duplicate imports
are diagnosed or deduplicated according to one documented rule.

## Import resolution

The resolver must specify, in order:

1. Relative imports
2. Standard-library imports such as `std:io`
3. Configured module search paths
4. Canonicalization and extension handling

The driver provides search roots. Semantic code does not inspect executable
paths or global strings.

## Cycles

**Required:** graph traversal detects cycles without recursion loops or score
propagation through cycles. The diagnostic presents the import-edge chain.

**Open:** either reject every module cycle initially, or permit cycles only
when their declarations can be collected and initialized without an ordering
dependency. Rejecting cycles is the simpler first contract.

## Namespaces and lookup

The specification must define whether types and values share a namespace.
Regardless of that choice, lookup is explicit:

```text
local lexical scopes
-> function parameters
-> module declarations
-> explicitly qualified imports
```

Imports do not silently inject every imported name. `module::member` resolves
against the selected imported module.

## Scope rules

- Parameters share the function body's outer lexical scope unless specified
  otherwise.
- A block introduces a lexical scope.
- Loop bindings exist only inside the loop body.
- Duplicate names in one namespace and scope are errors.
- Shadowing policy must be documented separately from duplicate declarations.
- Anonymous functions retain the defining module and lexical environment even
  if closure capture is not yet supported.
- Methods retain both their defining module and containing type.

## Prototype findings

**Current:** names are eagerly rewritten with module prefixes, module ordering
uses an import score, and lookup depends on global `current_scope` plus a
current module. These mechanisms are not part of the intended contract.
