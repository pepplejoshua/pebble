# Typed IR and Caching

## Stable IDs

Compiler entities are stored centrally and referenced by small IDs:

```text
SourceID  ModuleID  ScopeID  SymbolID  TypeID  ExprID  FunctionID
```

An ID is stable for the lifetime of one compilation snapshot. It avoids pointer
identity, makes ownership explicit, and provides a compact key for memoized
queries.

## Concrete type interning

[05a Semantic Type Store](05a-semantic-type-store.md) is the authoritative
contract. The shape below is a compact summary, not a second public API.

The concrete `TypeStore` has one predictable operation:

```text
Intern(TypeKey) -> TypeID
```

A `TypeKey` contains semantic identity, never a generated display string:

```text
Builtin(kind)
Pointer(TypeID)
Slice(TypeID)
Array(length, TypeID)
Tuple([TypeID...], length >= 1)
Optional(TypeID)
Function(convention, [TypeID...], return TypeID, variadic)
Nominal(SymbolID, [TypeID...])
TypeParameter(SymbolID)
```

Equal keys return the same `TypeID`. Hash collisions are resolved with complete
structural equality. Empty tuple keys are invalid. Aliases resolve before
interning. All structs, unions, tagged unions, enums, opaque external types,
and generic nominal instances use declaration identity plus concrete
arguments; their field spelling or generated C name is not their identity.

Inference variables, literal types, error types used for recovery, and
unresolved declarations remain outside this store. Interned values are
immutable. Display formatting and target layout are separate queries over a
`TypeID`.

Stable IDs do not by themselves implement caching. Caching additionally needs:

- a query key;
- a cached result;
- recorded dependencies;
- input revisions or fingerprints;
- invalidation rules.

## Typed IR

The typed IR is produced after successful constraint solving. It contains:

- resolved `SymbolID`s instead of source names;
- a `TypeID` for every expression and place;
- explicit coercion nodes;
- resolved call kind and calling convention;
- resolved fields, variants, and methods;
- structured control flow;
- generic obligations and specialization references;
- source spans for diagnostics.

It contains no unresolved lookup or heuristic type choice. Backend-specific C
names are assigned later.

## Useful first caches

Before incremental compilation, ordinary within-run memoization is useful:

```text
ResolveName(ScopeID, Name) -> SymbolID
ResolveType(TypeSyntaxID, EnvironmentID) -> TypeID
TypeOf(ExprID) -> TypeID
LayoutOf(TypeID, TargetID) -> Layout
Specialize(GenericSymbolID, []TypeID) -> FunctionID
LowerFunction(FunctionID, TargetID) -> LoweredFunction
```

The central stores ensure every phase can refer to the same entity without
holding fragile pointers into another phase's data.

## Incremental direction

For a later long-lived compiler or language server, query execution records
which other queries it reads. Changing a source file increments its revision.
A cached result is reused only if its transitive inputs remain valid.

Across process runs, arena indices are not stable enough. Persistent caches need
durable keys such as compiler version, target, runtime ABI version, canonical
module identity, declaration identity, and a content fingerprint. Persistent
caching is explicitly deferred until deterministic clean builds exist.

## Example invalidation chain

```text
edit function body
  -> parse(file) changes
  -> body constraints(function) changes
  -> typed body(function) changes
  -> specializations depending on that body change
  -> lowered C for those functions changes
```

Unrelated modules need not be invalidated if their recorded dependency surfaces
are unchanged.
