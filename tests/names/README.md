# Name-resolution fixtures

Files under `valid` must resolve without `N` diagnostics. Directories under
`invalid/<CODE>` must produce the named stable diagnostic. `recovery` proves
that independent subtrees continue through damaged syntax.

The corpus covers module and local forward behavior, sequential bindings,
duplicates across kinds, nested shadowing, parameter/body collision, sibling
reuse, block and loop lifetimes, type parameters, aggregate members and
methods, qualified lookup and shadowing, missing members, captures, and
identity-directed neutral brackets.
