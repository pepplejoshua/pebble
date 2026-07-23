# Module Graph Fixtures

Each case directory is mounted as an in-memory source provider. `main.peb` is
the entry module in package `app`; `std/` is the configured standard-library
root; and `roots/a` and `roots/b` are configured package search roots owned by
`package-a` and `package-b`, respectively.

The harness marks `M0002/unreadable/blocked.peb` unreadable without depending
on host permissions. The two `M0007` cases lower the depth and module limits in
their `BuildConfig`. No fixture depends on the process working directory.
