# Pebble handoff for Claude

Repository: `/Users/iwarilama/Desktop/Code/pebble`

## Current state

Read `spec/compiler/proposals/07-generics-implementation-plan.md` in full.
Phase 07 generics slices 07.1 through 07.6e are complete, verified, committed,
and pushed. The plan's completed-slices section is authoritative. Do not reopen
completed work unless a new test exposes a real defect.

The next generics work must be a new, explicitly scoped slice. Do not invent a
07.7 implementation from the rough notes. First inspect the adjacent specs and
return a short proposal with exact files, acceptance criteria, and verification.

Known deferred items:

- `RequirementLiteralFits` checking is not implemented.
- Expression-bodied functions still have the pre-existing empty-block lowering
  bug documented under 07.3f.
- ABI/convention behavior for generic bare values was flagged as a possible
  follow-up, but 07.4b did not expose a failure.

## Recent structural refactor

These changes are behavior-preserving file splits and have passed the full
verification suite:

- TIR builder and dumper split into `builder.go` and `dump.go`.
- `ir_builder.go` split into control, value, place, literals, calls, and
  operators files.
- `record.go` split with `control_arena.go`.
- `control_facts.go` split with `control_predicates.go`.
- `solve_handoff.go` split with `frozen_audit.go`.
- `control_validation.go` split with `control_flow_validation.go`.

The current branch is clean and pushed. These refactors should not be mixed
with the next semantic/spec slice.

## Orc rules

For repository work, use one narrow blocking Orc task at a time:

```bash
orc run --claude --model opencode-go/deepseek-v4-flash \
  --prompt-file /tmp/orc_task_<name>.md "<short summary>"
```

Use `--claude` because Claude is dispatching. Use Flash only. Kimi is banned.
Keep each brief's file allow-list explicit, require `Do not commit or push`,
and require literal verification output with:

```bash
GOCACHE=/tmp/pebble-orc-gocache go test ./... -count=1
GOCACHE=/tmp/pebble-orc-gocache go test -race -count=1 ./...
GOCACHE=/tmp/pebble-orc-gocache go vet ./...
GOCACHE=/tmp/pebble-orc-gocache go build ./...
git diff --check
```

Wait on the same terminal session until Orc exits. Do not interrupt a quiet
worker. After completion, inspect the real diff and independently rerun the
full checks. Do not trust a worklog's `tests pass` claim by itself.

Workers leave changes uncommitted. The supervisor reviews and commits logical
implementation changes only after independent verification.
