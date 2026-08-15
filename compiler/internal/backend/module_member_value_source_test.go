package backend

import (
	"bytes"
	"strings"
	"testing"
)

// Phase 3 #15 — "Identifier, module member, partial member" value-source
// shapes. Investigation findings (2026-08-12):
//
//   - Plain identifier reads (local/param/module-level var/let) and the
//     base-less `.name` enum-variant shorthand both already work across every
//     value position probed (return, call argument, local init, comparison,
//     store, switch subject, optional payload, struct-field value, generic
//     argument, deref). The qualified module-member (lib::name) value path is
//     already proven end-to-end by
//     TestEmitImportedQualifiedValueFunctionTypePathsCompileAndRuns.
//   - REAL GAP fixed here: a module-member / same-file global / extern
//     VARIABLE read used as a struct-FIELD construction value
//     (`Point.{ x = lib::counter }`) was checker-accepted but rejected at
//     Emit with "references symbol N, which is not a local declared earlier in
//     the entry body". collectReferencedGlobals (globals.go) and
//     collectReferencedExternData (extern_data.go) walked only node.Children,
//     but a struct construction stores its field values in node.Fields
//     ([]FieldInit), so a global/extern read nested as a field value was never
//     discovered as referenced — its file-scope storage/declaration was never
//     emitted and the C referenced an undeclared pebble_global_<id>.
//     Both walks now special-case RecordConstruct and walk its Fields,
//     mirroring collectStructTypesWalk/collectDirectCalls.

// TestModuleMemberGlobalAsStructFieldValueCompilesAndRuns is the exact repro
// of the fixed gap, in both its qualified-module-member and same-file-global
// forms: a mutable global read as a struct field's construction value. Before
// the fix each case failed Emit ("references symbol N, which is not a local
// declared earlier in the entry body"); now each emits real file-scope storage
// and the field reads it back.
func TestModuleMemberGlobalAsStructFieldValueCompilesAndRuns(t *testing.T) {
	t.Parallel()
	emitAndRunProvider(t, fixtureProvider{
		"main.peb": []byte(`import "./lib";
type P = struct { x int; };
fn main() int {
    var p P = P.{ x = lib::counter };
    return p.x + 2;
}`),
		"lib.peb": []byte("var counter int = 40;\n"),
	}, 42)
}

func TestSameFileGlobalAsStructFieldValueCompilesAndRuns(t *testing.T) {
	t.Parallel()
	emitAndRun(t, "var g int = 40;\ntype P = struct { x int; };\nfn main() int {\n    var p P = P.{ x = g };\n    return p.x + 2;\n}", false, 42, false)
}

// TestNestedGlobalAsStructFieldValueCompilesAndRuns proves the fix reaches
// through nested construction: a global read inside an Inner struct literal
// that is itself a field value of an Outer literal.
func TestNestedGlobalAsStructFieldValueCompilesAndRuns(t *testing.T) {
	t.Parallel()
	emitAndRun(t, "var g int = 40;\ntype Inner = struct { v int; };\ntype Outer = struct { inner Inner; };\nfn main() int {\n    var o Outer = Outer.{ inner = Inner.{ v = g } };\n    return o.inner.v + 2;\n}", false, 42, false)
}

// TestExternVarAsStructFieldValueCompilesAndRuns is the extern-data mirror of
// the fix: an extern variable read as a struct field's construction value must
// emit the extern's real-C-name forward declaration (not fail Emit), and the
// linked program must observe the external value. Mirrors the
// compileAndRunWithShim pattern from extern_data_test.go.
func TestExternVarAsStructFieldValueCompilesAndRuns(t *testing.T) {
	t.Parallel()
	unit, snapshot, entryID, sources, resolution := buildFixtureWithSymbols(t, `extern {
    var shim_seed int;
}
type P = struct { x int; };
fn main() int {
    var p P = P.{ x = shim_seed };
    return p.x;
}`)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, resolution, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "extern int64_t shim_seed;") {
		t.Fatalf("emitted C missing extern declaration:\n%s", out)
	}
	compileAndRunWithShim(t, buf.Bytes(), "#include <stdint.h>\nint64_t shim_seed = 5;\n", 5)
}
