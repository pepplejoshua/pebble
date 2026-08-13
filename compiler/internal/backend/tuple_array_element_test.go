package backend

import (
	"testing"
)

func TestEmitTupleWithArrayElementLocalDeclarationAndOrdinalReadCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A ([3]i32, i32) tuple is declared from a tuple literal and its second
	// element (ordinal 1) is read back into the return value: the array's own
	// pebble_array_<id>_t typedef must be emitted before the tuple typedef that
	// names it, and t.1 = 5 is the process exit code.
	emitAndRun(t, "fn main() i32 { var t ([3]i32, i32) = ([1,2,3], 5); return t.1; }", false, 5, false)
}

func TestEmitTupleWithArrayElementIndexedSubReadCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// An indexed read into the array-typed first element of a tuple.
	// Store t.0 in an array-typed local first, then index into that local.
	// tArr[0] = 1, tArr[1] = 2, tArr[2] = 3, so sum = 6.
	source := `
fn main() i32 {
    var t ([3]i32, i32) = ([1,2,3], 5);
    var tArr [3]i32 = t.0;
    return tArr[0] + tArr[1] + tArr[2];
}`
	emitAndRun(t, source, false, 6, false)
}

func TestEmitTupleWithArrayElementAsCallArgumentLiteralCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// Passing a tuple literal with an array element directly as a call argument
	// to a helper that reads its second element. The helper receives the tuple
	// by value and returns t.1 = 7.
	source := `
fn takeTuple(t ([3]i32, i32)) i32 { return t.1; }
fn main() i32 { return takeTuple(([10,20,30], 7)); }`
	emitAndRun(t, source, false, 7, false)
}

func TestEmitTupleWithArrayElementAsCallArgumentLocalCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// Passing an existing tuple local (with an array element) as a call argument
	// to a helper that reads its second element. The helper returns t.1 = 42.
	source := `
fn takeTuple(t ([3]i32, i32)) i32 { return t.1; }
fn main() i32 { var t ([3]i32, i32) = ([1,2,3], 42); return takeTuple(t); }`
	emitAndRun(t, source, false, 42, false)
}

func TestEmitTupleWithArrayElementReturnCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// Returning a tuple whose first element is an array from a helper, then
	// reading the array element in main. The helper returns ([1,2,3], 99),
	// and main reads t.1 = 99.
	source := `
fn makeTuple() ([3]i32, i32) { return ([1,2,3], 99); }
fn main() i32 { var r ([3]i32, i32) = makeTuple(); return r.1; }`
	emitAndRun(t, source, false, 99, false)
}

func TestEmitTupleWithArrayElementAsHelperResultUsedInExpressionCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A helper returns a tuple with an array element; main uses both elements
	// — sums the array elements (1+2+3=6) and adds r.1 (10) = 16.
	source := `
fn makeTuple() ([3]i32, i32) { return ([1,2,3], 10); }
fn main() i32 {
    var r ([3]i32, i32) = makeTuple();
    var arr [3]i32 = r.0;
    return arr[0] + arr[1] + arr[2] + r.1;
}`
	emitAndRun(t, source, false, 16, false)
}

func TestEmitTwoTupleLocalsWithArrayElementsCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// Two separate tuple locals each containing an array element, ensuring
	// the array typedefs are collected and emitted correctly for both.
	source := `
fn main() i32 {
    var a ([2]i32, i32) = ([10,20], 1);
    var b ([3]i32, i32) = ([30,40,50], 2);
    return a.1 + b.1;
}`
	emitAndRun(t, source, false, 3, false)
}
