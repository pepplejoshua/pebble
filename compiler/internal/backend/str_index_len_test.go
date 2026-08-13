package backend

import "testing"

// TestStrIndexLenRead proves `.len` reads directly on a str-typed indexed
// lvalue (`s[i].len`, `a[i].len`) emit a correct width-suffixed
// pebble_rt_checked_index_<suffix> and return the element's true runtime
// length. Before the fix the .len Load's own uint width was threaded into the
// receiver's checked-index construction, so the helper came out with an empty
// suffix (pebble_rt_checked_index_, an undeclared function at cc time).
func TestStrIndexLenRead(t *testing.T) {
	// The exact repro: a []str slice element's .len, cast to int.
	emitAndRun(t, `fn main() int {
    var a [3]str = ["a", "bb", "ccc"];
    var s []str = a[:];
    return s[1].len as int;
}`, false, 2, false)

	// .len on a str slice element accessed via a NON-literal (local variable)
	// index, confirming the width threads correctly through the checked-index
	// argument too.
	emitAndRun(t, `fn main() int {
    var a [3]str = ["a", "bb", "ccc"];
    var s []str = a[:];
    let i int = 1;
    return s[i].len as int;
}`, false, 2, false)

	// .len on a str FIXED ARRAY element — the array branch of the same
	// CheckedIndexPlace lowering (confirmed shares the code path: before the
	// fix it emitted the same empty-suffix helper).
	emitAndRun(t, `fn main() int {
    var a [3]str = ["a", "bb", "ccc"];
    return a[1].len as int;
}`, false, 2, false)

	// .len read through a local-variable index on a fixed array, and the
	// last element's length, so both ends of the array are proven.
	emitAndRun(t, `fn main() int {
    var a [3]str = ["a", "bb", "ccc"];
    let i int = 2;
    return a[i].len as int;
}`, false, 3, false)

	// .len in a uint value position (a uint local's initializer) — the
	// buildUintExpr path, which already threaded the entry width; a
	// regression guard that the fix did not disturb it.
	emitAndRun(t, `fn main() int {
    var a [3]str = ["a", "bb", "ccc"];
    var s []str = a[:];
    var u uint = s[1].len;
    return u as int;
}`, false, 2, false)

	// An out-of-bounds .len read still bounds-checks: s has 3 elements, so
	// s[3].len must panic (abnormal termination) through the now-suffixed
	// checked-index helper rather than read garbage.
	emitAndRun(t, `fn main() int {
    var a [3]str = ["a", "bb", "ccc"];
    var s []str = a[:];
    return s[3].len as int;
}`, false, 0, true)
}

// TestStrIndexLenRegression proves adjacent .len shapes that already worked
// still work after the fix: a slice-typed struct field's .len on an indexed
// struct element, and an ordinary str local's .len read not going through an
// index at all.
func TestStrIndexLenRegression(t *testing.T) {
	// A slice-typed struct field's .len on an indexed struct element
	// (arr[1].items.len) — the same structural-.len-through-a-CheckedIndex
	// shape with a slice (not str) receiver.
	emitAndRun(t, `type Bag = struct { items []int; };
fn main() int {
    var a [3]int = [1, 2, 3];
    var b Bag = Bag.{ items = a[:] };
    var arr [2]Bag = [b, b];
    return arr[1].items.len as int;
}`, false, 3, false)

	// A plain str local's .len, no index involved — the receiver is a
	// StoragePlace, which never built a checked index.
	emitAndRun(t, `fn main() int {
    let s str = "hello";
    return s.len as int;
}`, false, 5, false)

	// A slice-typed struct field's .len on a plain struct local (no index) —
	// the receiver is a StoragePlace through one FieldPlace.
	emitAndRun(t, `type Bag = struct { items []int; };
fn main() int {
    var a [3]int = [1, 2, 3];
    var b Bag = Bag.{ items = a[:] };
    return b.items.len as int;
}`, false, 3, false)
}
