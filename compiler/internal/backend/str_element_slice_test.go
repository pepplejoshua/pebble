package backend

import "testing"

// TestStrElementSliceLocalDeclarationAndIndexRead proves a []str local
// declared from an array-literal base, with index-read of individual elements.
func TestStrElementSliceLocalDeclarationAndIndexRead(t *testing.T) {
	// A [3]str array sliced to []str, then index-sliced into individual chars.
	emitAndRun(t, `fn main() int {
    var a [3]str = ["hi", "x", "bye"];
    var s []str = a[:];
    // s[0] is "hi", its first char is 'h' (104).
    return s[0][0] as int;
}`, false, 104, false)

	// Index read of the second element's first char.
	emitAndRun(t, `fn main() int {
    var a [3]str = ["hi", "world", "bye"];
    var s []str = a[:];
    return s[1][0] as int;
}`, false, 119, false)

	// Index read of last element's last char ('e' = 101).
	// Use a helper to compute len-based offset since the checker anchors
	// the inner index to the element type (str -> pebble_rt_str_char_at_i32).
	emitAndRun(t, `fn last_char(s str) int {
    return s[s.len - 1] as int;
}
fn main() int {
    var a [3]str = ["hi", "world", "bye"];
    var s []str = a[:];
    return last_char(s[2]);
}`, false, 101, false)
}

// TestStrElementSliceIndexedWrite proves writing back into a str slice element.
func TestStrElementSliceIndexedWrite(t *testing.T) {
	// Write a new string literal into a slice element, then read it back.
	emitAndRun(t, `fn main() int {
    var a [3]str = ["old", "middle", "end"];
    var s []str = a[:];
    s[0] = "new";
    return s[0][0] as int;
}`, false, 110, false)

	// Write into middle element, verify via length using a helper function
	// (avoids the pre-existing CheckedIndex width bug when .len is accessed
	// directly on a str-typed indexed expression like s[1].len).
	emitAndRun(t, `fn str_len(s str) int {
    return s.len as int;
}
fn main() int {
    var a [3]str = ["a", "bb", "ccc"];
    var s []str = a[:];
    s[1] = "hello";
    return str_len(s[1]);
}`, false, 5, false)
}

// TestStrElementSliceParameter proves passing a []str to a helper function,
// both from an inline literal construction and from an existing local.
func TestStrElementSliceParameter(t *testing.T) {
	// Inline literal slice passed as parameter.
	emitAndRun(t, `fn take_strs(s []str) int {
    return s.len as int;
}
fn main() int {
    var a [3]str = ["one", "two", "three"];
    var s []str = a[:];
    return take_strs(s);
}`, false, 3, false)

	// Existing local passed as parameter.
	emitAndRun(t, `fn take_strs(s []str) int {
    return s.len as int;
}
fn main() int {
    var a [3]str = ["alpha", "beta", "gamma"];
    var s []str = a[:];
    return take_strs(s);
}`, false, 3, false)

	// Parameter that reads an element from the passed slice.
	emitAndRun(t, `fn first_char(s []str) int {
    return s[0][0] as int;
}
fn main() int {
    var a [2]str = ["pebble", "compiler"];
    var s []str = a[:];
    return first_char(s);
}`, false, 112, false)
}

// TestStrElementSliceReturn proves returning a []str from a helper, both
// directly and forwarded through another call.
func TestStrElementSliceReturn(t *testing.T) {
	// Direct return of a caller-owned slice.
	// Use char-at to verify content (avoiding .len on str-indexed expr which
	// triggers a pre-existing CheckedIndex width-resolution bug).
	emitAndRun(t, `fn identity(s []str) []str {
    return s;
}
fn main() int {
    var a [2]str = ["foo", "bar"];
    var s []str = a[:];
    var t []str = identity(s);
    return t[0][0] as int;
}`, false, 102, false)

	// Forwarded through two calls.
	emitAndRun(t, `fn pass_through(s []str) []str {
    return s;
}
fn identity(s []str) []str {
    return pass_through(s);
}
fn main() int {
    var a [2]str = ["abc", "def"];
    var s []str = a[:];
    var t []str = identity(s);
    return t[1][0] as int;
}`, false, 100, false)

	// Return from a helper that receives a slice and returns it unchanged,
	// verified by indexing into the returned slice.
	emitAndRun(t, `fn view_strings(s []str) []str {
    return s;
}
fn main() int {
    var a [3]str = ["a", "bb", "ccc"];
    var s []str = a[:];
    var v []str = view_strings(s);
    return v[2][v[2].len - 1] as int;
}`, false, 99, false)
}

// TestStrElementSliceLenProves the .len property of a []str slice works
// across different construction shapes (full slice, partial range, re-slice).
func TestStrElementSliceLen(t *testing.T) {
	// Full slice.
	emitAndRun(t, `fn main() int {
    var a [5]str = ["a", "b", "c", "d", "e"];
    var s []str = a[:];
    return s.len as int;
}`, false, 5, false)

	// Partial range slice.
	emitAndRun(t, `fn main() int {
    var a [5]str = ["a", "b", "c", "d", "e"];
    var s []str = a[1:4];
    return s.len as int;
}`, false, 3, false)

	// Re-slice of an existing []str local.
	emitAndRun(t, `fn main() int {
    var a [5]str = ["a", "b", "c", "d", "e"];
    var s []str = a[:];
    var t []str = s[1:3];
    return t.len as int;
}`, false, 2, false)
}

// TestStrElementSliceMultiElementDistinctValues proves a multi-element []str
// with distinct string values works correctly — catches any indexing/copy-offset
// bug where elements might overlap or shift. Each sub-test verifies one element
// independently to ensure no cross-contamination.
func TestStrElementSliceMultiElementDistinctValues(t *testing.T) {
	// Four distinct strings, each verified by its unique first character.
	// apple[0]='a'=97, banana[0]='b'=98, cherry[0]='c'=99, date[0]='d'=100
	// Use separate helpers so each element is checked independently.
	emitAndRun(t, `fn get_first_char(s str) int {
    return s[0] as int;
}
fn main() int {
    var a [4]str = ["apple", "banana", "cherry", "date"];
    var s []str = a[:];
    if get_first_char(s[0]) != 97 { return 1; }
    if get_first_char(s[1]) != 98 { return 2; }
    if get_first_char(s[2]) != 99 { return 3; }
    if get_first_char(s[3]) != 100 { return 4; }
    return 0;
}`, false, 0, false)

	// Different lengths to verify no buffer overlap confusion.
	// Verify each element's length independently.
	emitAndRun(t, `fn get_len(s str) int {
    return s.len as int;
}
fn main() int {
    var a [4]str = ["a", "bbbbb", "cccccc", "ddddddd"];
    var s []str = a[:];
    if get_len(s[0]) != 1 { return 1; }
    if get_len(s[1]) != 5 { return 2; }
    if get_len(s[2]) != 6 { return 3; }
    if get_len(s[3]) != 7 { return 4; }
    return 0;
}`, false, 0, false)

	// Mixed short and long strings, verify each by first char.
	// Z=90, y=121, X=88
	emitAndRun(t, `fn get_first_char(s str) int {
    return s[0] as int;
}
fn main() int {
    var a [3]str = ["Z", "yYyYyYyYy", "XxX"];
    var s []str = a[:];
    if get_first_char(s[0]) != 90 { return 1; }
    if get_first_char(s[1]) != 121 { return 2; }
    if get_first_char(s[2]) != 88 { return 3; }
    return 0;
}`, false, 0, false)
}

// TestStrElementSliceParameterAndReturnCombined proves a helper that takes
// a []str parameter AND returns a []str, combined with index operations.
func TestStrElementSliceParameterAndReturnCombined(t *testing.T) {
	// Helper takes []str, returns a sub-slice.
	emitAndRun(t, `fn tail(s []str) []str {
    return s[1:];
}
fn main() int {
    var a [3]str = ["first", "second", "third"];
    var s []str = a[:];
    var t []str = tail(s);
    return t[0][0] as int;
}`, false, 115, false)

	// Two helpers chained: one takes and returns, another does the same.
	emitAndRun(t, `fn dup(s []str) []str {
    return s;
}
fn get_second(s []str) str {
    return s[1];
}
fn main() int {
    var a [3]str = ["red", "green", "blue"];
    var s []str = a[:];
    var t []str = dup(s);
    var picked = get_second(t);
    return picked[0] as int;
}`, false, 103, false)
}
