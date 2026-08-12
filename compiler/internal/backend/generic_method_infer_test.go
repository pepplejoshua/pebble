package backend

import "testing"

// Phase 3 #10 — generic method type-parameter inference from a function-value
// argument. A generic method declaring its own type parameter beyond the
// containing type's (here R in `fn convert[K, R](self Outer[K], conv fn(K) R)`)
// failed to check at all when R is only inferable from a bare function-value
// argument's concrete signature: the walker's direct-call rule that grounds an
// inferred type argument from a non-literal argument (Equal(source,
// destination) in call_facts.go's finishCall) never fires for a method call,
// because the walker never resolves the method's callee symbol — only the
// solver's callMember/selectMethod does, at solve time. Fixed by adding the
// same grounding inside callMember (internal/infer/instantiate.go), once the
// resolved method is known to declare its own type parameters beyond the
// receiver's inherited ones.

// TestEmitGenericMethodTypeParamFromFunctionValueArgument proves the fixed
// shape: R is inferred purely from twice's concrete fn(int) int signature,
// with no explicit type arguments at the call site.
func TestEmitGenericMethodTypeParamFromFunctionValueArgument(t *testing.T) {
	emitAndRun(t, `type Inner[T] = struct { val T; };
type Outer[K] = struct { inner Inner[K]; fn convert[K, R](self Outer[K], conv fn(K) R) Inner[R] { return Inner[R].{ val = conv(self.inner.val) }; } };
fn twice(x int) int { return x * 2; }
fn main() int {
    var o Outer[int] = Outer[int].{ inner = Inner[int].{ val = 3 } };
    let r = o.convert(twice);
    return r.val;
}`, false, 6, false)
}

// TestEmitGenericMethodTypeParamAlsoInPlainParameter proves the fix doesn't
// regress the case where the method's own extra type param ALSO appears in a
// plain (non-function-typed) parameter alongside the function-value argument.
func TestEmitGenericMethodTypeParamAlsoInPlainParameter(t *testing.T) {
	emitAndRun(t, `type Inner[T] = struct { val T; };
type Outer[K] = struct { inner Inner[K]; fn convert[K, R](self Outer[K], conv fn(K) R, fallback R) Inner[R] { return Inner[R].{ val = conv(self.inner.val) }; } };
fn twice(x int) int { return x * 2; }
fn main() int {
    var o Outer[int] = Outer[int].{ inner = Inner[int].{ val = 3 } };
    let r = o.convert(twice, 0);
    return r.val;
}`, false, 6, false)
}

// TestEmitGenericMethodNoExtraTypeParamStillWorks proves the fix doesn't
// regress the already-working case: a generic method whose function-typed
// parameter reuses the RECEIVER's own type parameter (no extra method-local
// type parameter to ground).
func TestEmitGenericMethodNoExtraTypeParamStillWorks(t *testing.T) {
	emitAndRun(t, `type Inner[T] = struct { val T; };
type Outer[K] = struct { inner Inner[K]; fn apply[K](self Outer[K], conv fn(K) K) Inner[K] { return Inner[K].{ val = conv(self.inner.val) }; } };
fn twice(x int) int { return x * 2; }
fn main() int {
    var o Outer[int] = Outer[int].{ inner = Inner[int].{ val = 3 } };
    let r = o.apply(twice);
    return r.val;
}`, false, 6, false)
}

// TestEmitGenericFreeFunctionTypeParamFromFunctionValueArgument proves the
// FREE-function counterpart of the fixed shape (already worked before this
// fix — the walker's finishCall rule covers a direct call's known symbol) as
// a regression guard alongside the method-side fix.
func TestEmitGenericFreeFunctionTypeParamFromFunctionValueArgument(t *testing.T) {
	emitAndRun(t, `type Inner[T] = struct { val T; };
fn convert[K, R](inner Inner[K], conv fn(K) R) Inner[R] { return Inner[R].{ val = conv(inner.val) }; }
fn twice(x int) int { return x * 2; }
fn main() int {
    var o Inner[int] = Inner[int].{ val = 3 };
    let r = convert[int, int](o, twice);
    return r.val;
}`, false, 6, false)
	emitAndRun(t, `fn apply[R](f fn(int) R) R { return f(3); }
fn twice(x int) int { return x * 2; }
fn main() int { return apply(twice); }`, false, 6, false)
}

// TestEmitGenericMethodTypeParamOnlyInStructFnParamRejects confirms the
// checker fix's boundary: the method's own type param now infers correctly
// even when the function-typed argument's parameter is a struct type (proving
// the checker fix is general, not narrowly scoped to scalar fn signatures),
// but the backend still rejects the function VALUE itself — a separate,
// already-documented, general limitation (a function-typed value's signature
// may only mention scalar/pointer parameter shapes, tracker 14's "Function
// type and function value" row), not a generics-specific gap.
func TestEmitGenericMethodTypeParamOnlyInStructFnParamRejects(t *testing.T) {
	emitAndRunRejects(t, `type Inner[T] = struct { val T; };
type Outer[K] = struct { inner Inner[K]; fn convert[K, R](self Outer[K], conv fn(Inner[K]) R) R { return conv(self.inner); } };
fn read(inner Inner[int]) int { return inner.val * 2; }
fn main() int {
    var o Outer[int] = Outer[int].{ inner = Inner[int].{ val = 3 } };
    let r = o.convert(read);
    return r;
}`, "a function-typed value's signature may only mention parameter shapes this backend can build as a call argument")
}
