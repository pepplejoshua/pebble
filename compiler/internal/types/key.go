package types

import (
	"slices"

	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
)

// Kind identifies the closed semantic shape of a TypeKey.
type Kind uint8

const (
	Builtin Kind = iota + 1
	Pointer
	Array
	Slice
	Tuple
	Optional
	Function
	Nominal
	TypeParameter
)

// BuiltinKind identifies one primitive semantic type.
type BuiltinKind uint8

const (
	Bool BuiltinKind = iota + 1
	Char
	Str
	Void
	Int
	Uint
	I8
	I16
	I32
	I64
	U8
	U16
	U32
	U64
	F32
	F64
)

// CallingConvention is a semantic component of a function type.
type CallingConvention uint8

const (
	Pebble CallingConvention = iota + 1
	C
)

// TypeKey is the closed immutable semantic description interned by Store.
// Its fields stay private so callers cannot create competing representations.
type TypeKey struct {
	kind        Kind
	builtin     BuiltinKind
	child       TypeID
	length      uint64
	elements    []TypeID
	convention  CallingConvention
	result      TypeID
	variadic    bool
	declaration symbol.SymbolID
}

func BuiltinKey(kind BuiltinKind) TypeKey { return TypeKey{kind: Builtin, builtin: kind} }
func PointerKey(pointee TypeID) TypeKey   { return TypeKey{kind: Pointer, child: pointee} }
func ArrayKey(length uint64, element TypeID) TypeKey {
	return TypeKey{kind: Array, length: length, child: element}
}
func SliceKey(element TypeID) TypeKey { return TypeKey{kind: Slice, child: element} }
func TupleKey(elements []TypeID) TypeKey {
	return TypeKey{kind: Tuple, elements: cloneIDs(elements)}
}
func OptionalKey(element TypeID) TypeKey { return TypeKey{kind: Optional, child: element} }
func FunctionKey(
	convention CallingConvention,
	parameters []TypeID,
	result TypeID,
	variadic bool,
) TypeKey {
	return TypeKey{
		kind:       Function,
		convention: convention,
		elements:   cloneIDs(parameters),
		result:     result,
		variadic:   variadic,
	}
}
func NominalKey(declaration symbol.SymbolID, arguments []TypeID) TypeKey {
	return TypeKey{
		kind:        Nominal,
		declaration: declaration,
		elements:    cloneIDs(arguments),
	}
}
func TypeParameterKey(declaration symbol.SymbolID) TypeKey {
	return TypeKey{kind: TypeParameter, declaration: declaration}
}

func (k TypeKey) Kind() Kind { return k.kind }

func (k TypeKey) Builtin() (BuiltinKind, bool) {
	if k.kind != Builtin {
		return 0, false
	}
	return k.builtin, true
}

func (k TypeKey) Child() (TypeID, bool) {
	switch k.kind {
	case Pointer, Slice, Optional:
		return k.child, true
	default:
		return 0, false
	}
}

func (k TypeKey) Array() (length uint64, element TypeID, ok bool) {
	if k.kind != Array {
		return 0, 0, false
	}
	return k.length, k.child, true
}

func (k TypeKey) Elements() ([]TypeID, bool) {
	if k.kind != Tuple {
		return nil, false
	}
	return cloneIDs(k.elements), true
}

func (k TypeKey) Function() (
	convention CallingConvention,
	parameters []TypeID,
	result TypeID,
	variadic bool,
	ok bool,
) {
	if k.kind != Function {
		return 0, nil, 0, false, false
	}
	return k.convention, cloneIDs(k.elements), k.result, k.variadic, true
}

func (k TypeKey) Nominal() (
	declaration symbol.SymbolID,
	arguments []TypeID,
	ok bool,
) {
	if k.kind != Nominal {
		return 0, nil, false
	}
	return k.declaration, cloneIDs(k.elements), true
}

func (k TypeKey) TypeParameter() (symbol.SymbolID, bool) {
	if k.kind != TypeParameter {
		return 0, false
	}
	return k.declaration, true
}

func (k TypeKey) clone() TypeKey {
	k.elements = cloneIDs(k.elements)
	return k
}

func equalKeys(a, b TypeKey) bool {
	return a.kind == b.kind &&
		a.builtin == b.builtin &&
		a.child == b.child &&
		a.length == b.length &&
		a.convention == b.convention &&
		a.result == b.result &&
		a.variadic == b.variadic &&
		a.declaration == b.declaration &&
		slices.Equal(a.elements, b.elements)
}

func cloneIDs(ids []TypeID) []TypeID {
	if len(ids) == 0 {
		return nil
	}
	return append([]TypeID(nil), ids...)
}

func hashKey(k TypeKey) uint64 {
	const (
		offset = uint64(14695981039346656037)
		prime  = uint64(1099511628211)
	)

	hash := offset
	mix := func(value uint64) {
		for range 8 {
			hash ^= value & 0xff
			hash *= prime
			value >>= 8
		}
	}

	mix(uint64(k.kind))
	mix(uint64(k.builtin))
	mix(uint64(k.child))
	mix(k.length)
	mix(uint64(k.convention))
	mix(uint64(k.result))
	if k.variadic {
		mix(1)
	} else {
		mix(0)
	}
	mix(uint64(k.declaration))
	mix(uint64(len(k.elements)))
	for _, element := range k.elements {
		mix(uint64(element))
	}
	return hash
}

func validBuiltin(kind BuiltinKind) bool { return kind >= Bool && kind <= F64 }
func validConvention(convention CallingConvention) bool {
	return convention == Pebble || convention == C
}
