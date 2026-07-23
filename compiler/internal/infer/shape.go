package infer

import (
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

type shapeKind uint8

const (
	shapeLeaf shapeKind = iota + 1
	shapePointer
	shapeArray
	shapeSlice
	shapeTuple
	shapeOptional
	shapeFunction
	shapeNominal
)

// Shape is a copied algebraic structure whose leaves are session terms.
type Shape struct {
	kind        shapeKind
	term        Term
	length      uint64
	convention  types.CallingConvention
	variadic    bool
	declaration symbol.SymbolID
	children    []Shape
}

func Leaf(term Term) Shape { return Shape{kind: shapeLeaf, term: term} }
func PointerShape(child Shape) Shape {
	return Shape{kind: shapePointer, children: []Shape{cloneShape(child)}}
}
func ArrayShape(length uint64, child Shape) Shape {
	return Shape{kind: shapeArray, length: length, children: []Shape{cloneShape(child)}}
}
func SliceShape(child Shape) Shape {
	return Shape{kind: shapeSlice, children: []Shape{cloneShape(child)}}
}
func TupleShape(children []Shape) Shape {
	return Shape{kind: shapeTuple, children: cloneShapes(children)}
}
func OptionalShape(child Shape) Shape {
	return Shape{kind: shapeOptional, children: []Shape{cloneShape(child)}}
}
func FunctionShape(convention types.CallingConvention, parameters []Shape, result Shape, variadic bool) Shape {
	children := cloneShapes(parameters)
	children = append(children, cloneShape(result))
	return Shape{kind: shapeFunction, convention: convention, variadic: variadic, children: children}
}
func NominalShape(declaration symbol.SymbolID, arguments []Shape) Shape {
	return Shape{kind: shapeNominal, declaration: declaration, children: cloneShapes(arguments)}
}

func cloneShape(value Shape) Shape {
	value.children = cloneShapes(value.children)
	return value
}
func cloneShapes(values []Shape) []Shape {
	if len(values) == 0 {
		return nil
	}
	out := make([]Shape, len(values))
	for i := range values {
		out[i] = cloneShape(values[i])
	}
	return out
}

func shapeComponents(value Shape) (uint32, bool) {
	var count uint64
	stack := []Shape{value}
	for len(stack) != 0 {
		last := len(stack) - 1
		v := stack[last]
		stack = stack[:last]
		count++
		if count > uint64(^uint32(0)) {
			return 0, false
		}
		for i := len(v.children) - 1; i >= 0; i-- {
			stack = append(stack, v.children[i])
		}
	}
	return uint32(count), true
}

func shapeDepth(value Shape) uint32 {
	type item struct {
		shape Shape
		depth uint32
	}
	maximum := uint32(0)
	stack := []item{{shape: value, depth: 1}}
	for len(stack) != 0 {
		last := len(stack) - 1
		current := stack[last]
		stack = stack[:last]
		if current.depth > maximum {
			maximum = current.depth
		}
		for i := len(current.shape.children) - 1; i >= 0; i-- {
			stack = append(stack, item{shape: current.shape.children[i], depth: current.depth + 1})
		}
	}
	return maximum
}

func validShape(value Shape, owner *sessionToken) bool {
	stack := []Shape{value}
	for len(stack) != 0 {
		last := len(stack) - 1
		v := stack[last]
		stack = stack[:last]
		switch v.kind {
		case shapeLeaf:
			if !v.term.belongs(owner) || len(v.children) != 0 {
				return false
			}
		case shapePointer, shapeArray, shapeSlice, shapeOptional:
			if len(v.children) != 1 {
				return false
			}
		case shapeTuple:
			if len(v.children) == 0 {
				return false
			}
		case shapeFunction:
			if len(v.children) == 0 || (v.convention != types.Pebble && v.convention != types.C) {
				return false
			}
		case shapeNominal:
			if v.declaration == 0 {
				return false
			}
		default:
			return false
		}
		for i := len(v.children) - 1; i >= 0; i-- {
			stack = append(stack, v.children[i])
		}
	}
	return true
}
