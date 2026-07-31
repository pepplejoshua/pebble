package tir

import (
	"fmt"

	"github.com/pepplejoshua/pebble/compiler/internal/module"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

// NodeKind identifies one closed typed-IR node tag. The exported constants form
// one contiguous nonzero range; maxNodeKind is the private exclusive bound.
type NodeKind uint8

// The exact 82-tag inventory, in the written order of the seven category
// blocks from the accepted 06b specification.
const (
	// Declarations and nonvalue structure.
	Module NodeKind = iota + 1
	Import
	TypeDeclaration
	FieldDeclaration
	VariantDeclaration
	FunctionDeclaration
	ExternDeclaration
	GlobalDeclaration
	LocalDeclaration
	ParameterDeclaration
	TypeParameterDeclaration
	TypeUse
	Block

	// Statements and control.
	Initialize
	Store
	CompoundStore
	ExpressionStatement
	Print
	Return
	ImplicitReturn
	If
	While
	RangeLoop
	For
	Switch
	SwitchCase
	Break
	Continue
	DeferRegister

	// Values.
	BoolLiteral
	CharLiteral
	StringLiteral
	IntegerLiteral
	FloatLiteral
	NilPointer
	NoneOptional
	SomeOptional
	TupleValue
	ArrayValue
	ArrayRepeat
	RecordConstruct
	HoistedFunctionValue
	SymbolValue
	EnumVariantValue
	ContextValue
	InterpolatedString
	SizeofType
	PrefixValue
	BinaryValue
	ShortCircuitValue
	FieldValue
	TupleElementValue
	GenericFunctionValue
	SourceAlias

	// Places and loads.
	StoragePlace
	DereferencePlace
	FieldPlace
	TuplePlace
	CheckedIndexPlace
	Load

	// Calls and construction.
	DirectCall
	IndirectCall
	MethodCall
	VariantConstruct

	// Coercions and runtime checks.
	IntegerCast
	IntegerToFloat
	FloatToInteger
	FloatCast
	OptionalInject
	TupleCoerce
	CheckedOptionalUnwrap
	CheckedIndex
	CheckedSlice
	CheckedArithmetic
	CheckedNegate
	CheckedShift
	EnumToInteger
	OptionalIntegerToEnum
	CheckedIntegerToEnum

	// Evaluation sequencing.
	TempBind
	TempRead
	Sequence

	maxNodeKind // private exclusive upper bound
)

// FirstNodeKind and LastNodeKind expose the closed range bounds for tests and
// dispatch tables. They are derived directly from the iota block above.
const (
	FirstNodeKind = Module
	LastNodeKind  = Sequence
)

// nodeKindCount is the exact number of valid tags.
const nodeKindCount = int(LastNodeKind - FirstNodeKind + 1)

// nodeCategory classifies a node for ownership and type rules.
type nodeCategory uint8

const (
	CategoryNonvalue nodeCategory = iota + 1
	CategoryValue
	CategoryPlace
)

func (c nodeCategory) String() string {
	switch c {
	case CategoryNonvalue:
		return "nonvalue"
	case CategoryValue:
		return "value"
	case CategoryPlace:
		return "place"
	default:
		return "unknown"
	}
}

// RequirementKind classifies a generic requirement published with the unit.
type RequirementKind uint8

const (
	RequirementNumeric RequirementKind = iota + 1
	RequirementIntegral
	RequirementOrdered
	RequirementEquatable
	RequirementLiteralFits
)

func (k RequirementKind) String() string {
	switch k {
	case RequirementNumeric:
		return "Numeric"
	case RequirementIntegral:
		return "Integral"
	case RequirementOrdered:
		return "Ordered"
	case RequirementEquatable:
		return "Equatable"
	case RequirementLiteralFits:
		return "LiteralFits"
	default:
		return "unknown"
	}
}

// LiteralKind classifies the canonical literal payload stored in a literal node.
type LiteralKind uint8

const (
	LiteralBool LiteralKind = iota + 1
	LiteralChar
	LiteralString
	LiteralInteger
	LiteralFloat
)

func (k LiteralKind) String() string {
	switch k {
	case LiteralBool:
		return "bool"
	case LiteralChar:
		return "char"
	case LiteralString:
		return "string"
	case LiteralInteger:
		return "integer"
	case LiteralFloat:
		return "float"
	default:
		return "unknown"
	}
}

// ContextAction records how a call propagates the hidden context or how a
// context expression produces a value.
type ContextAction uint8

const (
	ContextNone ContextAction = iota + 1
	ContextForward
	ContextExpr
	ContextIndirect
)

func (a ContextAction) String() string {
	switch a {
	case ContextNone:
		return "NoContext"
	case ContextForward:
		return "ForwardCurrentContext"
	case ContextExpr:
		return "ContextExpr"
	case ContextIndirect:
		return "ContextIndirect"
	default:
		return "unknown"
	}
}

// Parameter is one authored parameter declaration carried by a function node.
type Parameter struct {
	Symbol symbol.SymbolID
	Type   types.TypeID
}

// FieldInit is one named field initializer carried by RecordConstruct.
type FieldInit struct {
	Field symbol.SymbolID
	Value NodeID
}

// Literal is the canonical decoded payload for a literal node.
type Literal struct {
	Kind       LiteralKind
	Bool       bool
	Char       rune
	String     string
	IntegerNum string
	IntegerDen string
	Float      string
}

// Requirement is one normalized generic requirement attached to the unit or to
// an instantiation reference.
type Requirement struct {
	Owner       symbol.SymbolID
	Parameter   symbol.SymbolID
	Kind        RequirementKind
	Subject     types.TypeID
	Origin      symbol.SyntaxRef
	Operator    syntax.TokenKind
	LiteralKind LiteralKind
	Numerator   string
	Denominator string
}

// Instantiation is one solved generic application referenced by value nodes.
type Instantiation struct {
	Site         symbol.SyntaxRef
	Declaration  symbol.SymbolID
	TypeArgs     []types.TypeID
	Requirements []Requirement
}

// ModuleDecl is the immutable module container stored in a Unit.
type ModuleDecl struct {
	ID           module.ModuleID
	Key          module.ModuleKey
	Source       source.ID
	Span         source.Span
	Imports      []ImportDecl
	Declarations []symbol.SymbolID
}

// ImportDecl is one resolved import edge stored in a ModuleDecl.
type ImportDecl struct {
	Span   source.Span
	Target module.ModuleID
}

// TypeDecl is the immutable type declaration container stored in a Unit.
type TypeDecl struct {
	Symbol  symbol.SymbolID
	Span    source.Span
	Members []symbol.SymbolID
	Node    NodeID
}

// FunctionDecl is the immutable function declaration container stored in a Unit.
type FunctionDecl struct {
	Symbol     symbol.SymbolID
	Span       source.Span
	FunctionID FunctionID
	Node       NodeID
}

// GlobalDecl is the immutable global declaration container stored in a Unit.
type GlobalDecl struct {
	Symbol symbol.SymbolID
	Span   source.Span
	Type   types.TypeID
	Node   NodeID
}

// Node is the closed, tag-typed IR payload. Every field has exactly one meaning
// for the tags that use it; unused fields must remain zero for any other tag.
type Node struct {
	Kind NodeKind

	// Type is required for value and place nodes; zero for nonvalue nodes.
	Type types.TypeID

	// FunctionType is the type of the callee symbol/value for call nodes
	// (DirectCall, IndirectCall, MethodCall). It is the function *value's* own
	// type, distinct from Type (the call's result type) and from ResultType
	// (a declaration's own declared result type).
	FunctionType types.TypeID

	// Span is the authored source span. Synthetic nodes set Origin instead.
	Span source.Span

	// Origin plus SyntheticRole describe a generated node that has no authored
	// span of its own.
	Origin        source.Span
	SyntheticRole string

	// Syntax names the surface occurrence, if any.
	Syntax symbol.SyntaxRef

	// Children are ordered evaluation operands.
	Children []NodeID

	// Symbol identities: declaration, member, variant, method, etc.
	Symbol symbol.SymbolID
	Member symbol.SymbolID

	// Function, region, target, and temp identities.
	Function     FunctionID
	Region       RegionID
	Target       RegionID
	TargetModule module.ModuleID
	Temp         TempID

	// Type-level data.
	TypeArg  types.TypeID
	TypeArgs []types.TypeID

	// Callable metadata.
	Parameters []Parameter
	ResultType types.TypeID
	Convention types.CallingConvention
	Variadic   bool
	Inline     bool
	HasBody    bool

	// Aggregate construction.
	Fields []FieldInit

	// Generic reference.
	GenericRef uint32

	// Operator data.
	Operator     syntax.TokenKind
	ShortCircuit bool

	// Literal payload.
	Literal Literal

	// Place metadata.
	Writable bool

	// Requirements attached directly to a declaration.
	Requirements []Requirement

	// Control-flow metadata.
	DeferChain       []NodeID
	HasElse          bool
	RangeInclusive   bool
	ConditionPresent bool
	CaseValue        symbol.SymbolID

	// SourceAlias is the sole tag that may set ExplicitCast.
	ExplicitCast bool

	// Context action for calls and context expressions.
	ContextAction ContextAction

	// Ordinal records tuple/field positions or other small indices without
	// reusing the TempID identity.
	Ordinal uint32
}

// nodeMeta is the per-tag category and dispatch metadata. It is populated from
// the same closed iota block so category, verifier, and dumper cannot drift.
type nodeMeta struct {
	Kind     NodeKind
	Name     string
	Category nodeCategory
}

// nodeMetas is indexed by Kind-1. It must contain exactly one entry for every
// tag in [FirstNodeKind, LastNodeKind].
var nodeMetas = [nodeKindCount]nodeMeta{
	{Module, "Module", CategoryNonvalue},
	{Import, "Import", CategoryNonvalue},
	{TypeDeclaration, "TypeDeclaration", CategoryNonvalue},
	{FieldDeclaration, "FieldDeclaration", CategoryNonvalue},
	{VariantDeclaration, "VariantDeclaration", CategoryNonvalue},
	{FunctionDeclaration, "FunctionDeclaration", CategoryNonvalue},
	{ExternDeclaration, "ExternDeclaration", CategoryNonvalue},
	{GlobalDeclaration, "GlobalDeclaration", CategoryNonvalue},
	{LocalDeclaration, "LocalDeclaration", CategoryNonvalue},
	{ParameterDeclaration, "ParameterDeclaration", CategoryNonvalue},
	{TypeParameterDeclaration, "TypeParameterDeclaration", CategoryNonvalue},
	{TypeUse, "TypeUse", CategoryNonvalue},
	{Block, "Block", CategoryNonvalue},

	{Initialize, "Initialize", CategoryNonvalue},
	{Store, "Store", CategoryNonvalue},
	{CompoundStore, "CompoundStore", CategoryNonvalue},
	{ExpressionStatement, "ExpressionStatement", CategoryNonvalue},
	{Print, "Print", CategoryNonvalue},
	{Return, "Return", CategoryNonvalue},
	{ImplicitReturn, "ImplicitReturn", CategoryNonvalue},
	{If, "If", CategoryNonvalue},
	{While, "While", CategoryNonvalue},
	{RangeLoop, "RangeLoop", CategoryNonvalue},
	{For, "For", CategoryNonvalue},
	{Switch, "Switch", CategoryNonvalue},
	{SwitchCase, "SwitchCase", CategoryNonvalue},
	{Break, "Break", CategoryNonvalue},
	{Continue, "Continue", CategoryNonvalue},
	{DeferRegister, "DeferRegister", CategoryNonvalue},

	{BoolLiteral, "BoolLiteral", CategoryValue},
	{CharLiteral, "CharLiteral", CategoryValue},
	{StringLiteral, "StringLiteral", CategoryValue},
	{IntegerLiteral, "IntegerLiteral", CategoryValue},
	{FloatLiteral, "FloatLiteral", CategoryValue},
	{NilPointer, "NilPointer", CategoryValue},
	{NoneOptional, "NoneOptional", CategoryValue},
	{SomeOptional, "SomeOptional", CategoryValue},
	{TupleValue, "TupleValue", CategoryValue},
	{ArrayValue, "ArrayValue", CategoryValue},
	{ArrayRepeat, "ArrayRepeat", CategoryValue},
	{RecordConstruct, "RecordConstruct", CategoryValue},
	{HoistedFunctionValue, "HoistedFunctionValue", CategoryValue},
	{SymbolValue, "SymbolValue", CategoryValue},
	{EnumVariantValue, "EnumVariantValue", CategoryValue},
	{ContextValue, "ContextValue", CategoryValue},
	{InterpolatedString, "InterpolatedString", CategoryValue},
	{SizeofType, "SizeofType", CategoryValue},
	{PrefixValue, "PrefixValue", CategoryValue},
	{BinaryValue, "BinaryValue", CategoryValue},
	{ShortCircuitValue, "ShortCircuitValue", CategoryValue},
	{FieldValue, "FieldValue", CategoryValue},
	{TupleElementValue, "TupleElementValue", CategoryValue},
	{GenericFunctionValue, "GenericFunctionValue", CategoryValue},
	{SourceAlias, "SourceAlias", CategoryValue},

	{StoragePlace, "StoragePlace", CategoryPlace},
	{DereferencePlace, "DereferencePlace", CategoryPlace},
	{FieldPlace, "FieldPlace", CategoryPlace},
	{TuplePlace, "TuplePlace", CategoryPlace},
	{CheckedIndexPlace, "CheckedIndexPlace", CategoryPlace},
	{Load, "Load", CategoryValue},

	{DirectCall, "DirectCall", CategoryValue},
	{IndirectCall, "IndirectCall", CategoryValue},
	{MethodCall, "MethodCall", CategoryValue},
	{VariantConstruct, "VariantConstruct", CategoryValue},

	{IntegerCast, "IntegerCast", CategoryValue},
	{IntegerToFloat, "IntegerToFloat", CategoryValue},
	{FloatToInteger, "FloatToInteger", CategoryValue},
	{FloatCast, "FloatCast", CategoryValue},
	{OptionalInject, "OptionalInject", CategoryValue},
	{TupleCoerce, "TupleCoerce", CategoryValue},
	{CheckedOptionalUnwrap, "CheckedOptionalUnwrap", CategoryValue},
	{CheckedIndex, "CheckedIndex", CategoryValue},
	{CheckedSlice, "CheckedSlice", CategoryValue},
	{CheckedArithmetic, "CheckedArithmetic", CategoryValue},
	{CheckedNegate, "CheckedNegate", CategoryValue},
	{CheckedShift, "CheckedShift", CategoryValue},
	{EnumToInteger, "EnumToInteger", CategoryValue},
	{OptionalIntegerToEnum, "OptionalIntegerToEnum", CategoryValue},
	{CheckedIntegerToEnum, "CheckedIntegerToEnum", CategoryValue},

	{TempBind, "TempBind", CategoryNonvalue},
	{TempRead, "TempRead", CategoryValue},
	{Sequence, "Sequence", CategoryValue},
}

// CategoryOf returns the category for a valid kind, or false for zero or
// out-of-range tags. It is the single source of category truth.
func CategoryOf(kind NodeKind) (nodeCategory, bool) {
	if kind < FirstNodeKind || kind > LastNodeKind {
		return 0, false
	}
	m := nodeMetas[kind-1]
	return m.Category, true
}

// NodeKindName returns the normalized name for a valid kind, or false for zero
// or out-of-range tags.
func NodeKindName(kind NodeKind) (string, bool) {
	if kind < FirstNodeKind || kind > LastNodeKind {
		return "", false
	}
	return nodeMetas[kind-1].Name, true
}

// NodeKinds returns the exact ordered list of all valid kinds. It is intended
// for totality tests.
func NodeKinds() []NodeKind {
	kinds := make([]NodeKind, nodeKindCount)
	for i := range nodeMetas {
		kinds[i] = nodeMetas[i].Kind
	}
	return kinds
}

// String returns the normalized tag name for valid kinds and a stable sentinel
// for invalid ones.
func (k NodeKind) String() string {
	if name, ok := NodeKindName(k); ok {
		return name
	}
	return fmt.Sprintf("NodeKind(%d)", k)
}

// IsValid reports whether kind is within the closed nonzero range.
func (k NodeKind) IsValid() bool { return k >= FirstNodeKind && k <= LastNodeKind }
