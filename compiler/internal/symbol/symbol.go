// Package symbol assigns stable declaration, scope, and reference identities.
package symbol

import (
	"fmt"
	"io"
	"sort"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/module"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
)

// Stable name-resolution diagnostic codes.
const (
	CodeUndefinedName    diagnostic.Code = "N0001"
	CodeDuplicate        diagnostic.Code = "N0002"
	CodeInvalidQualifier diagnostic.Code = "N0003"
	CodeMissingMember    diagnostic.Code = "N0004"
	CodeInvalidCategory  diagnostic.Code = "N0005"
	CodeResourceLimit    diagnostic.Code = "N0006"
	CodeReservedBuiltin  diagnostic.Code = "N0007"
)

const (
	DefaultMaxSymbols     uint32 = 1 << 20
	DefaultMaxScopes      uint32 = 1 << 18
	DefaultMaxScopeDepth  uint32 = 256
	DefaultMaxDiagnostics uint32 = 50
)

// Config bounds one resolver invocation. Zero values select package defaults.
type Config struct {
	MaxSymbols     uint32
	MaxScopes      uint32
	MaxScopeDepth  uint32
	MaxDiagnostics uint32
}

// ModuleID is the module graph's compilation-local identity.
type ModuleID = module.ModuleID

// ScopeID and SymbolID are result-local stable identities. Zero is invalid.
type ScopeID uint32
type SymbolID uint32

// SyntaxRef identifies a node in one graph module's immutable syntax tree.
type SyntaxRef struct {
	Module ModuleID
	Node   syntax.NodeID
}

// SymbolKind classifies collected declarations without assigning a type.
type SymbolKind uint8

const (
	SymbolError SymbolKind = iota + 1
	SymbolModule
	SymbolType
	SymbolFunction
	SymbolBinding
	SymbolParameter
	SymbolLoopBinding
	SymbolTypeParameter
	SymbolField
	SymbolVariant
	SymbolMethod
	SymbolExternType
	SymbolExternFunction
	SymbolExternBinding
	SymbolBuiltinType
)

func (k SymbolKind) String() string {
	switch k {
	case SymbolError:
		return "error"
	case SymbolModule:
		return "module"
	case SymbolType:
		return "type"
	case SymbolFunction:
		return "function"
	case SymbolBinding:
		return "binding"
	case SymbolParameter:
		return "parameter"
	case SymbolLoopBinding:
		return "loop binding"
	case SymbolTypeParameter:
		return "type parameter"
	case SymbolField:
		return "field"
	case SymbolVariant:
		return "variant"
	case SymbolMethod:
		return "method"
	case SymbolExternType:
		return "extern type"
	case SymbolExternFunction:
		return "extern function"
	case SymbolExternBinding:
		return "extern binding"
	case SymbolBuiltinType:
		return "builtin type"
	default:
		return "unknown"
	}
}

// BuiltinType identifies a predeclared type independently of its spelling.
// Values deliberately follow the semantic type store's fixed builtin order,
// but the symbol package does not depend on the types package.
type BuiltinType uint8

const (
	BuiltinBool BuiltinType = iota + 1
	BuiltinChar
	BuiltinStr
	BuiltinVoid
	BuiltinInt
	BuiltinUint
	BuiltinI8
	BuiltinI16
	BuiltinI32
	BuiltinI64
	BuiltinU8
	BuiltinU16
	BuiltinU32
	BuiltinU64
	BuiltinF32
	BuiltinF64
)

var builtinNames = [...]string{
	BuiltinBool: "bool", BuiltinChar: "char", BuiltinStr: "str", BuiltinVoid: "void",
	BuiltinInt: "int", BuiltinUint: "uint", BuiltinI8: "i8", BuiltinI16: "i16",
	BuiltinI32: "i32", BuiltinI64: "i64", BuiltinU8: "u8", BuiltinU16: "u16",
	BuiltinU32: "u32", BuiltinU64: "u64", BuiltinF32: "f32", BuiltinF64: "f64",
}

func (b BuiltinType) String() string {
	if int(b) < len(builtinNames) {
		return builtinNames[b]
	}
	return "unknown"
}

// ScopeKind describes an authored or declaration environment.
type ScopeKind uint8

const (
	ScopePrelude ScopeKind = iota + 1
	ScopeModule
	ScopeType
	ScopeFunction
	ScopeBlock
	ScopeRangeLoop
	ScopeFor
)

func (k ScopeKind) String() string {
	switch k {
	case ScopePrelude:
		return "prelude"
	case ScopeModule:
		return "module"
	case ScopeType:
		return "type"
	case ScopeFunction:
		return "function"
	case ScopeBlock:
		return "block"
	case ScopeRangeLoop:
		return "range-loop"
	case ScopeFor:
		return "for"
	default:
		return "unknown"
	}
}

// Symbol is an immutable semantic declaration value returned by SymbolStore.
type Symbol struct {
	ID           SymbolID
	Name         string
	Kind         SymbolKind
	Span         source.Span
	Module       ModuleID
	Scope        ScopeID
	Declaration  SyntaxRef
	Containing   SymbolID
	ImportTarget ModuleID
	Generic      bool
	Builtin      BuiltinType
	Error        bool
}

// Scope is an immutable lexical environment value returned by ScopeStore.
type Scope struct {
	ID      ScopeID
	Kind    ScopeKind
	Parent  ScopeID
	Module  ModuleID
	Owner   SymbolID
	Origin  SyntaxRef
	Depth   uint32
	Symbols []SymbolID
}

// SymbolStore owns symbols in deterministic ID order.
type SymbolStore struct{ values []Symbol }

func (s *SymbolStore) Len() int {
	if s == nil {
		return 0
	}
	return len(s.values)
}
func (s *SymbolStore) Symbol(id SymbolID) (Symbol, bool) {
	if s == nil || id == 0 || uint64(id) > uint64(len(s.values)) {
		return Symbol{}, false
	}
	return s.values[id-1], true
}
func (s *SymbolStore) All() []Symbol {
	if s == nil {
		return nil
	}
	return append([]Symbol(nil), s.values...)
}

// ScopeStore owns scopes in deterministic ID order.
type ScopeStore struct{ values []Scope }

func (s *ScopeStore) Len() int {
	if s == nil {
		return 0
	}
	return len(s.values)
}
func (s *ScopeStore) Scope(id ScopeID) (Scope, bool) {
	if s == nil || id == 0 || uint64(id) > uint64(len(s.values)) {
		return Scope{}, false
	}
	v := s.values[id-1]
	v.Symbols = append([]SymbolID(nil), v.Symbols...)
	return v, true
}
func (s *ScopeStore) All() []Scope {
	if s == nil {
		return nil
	}
	result := make([]Scope, len(s.values))
	for i := range s.values {
		result[i], _ = s.Scope(ScopeID(i + 1))
	}
	return result
}

// ResolutionState distinguishes successful, failed, and intentionally deferred names.
type ResolutionState uint8

const (
	ResolutionResolved ResolutionState = iota + 1
	ResolutionError
	ResolutionDeferred
)

// Resolution is one authored name's stable result.
type Resolution struct {
	Syntax SyntaxRef
	Symbol SymbolID
	State  ResolutionState
}

// BracketMode records how a neutral BracketApply was traversed.
type BracketMode uint8

const (
	BracketDeferred BracketMode = iota + 1
	BracketTypeNames
	BracketValueNames
)

// Capture records one anonymous function's outer binding in first-reference order.
type Capture struct {
	Function SyntaxRef
	Symbol   SymbolID
}

// Result is the immutable output of one Resolve invocation.
type Result struct {
	Scopes       *ScopeStore
	Symbols      *SymbolStore
	prelude      ScopeID
	builtins     [BuiltinF64 + 1]SymbolID
	references   map[SyntaxRef]Resolution
	qualifiers   map[SyntaxRef]ModuleID
	brackets     map[SyntaxRef]BracketMode
	captures     map[SyntaxRef][]SymbolID
	captureOrder []SyntaxRef
	members      map[SymbolID][]SymbolID
}

func (r *Result) Prelude() ScopeID {
	if r == nil {
		return 0
	}
	return r.prelude
}

func (r *Result) Builtin(kind BuiltinType) (SymbolID, bool) {
	if r == nil || kind == 0 || int(kind) >= len(r.builtins) {
		return 0, false
	}
	id := r.builtins[kind]
	return id, id != 0
}

func (r *Result) Reference(ref SyntaxRef) (Resolution, bool) {
	v, ok := r.references[ref]
	return v, ok
}
func (r *Result) References() []Resolution                  { return orderedResolutions(r.references) }
func (r *Result) Qualifier(ref SyntaxRef) (ModuleID, bool)  { v, ok := r.qualifiers[ref]; return v, ok }
func (r *Result) Bracket(ref SyntaxRef) (BracketMode, bool) { v, ok := r.brackets[ref]; return v, ok }
func (r *Result) Captures(function SyntaxRef) []SymbolID {
	return append([]SymbolID(nil), r.captures[function]...)
}
func (r *Result) CaptureList() []Capture {
	var out []Capture
	for _, fn := range r.captureOrder {
		for _, id := range r.captures[fn] {
			out = append(out, Capture{Function: fn, Symbol: id})
		}
	}
	return out
}
func (r *Result) Members(owner SymbolID) []SymbolID {
	return append([]SymbolID(nil), r.members[owner]...)
}

func orderedResolutions(values map[SyntaxRef]Resolution) []Resolution {
	out := make([]Resolution, 0, len(values))
	for _, value := range values {
		out = append(out, value)
	}
	sort.Slice(out, func(i, j int) bool {
		if out[i].Syntax.Module != out[j].Syntax.Module {
			return out[i].Syntax.Module < out[j].Syntax.Module
		}
		return out[i].Syntax.Node < out[j].Syntax.Node
	})
	return out
}

// Dump writes a deterministic checker-facing snapshot of scopes, symbols, and mappings.
func (r *Result) Dump(w io.Writer) error {
	if r == nil {
		return nil
	}
	for _, scope := range r.Scopes.All() {
		if _, err := fmt.Fprintf(w, "scope %d %s parent=%d module=%d owner=%d depth=%d symbols=%v\n", scope.ID, scope.Kind, scope.Parent, scope.Module, scope.Owner, scope.Depth, scope.Symbols); err != nil {
			return err
		}
	}
	for _, symbol := range r.Symbols.All() {
		if _, err := fmt.Fprintf(w, "symbol %d %s %q module=%d scope=%d node=%d containing=%d target=%d generic=%t builtin=%s error=%t\n", symbol.ID, symbol.Kind, symbol.Name, symbol.Module, symbol.Scope, symbol.Declaration.Node, symbol.Containing, symbol.ImportTarget, symbol.Generic, symbol.Builtin, symbol.Error); err != nil {
			return err
		}
	}
	for _, ref := range r.References() {
		if _, err := fmt.Fprintf(w, "reference %d:%d state=%d symbol=%d\n", ref.Syntax.Module, ref.Syntax.Node, ref.State, ref.Symbol); err != nil {
			return err
		}
	}
	qualifiers := make([]SyntaxRef, 0, len(r.qualifiers))
	for ref := range r.qualifiers {
		qualifiers = append(qualifiers, ref)
	}
	sort.Slice(qualifiers, func(i, j int) bool {
		if qualifiers[i].Module != qualifiers[j].Module {
			return qualifiers[i].Module < qualifiers[j].Module
		}
		return qualifiers[i].Node < qualifiers[j].Node
	})
	for _, ref := range qualifiers {
		if _, err := fmt.Fprintf(w, "qualifier %d:%d module=%d\n", ref.Module, ref.Node, r.qualifiers[ref]); err != nil {
			return err
		}
	}
	for _, capture := range r.CaptureList() {
		if _, err := fmt.Fprintf(w, "capture %d:%d symbol=%d\n", capture.Function.Module, capture.Function.Node, capture.Symbol); err != nil {
			return err
		}
	}
	return nil
}
