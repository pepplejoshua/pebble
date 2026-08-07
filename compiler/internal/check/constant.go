package check

import (
	"fmt"
	"math/big"
	"strconv"
	"strings"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/module"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
)

const CodeInvalidConstant diagnostic.Code = "C0614"

type constantKind uint8

const (
	constantInteger constantKind = iota + 1
	constantBoolean
	constantCharacter
	constantString
	constantFloat
	constantEnum
)

type constantValue struct {
	Kind        constantKind
	Integer     *big.Int
	Boolean     bool
	Character   rune
	String      string
	Float       float64
	EnumType    symbol.SymbolID
	EnumVariant symbol.SymbolID
	EnumOrdinal uint32
}

func (v constantValue) clone() constantValue {
	if v.Integer != nil {
		v.Integer = new(big.Int).Set(v.Integer)
	}
	return v
}

type constantState uint8

const (
	constantKnown constantState = iota + 1
	constantError
	constantUnavailable
)

type constantResult struct {
	State      constantState
	Value      constantValue
	contextual bool
}

func (r constantResult) clone() constantResult {
	r.Value = r.Value.clone()
	return r
}

type memoColor uint8

const (
	memoGray memoColor = iota + 1
	memoBlack
)

type constantEvaluator struct {
	inputs      Inputs
	config      Config
	diagnostics *diagnostic.DiagnosticSet
	modules     map[module.ModuleID]module.Module
	valid       bool

	memo        map[symbol.SyntaxRef]constantResult
	memoDepth   map[symbol.SyntaxRef]constantDepth
	order       []symbol.SyntaxRef
	arrayMemo   map[symbol.SyntaxRef]infer.ArrayLengthResult
	symbolColor map[symbol.SymbolID]memoColor
	symbolMemo  map[symbol.SymbolID]constantResult
	active      []symbol.SymbolID
	activeIndex map[symbol.SymbolID]int

	operations uint64
	budget     *generationDiagnosticBudget
}

type constantDepth struct {
	height   uint32
	children [2]symbol.SyntaxRef
	count    uint8
}

type constantFrame struct {
	ref         symbol.SyntaxRef
	depth       uint32
	phase       uint8
	node        syntax.Node
	left        symbol.SyntaxRef
	right       symbol.SyntaxRef
	symbol      symbol.SymbolID
	initializer symbol.SyntaxRef
	blocked     constantResult
}

func newConstantEvaluator(inputs Inputs, diagnostics *diagnostic.DiagnosticSet, config Config) *constantEvaluator {
	return newConstantEvaluatorWithBudget(inputs, diagnostics, config, nil)
}

func newConstantEvaluatorWithBudget(inputs Inputs, diagnostics *diagnostic.DiagnosticSet, config Config, budget *generationDiagnosticBudget) *constantEvaluator {
	if diagnostics == nil {
		diagnostics = diagnostic.NewDiagnosticSet()
	}
	config = normalizeConfig(config)
	e := &constantEvaluator{
		inputs: inputs, config: config, diagnostics: diagnostics,
		modules: make(map[module.ModuleID]module.Module), memo: make(map[symbol.SyntaxRef]constantResult),
		memoDepth: make(map[symbol.SyntaxRef]constantDepth),
		arrayMemo: make(map[symbol.SyntaxRef]infer.ArrayLengthResult), symbolColor: make(map[symbol.SymbolID]memoColor),
		symbolMemo: make(map[symbol.SymbolID]constantResult), activeIndex: make(map[symbol.SymbolID]int),
	}
	if budget == nil {
		budget = newGenerationDiagnosticBudget(diagnostics, config.MaxDiagnostics)
	}
	e.budget = budget
	if inputs.Graph == nil || inputs.Sources == nil || inputs.Resolution == nil || inputs.Resolution.Symbols == nil || inputs.Resolution.Scopes == nil {
		return e
	}
	for _, value := range inputs.Graph.Modules() {
		if value.ID == 0 || value.Tree == nil {
			return e
		}
		if _, ok := inputs.Sources.File(value.Source); !ok {
			return e
		}
		e.modules[value.ID] = value
	}
	e.valid = len(e.modules) != 0
	return e
}

func (e *constantEvaluator) ArrayLength(ref symbol.SyntaxRef) infer.ArrayLengthResult {
	if e == nil {
		return infer.ArrayLengthResult{State: infer.ArrayLengthUnavailable}
	}
	if result, ok := e.arrayMemo[ref]; ok {
		return result
	}
	result := infer.ArrayLengthResult{State: infer.ArrayLengthUnavailable}
	constant := e.evaluate(ref)
	switch constant.State {
	case constantError:
		result.State = infer.ArrayLengthError
	case constantUnavailable:
		result.State = infer.ArrayLengthUnavailable
	case constantKnown:
		if constant.Value.Kind != constantInteger {
			result.State = e.arrayFailure(ref, "array length must be an integer constant")
		} else if constant.Value.Integer.Sign() < 0 {
			result.State = e.arrayFailure(ref, "array length must be nonnegative")
		} else if !constant.Value.Integer.IsUint64() {
			result.State = e.arrayFailure(ref, "array length does not fit uint64")
		} else {
			result.State = infer.ArrayLengthKnown
			result.Value = constant.Value.Integer.Uint64()
		}
	}
	e.arrayMemo[ref] = result
	return result
}

func (e *constantEvaluator) arrayFailure(ref symbol.SyntaxRef, message string) infer.ArrayLengthState {
	if e.report(ref, message) {
		return infer.ArrayLengthError
	}
	return infer.ArrayLengthUnavailable
}

func (e *constantEvaluator) evaluate(ref symbol.SyntaxRef) constantResult {
	if e == nil || !e.valid || !e.validRef(ref) {
		return constantResult{State: constantUnavailable}
	}
	if result, ok := e.memo[ref]; ok {
		return result.clone()
	}
	stack := []constantFrame{{ref: ref, depth: 1}}
	for len(stack) != 0 {
		frame := &stack[len(stack)-1]
		if _, done := e.memo[frame.ref]; done {
			stack = stack[:len(stack)-1]
			continue
		}
		switch frame.phase {
		case 0:
			if frame.depth == 0 || frame.depth > e.config.MaxConstantDepth {
				result := e.depthFailure(frame.ref)
				if !e.propagate(&stack, result) {
					e.finish(frame.ref, result)
				}
				continue
			}
			if e.operations >= e.config.MaxConstantOperations {
				e.finish(frame.ref, e.failure(frame.ref, fmt.Sprintf("constant operation limit of %d exceeded", e.config.MaxConstantOperations)))
				continue
			}
			node, ok := e.node(frame.ref)
			if !ok {
				e.finish(frame.ref, constantResult{State: constantUnavailable})
				continue
			}
			e.operations++
			frame.node = node
			switch node.Kind() {
			case syntax.Literal:
				e.finish(frame.ref, e.literal(frame.ref, node))
			case syntax.GroupedTerm:
				children := node.Children()
				if len(children) != 1 {
					e.finish(frame.ref, constantResult{State: constantUnavailable})
					continue
				}
				frame.left = symbol.SyntaxRef{Module: frame.ref.Module, Node: children[0]}
				frame.phase = 1
				frame.blocked = e.push(&stack, frame.left, frame.depth+1)
			case syntax.PrefixTerm:
				children := node.Children()
				if len(children) != 1 {
					e.finish(frame.ref, constantResult{State: constantUnavailable})
					continue
				}
				frame.left = symbol.SyntaxRef{Module: frame.ref.Module, Node: children[0]}
				frame.phase = 2
				frame.blocked = e.push(&stack, frame.left, frame.depth+1)
			case syntax.BinaryExpr:
				children := node.Children()
				if len(children) != 2 {
					e.finish(frame.ref, constantResult{State: constantUnavailable})
					continue
				}
				frame.left = symbol.SyntaxRef{Module: frame.ref.Module, Node: children[0]}
				frame.right = symbol.SyntaxRef{Module: frame.ref.Module, Node: children[1]}
				frame.phase = 3
				frame.blocked = e.push(&stack, frame.left, frame.depth+1)
			case syntax.Name, syntax.Path, syntax.MemberExpr:
				e.startReference(frame, &stack)
			default:
				e.finish(frame.ref, e.failure(frame.ref, fmt.Sprintf("%s is not a constant expression", node.Kind())))
			}
		case 1:
			result := e.memo[frame.left]
			if frame.blocked.State != 0 {
				result = frame.blocked
			}
			if result.contextual && e.propagate(&stack, result) {
				continue
			}
			e.finish(frame.ref, result, frame.left)
		case 2:
			result := frame.blocked
			if result.State == 0 {
				result = e.applyPrefix(frame.ref, frame.node.Token(), e.memo[frame.left])
			}
			if result.contextual && e.propagate(&stack, result) {
				continue
			}
			e.finish(frame.ref, result, frame.left)
		case 3:
			if frame.blocked.State != 0 {
				if frame.blocked.contextual && e.propagate(&stack, frame.blocked) {
					continue
				}
				e.finish(frame.ref, frame.blocked, frame.left)
				continue
			}
			left := e.memo[frame.left]
			if left.State != constantKnown {
				e.finish(frame.ref, left, frame.left)
				continue
			}
			if frame.node.Token() == syntax.LogicalAnd && left.Value.Kind == constantBoolean && !left.Value.Boolean {
				e.finish(frame.ref, left, frame.left)
				continue
			}
			if frame.node.Token() == syntax.LogicalOr && left.Value.Kind == constantBoolean && left.Value.Boolean {
				e.finish(frame.ref, left, frame.left)
				continue
			}
			frame.phase = 4
			frame.blocked = e.push(&stack, frame.right, frame.depth+1)
		case 4:
			result := frame.blocked
			if result.State == 0 {
				result = e.applyBinary(frame.ref, frame.node.Token(), e.memo[frame.left], e.memo[frame.right])
			}
			if result.contextual && e.propagate(&stack, result) {
				continue
			}
			e.finish(frame.ref, result, frame.left, frame.right)
		case 5:
			result := frame.blocked
			if result.State == 0 {
				result = e.memo[frame.initializer]
			}
			if result.contextual && e.symbolColor[frame.symbol] == memoGray {
				delete(e.symbolColor, frame.symbol)
				delete(e.symbolMemo, frame.symbol)
			} else if e.symbolColor[frame.symbol] == memoGray {
				e.symbolColor[frame.symbol] = memoBlack
				e.symbolMemo[frame.symbol] = result.clone()
			}
			e.leaveSymbol(frame.symbol)
			if result.contextual && e.propagate(&stack, result) {
				continue
			}
			if frame.blocked.State != 0 {
				e.finish(frame.ref, frame.blocked, frame.initializer)
			} else {
				e.finish(frame.ref, e.symbolMemo[frame.symbol], frame.initializer)
			}
		}
	}
	return e.memo[ref].clone()
}

func (e *constantEvaluator) push(stack *[]constantFrame, ref symbol.SyntaxRef, depth uint32) constantResult {
	if result, ok := e.memo[ref]; ok {
		if result.State != constantKnown {
			return constantResult{}
		}
		info := e.memoDepth[ref]
		if depth == 0 || depth > e.config.MaxConstantDepth {
			return e.depthFailure(ref)
		}
		remaining := e.config.MaxConstantDepth - depth + 1
		if info.height > remaining {
			origin := e.depthOrigin(ref, remaining)
			return e.depthFailure(origin)
		}
		return constantResult{}
	}
	*stack = append(*stack, constantFrame{ref: ref, depth: depth})
	return constantResult{}
}

func (e *constantEvaluator) depthFailure(ref symbol.SyntaxRef) constantResult {
	result := e.failure(ref, fmt.Sprintf("constant depth limit of %d exceeded", e.config.MaxConstantDepth))
	result.contextual = true
	return result
}

func (e *constantEvaluator) propagate(stack *[]constantFrame, result constantResult) bool {
	if len(*stack) <= 1 {
		return false
	}
	*stack = (*stack)[:len(*stack)-1]
	(*stack)[len(*stack)-1].blocked = result
	return true
}

func (e *constantEvaluator) startReference(frame *constantFrame, stack *[]constantFrame) {
	id, ok := e.referenceSymbol(frame.ref, frame.node)
	if !ok {
		e.finish(frame.ref, constantResult{State: constantUnavailable})
		return
	}
	value, enum := e.enumValue(id)
	if enum {
		e.finish(frame.ref, constantResult{State: constantKnown, Value: value})
		return
	}
	if e.symbolColor[id] == memoGray {
		start := e.activeIndex[id]
		members := e.active[start:]
		result := e.failure(e.cycleOrigin(members), "constant declaration cycle")
		for _, member := range members {
			e.symbolColor[member] = memoBlack
			e.symbolMemo[member] = result
		}
		e.finish(frame.ref, result)
		return
	}
	initializer, ok, semantic := e.bindingInitializer(id)
	if !ok {
		if semantic {
			e.finish(frame.ref, e.failure(frame.ref, "reference is not a module-level let constant"))
		} else {
			e.finish(frame.ref, constantResult{State: constantUnavailable})
		}
		return
	}
	frame.symbol = id
	frame.initializer = initializer
	frame.phase = 5
	if e.symbolColor[id] == memoBlack {
		frame.blocked = e.push(stack, initializer, frame.depth+1)
		return
	}
	e.symbolColor[id] = memoGray
	e.activeIndex[id] = len(e.active)
	e.active = append(e.active, id)
	frame.blocked = e.push(stack, initializer, frame.depth+1)
}

func (e *constantEvaluator) cycleOrigin(members []symbol.SymbolID) symbol.SyntaxRef {
	var best symbol.SyntaxRef
	var bestSpan source.Span
	for _, id := range members {
		initializer, ok, _ := e.bindingInitializer(id)
		if !ok {
			continue
		}
		node, ok := e.node(initializer)
		if !ok {
			continue
		}
		span := node.Span()
		if best == (symbol.SyntaxRef{}) || span.Source < bestSpan.Source || span.Source == bestSpan.Source && (span.Start < bestSpan.Start || span.Start == bestSpan.Start && span.End < bestSpan.End) {
			best, bestSpan = initializer, span
		}
	}
	return best
}

func (e *constantEvaluator) leaveSymbol(id symbol.SymbolID) {
	index, ok := e.activeIndex[id]
	if !ok {
		return
	}
	delete(e.activeIndex, id)
	if index == len(e.active)-1 {
		e.active = e.active[:index]
	}
}

func (e *constantEvaluator) finish(ref symbol.SyntaxRef, result constantResult, children ...symbol.SyntaxRef) {
	if _, exists := e.memo[ref]; exists {
		return
	}
	result.contextual = false
	e.memo[ref] = result.clone()
	info := constantDepth{height: 1, count: uint8(len(children))}
	for i, child := range children {
		info.children[i] = child
		childHeight := e.memoDepth[child].height
		if childHeight < ^uint32(0) && childHeight+1 > info.height {
			info.height = childHeight + 1
		}
	}
	e.memoDepth[ref] = info
	e.order = append(e.order, ref)
}

func (e *constantEvaluator) depthOrigin(ref symbol.SyntaxRef, remaining uint32) symbol.SyntaxRef {
	for remaining != 0 {
		info, ok := e.memoDepth[ref]
		if !ok || info.height <= remaining || info.count == 0 {
			break
		}
		remaining--
		found := false
		for i := uint8(0); i < info.count; i++ {
			child := info.children[i]
			if e.memoDepth[child].height > remaining {
				ref = child
				found = true
				break
			}
		}
		if !found {
			break
		}
	}
	return ref
}

func (e *constantEvaluator) literal(ref symbol.SyntaxRef, node syntax.Node) constantResult {
	switch node.Token() {
	case syntax.IntegerLiteral:
		text, ok := e.text(node.Span())
		if !ok {
			return constantResult{State: constantUnavailable}
		}
		value, ok := e.parseInteger(text)
		if !ok {
			return e.failure(ref, "integer constant exceeds its magnitude-bit limit")
		}
		return constantResult{State: constantKnown, Value: constantValue{Kind: constantInteger, Integer: value}}
	case syntax.KwTrue:
		return constantResult{State: constantKnown, Value: constantValue{Kind: constantBoolean, Boolean: true}}
	case syntax.KwFalse:
		return constantResult{State: constantKnown, Value: constantValue{Kind: constantBoolean}}
	case syntax.CharacterLiteral:
		decoded, ok := node.DecodedLiteral()
		if !ok || decoded.Kind != syntax.DecodedCharacter {
			return constantResult{State: constantUnavailable}
		}
		return constantResult{State: constantKnown, Value: constantValue{Kind: constantCharacter, Character: decoded.Rune}}
	case syntax.StringLiteral:
		decoded, ok := node.DecodedLiteral()
		if !ok || decoded.Kind != syntax.DecodedString {
			return constantResult{State: constantUnavailable}
		}
		return constantResult{State: constantKnown, Value: constantValue{Kind: constantString, String: decoded.Text}}
	case syntax.FloatLiteral:
		text, ok := e.text(node.Span())
		if !ok {
			return constantResult{State: constantUnavailable}
		}
		parsed, err := strconv.ParseFloat(string(text), 64)
		if err != nil {
			return e.failure(ref, "floating constant could not be parsed")
		}
		return constantResult{State: constantKnown, Value: constantValue{Kind: constantFloat, Float: parsed}}
	default:
		return e.failure(ref, fmt.Sprintf("%s is not an accepted constant literal", node.Token()))
	}
}

func (e *constantEvaluator) parseInteger(spelling []byte) (*big.Int, bool) {
	base := 10
	start := 0
	if len(spelling) >= 2 && spelling[0] == '0' {
		switch spelling[1] {
		case 'x', 'X':
			base, start = 16, 2
		case 'b', 'B':
			base, start = 2, 2
		case 'o', 'O':
			base, start = 8, 2
		}
	}
	significantStart := -1
	significantDigits := 0
	for i := start; i < len(spelling); i++ {
		if spelling[i] == '_' {
			continue
		}
		if significantStart < 0 && spelling[i] == '0' {
			continue
		}
		if significantStart < 0 {
			significantStart = i
		}
		significantDigits++
	}
	if significantStart < 0 {
		return new(big.Int), true
	}
	limit := new(big.Int).Lsh(big.NewInt(1), uint(e.config.MaxConstantBits))
	limit.Sub(limit, big.NewInt(1))
	maximum := limit.Text(base)
	if significantDigits > len(maximum) {
		return nil, false
	}
	if significantDigits == len(maximum) {
		maximumIndex := 0
		for i := significantStart; i < len(spelling); i++ {
			digit := spelling[i]
			if digit == '_' {
				continue
			}
			if digit >= 'A' && digit <= 'F' {
				digit += 'a' - 'A'
			}
			if digit < maximum[maximumIndex] {
				break
			}
			if digit > maximum[maximumIndex] {
				return nil, false
			}
			maximumIndex++
		}
	}
	digits := make([]byte, 0, significantDigits)
	for i := significantStart; i < len(spelling); i++ {
		if spelling[i] != '_' {
			digits = append(digits, spelling[i])
		}
	}
	value, ok := new(big.Int).SetString(string(digits), base)
	return value, ok
}

func (e *constantEvaluator) applyPrefix(ref symbol.SyntaxRef, op syntax.TokenKind, operand constantResult) constantResult {
	if operand.State != constantKnown {
		return operand
	}
	switch op {
	case syntax.Minus:
		if operand.Value.Kind != constantInteger {
			return e.failure(ref, "unary - requires an integer constant")
		}
		value := new(big.Int).Neg(operand.Value.Integer)
		return e.integerResult(ref, value)
	case syntax.Tilde:
		if operand.Value.Kind != constantInteger {
			return e.failure(ref, "unary ~ requires an integer constant")
		}
		if operand.Value.Integer.Sign() >= 0 && !e.magnitudeCanGrowByOne(operand.Value.Integer) {
			return e.failure(ref, "constant integer magnitude-bit limit exceeded")
		}
		return e.integerResult(ref, new(big.Int).Not(operand.Value.Integer))
	case syntax.Bang:
		if operand.Value.Kind != constantBoolean {
			return e.failure(ref, "unary ! requires a Boolean constant")
		}
		return constantResult{State: constantKnown, Value: constantValue{Kind: constantBoolean, Boolean: !operand.Value.Boolean}}
	default:
		return e.failure(ref, fmt.Sprintf("operator %s is not accepted in a constant expression", op))
	}
}

func (e *constantEvaluator) applyBinary(ref symbol.SyntaxRef, op syntax.TokenKind, left, right constantResult) constantResult {
	if left.State != constantKnown {
		return left
	}
	if right.State != constantKnown {
		return right
	}
	if op == syntax.Equal || op == syntax.NotEqual {
		equal, ok := equalConstants(left.Value, right.Value)
		if !ok {
			return e.failure(ref, "constant equality operands must have the same kind")
		}
		if op == syntax.NotEqual {
			equal = !equal
		}
		return booleanConstant(equal)
	}
	if op == syntax.Less || op == syntax.LessEqual || op == syntax.Greater || op == syntax.GreaterEqual {
		comparison, ok := compareConstants(left.Value, right.Value)
		if !ok {
			return e.failure(ref, "ordered constant comparison requires identical ordered scalar kinds")
		}
		return booleanConstant(compareByOperator(op, comparison))
	}
	if op == syntax.LogicalAnd || op == syntax.LogicalOr {
		if left.Value.Kind != constantBoolean || right.Value.Kind != constantBoolean {
			return e.failure(ref, "logical constant operation requires Boolean operands")
		}
		if op == syntax.LogicalAnd {
			return booleanConstant(left.Value.Boolean && right.Value.Boolean)
		}
		return booleanConstant(left.Value.Boolean || right.Value.Boolean)
	}
	if left.Value.Kind != constantInteger || right.Value.Kind != constantInteger {
		return e.failure(ref, fmt.Sprintf("operator %s requires integer constants", op))
	}
	a, b := left.Value.Integer, right.Value.Integer
	var value *big.Int
	switch op {
	case syntax.Plus:
		if !e.additionFits(a, b) {
			return e.failure(ref, "constant addition exceeds magnitude-bit limit")
		}
		value = new(big.Int).Add(a, b)
	case syntax.Minus:
		if !e.additionFits(a, new(big.Int).Neg(b)) {
			return e.failure(ref, "constant subtraction exceeds magnitude-bit limit")
		}
		value = new(big.Int).Sub(a, b)
	case syntax.Star:
		if !e.multiplicationFits(a, b) {
			return e.failure(ref, "constant multiplication exceeds magnitude-bit limit")
		}
		value = new(big.Int).Mul(a, b)
	case syntax.Slash:
		if b.Sign() == 0 {
			return e.failure(ref, "division by zero in constant expression")
		}
		value = new(big.Int).Quo(a, b)
	case syntax.Percent:
		if b.Sign() == 0 {
			return e.failure(ref, "remainder by zero in constant expression")
		}
		value = new(big.Int).Rem(a, b)
	case syntax.ShiftLeft, syntax.ShiftRight:
		if b.Sign() < 0 || !b.IsUint64() || b.Uint64() > uint64(e.config.MaxConstantBits) || b.Uint64() > uint64(^uint(0)) {
			return e.failure(ref, "invalid or over-limit constant shift")
		}
		shift := uint(b.Uint64())
		if op == syntax.ShiftLeft {
			if a.Sign() != 0 && uint64(a.BitLen())+uint64(shift) > uint64(e.config.MaxConstantBits) {
				return e.failure(ref, "constant shift exceeds magnitude-bit limit")
			}
			value = new(big.Int).Lsh(a, shift)
		} else {
			value = new(big.Int).Rsh(a, shift)
		}
	case syntax.Ampersand:
		var ok bool
		value, ok = e.bitwiseResult(op, a, b)
		if !ok {
			return e.failure(ref, "constant bitwise operation exceeds magnitude-bit limit")
		}
	case syntax.Pipe:
		var ok bool
		value, ok = e.bitwiseResult(op, a, b)
		if !ok {
			return e.failure(ref, "constant bitwise operation exceeds magnitude-bit limit")
		}
	case syntax.Caret:
		var ok bool
		value, ok = e.bitwiseResult(op, a, b)
		if !ok {
			return e.failure(ref, "constant bitwise operation exceeds magnitude-bit limit")
		}
	default:
		return e.failure(ref, fmt.Sprintf("operator %s is not accepted in a constant expression", op))
	}
	return e.integerResult(ref, value)
}

func (e *constantEvaluator) maximumMagnitude() *big.Int {
	maximum := new(big.Int).Lsh(big.NewInt(1), uint(e.config.MaxConstantBits))
	return maximum.Sub(maximum, big.NewInt(1))
}

func (e *constantEvaluator) magnitudeCanGrowByOne(value *big.Int) bool {
	return new(big.Int).Abs(value).Cmp(e.maximumMagnitude()) < 0
}

func (e *constantEvaluator) additionFits(a, b *big.Int) bool {
	if a.Sign() == 0 || b.Sign() == 0 || a.Sign() != b.Sign() {
		return true
	}
	remaining := new(big.Int).Sub(e.maximumMagnitude(), new(big.Int).Abs(a))
	return new(big.Int).Abs(b).Cmp(remaining) <= 0
}

func (e *constantEvaluator) multiplicationFits(a, b *big.Int) bool {
	if a.Sign() == 0 || b.Sign() == 0 {
		return true
	}
	quotient := new(big.Int).Quo(e.maximumMagnitude(), new(big.Int).Abs(a))
	return new(big.Int).Abs(b).Cmp(quotient) <= 0
}

func (e *constantEvaluator) bitwiseResult(op syntax.TokenKind, a, b *big.Int) (*big.Int, bool) {
	aNegative, bNegative := a.Sign() < 0, b.Sign() < 0
	x, y := a, b
	if aNegative {
		x = new(big.Int).Not(a)
	}
	if bNegative {
		y = new(big.Int).Not(b)
	}
	value := new(big.Int)
	negative := false
	switch op {
	case syntax.Ampersand:
		switch {
		case aNegative && bNegative:
			value.Or(x, y)
			negative = true
		case aNegative:
			value.AndNot(y, x)
		case bNegative:
			value.AndNot(x, y)
		default:
			value.And(x, y)
		}
	case syntax.Pipe:
		switch {
		case aNegative && bNegative:
			value.And(x, y)
			negative = true
		case aNegative:
			value.AndNot(x, y)
			negative = true
		case bNegative:
			value.AndNot(y, x)
			negative = true
		default:
			value.Or(x, y)
		}
	case syntax.Caret:
		value.Xor(x, y)
		negative = aNegative != bNegative
	default:
		return nil, false
	}
	if !negative {
		return value, true
	}
	if !e.magnitudeCanGrowByOne(value) {
		return nil, false
	}
	return value.Not(value), true
}

func (e *constantEvaluator) integerResult(ref symbol.SyntaxRef, value *big.Int) constantResult {
	if value == nil || uint64(value.BitLen()) > uint64(e.config.MaxConstantBits) {
		return e.failure(ref, "constant integer magnitude-bit limit exceeded")
	}
	return constantResult{State: constantKnown, Value: constantValue{Kind: constantInteger, Integer: value}}
}

func booleanConstant(value bool) constantResult {
	return constantResult{State: constantKnown, Value: constantValue{Kind: constantBoolean, Boolean: value}}
}

func equalConstants(a, b constantValue) (bool, bool) {
	if a.Kind != b.Kind {
		return false, false
	}
	switch a.Kind {
	case constantInteger:
		return a.Integer.Cmp(b.Integer) == 0, true
	case constantBoolean:
		return a.Boolean == b.Boolean, true
	case constantCharacter:
		return a.Character == b.Character, true
	case constantString:
		return a.String == b.String, true
	case constantEnum:
		return a.EnumType == b.EnumType && a.EnumVariant == b.EnumVariant, a.EnumType == b.EnumType
	default:
		return false, false
	}
}

func compareConstants(a, b constantValue) (int, bool) {
	if a.Kind != b.Kind {
		return 0, false
	}
	switch a.Kind {
	case constantInteger:
		return a.Integer.Cmp(b.Integer), true
	case constantCharacter:
		if a.Character < b.Character {
			return -1, true
		}
		if a.Character > b.Character {
			return 1, true
		}
		return 0, true
	case constantString:
		return strings.Compare(a.String, b.String), true
	case constantEnum:
		if a.EnumType != b.EnumType {
			return 0, false
		}
		if a.EnumOrdinal < b.EnumOrdinal {
			return -1, true
		}
		if a.EnumOrdinal > b.EnumOrdinal {
			return 1, true
		}
		return 0, true
	default:
		return 0, false
	}
}

func compareByOperator(op syntax.TokenKind, value int) bool {
	switch op {
	case syntax.Less:
		return value < 0
	case syntax.LessEqual:
		return value <= 0
	case syntax.Greater:
		return value > 0
	case syntax.GreaterEqual:
		return value >= 0
	}
	return false
}

func (e *constantEvaluator) referenceSymbol(ref symbol.SyntaxRef, node syntax.Node) (symbol.SymbolID, bool) {
	query := ref
	if node.Kind() == syntax.Path || node.Kind() == syntax.MemberExpr {
		children := node.Children()
		if len(children) == 0 {
			return 0, false
		}
		query.Node = children[len(children)-1]
	}
	resolution, ok := e.inputs.Resolution.Reference(query)
	return resolution.Symbol, ok && resolution.State == symbol.ResolutionResolved && resolution.Symbol != 0
}

func (e *constantEvaluator) bindingInitializer(id symbol.SymbolID) (symbol.SyntaxRef, bool, bool) {
	value, ok := e.inputs.Resolution.Symbols.Symbol(id)
	if !ok {
		return symbol.SyntaxRef{}, false, false
	}
	if value.Kind != symbol.SymbolBinding || value.Containing != 0 {
		return symbol.SyntaxRef{}, false, true
	}
	scope, ok := e.inputs.Resolution.Scopes.Scope(value.Scope)
	if !ok {
		return symbol.SyntaxRef{}, false, false
	}
	if scope.Kind != symbol.ScopeModule {
		return symbol.SyntaxRef{}, false, true
	}
	item, ok := e.modules[value.Declaration.Module]
	if !ok {
		return symbol.SyntaxRef{}, false, false
	}
	node, ok := item.Tree.Node(value.Declaration.Node)
	if !ok || node.Kind() != syntax.BindingDecl {
		return symbol.SyntaxRef{}, false, false
	}
	if node.Token() != syntax.KwLet || node.Data()&syntax.BindingInitializerPresent == 0 {
		return symbol.SyntaxRef{}, false, true
	}
	children := node.Children()
	index := 1
	if node.Data()&syntax.BindingTypePresent != 0 {
		index++
	}
	if index >= len(children) {
		return symbol.SyntaxRef{}, false, false
	}
	return symbol.SyntaxRef{Module: value.Declaration.Module, Node: children[index]}, true, true
}

func (e *constantEvaluator) enumValue(id symbol.SymbolID) (constantValue, bool) {
	variant, ok := e.inputs.Resolution.Symbols.Symbol(id)
	if !ok || variant.Kind != symbol.SymbolVariant || variant.Containing == 0 {
		return constantValue{}, false
	}
	owner, ok := e.inputs.Resolution.Symbols.Symbol(variant.Containing)
	if !ok {
		return constantValue{}, false
	}
	item, ok := e.modules[owner.Declaration.Module]
	if !ok {
		return constantValue{}, false
	}
	decl, ok := item.Tree.Node(owner.Declaration.Node)
	if !ok {
		return constantValue{}, false
	}
	isEnum := false
	for _, child := range decl.Children() {
		if node, exists := item.Tree.Node(child); exists && node.Kind() == syntax.EnumType {
			isEnum = true
			break
		}
	}
	if !isEnum {
		return constantValue{}, false
	}
	for ordinal, member := range e.inputs.Resolution.Members(owner.ID) {
		if member == id {
			return constantValue{Kind: constantEnum, EnumType: owner.ID, EnumVariant: id, EnumOrdinal: uint32(ordinal)}, true
		}
	}
	return constantValue{}, false
}

func (e *constantEvaluator) failure(ref symbol.SyntaxRef, message string) constantResult {
	if e.report(ref, message) {
		return constantResult{State: constantError}
	}
	return constantResult{State: constantUnavailable}
}

func (e *constantEvaluator) report(ref symbol.SyntaxRef, message string) bool {
	if e == nil {
		return false
	}
	node, ok := e.node(ref)
	if !ok {
		return false
	}
	span := node.Span()
	return e.budget.add(diagnostic.Diagnostic{
		Severity: diagnostic.Error,
		Code:     CodeInvalidConstant,
		Message:  message,
		Primary:  diagnostic.Label{Span: span},
	})
}

func (e *constantEvaluator) node(ref symbol.SyntaxRef) (syntax.Node, bool) {
	item, ok := e.modules[ref.Module]
	if !ok || item.Tree == nil {
		return syntax.Node{}, false
	}
	return item.Tree.Node(ref.Node)
}
func (e *constantEvaluator) validRef(ref symbol.SyntaxRef) bool { _, ok := e.node(ref); return ok }
func (e *constantEvaluator) text(span source.Span) ([]byte, bool) {
	file, ok := e.inputs.Sources.File(span.Source)
	if !ok {
		return nil, false
	}
	text := file.Slice(span)
	return text, text != nil
}

type frozenConstant struct {
	Syntax symbol.SyntaxRef
	Result constantResult
}
type frozenConstants struct{ values []frozenConstant }

func (e *constantEvaluator) freeze() frozenConstants {
	values := make([]frozenConstant, 0, len(e.order))
	for _, ref := range e.order {
		values = append(values, frozenConstant{Syntax: ref, Result: e.memo[ref].clone()})
	}
	return frozenConstants{values: values}
}
func (f frozenConstants) All() []frozenConstant {
	out := make([]frozenConstant, len(f.values))
	for i := range f.values {
		out[i] = frozenConstant{Syntax: f.values[i].Syntax, Result: f.values[i].Result.clone()}
	}
	return out
}
func (f frozenConstants) Constant(ref symbol.SyntaxRef) (constantResult, bool) {
	for _, value := range f.values {
		if value.Syntax == ref {
			return value.Result.clone(), true
		}
	}
	return constantResult{}, false
}
