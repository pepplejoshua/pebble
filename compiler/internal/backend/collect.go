package backend

import (
	"fmt"

	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/tir"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

// findEntryDeclaration locates the FunctionDeclaration node for entrySymbol.
// A specialization would carry non-empty TypeArgs; the entry cannot be
// generic, so those are deliberately excluded rather than assumed absent.
func findEntryDeclaration(unit *tir.Unit, entrySymbol symbol.SymbolID) (tir.Node, error) {
	return findFunctionDeclaration(unit, entrySymbol, "entry function")
}

// findFunctionDeclaration locates the non-generic FunctionDeclaration node
// for the given function symbol, generalizing findEntryDeclaration to any
// function the reachability walk resolves. Every typed-IR function this
// backend emits — the entry and every called helper — has exactly one such
// declaration; a generic instance would carry non-empty TypeArgs and is
// excluded, since generic calls are not lowered here.
func findFunctionDeclaration(unit *tir.Unit, symbolID symbol.SymbolID, what string) (tir.Node, error) {
	for _, node := range unit.Nodes() {
		if (node.Kind != tir.FunctionDeclaration && node.Kind != tir.ExternDeclaration) || node.Symbol != symbolID || len(node.TypeArgs) != 0 {
			continue
		}
		return node, nil
	}
	return tir.Node{}, fmt.Errorf("%s not found in unit: no non-generic FunctionDeclaration or ExternDeclaration for symbol %d", what, symbolID)
}

func findCalledFunctionDeclaration(unit *tir.Unit, symbolID symbol.SymbolID, typeArgs []types.TypeID) (tir.Node, error) {
	for _, node := range unit.Nodes() {
		if node.Kind != tir.FunctionDeclaration || node.Symbol != symbolID || len(node.TypeArgs) != len(typeArgs) {
			continue
		}
		match := true
		for i := range typeArgs {
			match = match && node.TypeArgs[i] == typeArgs[i]
		}
		if match {
			return node, nil
		}
	}
	return tir.Node{}, fmt.Errorf("called function symbol %d specialization not found", symbolID)
}

func findCalledFunctionByResult(unit *tir.Unit, symbolID symbol.SymbolID, result types.TypeID) (tir.Node, error) {
	for _, node := range unit.Nodes() {
		if node.Kind == tir.FunctionDeclaration && node.Symbol == symbolID && len(node.TypeArgs) != 0 && node.ResultType == result {
			return node, nil
		}
	}
	return tir.Node{}, fmt.Errorf("called function symbol %d concrete specialization not found", symbolID)
}

func findCallDeclaration(unit *tir.Unit, snapshot *types.Snapshot, call tir.Node) (tir.Node, error) {
	if len(call.TypeArgs) != 0 {
		return findCalledFunctionDeclaration(unit, call.Symbol, call.TypeArgs)
	}
	decl, err := findFunctionDeclaration(unit, call.Symbol, "called function")
	if err != nil {
		decl, err = findCalledFunctionByResult(unit, call.Symbol, call.Type)
	}
	if err != nil {
		return tir.Node{}, err
	}
	// A generic struct method whose own parameter/result types reference the
	// containing struct's type parameter directly is a single symbolic
	// declaration shared by every instantiation; every call site that reads
	// the callee's declared signature (the aggregate-result-match checks in
	// the call initializers, the store-value shape dispatch, buildExpr's call
	// handling) must see the SAME substituted signature the reachability walk
	// discovered the method's helper under — so the concrete instantiation is
	// applied here, from the call site's own receiver, exactly as
	// buildDirectCallArgs applies it to the argument grammar and C name.
	if substitutions := genericStructMethodSubstitutions(unit, snapshot, call, decl); substitutions != nil {
		return substituteDeclarationSignature(snapshot, decl, substitutions), nil
	}
	return decl, nil
}

// findEntryBody follows the entry declaration's FunctionID to its FunctionDecl
// and resolves that declaration's body node. The body node is a distinct
// Block entry in unit.Nodes(), separate from the FunctionDeclaration node
// found by findEntryDeclaration. It returns both the resolved Block node and
// its NodeID, so the caller can pass the ID into the recursive buildBlock.
func findEntryBody(unit *tir.Unit, decl tir.Node) (tir.Node, tir.NodeID, error) {
	return findFunctionBody(unit, decl, "entry function")
}

// findFunctionBody resolves the body Block for any function declaration,
// generalizing findEntryBody to a called helper: it follows the declaration's
// FunctionID to its FunctionDecl container and resolves that container's body
// node, returning both the Block node and its NodeID.
func findFunctionBody(unit *tir.Unit, decl tir.Node, what string) (tir.Node, tir.NodeID, error) {
	for _, fd := range unit.FunctionDeclarations() {
		if fd.FunctionID != decl.Function {
			continue
		}
		block, ok := unit.Node(fd.Node)
		if !ok {
			return tir.Node{}, 0, fmt.Errorf("%s body not found in unit: FunctionDecl %d has invalid body node %d", what, fd.FunctionID, fd.Node)
		}
		if block.Kind != tir.Block {
			return tir.Node{}, 0, fmt.Errorf("%s body is a %s, want a Block", what, block.Kind)
		}
		return block, fd.Node, nil
	}
	return tir.Node{}, 0, fmt.Errorf("%s body declaration not found in unit: no FunctionDecl for FunctionID %d", what, decl.Function)
}

// collectArrayTypes resolves, in first-encountered order, every array type
// the emitted program actually references: the entry body (root) followed by
// every reachable helper's body, each walked by the same Children + DeferChain
// traversal collectDirectCalls uses, plus each reachable helper's own declared
// parameter types and result type (the collectHelperArrayTypes scan, which
// covers an array type referenced only by a helper's C signature — an
// array-typed parameter or array-returning helper names
// pebble_array_<typeID>_t even if no reachable body ever constructs an array
// of that type). The walk closes the same compounding gap the tagged-union
// sizeof fix (f2e8c62) closed for union enums: a bare `sizeof [N]T` with no
// other reference to that array type anywhere in the program must still force
// the array's typedef to be emitted, or the lowered sizeof(pebble_array_
// <typeID>_t) names an undeclared C type. Array types referenced only as
// entry-body locals need no typedef here — such a local is emitted as a plain
// C array (element C type + `[length]`, see buildArrayLocalDeclaration), not
// as its pebble_array_<typeID>_t typedef — so the walk deliberately does not
// collect every array-typed local, matching what the emitted C actually
// references. The caller deduplicates (see Emit) so each distinct array type
// yields exactly one typedef, emitted before any function definition in the
// final output.
func collectArrayTypes(unit *tir.Unit, snapshot *types.Snapshot, entryBlockID tir.NodeID, helpers []helperInfo) ([]types.TypeID, error) {
	var collected []types.TypeID
	if err := collectArrayTypesWalk(unit, snapshot, entryBlockID, &collected); err != nil {
		return nil, err
	}
	for _, helper := range helpers {
		if err := collectArrayTypesWalk(unit, snapshot, helper.block, &collected); err != nil {
			return nil, err
		}
		for _, param := range helper.decl.Parameters {
			if isArray(snapshot, param.Type) {
				collected = append(collected, param.Type)
			}
		}
		if isArray(snapshot, helper.decl.ResultType) {
			collected = append(collected, helper.decl.ResultType)
		}
	}
	seen := make(map[types.TypeID]bool, len(collected))
	var deduplicated []types.TypeID
	for _, id := range collected {
		if seen[id] {
			continue
		}
		seen[id] = true
		deduplicated = append(deduplicated, id)
	}
	return deduplicated, nil
}

// collectArrayTypesWalk appends every array type encountered in the tree
// rooted at nodeID to out, in first-encountered order, following Children and
// DeferChain exactly like collectDirectCalls so it visits the same reachable
// region of the node graph the body builders consume. Two node shapes carry an
// array type the emitted C actually references a typedef for: a SizeofType
// node whose TypeArg is an array type — a bare `sizeof [N]T` references the
// array's pebble_array_<typeID>_t typedef even though nothing else in the
// program may construct or pass an array of that type, so only the SizeofType
// node carries it (the same SizeofType collection gap collectUnionTypesWalk
// closes for union enums) — and a print statement operand of array type
// (composite print slice 2), whose direct-sequential-fprintf lowering
// materializes the value into a pebble_array_<typeID>_t temp that also
// references the typedef. An ordinary array-typed local, by contrast, is
// deliberately NOT collected: it emits as a raw C array (`int32_t
// pebble_local_<sym>[N]`) that never references the typedef.
func collectArrayTypesWalk(unit *tir.Unit, snapshot *types.Snapshot, nodeID tir.NodeID, out *[]types.TypeID) error {
	node, ok := unit.Node(nodeID)
	if !ok {
		return fmt.Errorf("array-type walk references invalid node %d", nodeID)
	}
	if node.Kind == tir.SizeofType && isArray(snapshot, node.TypeArg) {
		// A bare `sizeof` of a fixed array (no construction or helper
		// signature anywhere, so no other node carries the type): the array's
		// pebble_array_<typeID>_t typedef must still be collected and emitted,
		// or the lowered sizeof(pebble_array_<typeID>_t) names an undeclared C
		// type (see sizeofCTypeName).
		*out = append(*out, node.TypeArg)
	}
	if node.Kind == tir.Print {
		// A print operand of array type: the print's direct-sequential-fprintf
		// lowering materializes the operand into a per-operand
		// pebble_array_<typeID>_t temp (see buildArrayPrintOperand), so the
		// array's typedef must be collected and emitted or the temp's
		// declaration names an undeclared C type. Every reachable print
		// operand node (a SymbolValue, an ArrayValue, a DirectCall, a Load,
		// or a SourceAlias wrapping one) carries the array Type on the operand
		// node itself, so checking the child's Type is sufficient.
		for _, childID := range node.Children {
			if child, ok := unit.Node(childID); ok && isArray(snapshot, child.Type) {
				*out = append(*out, child.Type)
			}
		}
	}
	if node.Kind == tir.Store {
		// A whole-array reassignment from a fresh array literal (`a = [7, 8,
		// 9];`, or `self.data = [7, 8, 9];` through a struct field): the
		// store's ArrayValue RHS lowers to a memcpy from a
		// pebble_array_<typeID>_t compound literal (see buildArrayStoreValue),
		// whose typedef the standalone array local never carries — the
		// assignment target is a raw C array (see buildArrayLocalDeclaration),
		// not its wrapper typedef. The ArrayValue child's own Type is the
		// array type (buildArrayStoreValue requires it to equal the place's
		// resolved array type), so the walk must collect it exactly like a
		// sizeof/print reference or the lowered memcpy names an undeclared C
		// type. A store whose RHS is anything else (a SymbolValue naming an
		// array-typed local, a scalar) never constructs the compound literal,
		// so no array type is collected for those.
		if len(node.Children) == 2 {
			if value, ok := unit.Node(node.Children[1]); ok && value.Kind == tir.ArrayValue && isArray(snapshot, value.Type) {
				*out = append(*out, value.Type)
			}
		}
	}
	for _, childID := range node.Children {
		if err := collectArrayTypesWalk(unit, snapshot, childID, out); err != nil {
			return err
		}
	}
	for _, deferID := range node.DeferChain {
		if err := collectArrayTypesWalk(unit, snapshot, deferID, out); err != nil {
			return err
		}
	}
	return nil
}

// discoverReachableHelpers finds exactly the set of non-entry functions the
// entry actually calls, transitively, by walking the entry's body for
// tir.DirectCall nodes and recursing into each newly-discovered callee's own
// body — a worklist/DFS over the direct-call edges starting from the entry,
// following into every function reached. Emitting exactly this reachable set
// (and nothing else) guarantees by construction that every emitted helper has
// at least one call site, so the mandated -Wall -Wextra -Werror build never
// warns about an unused static function. Each reached callee is validated
// (Pebble-convention, parameters each of the entry's width or bool, result
// exactly the entry's width —
// validateHelperSignature) and its body located (findFunctionBody) before
// recursing. The returned slice is a post-order of the walk — callees before
// callers — which is the emission order that keeps every call in the emitted
// C text forward (definition before use); since buildHelperPrototypes now
// emits a forward declaration for every reachable helper before any
// definition, that ordering is no longer a correctness requirement — a cycle
// (a function that can reach itself, directly or through others) is simply
// skipped, not rejected, so recursive and mutually-recursive functions are
// discovered and emitted like any other reachable helper. A cycle passing
// THROUGH the entry function is the one cycle shape still rejected (the entry
// has the fixed C name pebble_user_main, not a pebble_fn_<symbolID> helper
// name the forward-declaration pass covers).
func discoverReachableHelpers(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, entryDecl tir.Node, entryBlockID tir.NodeID, width types.BuiltinKind) ([]helperInfo, error) {
	walk := &reachabilityWalk{
		st:       st,
		unit:     unit,
		snapshot: snapshot,
		width:    width,
		entry:    entryDecl.Symbol,
		done:     make(map[helperKey]bool),
	}
	if err := walk.visit(entryDecl, entryBlockID, nil); err != nil {
		return nil, err
	}
	return walk.order, nil
}

// collectDirectCalls appends every tir.DirectCall node in the tree rooted at
// nodeID, following Children and DeferChain, plus — since the function-values
// slice — every tir.HoistedFunctionValue node (a bare top-level function
// reference used as a value, e.g. the initializer of a function-typed local or
// the callee of an indirect call), whose referenced function must be emitted
// as a helper even though no DirectCall ever invokes it, and — since the
// bool-interpolation print slice — every value part of a tir.InterpolatedString
// (a helper call used as an interpolated value, e.g. `s.contains(...)` in
// `print \`found? {s.contains("x")}\`;`, whose parts store their evaluated
// value nodes in Parts[].Value rather than Children). The typed-IR node
// graph is
// single-parented, so this walk terminates and each node is visited at most
// once per path. A DeferRegister child is skipped here: the deferred statement
// inside it is only ever emitted at exit points whose DeferChain references
// the register, so a call inside it is reachable exactly when some exit's
// DeferChain walk (below, which DOES recurse into the register's children)
// reaches it. Walking the register's children at its registration position
// would instead treat a defer that never fires — registered in a region no
// exit of the program leaves through — as making its callee reachable,
// emitting a helper no emitted call site ever invokes and tripping
// -Wunused-function under the mandated -Wall -Wextra -Werror build. (This also
// keeps a deferred Store whose value is a helper call consistent: the callee
// is emitted only when that defer actually fires.)
func collectDirectCalls(unit *tir.Unit, nodeID tir.NodeID, out *[]tir.Node) error {
	node, ok := unit.Node(nodeID)
	if !ok {
		return fmt.Errorf("reachability walk references invalid node %d", nodeID)
	}
	if node.Kind == tir.DirectCall || node.Kind == tir.MethodCall || node.Kind == tir.HoistedFunctionValue || node.Kind == tir.GenericFunctionValue {
		*out = append(*out, node)
	}
	if node.Kind == tir.RecordConstruct {
		// A struct construction's field values (`Table.{ op = add }`,
		// function-types slice 2) are stored in node.Fields
		// ([]FieldInit{Field, Value}), NOT node.Children (confirmed by reading
		// collectStructTypesWalk's identical special-case for the same
		// reason) — so a HoistedFunctionValue used only as a field's
		// construction value (e.g. `add` in `Table.{ op = add }`) is walked
		// explicitly here; otherwise the referenced function is never
		// discovered as reachable and its pebble_fn_<symbolID> definition is
		// never emitted, leaving the struct's field initializer referencing
		// an undeclared identifier.
		for _, field := range node.Fields {
			if err := collectDirectCalls(unit, field.Value, out); err != nil {
				return err
			}
		}
	}
	if node.Kind == tir.InterpolatedString {
		for _, part := range node.Parts {
			if part.Kind == tir.InterpolationValuePart {
				if err := collectDirectCalls(unit, part.Value, out); err != nil {
					return err
				}
			}
		}
	}
	for _, childID := range node.Children {
		if child, ok := unit.Node(childID); ok && child.Kind == tir.DeferRegister {
			continue
		}
		if err := collectDirectCalls(unit, childID, out); err != nil {
			return err
		}
	}
	for _, deferID := range node.DeferChain {
		if err := collectDirectCalls(unit, deferID, out); err != nil {
			return err
		}
	}
	return nil
}

// collectTupleTypes appends, in first-encountered order, the tuple TypeID of
// every tuple type the emitted program actually references: the entry body
// (root) followed by every reachable helper's body, each walked by the same
// Children + DeferChain traversal collectDirectCalls uses. A tuple type is
// referenced in exactly four places in the emitted C — a tuple-typed local's
// declaration (an Initialize whose initializer value carries the tuple type), a
// tuple construction (a TupleValue, whose Type is the tuple type), a
// tuple-typed parameter of a reachable helper (a FunctionDeclaration.Parameters
// entry's Type), and a tuple-typed result of a reachable helper (a
// FunctionDeclaration.ResultType, whose typedef its C signature names as its
// return type) — so collecting exactly those node shapes, each reachable
// helper's Parameters list, and each reachable helper's ResultType guarantees
// every typedef the program needs is discovered. The Parameters/ResultType
// coverage closes a real gap: a tuple type used only as a parameter type or
// only as a helper's result type (never constructed in any reachable body)
// still needs its typedef emitted, since the helper's C signature names
// pebble_tuple_<typeID>_t. The caller deduplicates (see Emit) so each distinct
// tuple type yields exactly one typedef, emitted before any function
// definition in the final output.
func collectTupleTypes(unit *tir.Unit, snapshot *types.Snapshot, entryBlockID tir.NodeID, helpers []helperInfo) ([]types.TypeID, error) {
	var collected []types.TypeID
	if err := collectTupleTypesWalk(unit, snapshot, entryBlockID, &collected); err != nil {
		return nil, err
	}
	for _, helper := range helpers {
		if err := collectTupleTypesWalk(unit, snapshot, helper.block, &collected); err != nil {
			return nil, err
		}
		// A reachable helper's own parameter list is a source of tuple types
		// the body walk cannot see: a tuple-typed parameter is referenced by
		// the helper's C signature even if no reachable body ever constructs a
		// tuple of that type, so its typedef must be discovered here too.
		for _, param := range helper.decl.Parameters {
			if isTuple(snapshot, param.Type) {
				collected = append(collected, param.Type)
			}
		}
		// A reachable helper's own result type is the same kind of source for
		// the typedef its C signature names as its return type (10.26): a
		// tuple-returning helper's C signature declares
		// pebble_tuple_<typeID>_t, so a tuple type that appears nowhere in any
		// reachable body still needs its typedef emitted. (For a reachable
		// tuple-returning helper the body walk usually finds the type anyway,
		// since the helper must produce a tuple to return; this scan closes the
		// same class of gap 10.24's Parameters scan closed, for the return side
		// — the type may be used only as the helper's result type.)
		if isTuple(snapshot, helper.decl.ResultType) {
			collected = append(collected, helper.decl.ResultType)
		}
	}
	seen := make(map[types.TypeID]bool, len(collected))
	var deduplicated []types.TypeID
	for _, id := range collected {
		if seen[id] {
			continue
		}
		seen[id] = true
		deduplicated = append(deduplicated, id)
	}
	return deduplicated, nil
}

// collectTupleTypesWalk appends every tuple type encountered in the tree rooted
// at nodeID to out, in first-encountered order, following Children and
// DeferChain exactly like collectDirectCalls so it visits the same reachable
// region of the node graph the body builders consume. Two node shapes carry a
// tuple type: a TupleValue node's own Type, and an Initialize whose initializer
// value carries a tuple type (a tuple-typed local declaration — the local's
// type is recorded on the initializer value node, not on the Initialize node
// itself, confirmed against a real fixture). A tuple initializer that is not a
// TupleValue (a whole-tuple copy of another local) is still a tuple-typed
// local and still needs its typedef; it is collected here by the Initialize
// rule even though buildLeadingStatement rejects that initializer shape.
func collectTupleTypesWalk(unit *tir.Unit, snapshot *types.Snapshot, nodeID tir.NodeID, out *[]types.TypeID) error {
	node, ok := unit.Node(nodeID)
	if !ok {
		return fmt.Errorf("tuple-type walk references invalid node %d", nodeID)
	}
	if node.Kind == tir.TupleValue {
		*out = append(*out, node.Type)
	}
	if node.Kind == tir.Initialize {
		for _, childID := range node.Children {
			if child, ok := unit.Node(childID); ok && isTuple(snapshot, child.Type) {
				*out = append(*out, child.Type)
			}
		}
	}
	for _, childID := range node.Children {
		if err := collectTupleTypesWalk(unit, snapshot, childID, out); err != nil {
			return err
		}
	}
	for _, deferID := range node.DeferChain {
		if err := collectTupleTypesWalk(unit, snapshot, deferID, out); err != nil {
			return err
		}
	}
	return nil
}

// collectOptionalTypes appends, in first-encountered order, the optional
// TypeID of every optional type the emitted program actually references: the
// entry body (root) followed by every reachable helper's body, each walked
// by the same Children + DeferChain traversal collectDirectCalls uses, plus
// each reachable helper's own declared parameter types and result type. An
// optional type is referenced in exactly two places in the emitted C — an
// optional-typed local's declaration (an Initialize whose initializer value
// carries the optional type) and a SomeOptional node (whose Type is the
// optional type) — so collecting exactly those two node shapes, each
// reachable helper's parameter list, and each reachable helper's result type
// guarantees every typedef the program needs is discovered (the parameter/
// result scans close the same gap the tuple/struct collection's own scans
// close: an optional type used only as a parameter type or only as a helper's
// result type, never constructed in any reachable body, still needs its
// typedef emitted since the helper's C signature names
// pebble_optional_<typeID>_t). The caller deduplicates so
// each distinct optional type yields exactly one typedef, emitted before any
// function definition in the final output.
func collectOptionalTypes(unit *tir.Unit, snapshot *types.Snapshot, entryBlockID tir.NodeID, helpers []helperInfo) ([]types.TypeID, error) {
	var collected []types.TypeID
	if err := collectOptionalTypesWalk(unit, snapshot, entryBlockID, &collected); err != nil {
		return nil, err
	}
	for _, helper := range helpers {
		if err := collectOptionalTypesWalk(unit, snapshot, helper.block, &collected); err != nil {
			return nil, err
		}
		// A reachable helper's own parameter list is a source of optional
		// types the body walk cannot see: an optional-typed parameter is
		// referenced by the helper's C signature (its C declaration names
		// pebble_optional_<typeID>_t) even if no reachable body ever
		// constructs or injects an optional of that type — the body walk only
		// collects SomeOptional nodes and Initialize children, so a bare
		// `g(5)` call-site argument (an OptionalInject whose type the walk
		// does not collect) or an unused `fn g(o ?int)` parameter would leave
		// the typedef unemitted. This is the same Parameters scan 10.24 added
		// for tuple/struct/slice parameters, applied to optional parameters.
		for _, param := range helper.decl.Parameters {
			if isOptional(snapshot, param.Type) {
				collected = append(collected, param.Type)
			}
		}
		// A reachable helper's own result type is a source of optional types
		// the body walk cannot see: an optional-returning helper's C signature
		// declares pebble_optional_<typeID>_t, so an optional type that appears
		// nowhere in any reachable body still needs its typedef emitted — the
		// same ResultType scan 10.26 added for tuple/struct result types. (For
		// a reachable optional-returning helper the caller must declare a
		// matching optional-typed local to consume the result, which the body
		// walk of that local's Initialize usually finds anyway; this scan
		// closes the same gap, for the return side.)
		if isOptional(snapshot, helper.decl.ResultType) {
			collected = append(collected, helper.decl.ResultType)
		}
	}
	seen := make(map[types.TypeID]bool, len(collected))
	var deduplicated []types.TypeID
	for _, id := range collected {
		if seen[id] {
			continue
		}
		seen[id] = true
		deduplicated = append(deduplicated, id)
	}
	return deduplicated, nil
}

// collectOptionalTypesWalk appends every optional type encountered in the tree
// rooted at nodeID to out, in first-encountered order, following Children and
// DeferChain exactly like collectDirectCalls so it visits the same reachable
// region of the node graph the body builders consume. Two node shapes carry an
// optional type: a SomeOptional node's own Type, and an Initialize whose
// initializer value carries an optional type (an optional-typed local
// declaration — confirmed against a real fixture: the local's type is recorded
// on the initializer value node, not on the Initialize node itself). The
// Initialize rule alone covers a `none`-initialized local too (a NoneOptional
// node carries its own optional Type exactly like SomeOptional does), so no
// separate NoneOptional case is needed here. A RecordConstruct's field values
// are the one more source the Children-only recursion would miss: an
// optional-typed struct field (`Box[int].{ value = some 5 }`) carries its
// SomeOptional field value in node.Fields, not node.Children, so the field
// value's optional Type is collected here — the same hole the struct walk's
// own RecordConstruct case closes for struct types.
func collectOptionalTypesWalk(unit *tir.Unit, snapshot *types.Snapshot, nodeID tir.NodeID, out *[]types.TypeID) error {
	node, ok := unit.Node(nodeID)
	if !ok {
		return fmt.Errorf("optional-type walk references invalid node %d", nodeID)
	}
	if node.Kind == tir.SomeOptional {
		if isOptional(snapshot, node.Type) {
			*out = append(*out, node.Type)
		}
	}
	if node.Kind == tir.Initialize {
		for _, childID := range node.Children {
			if child, ok := unit.Node(childID); ok && isOptional(snapshot, child.Type) {
				*out = append(*out, child.Type)
			}
		}
	}
	if node.Kind == tir.RecordConstruct {
		for _, field := range node.Fields {
			if value, ok := unit.Node(field.Value); ok && isOptional(snapshot, value.Type) {
				*out = append(*out, value.Type)
			}
		}
	}
	for _, childID := range node.Children {
		if err := collectOptionalTypesWalk(unit, snapshot, childID, out); err != nil {
			return err
		}
	}
	for _, deferID := range node.DeferChain {
		if err := collectOptionalTypesWalk(unit, snapshot, deferID, out); err != nil {
			return err
		}
	}
	return nil
}

// collectSliceTypes resolves, in first-encountered order, every slice type
// the emitted program actually references: the entry body (root) followed by
// every reachable helper's body, each walked by the same Children +
// DeferChain traversal collectDirectCalls uses. A slice type is referenced
// by exactly two node shapes — a CheckedSlice node (a slice expression
// whose Type is the slice type) and an Initialize whose initializer value
// carries a slice type (a slice-typed local declaration) — so collecting
// exactly those shapes guarantees every typedef the program needs is
// discovered. The returned sliceInfos are deduplicated by slice TypeID, so
// every distinct slice type yields exactly one typedef, emitted before any
// function definition in the final output.
func collectSliceTypes(unit *tir.Unit, snapshot *types.Snapshot, entryBlockID tir.NodeID, helpers []helperInfo) ([]sliceInfo, error) {
	var collected []types.TypeID
	if err := collectSliceTypesWalk(unit, snapshot, entryBlockID, &collected); err != nil {
		return nil, err
	}
	for _, helper := range helpers {
		if err := collectSliceTypesWalk(unit, snapshot, helper.block, &collected); err != nil {
			return nil, err
		}
		// A reachable helper's own parameter list is a source of slice types
		// the body walk cannot see: a slice-typed parameter is referenced by
		// the helper's C signature even if no reachable body ever constructs a
		// slice of that type (its slice values arrive as already-built
		// forwards through the call boundary), so its typedef must be
		// discovered here too — the same Parameters scan 10.24 added for
		// tuples/structs.
		for _, param := range helper.decl.Parameters {
			if isSlice(snapshot, param.Type) {
				collected = append(collected, param.Type)
			}
		}
		// A reachable helper's own result type is the same kind of source for
		// the typedef its C signature names as its return type (10.26): a
		// slice-returning helper's C signature declares
		// pebble_slice_<typeID>_t, so a slice type that appears nowhere in any
		// reachable body still needs its typedef emitted. (For a reachable
		// slice-returning helper the body walk usually finds the type anyway,
		// since the helper must produce a slice to return — a CheckedSlice
		// construction or a forward of a slice-typed local/parameter whose own
		// construction lives elsewhere in the reachable program; this scan
		// closes the same class of gap 10.24's Parameters scan closed, for the
		// return side.)
		if isSlice(snapshot, helper.decl.ResultType) {
			collected = append(collected, helper.decl.ResultType)
		}
	}
	seen := make(map[types.TypeID]bool, len(collected))
	var infos []sliceInfo
	for _, id := range collected {
		if seen[id] {
			continue
		}
		seen[id] = true
		info, err := resolveSliceInfo(snapshot, id)
		if err != nil {
			return nil, err
		}
		infos = append(infos, info)
	}
	return infos, nil
}

// collectSliceTypesWalk appends every slice type encountered in the tree
// rooted at nodeID to out, in first-encountered order, following Children
// and DeferChain exactly like collectDirectCalls so it visits the same
// reachable region of the node graph the body builders consume. Two node
// shapes carry a slice type: a CheckedSlice node's own Type (a slice
// expression), and an Initialize whose initializer value carries a slice
// type (a slice-typed local declaration — the local's type is recorded on
// the initializer value node, not on the Initialize node itself, the same
// pattern every other aggregate collection made).
func collectSliceTypesWalk(unit *tir.Unit, snapshot *types.Snapshot, nodeID tir.NodeID, out *[]types.TypeID) error {
	node, ok := unit.Node(nodeID)
	if !ok {
		return fmt.Errorf("slice-type walk references invalid node %d", nodeID)
	}
	if (node.Kind == tir.CheckedSlice || node.Kind == tir.SliceFromRaw) && isSlice(snapshot, node.Type) {
		*out = append(*out, node.Type)
	}
	if node.Kind == tir.FieldPlace && isSlice(snapshot, node.Type) {
		*out = append(*out, node.Type)
	}
	if node.Kind == tir.Initialize {
		for _, childID := range node.Children {
			if child, ok := unit.Node(childID); ok && isSlice(snapshot, child.Type) {
				*out = append(*out, child.Type)
			}
		}
	}
	for _, childID := range node.Children {
		if err := collectSliceTypesWalk(unit, snapshot, childID, out); err != nil {
			return err
		}
	}
	for _, deferID := range node.DeferChain {
		if err := collectSliceTypesWalk(unit, snapshot, deferID, out); err != nil {
			return err
		}
	}
	return nil
}

// collectFunctionTypes resolves, in first-encountered order, every function
// type the emitted program actually references as a first-class value: the
// entry body (root) followed by every reachable helper's body, each walked by
// the same Children + DeferChain traversal collectDirectCalls uses. A function
// type is referenced by exactly two node shapes — a value node whose own Type
// is the function type (a HoistedFunctionValue, the bare top-level function
// reference that seeds a function-typed local or an indirect call's callee,
// or a function-typed SymbolValue) and an IndirectCall's FunctionType field
// (the callee's own function type, which the general indirect call resolves
// its parameter list from) — so collecting exactly those shapes guarantees
// every fnptr typedef the program needs is discovered. The returned IDs are
// deduplicated by function TypeID, so every distinct function type yields
// exactly one typedef, emitted before any function definition in the final
// output.
func collectFunctionTypes(unit *tir.Unit, snapshot *types.Snapshot, entryBlockID tir.NodeID, helpers []helperInfo) ([]types.TypeID, error) {
	var collected []types.TypeID
	if err := collectFunctionTypesWalk(unit, snapshot, entryBlockID, &collected); err != nil {
		return nil, err
	}
	for _, helper := range helpers {
		if err := collectFunctionTypesWalk(unit, snapshot, helper.block, &collected); err != nil {
			return nil, err
		}
		// A reachable helper's own parameter list is a source of function
		// types the body walk cannot see: a function-typed parameter is
		// referenced by the helper's C signature even if no reachable body
		// ever mentions the parameter (a fn-typed parameter never read in the
		// body, or forwarded only through a call whose own FunctionType is a
		// DIFFERENT fn type), so its typedef must be discovered here too —
		// mirroring collectTupleTypes/collectStructTypes' identical
		// Parameters scan.
		for _, param := range helper.decl.Parameters {
			if isFunctionType(snapshot, param.Type) {
				collected = append(collected, param.Type)
			}
		}
		// A reachable helper's own result type is the same kind of source for
		// the typedef its C signature names as its return type: a
		// function-returning helper's C signature declares
		// pebble_fnptr_<typeID>_t, so a function type that appears nowhere in
		// any reachable body still needs its typedef emitted. (For a reachable
		// function-returning helper the body walk usually finds the type
		// anyway, since the helper must produce a function value to return;
		// this scan closes the same class of gap the Parameters scan above
		// closes, for the return side.)
		if isFunctionType(snapshot, helper.decl.ResultType) {
			collected = append(collected, helper.decl.ResultType)
		}
	}
	seen := make(map[types.TypeID]bool, len(collected))
	var deduplicated []types.TypeID
	for _, id := range collected {
		if seen[id] {
			continue
		}
		seen[id] = true
		deduplicated = append(deduplicated, id)
	}
	return deduplicated, nil
}

// collectFunctionTypesWalk appends every function type encountered in the tree
// rooted at nodeID to out, in first-encountered order, following Children and
// DeferChain exactly like collectDirectCalls so it visits the same reachable
// region of the node graph the body builders consume. Two node KINDS carry a
// first-class function type: a HoistedFunctionValue (a bare top-level
// function reference) and a function-typed SymbolValue (a reference to an
// in-scope function-typed local) — both confirmed the only two shapes
// buildFunctionValue handles. This is deliberately narrower than "any node
// whose own Type is a function type": the built-in Allocator's alloc/
// realloc/free fields are ALSO function-typed (a FieldValue/FieldPlace node
// accessing them has Type = fn(*void, uint) *void), but they are read
// through the allocator-specific indirect-call path (buildIndirectCall's
// allocatorCallee branch) using the runtime's own pre-existing
// PebbleAllocFn/PebbleReallocFn/PebbleFreeFn typedefs, never this general
// pebble_fnptr_<typeID>_t mechanism — collecting them here would wrongly
// demand a general function typedef for the runtime's own callback ABI
// (PebbleAllocFn/PebbleReallocFn/PebbleFreeFn), which is out of scope here,
// breaking every allocator call in the program. An IndirectCall's own FunctionType field is collected too, but
// only for the GENERAL case (an allocator IndirectCall never sets
// FunctionType, confirmed against a real fixture, so the != 0 guard already
// excludes it; the explicit node-kind restriction above is the primary,
// intentional guard).
func collectFunctionTypesWalk(unit *tir.Unit, snapshot *types.Snapshot, nodeID tir.NodeID, out *[]types.TypeID) error {
	node, ok := unit.Node(nodeID)
	if !ok {
		return fmt.Errorf("function-type walk references invalid node %d", nodeID)
	}
	if (node.Kind == tir.HoistedFunctionValue || node.Kind == tir.SymbolValue || node.Kind == tir.GenericFunctionValue) && isFunctionType(snapshot, node.Type) {
		*out = append(*out, node.Type)
	}
	if node.Kind == tir.RecordConstruct {
		// A struct construction's field values (`Table.{ op = add }`,
		// function-types slice 2) are NOT part of node.Children — they are
		// stored separately in node.Fields ([]FieldInit{Field, Value}),
		// confirmed by reading collectStructTypesWalk's identical need to
		// special-case RecordConstruct for the same reason — so a function
		// -typed field's value (a HoistedFunctionValue/SymbolValue the
		// Children-following recursion below would otherwise never reach) is
		// walked explicitly here.
		for _, field := range node.Fields {
			if err := collectFunctionTypesWalk(unit, snapshot, field.Value, out); err != nil {
				return err
			}
		}
	}
	if node.Kind == tir.IndirectCall && node.FunctionType != 0 && isFunctionType(snapshot, node.FunctionType) {
		// The checker sets IndirectCall.FunctionType on BOTH the allocator's
		// built-in indirect call and the general case (confirmed via a real
		// fixture — an earlier version of this check assumed it was set only
		// for the general case, which was wrong and caused every allocator
		// call in the program to be misidentified as needing a general
		// pebble_fnptr_<typeID>_t typedef for the allocator's own *void
		// -parameter runtime-callback ABI, which this backend does not emit
		// general function typedefs for, breaking every allocator call).
		// indirectCalleePlace is the
		// single shared signal (used by buildIndirectCall too) that actually
		// distinguishes the two: the allocator case's callee unwraps to a
		// FieldPlace/FieldValue, which is excluded here.
		if _, isAllocator, ok := indirectCalleePlace(unit, node); ok && !isAllocator {
			*out = append(*out, node.FunctionType)
		}
	}
	for _, childID := range node.Children {
		if err := collectFunctionTypesWalk(unit, snapshot, childID, out); err != nil {
			return err
		}
	}
	for _, deferID := range node.DeferChain {
		if err := collectFunctionTypesWalk(unit, snapshot, deferID, out); err != nil {
			return err
		}
	}
	return nil
}

// collectStructTypes resolves, in first-encountered order, every struct type
// the emitted program actually references: the entry body (root) followed by
// every reachable helper's body, each walked by the same Children + DeferChain
// traversal collectDirectCalls uses. A struct type is referenced in exactly
// five places in the emitted C — a struct-typed local's declaration (an
// Initialize whose initializer value carries the struct type), a struct
// construction (a RecordConstruct, whose Type is the struct type), a
// struct-typed parameter of a reachable helper (a FunctionDeclaration.Parameters
// entry's Type), a struct-typed result of a reachable helper (a
// FunctionDeclaration.ResultType, whose typedef its C signature names as its
// return type), and a bare `sizeof Pair` expression (a SizeofType node whose
// TypeArg is the struct type — with no other reference to the type anywhere
// the walk must still collect it, mirroring collectUnionTypesWalk and
// collectArrayTypesWalk) — so collecting exactly those node shapes, each
// reachable helper's Parameters list, and each reachable helper's ResultType
// guarantees every typedef the program needs is discovered. The Parameters/ResultType
// coverage closes a real gap: a struct type used only as a parameter type or
// only as a helper's result type (never constructed in any reachable body)
// still needs its typedef emitted, since the helper's C signature names
// pebble_struct_<typeID>_t. The walk also accumulates each field's resolved
// type from the same nodes (a RecordConstruct field value's own type, and a
// FieldPlace's Type), since the FieldDeclaration nodes in the unit carry only
// the field's symbol, never its type (confirmed against a real fixture — a
// further lookup is required, the same kind of confirmation 10.18 did for
// FunctionDeclaration.Parameters). The returned structInfos are deduplicated
// by struct TypeID and each resolved to its declared field order, so every
// distinct struct type yields exactly one typedef, emitted before any function
// definition in the final output.
func collectStructTypes(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, entryBlockID tir.NodeID, helpers []helperInfo, optionalTypes []types.TypeID) ([]structInfo, error) {
	fieldTypes := make(map[symbol.SymbolID]types.TypeID)
	var collected []types.TypeID
	if err := collectStructTypesWalk(unit, snapshot, entryBlockID, &collected, fieldTypes); err != nil {
		return nil, err
	}
	// An optional type's payload is a source of struct types the body/helper
	// walks cannot see: `var o ?P = none;` never constructs a P anywhere, so
	// no FieldPlace/RecordConstruct evidence for it exists, but the
	// optional's own C struct typedef still names P's typedef as its .value
	// field type (optionalPayloadCType) and P's typedef must exist for that
	// to compile — mirroring the Parameters/ResultType scans below, for the
	// optional-payload source specifically. optionalTypes is the caller's
	// already-collected list of every optional type reachable in the
	// program (collectOptionalTypes runs first — see Emit).
	for _, optionalType := range optionalTypes {
		key, ok := snapshot.Key(optionalType)
		if !ok || key.Kind() != types.Optional {
			continue
		}
		payload, ok := key.Child()
		if !ok {
			continue
		}
		if isStruct(snapshot, payload) && !isEnumType(unit, snapshot, payload) {
			collected = append(collected, payload)
		}
	}
	for _, helper := range helpers {
		if err := collectStructTypesWalk(unit, snapshot, helper.block, &collected, fieldTypes); err != nil {
			return nil, err
		}
		// A reachable helper's own parameter list is a source of struct types
		// the body walk cannot see: a struct-typed parameter is referenced by
		// the helper's C signature even if no reachable body ever constructs a
		// struct of that type, so its typedef must be discovered here too.
		for _, param := range helper.decl.Parameters {
			if isStruct(snapshot, param.Type) && runtimeType(unit, snapshot, param.Type) == 0 && !isDefinitelyEnumType(unit, snapshot, param.Type) && !isOpaqueExternType(st, snapshot, param.Type) {
				collected = append(collected, param.Type)
			}
			// A pointer-typed parameter whose pointee is a struct (including
			// a pointer-receiver method's self parameter) references the
			// pointee's typedef in its own C signature, the same reason a
			// plain struct parameter does above.
			if isPointer(snapshot, param.Type) {
				if pointee, ok := pointerPointeeType(snapshot, param.Type); ok && isStruct(snapshot, pointee) && runtimeType(unit, snapshot, pointee) == 0 && !isDefinitelyEnumType(unit, snapshot, pointee) && !isOpaqueExternType(st, snapshot, pointee) {
					collected = append(collected, pointee)
				}
			}
		}
		// A reachable helper's own result type is the same kind of source for
		// the typedef its C signature names as its return type (10.26): a
		// struct-returning helper's C signature declares
		// pebble_struct_<typeID>_t, so a struct type that appears nowhere in
		// any reachable body still needs its typedef emitted, mirroring the
		// Parameters scan above. (For a reachable struct-returning helper the
		// body walk usually finds the type anyway, since the helper must
		// produce a struct to return — and resolveStructInfo still needs the
		// field types the body walk accumulates — so this closes the same class
		// of gap 10.24's Parameters scan closed, for the return side.)
		if isStruct(snapshot, helper.decl.ResultType) && runtimeType(unit, snapshot, helper.decl.ResultType) == 0 && !isDefinitelyEnumType(unit, snapshot, helper.decl.ResultType) && !isOpaqueExternType(st, snapshot, helper.decl.ResultType) {
			collected = append(collected, helper.decl.ResultType)
		}
		if isPointer(snapshot, helper.decl.ResultType) {
			if pointee, ok := pointerPointeeType(snapshot, helper.decl.ResultType); ok && isStruct(snapshot, pointee) && runtimeType(unit, snapshot, pointee) == 0 && !isDefinitelyEnumType(unit, snapshot, pointee) && !isOpaqueExternType(st, snapshot, pointee) {
				collected = append(collected, pointee)
			}
		}
	}
	seen := make(map[types.TypeID]bool, len(collected))
	var infos []structInfo
	for _, id := range collected {
		if seen[id] {
			continue
		}
		seen[id] = true
		info, err := resolveStructInfo(unit, snapshot, id, fieldTypes)
		if err != nil {
			return nil, err
		}
		infos = append(infos, info)
	}
	return infos, nil
}

// collectStructTypesWalk appends every struct type encountered in the tree
// rooted at nodeID to out, in first-encountered order, following Children and
// DeferChain exactly like collectDirectCalls so it visits the same reachable
// region of the node graph the body builders consume. Three node shapes carry
// a struct type: a RecordConstruct node's own Type, an Initialize whose
// initializer value carries a struct type (a struct-typed local declaration —
// the local's type is recorded on the initializer value node, not on the
// Initialize node itself, confirmed against a real fixture, the same finding
// tuple/array/optional collection made), and a SizeofType node whose TypeArg
// is a struct type (a bare `sizeof Pair` with no other reference to the type
// anywhere — no construction, field access, local, or helper signature — so
// only the SizeofType node carries it, the same SizeofType collection gap
// collectUnionTypesWalk and collectArrayTypesWalk close for their own kinds).
// A RecordConstruct whose own type is a
// compiler-builtin runtime type (Allocator, Context) is excluded — its C type
// is the hand-written PebbleAllocator / PebbleContext (never a per-TypeID
// pebble_struct_<id>_t typedef, which this pass exists to emit), and every
// value position names that hand-written type, so collecting it would only
// emit a dead, ABI-misleading typedef. The same walk also records, in
// fieldTypes, every field symbol's resolved type from exactly the two nodes
// that carry it: a RecordConstruct field value node's own Type, and a
// FieldPlace node's Type — the only in-unit sources of a field's type, since
// the FieldDeclaration node carries no type.
func collectStructTypesWalk(unit *tir.Unit, snapshot *types.Snapshot, nodeID tir.NodeID, out *[]types.TypeID, fieldTypes map[symbol.SymbolID]types.TypeID) error {
	node, ok := unit.Node(nodeID)
	if !ok {
		return fmt.Errorf("struct-type walk references invalid node %d", nodeID)
	}
	if node.Kind == tir.SizeofType && isStruct(snapshot, node.TypeArg) && runtimeType(unit, snapshot, node.TypeArg) == 0 && !isEnumType(unit, snapshot, node.TypeArg) {
		// A bare `sizeof` of a plain struct (no construction, field access,
		// local declaration, or helper signature anywhere, so no other node
		// carries the type): the struct's pebble_struct_<typeID>_t typedef
		// must still be collected and emitted, or the lowered
		// sizeof(pebble_struct_<typeID>_t) names an undeclared C type (see
		// sizeofCTypeName). The same guard every other struct-collection shape
		// uses applies here: a compiler-builtin runtime type (Allocator,
		// Context) is sized through its hand-written C type (PebbleAllocator /
		// PebbleContext, see sizeofCTypeName), never a per-TypeID struct
		// typedef, and an enum-shaped type (a plain enum or tagged union)
		// is collected by collectEnumTypes/collectUnionTypes instead.
		*out = append(*out, node.TypeArg)
	}
	if node.Kind == tir.RecordConstruct {
		// A compiler-builtin runtime type (Allocator, Context) is Nominal like
		// a struct but its C representation is the hand-written runtime ABI
		// type PebbleAllocator / PebbleContext, never a per-TypeID
		// pebble_struct_<id>_t typedef: every Allocator value position — a
		// local, a parameter, a return, a call argument, a struct field — names
		// that hand-written type via runtimeTypeName, and a construction is
		// emitted by buildRuntimeAllocatorBraceList (or the same ABI shape in
		// buildStructValueExpr), so collecting it here would only emit a dead
		// typedef that mirrors neither the real PebbleAllocator layout nor
		// anything the emitted C references. A source-level Allocator literal
		// lowers to a RecordConstruct (e.g. std/alloc.peb's Allocator.{ ptr,
		// alloc, realloc, free }), so a runtime-typed construction must be
		// excluded from the struct collection here — the same runtimeType guard
		// the Initialize, Parameters, and ResultType collection paths already
		// use. Its field
		// value types are still recorded and its field values still recurse
		// below, so nested ordinary structs inside a runtime construction keep
		// their typedefs.
		if runtimeType(unit, snapshot, node.Type) == 0 {
			*out = append(*out, node.Type)
		}
		for _, field := range node.Fields {
			if value, ok := unit.Node(field.Value); ok && value.Type != 0 {
				fieldTypes[field.Field] = value.Type
			}
			// A construction's field values are stored in node.Fields
			// ([]FieldInit), NOT node.Children, so the Children-following
			// recursion below never reaches a NESTED construction used only as
			// this struct's field value (e.g. `Outer[int].{ inner = Inner[int].{
			// val = 5 } }` — the Inner[int] RecordConstruct lives at
			// Fields[0].Value). Without this recursive walk the nested struct
			// type is never collected, so no C typedef is emitted for it and
			// the outer struct's field read resolves to an unresolved TypeID —
			// the same "Fields isn't in Children" gap collectFunctionTypesWalk
			// (0b6ed32) and collectOptionalTypesWalk (8c339d3) already close.
			if err := collectStructTypesWalk(unit, snapshot, field.Value, out, fieldTypes); err != nil {
				return err
			}
		}
	}
	if node.Kind == tir.FieldPlace {
		if node.Member != 0 && node.Type != 0 {
			fieldTypes[node.Member] = node.Type
		}
	}
	if node.Kind == tir.Initialize {
		for _, childID := range node.Children {
			// A struct-typed local's initializer carries the struct type; an
			// enum-typed local's initializer also carries a Nominal type (a
			// plain enum is Nominal exactly like a struct — see isEnumType),
			// so an enum child must be excluded here or it would be collected
			// as a struct and resolveStructInfo would fail trying to resolve
			// its members as fields. Enums are collected by
			// collectEnumTypes instead.
			if child, ok := unit.Node(childID); ok && isStruct(snapshot, child.Type) && runtimeType(unit, snapshot, child.Type) == 0 && !isEnumType(unit, snapshot, child.Type) {
				*out = append(*out, child.Type)
			}
			// A pointer-typed local whose pointee is a struct (`let p *Point =
			// ...;`) references the pointee's typedef in its own C
			// declaration (`pebble_struct_<id>_t *`), even though the local's
			// own Type is the pointer type, not the struct type — the body
			// walk above only ever inspects a node's own Type, so this case
			// is collected separately here.
			if child, ok := unit.Node(childID); ok && isPointer(snapshot, child.Type) {
				if pointee, ok := pointerPointeeType(snapshot, child.Type); ok && isStruct(snapshot, pointee) && runtimeType(unit, snapshot, pointee) == 0 && !isEnumType(unit, snapshot, pointee) {
					*out = append(*out, pointee)
				}
			}
		}
	}
	for _, childID := range node.Children {
		if err := collectStructTypesWalk(unit, snapshot, childID, out, fieldTypes); err != nil {
			return err
		}
	}
	for _, deferID := range node.DeferChain {
		if err := collectStructTypesWalk(unit, snapshot, deferID, out, fieldTypes); err != nil {
			return err
		}
	}
	return nil
}

// findTypeDeclaration locates the TypeDecl container (its ordered Members list
// names the struct's declared fields) for a type declaration symbol. The
// TypeDeclaration *node* in the unit carries only the Symbol — its field list
// is on the TypeDecl container the builder published alongside it (the same
// division the unit makes between FunctionDeclaration nodes and FunctionDecl
// containers), so the container is the authoritative declared-field-order
// source.
func findTypeDeclaration(unit *tir.Unit, symbolID symbol.SymbolID) (tir.TypeDecl, bool) {
	for _, td := range unit.TypeDeclarations() {
		if td.Symbol == symbolID {
			return td, true
		}
	}
	return tir.TypeDecl{}, false
}

// collectEnumTypes resolves, in first-encountered order, every plain enum type
// the emitted program actually references: the entry body (root) followed by
// every reachable helper's body, each walked by the same Children + DeferChain
// traversal collectDirectCalls uses. A plain enum type is referenced in the
// emitted C in exactly two node shapes — an enum-typed local's declaration
// (an Initialize whose initializer value is an EnumVariantValue or a
// zero-payload VariantConstruct carrying the enum type) and a bare
// EnumVariantValue / VariantConstruct value node (whose own Type is the enum
// type) — so collecting exactly those shapes guarantees every typedef the
// program needs is discovered, exactly like collectTupleTypes /
// collectStructTypes discover their types from construction nodes. A
// VariantConstruct carrying one or more payload children is the only
// in-IR record of a variant with a non-void payload type — the tagged-union
// (union enum) form — and since 10.35 that is handled by collectUnionTypes
// instead: this pass excludes every such type (unions, threaded in as the
// caller's union map) so a tagged union is never emitted as a plain enum
// typedef (its discriminant enum typedef is emitted as the tag of its tagged
// struct instead, see buildUnionTypedef). The returned enumInfos are
// deduplicated by enum TypeID and each resolved to its declared variant order,
// so every distinct plain enum type yields exactly one typedef, emitted before
// any function definition in the final output. Enum-typed helper
// parameters/results are rejected earlier by validateHelperSignature (before a
// reachable helper is ever collected), so no Parameters/ResultType scan is
// needed here, mirroring how those two scans exist only to close struct/tuple
// param-result gaps.
func collectEnumTypes(unit *tir.Unit, snapshot *types.Snapshot, entryBlockID tir.NodeID, helpers []helperInfo, unions map[types.TypeID]unionInfo) ([]enumInfo, error) {
	var collected []types.TypeID
	if err := collectEnumTypesWalk(unit, snapshot, entryBlockID, &collected); err != nil {
		return nil, err
	}
	for _, helper := range helpers {
		if err := collectEnumTypesWalk(unit, snapshot, helper.block, &collected); err != nil {
			return nil, err
		}
	}
	seen := make(map[types.TypeID]bool, len(collected))
	var infos []enumInfo
	for _, id := range collected {
		if seen[id] {
			continue
		}
		seen[id] = true
		if _, isUnion := unions[id]; isUnion {
			// A tagged-union type is collected by this walk exactly like a
			// plain enum (its variants are enum-shaped — isEnumType returns
			// true for it), but it is not a plain enum: its discriminant enum
			// typedef is emitted as the tag field of its tagged struct (see
			// buildUnionTypedef), so it must be excluded from the plain-enum
			// typedef list or the same pebble_enum_<typeID>_t typedef would be
			// emitted twice.
			continue
		}
		info, err := resolveEnumInfo(unit, snapshot, id)
		if err != nil {
			return nil, err
		}
		infos = append(infos, info)
	}
	return infos, nil
}

// collectEnumTypesWalk appends every enum type encountered in the tree
// rooted at nodeID to out, in first-encountered order, following Children and
// DeferChain exactly like collectDirectCalls so it visits the same reachable
// region of the node graph the body builders consume. Five node shapes carry
// an enum type: an EnumVariantValue node's own Type (a variant literal,
// e.g. Color.green), a VariantConstruct node's own Type (a variant
// construction, e.g. Color.red() — the parenthesized-call form of a plain
// enum's payload-less variant, or e.g. Choice.value(5) — a tagged-union
// payload-carrying construction, which this walk collects exactly the same way
// and the caller filters out as a tagged union; see collectUnionTypes), a
// CheckedIntegerToEnum node's own Type (an integer cast to an enum, e.g.
// `5 as Color` — the node's Type is the destination enum type, so a cast that
// never participates in a local declaration still gets its typedef emitted),
// an OptionalIntegerToEnum node's payload type (an integer cast to an optional
// enum, e.g. `5 as ?Color` — the node's own Type is the OPTIONAL type, so the
// destination enum is its payload), and a SizeofType node whose TypeArg is an
// enum type (a bare `sizeof Color` with no other reference to the type
// anywhere, so only the SizeofType node carries it — the same SizeofType
// collection gap collectUnionTypesWalk and collectArrayTypesWalk close for
// their own kinds), and an
// Initialize whose initializer value carries an enum type (an enum-typed local
// declaration — the local's type is recorded on the initializer value node,
// not on the Initialize node itself, confirmed against a real fixture, the
// same finding every aggregate collection made). The Initialize rule also
// collects an enum type used as a local's declared type with a rejected
// initializer shape (a whole-copy of another enum local), so its typedef is
// still emitted before the builder rejects the initializer — mirroring
// collectTupleTypesWalk's own rule. A payload-carrying VariantConstruct is no
// longer rejected here (10.35): it is the tagged-union construction, collected
// by collectUnionTypes, and the caller filters the type out of the plain-enum
// results.
func collectEnumTypesWalk(unit *tir.Unit, snapshot *types.Snapshot, nodeID tir.NodeID, out *[]types.TypeID) error {
	node, ok := unit.Node(nodeID)
	if !ok {
		return fmt.Errorf("enum-type walk references invalid node %d", nodeID)
	}
	if node.Kind == tir.SizeofType && isEnumType(unit, snapshot, node.TypeArg) {
		// A bare `sizeof` of a plain enum (no variant literal, construction,
		// cast, or local declaration anywhere, so no other node carries the
		// type): the enum's pebble_enum_<typeID>_t typedef must still be
		// collected and emitted, or the lowered sizeof(pebble_enum_<typeID>_t)
		// names an undeclared C type (see sizeofCTypeName). The caller's
		// tagged-union filter applies the same way it does to the
		// variant-shape rules above — a union enum is enum-shaped too, but is
		// not a plain enum, and collectUnionTypesWalk collects it for the
		// union typedef pair.
		*out = append(*out, node.TypeArg)
	}
	if node.Kind == tir.EnumVariantValue {
		*out = append(*out, node.Type)
	}
	if node.Kind == tir.VariantConstruct {
		*out = append(*out, node.Type)
	}
	if node.Kind == tir.CheckedIntegerToEnum {
		// An integer cast to an enum (`5 as Color`): the node's own Type IS
		// the destination enum type, collected exactly like an
		// EnumVariantValue's — a cast that is never part of a local
		// declaration (e.g. a store's new value or a switch subject standing
		// alone) would otherwise leave the enum typedef unemitted. The
		// caller's tagged-union filter applies the same way it does to the
		// variant-shape rules above (a checked cast can only target a plain
		// enum in real source — the checker routes an integer to a
		// NominalEnum destination only — but the filter keeps the invariant
		// uniform).
		*out = append(*out, node.Type)
	}
	if node.Kind == tir.OptionalIntegerToEnum {
		// An integer cast to an optional enum (`5 as ?Color`): unlike the
		// checked cast, the node's own Type is the OPTIONAL type, not the
		// enum, so the destination enum is the optional's payload — resolved
		// here so the optional typedef's value field
		// (pebble_enum_<typeID>_t, see optionalPayloadCType) always has its
		// enum typedef emitted ahead of it.
		if key, ok := snapshot.Key(node.Type); ok && key.Kind() == types.Optional {
			if child, ok := key.Child(); ok && isEnumType(unit, snapshot, child) {
				*out = append(*out, child)
			}
		}
	}
	if node.Kind == tir.Initialize {
		for _, childID := range node.Children {
			if child, ok := unit.Node(childID); ok && isEnumType(unit, snapshot, child.Type) {
				*out = append(*out, child.Type)
			}
		}
	}
	for _, childID := range node.Children {
		if err := collectEnumTypesWalk(unit, snapshot, childID, out); err != nil {
			return err
		}
	}
	for _, deferID := range node.DeferChain {
		if err := collectEnumTypesWalk(unit, snapshot, deferID, out); err != nil {
			return err
		}
	}
	return nil
}

// collectUnionTypes resolves, in first-encountered order, every tagged-union
// type the emitted program actually references: the entry body (root) followed
// by every reachable helper's body, each walked by the same Children +
// DeferChain traversal collectDirectCalls uses. A tagged-union type is
// referenced by exactly one node shape — a payload-carrying VariantConstruct
// (e.g. Choice.value(5), a VariantConstruct whose Type is the union's own
// TypeID and whose Children are the payload expression(s); a plain enum's
// variants are payload-less, and the checker rejects calling one with an
// argument, C0604, so a payload-carrying VariantConstruct can only be a union
// enum). Each constructed variant's payload type is resolved from its own
// construction site's payload child Type (the checker anchors every
// construction of a variant to its one declared payload type, so all sites of
// a variant agree — confirmed against real fixtures at three payload shapes),
// and must be exactly the entry's resolved width, bool, or str — a tuple/
// struct/array/optional/nested-enum payload is a clean rejection naming what
// is unsupported, never guessed at, enforced here in the collection walk where
// each variant's payload type is first resolved. width is threaded so the
// payload gate can be enforced against the entry's own width. The returned
// unionInfos are deduplicated by union TypeID and each resolved to its
// declared variant order plus its constructed members, so every distinct union
// type yields exactly one tagged struct typedef (plus its tag enum typedef),
// emitted before any function definition in the final output.
func collectUnionTypes(unit *tir.Unit, snapshot *types.Snapshot, width types.BuiltinKind, entryBlockID tir.NodeID, helpers []helperInfo) ([]unionInfo, error) {
	payloads := make(map[types.TypeID]map[symbol.SymbolID]types.TypeID)
	var collected []types.TypeID
	if err := collectUnionTypesWalk(unit, snapshot, width, entryBlockID, &collected, payloads); err != nil {
		return nil, err
	}
	for _, helper := range helpers {
		if err := collectUnionTypesWalk(unit, snapshot, width, helper.block, &collected, payloads); err != nil {
			return nil, err
		}
	}
	seen := make(map[types.TypeID]bool, len(collected))
	var infos []unionInfo
	for _, id := range collected {
		if seen[id] {
			continue
		}
		seen[id] = true
		info, err := resolveUnionInfo(unit, snapshot, id, payloads[id])
		if err != nil {
			return nil, err
		}
		infos = append(infos, info)
	}
	return infos, nil
}

// collectUnionTypesWalk appends every tagged-union type encountered in the
// tree rooted at nodeID to out, in first-encountered order, following Children
// and DeferChain exactly like collectDirectCalls so it visits the same
// reachable region of the node graph the body builders consume. Two node
// shapes carry a tagged-union type. The primary one is a VariantConstruct
// with one or more children: its Type is the union's own TypeID, its Member
// the variant symbol, and each child the payload expression whose own Type is
// the variant's declared payload type (confirmed against real fixtures). The
// walk records node.Type as a union type and, for each construction, the
// payload type under the variant's member symbol; a second construction of the
// same variant must carry the same payload type (the checker enforces one
// declared type per variant, so this is guaranteed for real source; a mismatch
// is a clean rejection for hand-built IR, never a guessed layout). The payload
// type is gated here — it must be exactly the entry's resolved width, bool, or
// str, since this backend emits exactly those three C types as union members;
// any other payload (a tuple/struct/array/optional/nested-enum) is a clean
// rejection naming what is unsupported. The second shape is a SizeofType node
// whose TypeArg is a union enum: a bare `sizeof Choice` references the union
// type with no construction anywhere, so only the SizeofType node carries it —
// and without this case the union's typedef pair would never be emitted and
// the lowered sizeof would name an undeclared C type (see sizeofCTypeName).
func collectUnionTypesWalk(unit *tir.Unit, snapshot *types.Snapshot, width types.BuiltinKind, nodeID tir.NodeID, out *[]types.TypeID, payloads map[types.TypeID]map[symbol.SymbolID]types.TypeID) error {
	node, ok := unit.Node(nodeID)
	if !ok {
		return fmt.Errorf("union-type walk references invalid node %d", nodeID)
	}
	if node.Kind == tir.SizeofType && isUnionEnumType(unit, snapshot, node.TypeArg) {
		// A bare `sizeof` of a tagged union (no construction anywhere, so no
		// VariantConstruct ever carries the type): the union's typedef pair
		// must still be collected and emitted, or the lowered
		// sizeof(pebble_union_<typeID>_t) names an undeclared C type. The
		// declaration-level isUnionEnumType test is what recognizes the type
		// here — the construction-based isTaggedUnionType would miss it.
		*out = append(*out, node.TypeArg)
	}
	if node.Kind == tir.VariantConstruct && len(node.Children) >= 1 {
		// A payload-carrying variant construction. node.Type is the union's
		// own type (the 7feaf0c checker fix publishes the variant's term as the
		// union type — confirmed against a real fixture: the VariantConstruct's
		// Type is the union TypeID, not the payload's type). The payload child
		// node's own Type is the variant's declared payload type, anchored by
		// the checker at every construction site (confirmed against real
		// fixtures at three payload shapes: an i32 literal, a bool literal, and
		// an i32 expression referencing a local).
		if len(node.Children) != 1 {
			return fmt.Errorf("union variant symbol %d is constructed with %d payload(s); a tagged-union variant carries exactly one payload of %s, bool, or str", node.Member, len(node.Children), wantName(width))
		}
		payloadNode, ok := unit.Node(node.Children[0])
		if !ok {
			return fmt.Errorf("union variant symbol %d references invalid payload node %d", node.Member, node.Children[0])
		}
		if !isWidth(snapshot, width, payloadNode.Type) && !isBool(snapshot, payloadNode.Type) && !isStr(snapshot, payloadNode.Type) {
			return fmt.Errorf("union variant symbol %d carries a payload of type %s; only a payload of %s, bool, or str is supported", node.Member, describeType(snapshot, payloadNode.Type), wantName(width))
		}
		byMember, seen := payloads[node.Type]
		if !seen {
			byMember = make(map[symbol.SymbolID]types.TypeID)
			payloads[node.Type] = byMember
		}
		if existing, ok := byMember[node.Member]; ok && existing != payloadNode.Type {
			return fmt.Errorf("union variant symbol %d is constructed with inconsistent payload types %s and %s", node.Member, describeType(snapshot, existing), describeType(snapshot, payloadNode.Type))
		}
		byMember[node.Member] = payloadNode.Type
		*out = append(*out, node.Type)
	}
	if node.Kind == tir.FieldPlace && len(node.Children) == 1 {
		base, ok := unit.Node(node.Children[0])
		ownerType := types.TypeID(0)
		if ok {
			ownerType = base.Type
			if pointee, pointer := pointerPointeeType(snapshot, ownerType); pointer {
				ownerType = pointee
			}
		}
		if ok && isUnionEnumType(unit, snapshot, ownerType) && node.Type != 0 && !isVoid(snapshot, node.Type) {
			byMember, seen := payloads[ownerType]
			if !seen {
				byMember = make(map[symbol.SymbolID]types.TypeID)
				payloads[ownerType] = byMember
			}
			byMember[node.Member] = node.Type
			*out = append(*out, ownerType)
		}
	}
	for _, childID := range node.Children {
		if err := collectUnionTypesWalk(unit, snapshot, width, childID, out, payloads); err != nil {
			return err
		}
	}
	for _, deferID := range node.DeferChain {
		if err := collectUnionTypesWalk(unit, snapshot, width, deferID, out, payloads); err != nil {
			return err
		}
	}
	return nil
}

// indexOfFunction returns the position of key in keys, or -1 if absent.
func indexOfFunction(keys []helperKey, key helperKey) int {
	for i, candidate := range keys {
		if candidate == key {
			return i
		}
	}
	return -1
}

// collectRuntimeAllocatorAdapters walks the reachable tree (the entry body
// followed by every reachable helper's body) for runtime Allocator
// RecordConstructs and registers, into the package-level st.allocatorAdapters
// map, the C bridge each callback field needs. The walk must run before any
// function body is emitted: a helper body that constructs an Allocator literal
// references its bridges by name in the construction's designated initializers,
// and C requires the bridges' declarations (prototypes) to precede those bodies,
// so the bridges are discovered here and Emit merges their prototypes into the
// helper-prototype pass. buildRuntimeAllocatorRecordDeclaration registers the
// same bridges again, idempotently, while it builds a construction's C text
// (the map is keyed by bridge name), so the two can never diverge on which
// bridge a construction references. The traversal mirrors collectDirectCalls
// (Children + DeferChain, skipping a DeferRegister child at its registration
// position so a deferred construction that never fires is never registered — a
// registered-but-unreferenced static bridge would trip -Wunused-function under
// the mandated -Wall -Wextra -Werror build), so the registered set is exactly
// the set the emitted bodies reference.
func collectRuntimeAllocatorAdapters(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, entryBlockID tir.NodeID, helpers []helperInfo) error {
	var walk func(nodeID tir.NodeID) error
	walk = func(nodeID tir.NodeID) error {
		node, ok := unit.Node(nodeID)
		if !ok {
			return fmt.Errorf("runtime-allocator walk references invalid node %d", nodeID)
		}
		if node.Kind == tir.RecordConstruct && runtimeType(unit, snapshot, node.Type) == symbol.RuntimeAllocator {
			info := unit.Runtime()
			for _, field := range node.Fields {
				switch field.Field {
				case info.AllocatorAlloc, info.AllocatorRealloc, info.AllocatorFree:
					valueNode, ok := unit.Node(field.Value)
					if !ok {
						return fmt.Errorf("runtime-allocator walk references invalid field value node %d", field.Value)
					}
					if _, err := buildRuntimeAllocatorCallbackAdapter(st, unit, snapshot, field.Field, valueNode, "runtime-allocator walk"); err != nil {
						return err
					}
				}
			}
		}
		if node.Kind == tir.RecordConstruct {
			// A construction's field values are stored in node.Fields
			// ([]FieldInit), NOT node.Children, so the Children-following
			// recursion below never reaches a NESTED construction used only as
			// this construction's field value — the same special-case every
			// collect*Types walk makes for this reason.
			for _, field := range node.Fields {
				if err := walk(field.Value); err != nil {
					return err
				}
			}
		}
		for _, childID := range node.Children {
			if child, ok := unit.Node(childID); ok && child.Kind == tir.DeferRegister {
				continue
			}
			if err := walk(childID); err != nil {
				return err
			}
		}
		for _, deferID := range node.DeferChain {
			if err := walk(deferID); err != nil {
				return err
			}
		}
		return nil
	}
	if err := walk(entryBlockID); err != nil {
		return err
	}
	for _, helper := range helpers {
		if err := walk(helper.block); err != nil {
			return err
		}
	}
	return nil
}
