package tir

import (
	"errors"
	"fmt"
	"io"

	"github.com/pepplejoshua/pebble/compiler/internal/source"
)

// Dump writes a deterministic normalized representation of the unit to w,
// bounded by MaxDumpBytes. It returns ErrDumpOverflow if the bound is reached.
func (u *Unit) Dump(w io.Writer) error {
	if u == nil {
		_, err := io.WriteString(w, "unit nil\n")
		return err
	}
	d := newDumper(w, u.config.MaxDumpBytes)
	return d.dumpUnit(u)
}

// countingWriter wraps an io.Writer and tracks bytes written against a limit.
type countingWriter struct {
	w     io.Writer
	limit uint64
	used  uint64
}

func newCountingWriter(w io.Writer, limit uint64) *countingWriter {
	return &countingWriter{w: w, limit: limit}
}

func (c *countingWriter) Write(p []byte) (int, error) {
	if c == nil || c.w == nil {
		return 0, errors.New("nil writer")
	}
	// Check before writing to avoid cutting UTF-8.
	if uint64(len(p)) > c.limit-c.used {
		return 0, ErrDumpOverflow
	}
	n, err := c.w.Write(p)
	c.used += uint64(n)
	return n, err
}

func (c *countingWriter) printf(format string, args ...any) error {
	// We could stream byte-by-byte, but for bounded dumping we preformat and
	// check the whole line/component. This keeps UTF-8 intact.
	s := fmt.Sprintf(format, args...)
	if uint64(len(s)) > c.limit-c.used {
		return ErrDumpOverflow
	}
	_, err := c.Write([]byte(s))
	return err
}

// printSlice writes a space-separated, bracket-enclosed slice incrementally,
// checking the byte budget after each element. This avoids pre-formatting the
// entire slice with Sprintf, which would allocate unboundedly before the
// budget check.
func (d *dumper) printSlice(name string, n int, elem func(i int) string) error {
	// Write " name=["
	if err := d.cw.printf(" %s=[", name); err != nil {
		return err
	}
	for i := 0; i < n; i++ {
		if i > 0 {
			if _, err := d.cw.Write([]byte{' '}); err != nil {
				return err
			}
		}
		if _, err := d.cw.Write([]byte(elem(i))); err != nil {
			return err
		}
	}
	_, err := d.cw.Write([]byte{']'})
	return err
}

type dumper struct {
	cw *countingWriter
}

func newDumper(w io.Writer, limit uint64) *dumper {
	return &dumper{cw: newCountingWriter(w, limit)}
}

func (d *dumper) dumpUnit(u *Unit) error {
	if err := d.cw.printf("unit nodes=%d components=%d snapshot=%d regions=%d functions=%d temps=%d\n",
		len(u.nodes), componentsInUnit(u), u.snapshot.Len(), u.regionCount, len(u.functions), u.tempCount); err != nil {
		return err
	}

	for _, m := range u.modules {
		if err := d.dumpModule(m); err != nil {
			return err
		}
	}
	for _, t := range u.typeDecls {
		if err := d.cw.printf("typedecl symbol=%d node=%d members=%v span=%s\n",
			t.Symbol, t.Node, t.Members, spanString(t.Span)); err != nil {
			return err
		}
	}
	for _, f := range u.functions {
		if err := d.cw.printf("funcdecl symbol=%d fid=%d node=%d span=%s\n",
			f.Symbol, f.FunctionID, f.Node, spanString(f.Span)); err != nil {
			return err
		}
	}
	for _, g := range u.globals {
		if err := d.cw.printf("global symbol=%d type=%d node=%d span=%s\n",
			g.Symbol, g.Type, g.Node, spanString(g.Span)); err != nil {
			return err
		}
	}
	for i, n := range u.nodes {
		if err := d.dumpNode(NodeID(i+1), n, u); err != nil {
			return err
		}
	}
	refs := u.SourceRefs()
	for _, ref := range refs {
		if err := d.cw.printf("sourcemap %d:%d -> %d\n", ref.Module, ref.Node, u.sourceMap[ref]); err != nil {
			return err
		}
	}
	for i, r := range u.requirements {
		if err := d.dumpRequirement(uint32(i), r); err != nil {
			return err
		}
	}
	for i, in := range u.instantiations {
		if err := d.dumpInstantiation(uint32(i), in); err != nil {
			return err
		}
	}
	return nil
}

func (d *dumper) dumpModule(m ModuleDecl) error {
	if err := d.cw.printf("module id=%d package=%q path=%q source=%d span=%s\n",
		m.ID, string(m.Key.Package), string(m.Key.Path), m.Source, spanString(m.Span)); err != nil {
		return err
	}
	for _, imp := range m.Imports {
		if err := d.cw.printf("  import target=%d span=%s\n", imp.Target, spanString(imp.Span)); err != nil {
			return err
		}
	}
	for _, decl := range m.Declarations {
		if err := d.cw.printf("  decl symbol=%d\n", decl); err != nil {
			return err
		}
	}
	return nil
}

func (d *dumper) dumpNode(id NodeID, n Node, u *Unit) error {
	name := n.Kind.String()
	span := spanString(n.Span)
	if n.Origin != (source.Span{}) {
		span = "origin=" + spanString(n.Origin)
		if n.SyntheticRole != "" {
			span += " role=" + n.SyntheticRole
		}
	}
	if err := d.cw.printf("node %d %s type=%d span=%s syntax=%d:%d",
		id, name, n.Type, span, n.Syntax.Module, n.Syntax.Node); err != nil {
		return err
	}

	switch n.Kind {
	case Module:
		if err := d.cw.printf(" symbol=%d", n.Symbol); err != nil {
			return err
		}
	case Import:
		if err := d.cw.printf(" target=%d", n.Symbol); err != nil {
			return err
		}
	case TypeDeclaration:
		if err := d.cw.printf(" symbol=%d", n.Symbol); err != nil {
			return err
		}
	case FieldDeclaration, VariantDeclaration, ParameterDeclaration, TypeParameterDeclaration:
		if err := d.cw.printf(" symbol=%d", n.Symbol); err != nil {
			return err
		}
	case FunctionDeclaration, ExternDeclaration:
		if err := d.cw.printf(" symbol=%d function=%d convention=%d variadic=%t inline=%t hasBody=%t",
			n.Symbol, n.Function, n.Convention, n.Variadic, n.Inline, n.HasBody); err != nil {
			return err
		}
	case GlobalDeclaration, LocalDeclaration:
		if err := d.cw.printf(" symbol=%d", n.Symbol); err != nil {
			return err
		}
	case TypeUse:
		if err := d.cw.printf(" typearg=%d", n.TypeArg); err != nil {
			return err
		}
	case Block:
		if err := d.cw.printf(" region=%d", n.Region); err != nil {
			return err
		}
	case Initialize:
		if err := d.cw.printf(" symbol=%d", n.Symbol); err != nil {
			return err
		}
	case Store:
		// payload printed via children
	case CompoundStore:
		if err := d.cw.printf(" operator=%d", n.Operator); err != nil {
			return err
		}
	case ExpressionStatement:
		// payload printed via children
	case Print:
		// payload printed via children
	case Return, ImplicitReturn:
		if err := d.cw.printf(" function=%d defer=%v", n.Function, n.DeferChain); err != nil {
			return err
		}
	case If, While, RangeLoop, For, Switch, SwitchCase:
		if err := d.cw.printf(" region=%d hasElse=%t rangeInclusive=%t case=%d",
			n.Region, n.HasElse, n.RangeInclusive, n.CaseValue); err != nil {
			return err
		}
	case Break, Continue:
		if err := d.cw.printf(" target=%d defer=%v", n.Target, n.DeferChain); err != nil {
			return err
		}
	case DeferRegister:
		if err := d.cw.printf(" region=%d", n.Region); err != nil {
			return err
		}
	case BoolLiteral:
		if err := d.cw.printf(" bool=%t", n.Literal.Bool); err != nil {
			return err
		}
	case CharLiteral:
		if err := d.cw.printf(" char=%q", n.Literal.Char); err != nil {
			return err
		}
	case StringLiteral:
		if err := d.cw.printf(" string=%q", n.Literal.String); err != nil {
			return err
		}
	case IntegerLiteral:
		if err := d.cw.printf(" int=%s/%s", n.Literal.IntegerNum, n.Literal.IntegerDen); err != nil {
			return err
		}
	case FloatLiteral:
		if err := d.cw.printf(" float=%q", n.Literal.Float); err != nil {
			return err
		}
	case NilPointer:
		// no additional payload
	case NoneOptional:
		// no additional payload
	case SomeOptional:
		// payload printed via children
	case TupleValue:
		// payload printed via children
	case ArrayValue:
		// payload printed via children
	case ArrayRepeat:
		// payload printed via children
	case RecordConstruct:
		if err := d.cw.printf(" symbol=%d fields=%d", n.Symbol, len(n.Fields)); err != nil {
			return err
		}
	case HoistedFunctionValue:
		if err := d.cw.printf(" symbol=%d function=%d", n.Symbol, n.Function); err != nil {
			return err
		}
	case SymbolValue:
		if err := d.cw.printf(" symbol=%d", n.Symbol); err != nil {
			return err
		}
	case EnumVariantValue:
		if err := d.cw.printf(" member=%d", n.Member); err != nil {
			return err
		}
	case ContextValue:
		if err := d.cw.printf(" context=%s", n.ContextAction.String()); err != nil {
			return err
		}
	case InterpolatedString:
		if err := d.printSlice("parts", len(n.Parts), func(i int) string {
			part := n.Parts[i]
			if part.Kind == InterpolationTextPart {
				return fmt.Sprintf("text=%q", part.Text)
			}
			return fmt.Sprintf("value=%d", part.Value)
		}); err != nil {
			return err
		}
	case SizeofType:
		if err := d.cw.printf(" typearg=%d", n.TypeArg); err != nil {
			return err
		}
	case PrefixValue, BinaryValue, CheckedArithmetic, CheckedNegate, CheckedShift:
		if err := d.cw.printf(" operator=%d", n.Operator); err != nil {
			return err
		}
	case ShortCircuitValue:
		if err := d.cw.printf(" operator=%d", n.Operator); err != nil {
			return err
		}
	case FieldValue, FieldPlace:
		if err := d.cw.printf(" member=%d", n.Member); err != nil {
			return err
		}
	case TupleElementValue, TuplePlace:
		if err := d.cw.printf(" ordinal=%d", n.Ordinal); err != nil {
			return err
		}
	case GenericFunctionValue:
		if err := d.cw.printf(" symbol=%d generic=%d", n.Symbol, n.GenericRef); err != nil {
			return err
		}
	case SourceAlias:
		if err := d.cw.printf(" explicitCast=%t", n.ExplicitCast); err != nil {
			return err
		}
	case AddressOf, SliceFromRaw:
		// payload printed via children
	case StoragePlace:
		if err := d.cw.printf(" symbol=%d writable=%t", n.Symbol, n.Writable); err != nil {
			return err
		}
	case DereferencePlace, CheckedIndexPlace:
		// payload printed via children
	case Load:
		// payload printed via children
	case TupleCoerce:
		if err := d.printSlice("typeargs", len(n.TypeArgs), func(i int) string {
			return fmt.Sprintf("%d", n.TypeArgs[i])
		}); err != nil {
			return err
		}
	case TempBind:
		if err := d.cw.printf(" temp=%d", n.Temp); err != nil {
			return err
		}
	case TempRead:
		if err := d.cw.printf(" temp=%d", n.Temp); err != nil {
			return err
		}
	case DirectCall, IndirectCall, MethodCall, VariantConstruct:
		if err := d.cw.printf(" convention=%d context=%s symbol=%d member=%d",
			n.Convention, n.ContextAction.String(), n.Symbol, n.Member); err != nil {
			return err
		}
	case IntegerCast, IntegerToFloat, FloatToInteger, FloatCast, OptionalInject, EnumToInteger, OptionalIntegerToEnum, CheckedIntegerToEnum, PointerCast:
		// payload printed via children
	case CheckedOptionalUnwrap:
		// payload printed via children
	case CheckedIndex:
		// payload printed via children
	case CheckedSlice:
		// payload printed via children
	case Sequence:
		// payload printed via children
	default:
		return fmt.Errorf("dumpNode: unhandled kind %s", n.Kind)
	}

	if len(n.Children) > 0 {
		if err := d.printSlice("children", len(n.Children), func(i int) string {
			return fmt.Sprintf("%d", n.Children[i])
		}); err != nil {
			return err
		}
	}
	if len(n.Parameters) > 0 {
		params := make([]string, len(n.Parameters))
		for i, p := range n.Parameters {
			params[i] = fmt.Sprintf("%d:%d", p.Symbol, p.Type)
		}
		if err := d.printSlice("params", len(params), func(i int) string {
			return params[i]
		}); err != nil {
			return err
		}
	}
	if len(n.TypeArgs) > 0 && n.Kind != TupleCoerce {
		if err := d.printSlice("typeargs", len(n.TypeArgs), func(i int) string {
			return fmt.Sprintf("%d", n.TypeArgs[i])
		}); err != nil {
			return err
		}
	}
	if n.Writable && isPlaceKind(n.Kind) {
		if err := d.cw.printf(" writable=%t", n.Writable); err != nil {
			return err
		}
	}

	return d.cw.printf("\n")
}

func (d *dumper) dumpRequirement(i uint32, r Requirement) error {
	return d.cw.printf("requirement %d owner=%d parameter=%d kind=%s subject=%d origin=%d:%d operator=%d lit=%s num=%q den=%q\n",
		i, r.Owner, r.Parameter, r.Kind.String(), r.Subject, r.Origin.Module, r.Origin.Node,
		r.Operator, r.LiteralKind.String(), r.Numerator, r.Denominator)
}

func (d *dumper) dumpInstantiation(i uint32, in Instantiation) error {
	if err := d.cw.printf("instantiation %d site=%d:%d declaration=%d typeargs=%v reqs=%d\n",
		i, in.Site.Module, in.Site.Node, in.Declaration, in.TypeArgs, len(in.Requirements)); err != nil {
		return err
	}
	return nil
}

func spanString(s source.Span) string {
	return fmt.Sprintf("%d[%d,%d)", s.Source, s.Start, s.End)
}

func isPlaceKind(k NodeKind) bool {
	switch k {
	case StoragePlace, DereferencePlace, FieldPlace, TuplePlace, CheckedIndexPlace:
		return true
	}
	return false
}

func componentsInUnit(u *Unit) uint64 {
	if u == nil {
		return 0
	}
	var c uint64
	for _, m := range u.modules {
		c += 1 + uint64(len(m.Imports)) + uint64(len(m.Declarations))
	}
	c += uint64(len(u.typeDecls))
	for _, t := range u.typeDecls {
		c += uint64(len(t.Members))
	}
	c += uint64(len(u.functions))
	c += uint64(len(u.globals))
	c += uint64(len(u.requirements))
	c += uint64(len(u.instantiations))
	for _, in := range u.instantiations {
		c += uint64(len(in.TypeArgs)) + uint64(len(in.Requirements))
	}
	for _, n := range u.nodes {
		c += 1 +
			uint64(len(n.Children)) +
			uint64(len(n.Parameters)) +
			uint64(len(n.Fields)) +
			uint64(len(n.Parts)) +
			uint64(len(n.TypeArgs)) +
			uint64(len(n.DeferChain)) +
			uint64(len(n.Requirements))
	}
	c += uint64(u.regionCount) + uint64(u.tempCount) + uint64(len(u.sourceMap))
	return c
}
