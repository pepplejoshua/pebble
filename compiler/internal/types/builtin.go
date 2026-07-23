package types

const builtinCount uint32 = 16

// Builtins contains the fixed primitive TypeIDs for one Store.
type Builtins struct {
	Bool TypeID
	Char TypeID
	Str  TypeID
	Void TypeID
	Int  TypeID
	Uint TypeID
	I8   TypeID
	I16  TypeID
	I32  TypeID
	I64  TypeID
	U8   TypeID
	U16  TypeID
	U32  TypeID
	U64  TypeID
	F32  TypeID
	F64  TypeID
}

func (s *Store) internBuiltins() error {
	kinds := [...]BuiltinKind{
		Bool, Char, Str, Void,
		Int, Uint,
		I8, I16, I32, I64,
		U8, U16, U32, U64,
		F32, F64,
	}
	ids := [builtinCount]TypeID{}
	for index, kind := range kinds {
		id, err := s.Intern(BuiltinKey(kind))
		if err != nil {
			return err
		}
		ids[index] = id
	}

	s.builtins = Builtins{
		Bool: ids[0], Char: ids[1], Str: ids[2], Void: ids[3],
		Int: ids[4], Uint: ids[5],
		I8: ids[6], I16: ids[7], I32: ids[8], I64: ids[9],
		U8: ids[10], U16: ids[11], U32: ids[12], U64: ids[13],
		F32: ids[14], F64: ids[15],
	}
	return nil
}
