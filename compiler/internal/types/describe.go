package types

// builtinName returns the short Pebble-source name for a builtin kind
// ("bool", "i32", "str", etc.), used in diagnostic messages.
func builtinName(k BuiltinKind) string {
	switch k {
	case Bool:
		return "bool"
	case Char:
		return "char"
	case Str:
		return "str"
	case Void:
		return "void"
	case Int:
		return "int"
	case Uint:
		return "uint"
	case I8:
		return "i8"
	case I16:
		return "i16"
	case I32:
		return "i32"
	case I64:
		return "i64"
	case U8:
		return "u8"
	case U16:
		return "u16"
	case U32:
		return "u32"
	case U64:
		return "u64"
	case F32:
		return "f32"
	case F64:
		return "f64"
	}
	return "<type>"
}

// DescribeKey returns a short human-readable name for a TypeKey, suitable for
// inclusion in diagnostic messages.
func DescribeKey(key TypeKey) string {
	if builtin, ok := key.Builtin(); ok {
		return builtinName(builtin)
	}
	switch key.kind {
	case Pointer:
		return "pointer"
	case Tuple:
		return "tuple"
	case Optional:
		return "optional"
	case Array:
		return "array"
	case Slice:
		return "slice"
	case Function:
		return "function"
	case Nominal:
		return "enum"
	}
	return "<type>"
}
