package syntax

import "github.com/pepplejoshua/pebble/compiler/internal/diagnostic"

// Lexer diagnostic codes are stable external identifiers. Keep the semantic
// names at call sites and the rendered codes here in one auditable registry.
const (
	codeUnsupportedCharacter           diagnostic.Code = "L0001"
	codeMalformedNumericLiteral        diagnostic.Code = "L0002"
	codeMalformedStringLiteral         diagnostic.Code = "L0003"
	codeInvalidEscapeSequence          diagnostic.Code = "L0004"
	codeInvalidCharacterLiteral        diagnostic.Code = "L0005"
	codeUnterminatedInterpolatedString diagnostic.Code = "L0006"
	codeUnterminatedInterpolationExpr  diagnostic.Code = "L0007"

	codeExpectedDeclaration diagnostic.Code = "P0001"
	codeExpectedName        diagnostic.Code = "P0002"
	codeExpectedExpression  diagnostic.Code = "P0003"
	codeExpectedType        diagnostic.Code = "P0004"
	codeExpectedToken       diagnostic.Code = "P0005"
	codeInvalidSyntax       diagnostic.Code = "P0006"
	codeNestingLimit        diagnostic.Code = "P0007"
)
