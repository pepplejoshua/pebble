package infer

const (
	HardMaxRecursiveDepth        uint32 = 1024
	DefaultMaxInferVariables     uint32 = 1 << 20
	DefaultMaxConstraints        uint32 = 1 << 22
	DefaultMaxShapeComponents    uint32 = 1 << 22
	DefaultMaxLiteralBytes       uint32 = 1 << 16
	DefaultMaxLiteralBits        uint32 = 1 << 20
	DefaultMaxLiteralExponent    uint32 = 1_000_000
	DefaultMaxAliasDepth         uint32 = 256
	DefaultMaxTypeSyntaxDepth    uint32 = 256
	DefaultMaxUnificationSteps   uint64 = 1 << 24
	DefaultMaxDecompositionSteps uint64 = 1 << 24
	DefaultMaxConstraintRequeues uint32 = 256
	DefaultMaxTotalRequeues      uint64 = 1 << 24
	DefaultMaxChoices            uint32 = 1 << 16
	DefaultMaxChoiceAlternatives uint32 = 8
	DefaultMaxChoiceStates       uint64 = 1 << 20
	DefaultMaxSolvedSlots        uint32 = 1 << 22
	DefaultMaxDiagnostics        uint32 = 50
)

// Config bounds one declaration preparation or solve session. Zero values
// select the documented defaults.
type Config struct {
	MaxInferVariables     uint32
	MaxConstraints        uint32
	MaxShapeComponents    uint32
	MaxLiteralBytes       uint32
	MaxLiteralBits        uint32
	MaxLiteralExponent    uint32
	MaxAliasDepth         uint32
	MaxTypeSyntaxDepth    uint32
	MaxUnificationSteps   uint64
	MaxDecompositionSteps uint64
	MaxConstraintRequeues uint32
	MaxTotalRequeues      uint64
	MaxChoices            uint32
	MaxChoiceAlternatives uint32
	MaxChoiceStates       uint64
	MaxSolvedSlots        uint32
	MaxDiagnostics        uint32
}

func normalizeConfig(c Config) Config {
	if c.MaxInferVariables == 0 {
		c.MaxInferVariables = DefaultMaxInferVariables
	}
	if c.MaxConstraints == 0 {
		c.MaxConstraints = DefaultMaxConstraints
	}
	if c.MaxShapeComponents == 0 {
		c.MaxShapeComponents = DefaultMaxShapeComponents
	}
	if c.MaxLiteralBytes == 0 {
		c.MaxLiteralBytes = DefaultMaxLiteralBytes
	}
	if c.MaxLiteralBits == 0 {
		c.MaxLiteralBits = DefaultMaxLiteralBits
	}
	if c.MaxLiteralExponent == 0 {
		c.MaxLiteralExponent = DefaultMaxLiteralExponent
	}
	if c.MaxAliasDepth == 0 {
		c.MaxAliasDepth = DefaultMaxAliasDepth
	} else if c.MaxAliasDepth > HardMaxRecursiveDepth {
		c.MaxAliasDepth = HardMaxRecursiveDepth
	}
	if c.MaxTypeSyntaxDepth == 0 {
		c.MaxTypeSyntaxDepth = DefaultMaxTypeSyntaxDepth
	} else if c.MaxTypeSyntaxDepth > HardMaxRecursiveDepth {
		c.MaxTypeSyntaxDepth = HardMaxRecursiveDepth
	}
	if c.MaxUnificationSteps == 0 {
		c.MaxUnificationSteps = DefaultMaxUnificationSteps
	}
	if c.MaxDecompositionSteps == 0 {
		c.MaxDecompositionSteps = DefaultMaxDecompositionSteps
	}
	if c.MaxConstraintRequeues == 0 {
		c.MaxConstraintRequeues = DefaultMaxConstraintRequeues
	}
	if c.MaxTotalRequeues == 0 {
		c.MaxTotalRequeues = DefaultMaxTotalRequeues
	}
	if c.MaxChoices == 0 {
		c.MaxChoices = DefaultMaxChoices
	}
	if c.MaxChoiceAlternatives == 0 {
		c.MaxChoiceAlternatives = DefaultMaxChoiceAlternatives
	}
	if c.MaxChoiceStates == 0 {
		c.MaxChoiceStates = DefaultMaxChoiceStates
	}
	if c.MaxSolvedSlots == 0 {
		c.MaxSolvedSlots = DefaultMaxSolvedSlots
	}
	if c.MaxDiagnostics == 0 {
		c.MaxDiagnostics = DefaultMaxDiagnostics
	}
	return c
}
