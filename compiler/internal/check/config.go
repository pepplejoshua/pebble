package check

import "github.com/pepplejoshua/pebble/compiler/internal/infer"

const (
	DefaultMaxSyntaxVisits        uint32 = 1 << 22
	DefaultMaxTraversalDepth      uint32 = 1 << 10
	DefaultMaxSemanticRecords     uint32 = 1 << 22
	DefaultMaxRecordComponents    uint32 = 1 << 23
	DefaultMaxControlDepth        uint32 = 1 << 10
	DefaultMaxTrackedPlaces       uint32 = 1 << 20
	DefaultMaxGenericRequirements uint32 = 1 << 20
	DefaultMaxConstantDepth       uint32 = 1 << 8
	DefaultMaxConstantOperations  uint64 = 1 << 20
	DefaultMaxConstantBits        uint32 = 1 << 20
	DefaultMaxDiagnostics         uint32 = 100
)

// Config bounds one phase-6 generation. Zero-valued limits select the
// documented defaults. Inference is forwarded unchanged by the later slice
// that prepares and creates the inference session.
type Config struct {
	Inference              infer.Config
	MaxSyntaxVisits        uint32
	MaxTraversalDepth      uint32
	MaxSemanticRecords     uint32
	MaxRecordComponents    uint32
	MaxControlDepth        uint32
	MaxTrackedPlaces       uint32
	MaxGenericRequirements uint32
	MaxConstantDepth       uint32
	MaxConstantOperations  uint64
	MaxConstantBits        uint32
	MaxDiagnostics         uint32
}

func normalizeConfig(config Config) Config {
	if config.MaxSyntaxVisits == 0 {
		config.MaxSyntaxVisits = DefaultMaxSyntaxVisits
	}
	if config.MaxTraversalDepth == 0 {
		config.MaxTraversalDepth = DefaultMaxTraversalDepth
	}
	if config.MaxSemanticRecords == 0 {
		config.MaxSemanticRecords = DefaultMaxSemanticRecords
	}
	if config.MaxRecordComponents == 0 {
		config.MaxRecordComponents = DefaultMaxRecordComponents
	}
	if config.MaxControlDepth == 0 {
		config.MaxControlDepth = DefaultMaxControlDepth
	}
	if config.MaxTrackedPlaces == 0 {
		config.MaxTrackedPlaces = DefaultMaxTrackedPlaces
	}
	if config.MaxGenericRequirements == 0 {
		config.MaxGenericRequirements = DefaultMaxGenericRequirements
	}
	if config.MaxConstantDepth == 0 {
		config.MaxConstantDepth = DefaultMaxConstantDepth
	}
	if config.MaxConstantOperations == 0 {
		config.MaxConstantOperations = DefaultMaxConstantOperations
	}
	if config.MaxConstantBits == 0 {
		config.MaxConstantBits = DefaultMaxConstantBits
	}
	if config.MaxDiagnostics == 0 {
		config.MaxDiagnostics = DefaultMaxDiagnostics
	}
	return config
}
