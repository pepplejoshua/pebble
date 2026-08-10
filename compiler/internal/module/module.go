// Package module discovers, loads, and validates Pebble module graphs.
package module

import (
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"strings"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
)

// Stable module diagnostic codes.
const (
	CodeInvalidImport      diagnostic.Code = "M0001"
	CodeModuleUnavailable  diagnostic.Code = "M0002"
	CodeDuplicateImport    diagnostic.Code = "M0003"
	CodeQualifierCollision diagnostic.Code = "M0004"
	CodeModuleCycle        diagnostic.Code = "M0005"
	CodeAmbiguousPackage   diagnostic.Code = "M0006"
	CodeResourceLimit      diagnostic.Code = "M0007"
)

const (
	DefaultMaxModules     uint32 = 4096
	DefaultMaxImportDepth uint32 = 256
	defaultMaxDiagnostics uint32 = 50
)

// PackageID identifies one configured compilation package.
type PackageID string

// StandardPackage is the package identity assigned to StandardRoot modules.
const StandardPackage PackageID = "std"

// CanonicalPath is a provider-owned, normalized source path.
type CanonicalPath string

// SearchRoot maps bare import spellings to a configured package root.
type SearchRoot struct {
	Package PackageID
	Path    string
}

// BuildConfig contains all path-resolution state for one graph build.
type BuildConfig struct {
	// EntryPath is the root module loaded first (unless a prelude is
	// configured) and becomes Graph.Root.
	EntryPath string
	// Package is the package identity assigned to the entry module.
	Package PackageID
	// PreludePath optionally names a module parsed and resolved before the
	// entry module. Its top-level declarations are injected into prelude
	// scope so they are visible to every other module without an explicit
	// import. An empty value loads the compiler's embedded runtime prelude
	// (compiler/prelude/runtime.peb), which declares Allocator and Context;
	// every compilation therefore sees them with no flag or filesystem
	// discovery required.
	PreludePath    string
	StandardRoot   string
	SearchRoots    []SearchRoot
	MaxModules     uint32
	MaxImportDepth uint32
}

// ProviderFailure classifies source-provider failures.
type ProviderFailure uint8

const (
	ProviderNotFound ProviderFailure = iota + 1
	ProviderUnreadable
	ProviderInvalidPath
)

// ProviderError preserves a provider failure category and underlying error.
type ProviderError struct {
	Kind ProviderFailure
	Path string
	Err  error
}

func (e *ProviderError) Error() string {
	if e.Err == nil {
		return e.Path
	}
	return fmt.Sprintf("%s: %v", e.Path, e.Err)
}

func (e *ProviderError) Unwrap() error { return e.Err }

// SourceProvider canonicalizes paths and reads immutable source bytes.
type SourceProvider interface {
	Canonicalize(path string) (CanonicalPath, error)
	ReadFile(path CanonicalPath) ([]byte, error)
}

// FileSystemProvider resolves symlinks and reads from the host filesystem.
type FileSystemProvider struct{}

func (FileSystemProvider) Canonicalize(path string) (CanonicalPath, error) {
	if strings.IndexByte(path, 0) >= 0 {
		return "", &ProviderError{Kind: ProviderInvalidPath, Path: path, Err: fs.ErrInvalid}
	}
	absolute, err := filepath.Abs(path)
	if err != nil {
		return "", classifyProviderError(path, err)
	}
	resolved, err := filepath.EvalSymlinks(absolute)
	if err != nil {
		return "", classifyProviderError(path, err)
	}
	return CanonicalPath(filepath.ToSlash(filepath.Clean(resolved))), nil
}

func (FileSystemProvider) ReadFile(path CanonicalPath) ([]byte, error) {
	contents, err := os.ReadFile(filepath.FromSlash(string(path)))
	if err != nil {
		return nil, classifyProviderError(string(path), err)
	}
	return contents, nil
}

func classifyProviderError(path string, err error) error {
	kind := ProviderUnreadable
	if errors.Is(err, fs.ErrNotExist) {
		kind = ProviderNotFound
	} else if errors.Is(err, fs.ErrInvalid) {
		kind = ProviderInvalidPath
	}
	return &ProviderError{Kind: kind, Path: path, Err: err}
}

// ModuleID identifies one module in one graph snapshot. Zero is invalid.
type ModuleID uint32

// ModuleKey is the canonical identity of one module.
type ModuleKey struct {
	Package PackageID
	Path    CanonicalPath
}

// ImportEdge retains one authored dependency edge.
type ImportEdge struct {
	Span      source.Span
	Spelling  string
	Qualifier string
	Target    ModuleID
}

// ModuleRole classifies a module's position in the compilation.
type ModuleRole uint8

const (
	// RoleNormal is every module loaded through the ordinary entry/import
	// discovery path.
	RoleNormal ModuleRole = iota + 1
	// RolePrelude marks the prelude module, parsed and resolved before the
	// entry module so its top-level declarations are visible everywhere.
	RolePrelude
)

// Module is an immutable graph value. Imports is copied by graph accessors.
type Module struct {
	ID      ModuleID
	Key     ModuleKey
	Source  source.ID
	Tree    *syntax.Tree
	Imports []ImportEdge
	Role    ModuleRole
}

// Graph is the immutable result of one deterministic graph build.
type Graph struct {
	Root       ModuleID
	Prelude    ModuleID
	modules    []Module
	byKey      map[ModuleKey]ModuleID
	dependency []ModuleID
}

// HasPrelude reports whether the graph was built with a configured prelude
// module.
func (g *Graph) HasPrelude() bool {
	return g != nil && g.Prelude != 0
}

// Len returns the number of successfully loaded reachable modules.
func (g *Graph) Len() int {
	if g == nil {
		return 0
	}
	return len(g.modules)
}

// Module returns an immutable copy of a module value.
func (g *Graph) Module(id ModuleID) (Module, bool) {
	if g == nil || id == 0 || uint64(id) > uint64(len(g.modules)) {
		return Module{}, false
	}
	value := g.modules[id-1]
	value.Imports = append([]ImportEdge(nil), value.Imports...)
	return value, true
}

// Modules returns modules in deterministic discovery/ID order.
func (g *Graph) Modules() []Module {
	if g == nil {
		return nil
	}
	result := make([]Module, len(g.modules))
	for i := range g.modules {
		result[i] = g.modules[i]
		result[i].Imports = append([]ImportEdge(nil), g.modules[i].Imports...)
	}
	return result
}

// Lookup returns the module with key, if it is in the graph.
func (g *Graph) Lookup(key ModuleKey) (ModuleID, bool) {
	if g == nil {
		return 0, false
	}
	id, ok := g.byKey[key]
	return id, ok
}

// DependencyOrder returns dependencies before their importers. It is defined
// for acyclic graphs and deterministic for a fixed graph.
func (g *Graph) DependencyOrder() []ModuleID {
	if g == nil {
		return nil
	}
	return append([]ModuleID(nil), g.dependency...)
}
