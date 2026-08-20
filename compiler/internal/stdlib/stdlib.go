// Package stdlib serves the Pebble standard library from a real on-disk std/
// directory when one is available, falling back to the embedded go:embed copy.
package stdlib

import (
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"strings"

	"github.com/pepplejoshua/pebble/compiler/internal/module"
	"github.com/pepplejoshua/pebble/compiler/std"
)

// StandardRoot is the BuildConfig.StandardRoot sentinel that routes std:
// imports to the embedded standard library instead of the host filesystem. It
// is collision-safe because module.FileSystemProvider always canonicalizes
// real paths to absolute, symlink-resolved paths, which cannot equal or be
// prefixed by a relative name containing a colon.
const StandardRoot = "std:embedded"

// Provider serves std: imports from either a real on-disk std/ directory or the
// embedded standard library, and delegates every other path to the wrapped
// provider.
type Provider struct {
	standardRoot string
	stdFS        fs.FS
	delegate     module.SourceProvider
}

// New returns a Provider that serves std: imports from delegate and the
// embedded standard library.
//
// When diskRoot is non-empty, std: imports are read from that real on-disk std/
// directory (via os.DirFS), so local edits to compiler/std/*.peb take effect on
// the very next compilation with no rebuild -- the layout `make install`
// creates by symlinking compiler/std next to the pebc binary. When diskRoot is
// empty, or the directory cannot be opened, std: imports fall back to the
// embedded go:embed copy, so a portable standalone pebc binary with no real
// std/ next to it keeps working exactly as before.
func New(delegate module.SourceProvider, diskRoot string) *Provider {
	if diskRoot != "" {
		if fsys, err := os.Stat(diskRoot); err == nil && fsys.IsDir() {
			return &Provider{standardRoot: StandardRoot, stdFS: os.DirFS(diskRoot), delegate: delegate}
		}
	}
	return &Provider{standardRoot: StandardRoot, stdFS: std.FS, delegate: delegate}
}

// Canonicalize routes paths under StandardRoot to the embedded filesystem and
// delegates everything else.
func (p *Provider) Canonicalize(path string) (module.CanonicalPath, error) {
	if p.isStandardPath(path) {
		return module.CanonicalPath(filepath.ToSlash(filepath.Clean(path))), nil
	}
	return p.delegate.Canonicalize(path)
}

// ReadFile routes paths under StandardRoot to the embedded filesystem and
// delegates everything else.
func (p *Provider) ReadFile(path module.CanonicalPath) ([]byte, error) {
	if p.isStandardPath(string(path)) {
		relative := strings.TrimPrefix(string(path), p.standardRoot+"/")
		contents, err := fs.ReadFile(p.stdFS, filepath.ToSlash(relative))
		if err != nil {
			return nil, classifyProviderError(string(path), err)
		}
		return contents, nil
	}
	return p.delegate.ReadFile(path)
}

func (p *Provider) isStandardPath(path string) bool {
	return path == p.standardRoot || strings.HasPrefix(path, p.standardRoot+"/")
}

func classifyProviderError(path string, err error) error {
	kind := module.ProviderUnreadable
	if errors.Is(err, fs.ErrNotExist) {
		kind = module.ProviderNotFound
	} else if errors.Is(err, fs.ErrInvalid) {
		kind = module.ProviderInvalidPath
	}
	return &module.ProviderError{Kind: kind, Path: path, Err: err}
}
