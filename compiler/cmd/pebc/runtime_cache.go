package main

import (
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"io"
	"io/fs"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"time"
)

// runtimeArchive returns the path to a prebuilt static library for the Pebble
// runtime, building and caching it on first use. The cache is keyed on a
// content hash of everything that affects the runtime's compilation (the
// runtime sources, the runtime headers, the -D define, and the cc identity),
// so once an archive exists for that key, subsequent builds skip recompiling
// runtime/src/*.c entirely and link straight against the cached archive.
func runtimeArchive(runtimeRoot, define string, cc string) (string, error) {
	cacheRoot, err := os.UserCacheDir()
	if err != nil {
		return "", fmt.Errorf("cannot determine user cache dir: %v", err)
	}
	key, err := runtimeCacheKey(runtimeRoot, define, cc)
	if err != nil {
		return "", err
	}
	archiveDir := filepath.Join(cacheRoot, "pebble", "runtime", key)
	finalPath := filepath.Join(archiveDir, "libpebble_rt.a")
	if info, err := os.Stat(finalPath); err == nil && !info.IsDir() && info.Size() > 0 {
		return finalPath, nil
	}
	if err := buildRuntimeArchive(runtimeRoot, define, cc, archiveDir, finalPath); err != nil {
		return "", err
	}
	return finalPath, nil
}

// buildRuntimeArchive compiles every runtime/src/*.c into object files and
// archives them into a static library at finalPath. The archive is written to
// a temporary file in the same directory and atomically renamed into place so
// concurrent pebc invocations building the same fresh cache cannot observe or
// corrupt a half-written archive.
func buildRuntimeArchive(runtimeRoot, define, cc, archiveDir, finalPath string) error {
	srcFiles, err := filepath.Glob(filepath.Join(runtimeRoot, "src", "*.c"))
	if err != nil {
		return fmt.Errorf("cannot glob runtime sources: %v", err)
	}
	if len(srcFiles) == 0 {
		return fmt.Errorf("no runtime sources found under %q", filepath.Join(runtimeRoot, "src"))
	}
	sort.Strings(srcFiles)
	includeDir := filepath.Join(runtimeRoot, "include")
	objDir, err := os.MkdirTemp("", "pebble-rt-obj-")
	if err != nil {
		return fmt.Errorf("cannot create temp obj dir: %v", err)
	}
	defer os.RemoveAll(objDir)
	objs := make([]string, 0, len(srcFiles))
	for _, src := range srcFiles {
		obj := filepath.Join(objDir, strings.TrimSuffix(filepath.Base(src), ".c")+".o")
		output, err := exec.Command(cc, "-std=c11", "-Wall", "-Wextra", "-Werror", define, "-I"+includeDir, "-c", src, "-o", obj).CombinedOutput()
		if err != nil {
			return fmt.Errorf("cc: %v\n%s", err, output)
		}
		objs = append(objs, obj)
	}
	if err := os.MkdirAll(archiveDir, 0o755); err != nil {
		return fmt.Errorf("cannot create cache dir %q: %v", archiveDir, err)
	}
	ar, err := exec.LookPath("ar")
	if err != nil {
		return fmt.Errorf("ar not on PATH: %v", err)
	}
	// Use a PID-unique temp name WITHOUT pre-creating the file: llvm-ar (and
	// GNU ar) refuse to write a fresh archive into a path that already exists
	// as an empty non-archive file. Two concurrent pebc invocations racing to
	// build the same cache key use distinct PID-based names, so they never
	// collide; whichever renames to finalPath second simply overwrites with
	// equivalent content.
	tmpPath := filepath.Join(archiveDir, fmt.Sprintf("libpebble_rt.a.tmp.%d.%d", os.Getpid(), time.Now().UnixNano()))
	defer os.Remove(tmpPath)
	arArgs := append([]string{"rcs", tmpPath}, objs...)
	if output, err := exec.Command(ar, arArgs...).CombinedOutput(); err != nil {
		return fmt.Errorf("ar: %v\n%s", err, output)
	}
	if err := os.Rename(tmpPath, finalPath); err != nil {
		return fmt.Errorf("cannot install cached archive: %v", err)
	}
	return nil
}

// runtimeCacheKey returns a content hash over everything that affects how the
// runtime compiles: the -D define, the cc binary's identity and version, the
// contents of runtime/src/*.c, and the contents of everything under
// runtime/include. Sources and headers are hashed in sorted order so the key
// is deterministic.
func runtimeCacheKey(runtimeRoot, define string, cc string) (string, error) {
	h := sha256.New()
	io.WriteString(h, define)
	io.WriteString(h, "\x00")
	io.WriteString(h, ccIdentity(cc))
	io.WriteString(h, "\x00")
	srcFiles, err := filepath.Glob(filepath.Join(runtimeRoot, "src", "*.c"))
	if err != nil {
		return "", fmt.Errorf("cannot glob runtime sources: %v", err)
	}
	if len(srcFiles) == 0 {
		return "", fmt.Errorf("no runtime sources found under %q", filepath.Join(runtimeRoot, "src"))
	}
	sort.Strings(srcFiles)
	if err := hashRuntimeFiles(h, runtimeRoot, srcFiles); err != nil {
		return "", err
	}
	var headers []string
	err = filepath.WalkDir(filepath.Join(runtimeRoot, "include"), func(path string, d fs.DirEntry, walkErr error) error {
		if walkErr != nil {
			return walkErr
		}
		if !d.IsDir() {
			headers = append(headers, path)
		}
		return nil
	})
	if err != nil {
		return "", fmt.Errorf("cannot walk runtime headers: %v", err)
	}
	sort.Strings(headers)
	if err := hashRuntimeFiles(h, runtimeRoot, headers); err != nil {
		return "", err
	}
	return hex.EncodeToString(h.Sum(nil)), nil
}

// hashRuntimeFiles writes each file's path relative to runtimeRoot (so the
// key does not depend on where the repo lives) followed by its contents into
// h, in the caller-provided (sorted) order.
func hashRuntimeFiles(h io.Writer, runtimeRoot string, files []string) error {
	for _, path := range files {
		rel, err := filepath.Rel(runtimeRoot, path)
		if err != nil {
			return fmt.Errorf("cannot relativize %q: %v", path, err)
		}
		io.WriteString(h, rel)
		io.WriteString(h, "\x00")
		f, err := os.Open(path)
		if err != nil {
			return err
		}
		if _, err := io.Copy(h, f); err != nil {
			f.Close()
			return err
		}
		if err := f.Close(); err != nil {
			return err
		}
		io.WriteString(h, "\x00")
	}
	return nil
}

// ccIdentity describes the cc binary enough to invalidate caches built by a
// different compiler. If its version cannot be queried, the resolved path is
// used on its own.
func ccIdentity(ccPath string) string {
	if output, err := exec.Command(ccPath, "--version").CombinedOutput(); err == nil {
		return ccPath + "\x00" + string(output)
	}
	return ccPath
}
