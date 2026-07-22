// Package source owns immutable source files and byte-offset spans.
package source

import (
	"bytes"
	"fmt"
	"sort"
	"unicode/utf8"
)

// ID identifies a source file within one compilation snapshot.
type ID uint32

// Span is a half-open byte range within one source file.
type Span struct {
	Source ID
	Start  uint32
	End    uint32
}

// NewSpan creates a span and clamps an inverted end to the start.
func NewSpan(id ID, start, end uint32) Span {
	if end < start {
		end = start
	}
	return Span{Source: id, Start: start, End: end}
}

// Position is a one-based source position.
type Position struct {
	Line   int
	Column int
}

// File is immutable after being added to a FileSet.
type File struct {
	id         ID
	path       string
	text       []byte
	lineStarts []uint32
}

// ID returns the file's compilation-local identity.
func (f *File) ID() ID { return f.id }

// Path returns the display path supplied when the file was added.
func (f *File) Path() string { return f.path }

// Text returns the immutable source bytes. Callers must not modify them.
func (f *File) Text() []byte { return f.text }

// Len returns the source length in bytes.
func (f *File) Len() uint32 { return uint32(len(f.text)) }

// Slice returns the bytes covered by span. A span for another file or outside
// the file returns nil.
func (f *File) Slice(span Span) []byte {
	if span.Source != f.id || span.Start > span.End || span.End > uint32(len(f.text)) {
		return nil
	}
	return f.text[span.Start:span.End]
}

// Position returns the one-based line and Unicode-scalar column at offset.
func (f *File) Position(offset uint32) Position {
	if offset > uint32(len(f.text)) {
		offset = uint32(len(f.text))
	}
	lineIndex := sort.Search(len(f.lineStarts), func(i int) bool {
		return f.lineStarts[i] > offset
	}) - 1
	if lineIndex < 0 {
		lineIndex = 0
	}
	start := f.lineStarts[lineIndex]
	column := 1
	for i := start; i < offset; {
		r, size := utf8.DecodeRune(f.text[i:offset])
		if r == '\t' {
			column += 4 - ((column - 1) % 4)
		} else {
			column++
		}
		i += uint32(size)
	}
	return Position{Line: lineIndex + 1, Column: column}
}

// Line returns a line without its line ending. Lines are one-based.
func (f *File) Line(line int) []byte {
	if line < 1 || line > len(f.lineStarts) {
		return nil
	}
	start := f.lineStarts[line-1]
	end := uint32(len(f.text))
	if line < len(f.lineStarts) {
		end = f.lineStarts[line]
	}
	for end > start && (f.text[end-1] == '\n' || f.text[end-1] == '\r') {
		end--
	}
	return f.text[start:end]
}

// FileSet owns source files for one compilation snapshot.
type FileSet struct {
	files []*File
}

// NewFileSet creates an empty source file set.
func NewFileSet() *FileSet { return &FileSet{} }

// Add validates and stores a source file. A leading UTF-8 BOM is discarded.
func (s *FileSet) Add(path string, contents []byte) (ID, error) {
	if bytes.HasPrefix(contents, []byte{0xef, 0xbb, 0xbf}) {
		contents = contents[3:]
	}
	if !utf8.Valid(contents) {
		return 0, fmt.Errorf("%s: source is not valid UTF-8", path)
	}
	if uint64(len(contents)) > uint64(^uint32(0)) {
		return 0, fmt.Errorf("%s: source is larger than 4 GiB", path)
	}

	id := ID(len(s.files))
	text := bytes.Clone(contents)
	file := &File{
		id:         id,
		path:       path,
		text:       text,
		lineStarts: buildLineStarts(text),
	}
	s.files = append(s.files, file)
	return id, nil
}

// File returns a file by ID.
func (s *FileSet) File(id ID) (*File, bool) {
	if uint64(id) >= uint64(len(s.files)) {
		return nil, false
	}
	return s.files[id], true
}

func buildLineStarts(text []byte) []uint32 {
	starts := []uint32{0}
	for i := 0; i < len(text); i++ {
		switch text[i] {
		case '\n':
			starts = append(starts, uint32(i+1))
		case '\r':
			if i+1 < len(text) && text[i+1] == '\n' {
				i++
			}
			starts = append(starts, uint32(i+1))
		}
	}
	return starts
}
