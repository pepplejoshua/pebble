// Package diagnostic defines structured compiler diagnostics.
package diagnostic

import (
	"encoding/json"
	"fmt"
	"io"
	"strings"

	"github.com/pepplejoshua/pebble/compiler/internal/source"
)

// Code is a stable, externally visible diagnostic identifier.
type Code string

// Severity controls whether a diagnostic prevents compilation.
type Severity uint8

const (
	Error Severity = iota
	Warning
	Note
)

func (s Severity) String() string {
	switch s {
	case Error:
		return "error"
	case Warning:
		return "warning"
	case Note:
		return "note"
	default:
		return "diagnostic"
	}
}

// Label points at source and explains its role in a diagnostic.
type Label struct {
	Span    source.Span
	Message string
}

// Diagnostic is independent of a particular renderer.
type Diagnostic struct {
	Severity Severity
	Code     Code
	Message  string
	Primary  Label
	Related  []Label
	Notes    []string
	Help     []string
}

// DiagnosticSet collects diagnostics in deterministic emission order.
type DiagnosticSet struct {
	items      []Diagnostic
	errorCount int
}

// NewDiagnosticSet creates an empty diagnostic set.
func NewDiagnosticSet() *DiagnosticSet { return &DiagnosticSet{} }

// Add records a diagnostic.
func (s *DiagnosticSet) Add(d Diagnostic) {
	s.items = append(s.items, d)
	if d.Severity == Error {
		s.errorCount++
	}
}

// Replace replaces the diagnostic at index without changing emission order.
func (s *DiagnosticSet) Replace(index int, d Diagnostic) bool {
	if s == nil || index < 0 || index >= len(s.items) {
		return false
	}

	previous := s.items[index]
	if previous.Severity == Error && d.Severity != Error {
		s.errorCount--
	} else if previous.Severity != Error && d.Severity == Error {
		s.errorCount++
	}
	s.items[index] = d
	return true
}

// Error records a primary error diagnostic.
func (s *DiagnosticSet) Error(code Code, message string, span source.Span) {
	s.Add(Diagnostic{
		Severity: Error,
		Code:     code,
		Message:  message,
		Primary:  Label{Span: span},
	})
}

// Items returns a copy of the collected diagnostics.
func (s *DiagnosticSet) Items() []Diagnostic {
	return append([]Diagnostic(nil), s.items...)
}

// Len returns the total diagnostic count.
func (s *DiagnosticSet) Len() int { return len(s.items) }

// ErrorCount returns the number of error diagnostics.
func (s *DiagnosticSet) ErrorCount() int { return s.errorCount }

// HasErrors reports whether compilation must stop.
func (s *DiagnosticSet) HasErrors() bool { return s.errorCount != 0 }

// RenderText writes stable, color-free diagnostics.
func RenderText(w io.Writer, sources *source.FileSet, diagnostics []Diagnostic) error {
	for _, d := range diagnostics {
		path, line, column, ok := formatSpan(sources, d.Primary.Span)
		if !ok {
			if _, err := fmt.Fprintf(w, "%s: %s\n", heading(d), d.Message); err != nil {
				return err
			}
			continue
		}

		if _, err := fmt.Fprintf(w, "%s:%d:%d: %s: %s\n", path, line, column, heading(d), d.Message); err != nil {
			return err
		}
		file, _ := sources.File(d.Primary.Span.Source)
		srcLine := string(file.Line(line))
		if srcLine != "" {
			if _, err := fmt.Fprintf(w, "  %s\n  %s^\n", srcLine, strings.Repeat(" ", max(column-1, 0))); err != nil {
				return err
			}
		}
		for _, related := range d.Related {
			relPath, relLine, relColumn, ok := formatSpan(sources, related.Span)
			if !ok {
				continue
			}
			if _, err := fmt.Fprintf(w, "  --> %s:%d:%d: %s\n", relPath, relLine, relColumn, related.Message); err != nil {
				return err
			}
		}
		for _, note := range d.Notes {
			if _, err := fmt.Fprintf(w, "  note: %s\n", note); err != nil {
				return err
			}
		}
		for _, help := range d.Help {
			if _, err := fmt.Fprintf(w, "  help: %s\n", help); err != nil {
				return err
			}
		}
	}
	return nil
}

// formatSpan resolves a span to a display path and one-based line and column.
func formatSpan(sources *source.FileSet, span source.Span) (path string, line, column int, ok bool) {
	file, ok := sources.File(span.Source)
	if !ok {
		return "", 0, 0, false
	}
	position := file.Position(span.Start)
	return file.Path(), position.Line, position.Column, true
}

// renderedLabel is the machine-readable form of one label.
type renderedLabel struct {
	Path    string `json:"path"`
	Line    int    `json:"line"`
	Column  int    `json:"column"`
	Message string `json:"message"`
}

// renderedDiagnostic is the machine-readable form of one diagnostic.
type renderedDiagnostic struct {
	Severity string          `json:"severity"`
	Code     string          `json:"code"`
	Message  string          `json:"message"`
	Path     string          `json:"path"`
	Line     int             `json:"line"`
	Column   int             `json:"column"`
	Label    string          `json:"label"`
	Related  []renderedLabel `json:"related"`
	Notes    []string        `json:"notes"`
	Help     []string        `json:"help"`
}

// RenderJSON writes machine-readable diagnostics in the same deterministic
// emission order RenderText preserves. Spans are resolved to path, line, and
// column via the same helper RenderText uses.
func RenderJSON(w io.Writer, sources *source.FileSet, diagnostics []Diagnostic) error {
	out := make([]renderedDiagnostic, 0, len(diagnostics))
	for _, d := range diagnostics {
		out = append(out, renderDiagnosticJSON(sources, d))
	}
	return json.NewEncoder(w).Encode(out)
}

func renderDiagnosticJSON(sources *source.FileSet, d Diagnostic) renderedDiagnostic {
	out := renderedDiagnostic{
		Severity: d.Severity.String(),
		Code:     string(d.Code),
		Message:  d.Message,
		Label:    d.Primary.Message,
		Related:  []renderedLabel{},
		Notes:    append([]string{}, d.Notes...),
		Help:     append([]string{}, d.Help...),
	}
	if path, line, column, ok := formatSpan(sources, d.Primary.Span); ok {
		out.Path = path
		out.Line = line
		out.Column = column
	}
	for _, related := range d.Related {
		label := renderedLabel{Message: related.Message}
		if path, line, column, ok := formatSpan(sources, related.Span); ok {
			label.Path = path
			label.Line = line
			label.Column = column
		}
		out.Related = append(out.Related, label)
	}
	return out
}

func heading(d Diagnostic) string {
	if d.Code == "" {
		return d.Severity.String()
	}
	return fmt.Sprintf("%s[%s]", d.Severity, d.Code)
}
