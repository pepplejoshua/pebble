// Package diagnostic defines structured compiler diagnostics.
package diagnostic

import (
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
		file, ok := sources.File(d.Primary.Span.Source)
		if !ok {
			if _, err := fmt.Fprintf(w, "%s: %s\n", heading(d), d.Message); err != nil {
				return err
			}
			continue
		}

		position := file.Position(d.Primary.Span.Start)
		if _, err := fmt.Fprintf(w, "%s:%d:%d: %s: %s\n", file.Path(), position.Line, position.Column, heading(d), d.Message); err != nil {
			return err
		}
		line := string(file.Line(position.Line))
		if line != "" {
			if _, err := fmt.Fprintf(w, "  %s\n  %s^\n", line, strings.Repeat(" ", max(position.Column-1, 0))); err != nil {
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

func heading(d Diagnostic) string {
	if d.Code == "" {
		return d.Severity.String()
	}
	return fmt.Sprintf("%s[%s]", d.Severity, d.Code)
}
