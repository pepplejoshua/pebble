package main

import (
	"flag"
	"fmt"
	"io"
	"os"

	"github.com/pepplejoshua/pebble/compiler/internal/backend"
	"github.com/pepplejoshua/pebble/compiler/internal/check"
	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/module"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/stdlib"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

func main() {
	os.Exit(run(os.Args[1:], os.Stdout, os.Stderr))
}

func run(args []string, stdout, stderr io.Writer) int {
	flags := flag.NewFlagSet("pebc", flag.ContinueOnError)
	flags.SetOutput(stderr)
	outputPath := flags.String("o", "", "write emitted C to path instead of stdout")
	if err := flags.Parse(args); err != nil {
		return 2
	}
	if flags.NArg() != 1 {
		fmt.Fprintln(stderr, "usage: pebc [-o path] <entry.peb>")
		return 2
	}

	provider := stdlib.New(module.FileSystemProvider{})
	sources := source.NewFileSet()
	diagnostics := diagnostic.NewDiagnosticSet()
	entryPath, err := provider.Canonicalize(flags.Arg(0))
	if err != nil {
		fmt.Fprintf(stderr, "pebc: cannot resolve entry %q: %v\n", flags.Arg(0), err)
		return 1
	}
	graph := module.Build(module.BuildConfig{EntryPath: string(entryPath), Package: "main", StandardRoot: stdlib.StandardRoot}, provider, sources, diagnostics)
	resolution := symbol.Resolve(graph, sources, diagnostics, symbol.Config{})
	store, err := types.New(types.Config{})
	if err != nil {
		fmt.Fprintf(stderr, "pebc: cannot initialize type store: %v\n", err)
		return 1
	}

	var entryID symbol.SymbolID
	for _, candidate := range resolution.Symbols.All() {
		if candidate.Name == "main" {
			entryID = candidate.ID
			break
		}
	}
	if entryID == 0 {
		if diagnostics.Len() > 0 {
			_ = diagnostic.RenderText(stderr, sources, diagnostics.Items())
		}
		fmt.Fprintln(stderr, "pebc: no main function found")
		return 1
	}

	inputs := check.Inputs{
		Graph: graph, Sources: sources, Resolution: resolution, Types: store,
		LiteralTarget: infer.LiteralTarget{WordBits: 64},
	}
	result := check.Check(inputs, diagnostics, check.Config{
		Entry: check.EntryPoint{Mode: check.EntryRequired, Symbol: entryID},
	})
	if !result.Successful() || diagnostics.Len() > 0 {
		if err := diagnostic.RenderText(stderr, sources, diagnostics.Items()); err != nil {
			fmt.Fprintf(stderr, "pebc: rendering diagnostics failed: %v\n", err)
		}
		return 1
	}
	unit := result.IR()
	if unit == nil {
		fmt.Fprintln(stderr, "pebc: internal error: checker returned no typed IR")
		return 1
	}

	var out io.Writer = stdout
	var file *os.File
	if *outputPath != "" {
		file, err = os.Create(*outputPath)
		if err != nil {
			fmt.Fprintf(stderr, "pebc: cannot create output %q: %v\n", *outputPath, err)
			return 1
		}
		defer file.Close()
		out = file
	}
	if err := backend.Emit(unit, unit.Snapshot(), entryID, sources, out); err != nil {
		fmt.Fprintf(stderr, "pebc: emission failed: %v\n", err)
		return 1
	}
	return 0
}
