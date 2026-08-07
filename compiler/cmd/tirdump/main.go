package main

import (
	"flag"
	"fmt"
	"os"

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
	flag.Parse()
	provider := stdlib.New(module.FileSystemProvider{})
	sources := source.NewFileSet()
	diagnostics := diagnostic.NewDiagnosticSet()
	path, err := provider.Canonicalize(flag.Arg(0))
	if err != nil {
		panic(err)
	}
	graph := module.Build(module.BuildConfig{EntryPath: string(path), Package: "main", StandardRoot: stdlib.StandardRoot}, provider, sources, diagnostics)
	resolution := symbol.Resolve(graph, sources, diagnostics, symbol.Config{})
	store, err := types.New(types.Config{})
	if err != nil {
		panic(err)
	}
	var entry symbol.SymbolID
	for _, candidate := range resolution.Symbols.All() {
		if candidate.Name == "main" {
			entry = candidate.ID
			break
		}
	}
	result := check.Check(check.Inputs{Graph: graph, Sources: sources, Resolution: resolution, Types: store, LiteralTarget: infer.LiteralTarget{WordBits: 64}}, diagnostics, check.Config{Entry: check.EntryPoint{Mode: check.EntryRequired, Symbol: entry}})
	if diagnostics.Len() != 0 {
		fmt.Fprintln(os.Stderr, diagnostics.Items())
	}
	if result.IR() == nil {
		panic("no IR")
	}
	if err := result.IR().Dump(os.Stdout); err != nil {
		panic(err)
	}
}
