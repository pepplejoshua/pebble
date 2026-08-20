# Pebble compiler install/uninstall.
#
# `make install` builds the pebc binary into Go's binary directory and symlinks
# compiler/std (this repo's compiler/std/) and runtime (the repo root's
# sibling runtime/ directory) next to it, so an installed binary always finds
# its standard library and runtime relative to ITSELF -- the same way Go finds
# GOROOT/src and Rust finds rust-src next to the toolchain. Symlinks (not
# copies) mean local edits to compiler/std/*.peb and runtime/ take effect on
# the very next pebc invocation with zero rebuild. This ports the
# intent of the old v1 C compiler's `cp -r std $(BINDIR)` install step
# (git 3d9ed8f), which let an installed binary find its stdlib next to itself.
#
# The target directory mirrors Go's own resolution order for `go install`:
# $GOBIN if set, otherwise $(go env GOPATH)/bin. Running `make install` here
# therefore produces a binary at exactly the same place `go install ./cmd/pebc`
# does today.

GOBIN ?= $(shell go env GOBIN)
TARGET ?= $(if $(GOBIN),$(GOBIN),$(shell go env GOPATH)/bin)

COMPILER_DIR := $(dir $(realpath $(firstword $(MAKEFILE_LIST))))
REPO_ROOT := $(realpath $(COMPILER_DIR)..)
STD_SRC := $(COMPILER_DIR)std
RUNTIME_SRC := $(REPO_ROOT)/runtime

.PHONY: install uninstall

install:
	@mkdir -p $(TARGET)
	cd $(COMPILER_DIR) && go build -o $(TARGET)/pebc ./cmd/pebc
	ln -sfn $(STD_SRC) $(TARGET)/std
	ln -sfn $(RUNTIME_SRC) $(TARGET)/runtime
	@echo "installed $(TARGET)/pebc"
	@echo "symlinked $(TARGET)/std -> $(STD_SRC)"
	@echo "symlinked $(TARGET)/runtime -> $(RUNTIME_SRC)"

# uninstall removes the binary and the two symlinks, but only touches the
# symlinks when they actually point back into this checkout -- $(TARGET)/std or
# $(TARGET)/runtime might legitimately be something unrelated on some machine,
# and must never be deleted blindly. The binary is only removed when it is not
# a directory.
uninstall:
	@if [ -d "$(TARGET)/pebc" ]; then \
		echo "not removing $(TARGET)/pebc: it is a directory"; \
	else \
		rm -f "$(TARGET)/pebc"; \
		echo "removed $(TARGET)/pebc"; \
	fi
	@for link in std runtime; do \
		path="$(TARGET)/$$link"; \
		if [ -L "$$path" ]; then \
			target="$$(readlink "$$path")"; \
			if [ "$$target" = "$(STD_SRC)" ] || [ "$$target" = "$(RUNTIME_SRC)" ]; then \
				rm -f "$$path"; \
				echo "removed symlink $$path -> $$target"; \
			else \
				echo "not removing $$path: symlink points to $$target, not into this checkout"; \
			fi; \
		elif [ -e "$$path" ]; then \
			echo "not removing $$path: exists but is not a symlink"; \
		else \
			echo "$$path: not present"; \
		fi; \
	done