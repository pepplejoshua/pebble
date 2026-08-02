# Pebble runtime build + smoke test.
#
# The compiler driver will define exactly one of PEBBLE_RT_MODE_SAFE or
# PEBBLE_RT_MODE_RELEASE when compiling generated C together with this
# runtime. The default target builds and runs the smoke test in SAFE
# mode; `make MODE=RELEASE` rebuilds and runs in RELEASE mode. Note the
# binary is rebuilt from scratch per mode (a make variable change is not
# a file change), so switching MODE performs a clean rebuild.

CC      ?= cc
CFLAGS  ?= -O2
MODE    ?= SAFE

CPPFLAGS := -Iinclude -DPEBBLE_RT_MODE_$(MODE)
WARNFLAGS := -Wall -Wextra -Werror -std=c11

SOURCES := src/context.c src/panic.c src/platform_host.c src/arith.c src/bounds.c src/optional.c
TEST_SRC := test/smoke_test.c
TEST_BIN := build/smoke_test

.PHONY: all run release clean

all: run

build:
	mkdir -p build

$(TEST_BIN): $(SOURCES) $(TEST_SRC) include/pebble_rt.h | build
	$(CC) $(WARNFLAGS) $(CPPFLAGS) $(CFLAGS) $(SOURCES) $(TEST_SRC) -o $@

run: $(TEST_BIN)
	./$(TEST_BIN)

# Swap the mode macro and rebuild from scratch; the header's mode-selection
# guard and PEBBLE_RT_CHECKS_ENABLED compute correctly in both.
release:
	$(MAKE) MODE=RELEASE clean
	$(MAKE) MODE=RELEASE run

clean:
	rm -rf build
