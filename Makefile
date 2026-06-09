PREFIX ?= /usr/local
BINDIR = $(PREFIX)/bin

CC = gcc
CFLAGS = -Wall -Wextra -g -I src
PROLE_CFLAGS = -Wall -Wextra -g -I prole/include -I pastel

# OBJS = src/alloc.o src/ast.o src/lexer.o src/parser.o src/options.o \
#        src/symbol.o src/type.o src/checker.o src/codegen.o src/module.o \
#        src/diagnostics.o src/temp_alloc.o src/main.o

OBJS = src/alloc.o src/ast.o src/lexer.o src/parser2.o src/options.o \
       src/symbol.o src/type.o src/checker.o src/codegen.o src/module.o \
       src/diagnostics.o src/temp_alloc.o src/main.o

PROLE_OBJS = prole/src/prole_allocator.o prole/src/prole_bytecode.o \
             prole/src/prole_disasm.o prole/src/prole_diag.o \
             prole/src/prole_validate.o
PROLE_SMOKE_BIN = /tmp/prole_disasm_smoke

# Auto-generate dependencies
DEPS = $(OBJS:.o=.d)
PROLE_DEPS = $(PROLE_OBJS:.o=.d)

# Tells make these dont product files
.PHONY: clean install uninstall smoke

pebc: $(OBJS)
	$(CC) $(CFLAGS) -o pebc $(OBJS) -fsanitize=address -g

prole/src/%.o: prole/src/%.c
	$(CC) $(PROLE_CFLAGS) -MMD -MP -c $< -o $@

%.o: %.c
	$(CC) $(CFLAGS) -MMD -MP -c $< -o $@

smoke: $(PROLE_OBJS)
	$(CC) $(PROLE_CFLAGS) -o $(PROLE_SMOKE_BIN) \
		prole/tests/disasm_smoke.c $(PROLE_OBJS)
	$(PROLE_SMOKE_BIN)

-include $(DEPS) $(PROLE_DEPS)

clean:
	rm -f pebc $(OBJS) $(DEPS) $(PROLE_OBJS) $(PROLE_DEPS) $(PROLE_SMOKE_BIN)

install: pebc
	install -d $(BINDIR)
	install -m 755 pebc $(BINDIR)
	cp -r std $(BINDIR)

uninstall:
	rm -f $(BINDIR)/pebc
	rm -rf $(BINDIR)/std
