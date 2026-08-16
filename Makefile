PREFIX ?= /usr/local
BINDIR = $(PREFIX)/bin

CC = gcc
CFLAGS = -Wall -Wextra -g -I src

# OBJS = src/alloc.o src/ast.o src/lexer.o src/parser.o src/options.o \
#        src/symbol.o src/type.o src/checker.o src/codegen.o src/module.o \
#        src/diagnostics.o src/temp_alloc.o src/main.o

OBJS = src/alloc.o src/ast.o src/lexer.o src/parser2.o src/options.o \
       src/symbol.o src/type.o src/checker.o src/codegen.o src/module.o \
       src/diagnostics.o src/temp_alloc.o src/main.o

# Auto-generate dependencies
DEPS = $(OBJS:.o=.d)

# Tells make these dont product files
.PHONY: clean install uninstall

pebcv1: $(OBJS)
	$(CC) $(CFLAGS) -o pebcv1 $(OBJS) -fsanitize=address -g

%.o: %.c
	$(CC) $(CFLAGS) -MMD -MP -c $< -o $@

-include $(DEPS)

clean:
	rm -f pebcv1 $(OBJS) $(DEPS)

install: pebcv1
	install -d $(BINDIR)
	install -m 755 pebcv1 $(BINDIR)
	cp -r compiler/std $(BINDIR)

uninstall:
	rm -f $(BINDIR)/pebcv1
	rm -rf $(BINDIR)/std
