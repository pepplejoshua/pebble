PREFIX ?= /usr/local
BINDIR = $(PREFIX)/bin

CC = gcc
CFLAGS = -Wall -Wextra -g -I src

OBJS = src/alloc.o src/ast.o src/lexer.o src/parser.o src/options.o \
       src/symbol.o src/type.o src/checker.o src/codegen.o src/module.o \
       src/main.o

# Auto-generate dependencies
DEPS = $(OBJS:.o=.d)

# Tells make these dont product files
.PHONY: clean install uninstall

pebc: $(OBJS)
	$(CC) $(CFLAGS) -o pebc $(OBJS) -fsanitize=address -g

%.o: %.c
	$(CC) $(CFLAGS) -MMD -MP -c $< -o $@

-include $(DEPS)

clean:
	rm -f pebc $(OBJS) $(DEPS)

install: pebc
	install -d $(BINDIR)
	install -m 755 pebc $(BINDIR)
	cp -r std $(BINDIR)

uninstall:
	rm -f $(BINDIR)/pebc
	rm -rf $(BINDIR)/std
