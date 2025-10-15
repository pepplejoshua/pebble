CC = gcc
CFLAGS = -Wall -Wextra -g -I src

OBJS = src/alloc.o src/ast.o src/lexer.o src/parser.o src/tests.o \
       src/symbol.o src/type.o src/checker.o src/codegen.o src/main.o

# Auto-generate dependencies
DEPS = $(OBJS:.o=.d)

peb: $(OBJS)
	$(CC) $(CFLAGS) -o peb $(OBJS)

%.o: %.c
	$(CC) $(CFLAGS) -MMD -MP -c $< -o $@

-include $(DEPS)

clean:
	rm -f peb $(OBJS) $(DEPS)
