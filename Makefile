CC = gcc
CFLAGS = -Wall -Wextra -g -I src

OBJS = src/alloc.o src/lexer.o src/parser.o src/options.o \
       src/symbol.o src/type.o src/checker.o src/codegen.o src/module.o \
       src/main.o

# Auto-generate dependencies
DEPS = $(OBJS:.o=.d)

pebc: $(OBJS)
	$(CC) $(CFLAGS) -o pebc $(OBJS)

%.o: %.c
	$(CC) $(CFLAGS) -MMD -MP -c $< -o $@

-include $(DEPS)

clean:
	rm -f pebc $(OBJS) $(DEPS)
