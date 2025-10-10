CC = gcc
CFLAGS = -Wall -Wextra -g -I utils -I deps -I ast
OBJS = utils/alloc.o ast/ast.o lexer.o parser.o symbol.o type.o checker.o main.o

pebble: $(OBJS)
	$(CC) $(CFLAGS) -o pebble $(OBJS)

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f pebble $(OBJS)
