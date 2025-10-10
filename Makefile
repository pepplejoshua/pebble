CC = gcc
CFLAGS = -Wall -Wextra -g -I utils -I deps
OBJS = utils/alloc.o lexer.o parser.o ast.o symbol.o type.o checker.o main.o

pebble: $(OBJS)
	$(CC) $(CFLAGS) -o pebble $(OBJS)

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f pebble $(OBJS)
