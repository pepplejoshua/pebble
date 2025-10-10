CC = gcc
CFLAGS = -Wall -Wextra -g
OBJS = alloc.o ast.o lexer.o parser.o symbol.o type.o checker.o main.o

pebble: $(OBJS)
	$(CC) $(CFLAGS) -o pebble $(OBJS)

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f pebble $(OBJS)
