CC = gcc
CFLAGS = -Wall -Wextra -g
# OBJS = alloc.o ast.o lexer.o parser.o symbol.o type.o checker.o main.o
OBJS = alloc.o ast.o symbol.o type.o main.o

peb: $(OBJS)
	$(CC) $(CFLAGS) -o peb $(OBJS)
	rm -f $(OBJS)

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f peb $(OBJS)
