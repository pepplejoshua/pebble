CC = gcc
CFLAGS = -Wall -Wextra -g -I src
# OBJS = alloc.o ast.o lexer.o parser.o symbol.o type.o checker.o main.o
OBJS = src/alloc.o src/ast.o src/symbol.o src/type.o src/main.o

peb: $(OBJS)
	$(CC) $(CFLAGS) -o peb $(OBJS)
	rm -f $(OBJS)

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f peb $(OBJS)
