CC = gcc
CFLAGS = -Wall -Wextra -g -I src
OBJS = src/alloc.o src/ast.o src/lexer.o src/parser.o src/symbol.o src/type.o src/main.o

peb: $(OBJS)
	$(CC) $(CFLAGS) -o peb $(OBJS)
	rm -f $(OBJS)

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f peb $(OBJS)
