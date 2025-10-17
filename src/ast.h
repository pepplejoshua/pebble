
#ifndef AST_H
#define AST_H

#include <stddef.h>  // For size_t
#include <stdbool.h> // For bool

// Forward declarations
typedef struct AstNode AstNode;
typedef struct Type Type;

// Source location for errors
typedef struct {
    const char *file;
    int line;
    int column;
} Location;

// Tagged union for AST nodes
typedef enum {
    AST_DECL_FUNCTION,
    AST_DECL_VARIABLE,
    AST_DECL_CONSTANT,
    AST_DECL_TYPE,

    AST_STMT_RETURN,
    AST_STMT_IF,
    AST_STMT_WHILE,
    AST_STMT_BLOCK,
    AST_STMT_EXPR,
    AST_STMT_ASSIGN,
    AST_STMT_PRINT,
    AST_STMT_BREAK,
    AST_STMT_CONTINUE,

    AST_EXPR_LITERAL_INT,
    AST_EXPR_LITERAL_FLOAT,
    AST_EXPR_LITERAL_STRING,
    AST_EXPR_LITERAL_BOOL,
    AST_EXPR_IDENTIFIER,
    AST_EXPR_BINARY_OP,
    AST_EXPR_UNARY_OP,
    AST_EXPR_CALL,
    AST_EXPR_INDEX,
    AST_EXPR_SLICE,
    AST_EXPR_MEMBER,
    AST_EXPR_TUPLE,
    AST_EXPR_STRUCT_LITERAL,
    AST_EXPR_ARRAY_LITERAL,
    AST_EXPR_IMPLICIT_CAST,

    // Type expressions
    AST_TYPE_NAMED,      // int, float, CustomType
    AST_TYPE_POINTER,    // *T
    AST_TYPE_ARRAY,      // [N]T
    AST_TYPE_SLICE,      // []T
    AST_TYPE_STRUCT,     // struct { ... }
    AST_TYPE_FUNCTION,   // fn(T1, T2) R
    AST_TYPE_TUPLE,      // (T1, T2, T3)
} AstKind;

// Binary/unary ops
typedef enum {
    BINOP_ADD, BINOP_SUB, BINOP_MUL, BINOP_DIV,
    BINOP_EQ, BINOP_NE, BINOP_LT, BINOP_LE, BINOP_GT, BINOP_GE,
    BINOP_AND, BINOP_OR
} BinaryOp;

typedef enum {
    UNOP_NEG, UNOP_NOT, UNOP_ADDR, UNOP_DEREF
} UnaryOp;

typedef struct {
  char *name;
  AstNode *type;
} FuncParam;

// The AST node struct
struct AstNode {
    AstKind kind;
    Location loc;
    Type *resolved_type;

    union {
        // Declarations
        struct {
          char *name;
          FuncParam *params;
          size_t param_count;
          AstNode *return_type;
          AstNode *body;
        } func_decl;
        struct { char *name; AstNode *type_expr; AstNode *init; } var_decl;
        struct { char *name; AstNode *type_expr; AstNode *value; } const_decl;
        struct { char *name; AstNode *type_expr; } type_decl;

        // Statements
        struct { AstNode *expr; } return_stmt;
        struct { AstNode *cond; AstNode *then_branch; AstNode *else_branch; } if_stmt;
        struct { AstNode *cond; AstNode *body; } while_stmt;
        struct { AstNode **stmts; size_t stmt_count; } block_stmt;
        struct { AstNode *expr; } expr_stmt;
        struct { AstNode *lhs; AstNode *rhs; } assign_stmt;
        struct { AstNode *expr; } print_stmt;

        // Expressions
        struct { long long value; } int_lit;
        struct { double value; } float_lit;
        struct { char *value; } str_lit;
        struct { bool value; } bool_lit;
        struct { char *name; } ident;
        struct { BinaryOp op; AstNode *left; AstNode *right; } binop;
        struct { UnaryOp op; AstNode *operand; } unop;
        struct { AstNode *func; AstNode **args; size_t arg_count; } call;
        struct { AstNode *array; AstNode *index; } index_expr;
        struct { AstNode *object; char *member; } member_expr;
        struct { AstNode *array; AstNode *start; AstNode *end; } slice_expr;
        struct {
          AstNode **elements;
          size_t element_count;
          Type* resolved_type;
        } tuple_expr;
        struct {
            char *type_name;        // "Point"
            char **field_names;     // ["x", "y"]
            AstNode **field_values; // [10, 20]
            size_t field_count;
        } struct_literal;
        struct {
          AstNode **elements;
          size_t element_count;
        } array_literal;
        struct {
          AstNode *expr;            // Expression being cast
          Type *target_type;
        } implicit_cast;

        // Type expressions
        struct { char *name; } type_named;
        struct { AstNode *base; } type_pointer;
        struct { AstNode *element; size_t size; } type_array;
        struct { AstNode *element; } type_slice;
        struct {
            char **field_names;
            AstNode **field_types;
            size_t field_count;
        } type_struct;
        struct {
            AstNode **param_types;
            size_t param_count;
            AstNode *return_type;
        } type_function;
        struct {
          AstNode **element_types;
          size_t element_count;
        } type_tuple;
    } data;
};

#endif
