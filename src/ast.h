#ifndef AST_H
#define AST_H

#include <stdbool.h> // For bool
#include <stddef.h>  // For size_t

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
  AST_DECL_EXTERN_FUNC,
  AST_DECL_EXTERN_TYPE,
  AST_DECL_EXTERN_BLOCK,
  AST_DECL_VARIABLE,
  AST_DECL_CONSTANT,
  AST_DECL_TYPE,
  AST_DECL_IMPORT,

  AST_STMT_RETURN,
  AST_STMT_IF,
  AST_STMT_WHILE,
  AST_STMT_LOOP,
  AST_STMT_FOR,
  AST_STMT_BLOCK,
  AST_STMT_EXPR,
  AST_STMT_ASSIGN,
  AST_STMT_PRINT,
  AST_STMT_BREAK,
  AST_STMT_CONTINUE,
  AST_STMT_CASE,
  AST_STMT_SWITCH,
  AST_STMT_DEFER,

  AST_EXPR_CONTEXT,
  AST_EXPR_LITERAL_INT,
  AST_EXPR_LITERAL_FLOAT,
  AST_EXPR_LITERAL_STRING,
  AST_EXPR_LITERAL_CHAR,
  AST_EXPR_LITERAL_BOOL,
  AST_EXPR_LITERAL_NIL,
  AST_EXPR_IDENTIFIER,
  AST_EXPR_BINARY_OP,
  AST_EXPR_UNARY_OP,
  AST_EXPR_CALL,
  AST_EXPR_INDEX,
  AST_EXPR_SLICE,
  AST_EXPR_MEMBER,
  AST_EXPR_MODULE_MEMBER,
  AST_EXPR_TUPLE,
  AST_EXPR_STRUCT_LITERAL,
  AST_EXPR_ARRAY_LITERAL,
  AST_EXPR_ARRAY_REPEAT,
  AST_EXPR_FUNCTION,
  AST_EXPR_IMPLICIT_CAST,
  AST_EXPR_SIZEOF,
  AST_EXPR_EXPLICIT_CAST,
  AST_EXPR_GROUPED_EXPR,
  AST_EXPR_SOME,          // some expr
  AST_EXPR_LITERAL_NONE,  // none
  AST_EXPR_FORCE_UNWRAP,  // expr!
  AST_EXPR_POSTFIX_INC,   // expr++

  // Type expressions
  AST_TYPE_NAMED,    // int, float, CustomType
  AST_TYPE_QUALIFIED_NAMED, // std::string
  AST_TYPE_POINTER,  // *T
  AST_TYPE_OPTIONAL, // ?T
  AST_TYPE_ARRAY,    // [N]T
  AST_TYPE_SLICE,    // []T
  AST_TYPE_STRUCT,   // struct { ... }
  AST_TYPE_FUNCTION, // fn(T1, T2) R
  AST_TYPE_TUPLE,    // (T1, T2, T3)
  AST_TYPE_ENUM,     // enum { IDENT*, ... }
} AstKind;

// Binary/unary ops
typedef enum {
  BINOP_ADD,
  BINOP_SUB,
  BINOP_MUL,
  BINOP_DIV,
  BINOP_EQ,
  BINOP_NE,
  BINOP_LT,
  BINOP_LE,
  BINOP_GT,
  BINOP_GE,
  BINOP_AND,
  BINOP_OR,
  BINOP_BIT_AND,
  BINOP_BIT_OR,
  BINOP_BIT_XOR,
  BINOP_BIT_SHL,
  BINOP_BIT_SHR,
} BinaryOp;

typedef enum { UNOP_NEG, UNOP_NOT, UNOP_ADDR, UNOP_DEREF, UNOP_BIT_NOT } UnaryOp;

typedef struct {
  char *name;
  AstNode *type;
  bool is_variadic;
} FuncParam;

// The AST node struct
struct AstNode {
  AstKind kind;
  Location loc;
  Type *resolved_type;

  union {
    // Declarations
    struct {
      AstNode *convention;
      char *name;
      char *qualified_name;
      FuncParam *params;
      size_t param_count;
      AstNode *return_type;
      AstNode *body;
    } func_decl;
    struct {
      char *name;
      char *qualified_name;
      AstNode *lib_name;
      FuncParam *params;
      size_t param_count;
      AstNode *return_type;
    } extern_func;
    struct {
      char *name;
      char *qualified_name;
    } extern_type;
    struct {
      AstNode *lib_name;
      AstNode **decls;
      size_t decls_count;
    } extern_block;
    struct {
      char *name;
      char *qualified_name;
      AstNode *type_expr;
      AstNode *init;
    } var_decl;
    struct {
      char *name;
      char *qualified_name;
      AstNode *type_expr;
      AstNode *value;
    } const_decl;
    struct {
      char *name;
      char *qualified_name;
      AstNode *type_expr;
    } type_decl;

    // Statements
    struct {
      AstNode *expr;
    } return_stmt;
    struct {
      AstNode *cond;
      AstNode *then_branch;
      AstNode *else_branch;
    } if_stmt;
    struct {
      AstNode *cond;
      AstNode *body;
    } while_stmt;
    struct {
      AstNode *start;
      AstNode *end;
      bool inclusive;
      AstNode *iterator_name;
      AstNode *body;
    } loop_stmt;
    struct {
      AstNode *init;
      AstNode *cond;
      AstNode *update;
      AstNode *body;
    } for_stmt;
    struct {
      AstNode **stmts;
      size_t stmt_count;
    } block_stmt;
    struct {
      AstNode *expr;
    } expr_stmt;
    struct {
      BinaryOp op;
      AstNode *lhs;
      AstNode *rhs;
    } assign_stmt;
    struct {
      AstNode **exprs;
      size_t expr_count;
    } print_stmt;
    struct {
      AstNode *switch_stmt; // Associated switch stmt
      AstNode *condition;
      AstNode *body;
    } case_stmt;
    struct {
      AstNode *condition;
      AstNode **cases;
      size_t case_count;
      // Optional
      AstNode *default_case;
    } switch_stmt;
    struct {
      AstNode *stmt;
    } defer_stmt;
    struct {
      AstNode *path_str;
    } import_stmt;

    // Expressions
    struct {
      long long value;
    } int_lit;
    struct {
      double value;
    } float_lit;
    struct {
      char *value;
    } str_lit;
    struct {
      char value;
    } char_lit;
    struct {
      bool value;
    } bool_lit;
    struct {
      char *name;
      char *qualified_name;
    } ident;
    struct {
      BinaryOp op;
      AstNode *left;
      AstNode *right;
    } binop;
    struct {
      UnaryOp op;
      AstNode *operand;
    } unop;
    struct {
      AstNode *func;
      AstNode **args;
      size_t arg_count;
    } call;
    struct {
      AstNode *array;
      AstNode *index;
    } index_expr;
    struct {
      AstNode *object;
      char *member;
    } member_expr;
    struct {
      AstNode *module;
      char *member;
    } mod_member_expr;
    struct {
      AstNode *operand;
    } postfix_inc;
    struct {
      AstNode *array;
      AstNode *start;
      AstNode *end;
    } slice_expr;
    struct {
      AstNode **elements;
      size_t element_count;
    } tuple_expr;
    struct {
      char *type_name;             // "Point"
      char *qualified_type_name;   // "main__Point"
      char **field_names;          // ["x", "y"]
      AstNode **field_values;      // [10, 20]
      size_t field_count;
    } struct_literal;
    struct {
      AstNode **elements;
      size_t element_count;
    } array_literal;
    struct {
      AstNode *value; // The expression to repeat
      size_t count;   // How many times (from integer literal)
    } array_repeat;
    struct {
      AstNode *convention;
      FuncParam *params;
      size_t param_count;
      AstNode *return_type;
      AstNode *body;
      char *symbol; // Name generated
    } func_expr;
    struct {
      AstNode *expr; // Expression being cast
      Type *target_type;
    } implicit_cast;
    struct {
      AstNode *type_expr; // type to get size of
    } sizeof_expr;
    struct {
      AstNode *expr; // Expression being cast
      AstNode *target_type;
    } explicit_cast;
    struct {
      AstNode *inner_expr;
    } grouped_expr;
    struct {
        AstNode *value;  // Expression being wrapped
    } some_expr;
    struct {
        AstNode *operand;  // Optional being unwrapped
    } force_unwrap;

    // Type expressions
    struct {
      char *name;
    } type_named;
    struct {
      char *mod_name;
      char *mem_name;
    } type_qualified_named;
    struct {
      AstNode *base;
    } type_pointer;
    struct {
      AstNode *base;
    } type_optional;
    struct {
      AstNode *element;
      size_t size;
    } type_array;
    struct {
      AstNode *element;
    } type_slice;
    struct {
      char **field_names;
      AstNode **field_types;
      size_t field_count;
    } type_struct;
    struct {
      char **variant_names;
      size_t variant_count;
    } type_enum;
    struct {
      AstNode *convention;
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
