#ifndef COMP_PARSER_NEW_H
#define COMP_PARSER_NEW_H

#include "types.h"
#include "arena.h"
#include "string.h"
#include "lexer.h"
#include "dataStructuresDefs.h"

typedef struct Scope Scope;
typedef Scope* ScopePtr;
defArray(ScopePtr);

typedef struct Expression Expression;
typedef Expression* ExpressionPtr;
defArray(ExpressionPtr);

// used when defining arguments during function declaration
typedef struct FunctionArg {
    String id;
    TypeInfo* type;
    Expression* initialValue; // the expression this argument should be initialized with
} FunctionArg;

defArray(FunctionArg);

typedef enum ExpressionType {
    // complex nodes
    ExpressionType_BINARY_EXPRESSION, // lsh + rhs
    ExpressionType_UNARY_EXPRESSION,  // -expr

    // leaf nodes
    ExpressionType_INT_LIT,           // 1
    ExpressionType_FLOAT_LIT,         // 1.0
    ExpressionType_STRING_LIT,        // "string"
    ExpressionType_BOOL_LIT,          // true/false
    ExpressionType_SYMBOL,            // foo
    ExpressionType_FUNCTION_CALL,     // bar()
    ExpressionType_FUNCTION_LIT,      // (arg: u64) -> u8 { ... }
} ExpressionType;

#pragma GCC diagnostic ignored "-Wunused-variable"
static char* ExpressionTypeStr[] = {
    [ExpressionType_BINARY_EXPRESSION] = "BINARY_EXPRESSION",
    [ExpressionType_UNARY_EXPRESSION]  = "UNARY_EXPRESSION",
    [ExpressionType_INT_LIT]           = "INT_LIT",
    [ExpressionType_FLOAT_LIT]         = "FLOAT_LIT",
    [ExpressionType_STRING_LIT]        = "STRING_LIT",
    [ExpressionType_BOOL_LIT]          = "BOOL_LIT",
    [ExpressionType_SYMBOL]            = "SYMBOL",
    [ExpressionType_FUNCTION_CALL]     = "FUNCTION_CALL",
    [ExpressionType_FUNCTION_LIT]      = "FUNCTION_LIT",
};
#pragma GCC diagnostic pop

typedef struct Expression {
    ExpressionType type;
    union Expr {
        struct BINARY_EXPRESSION {
            Token operator;
            Expression* rhs;
            Expression* lhs;
        } BINARY_EXPRESSION;
        struct UNARY_EXPRESSION {
            Token operator;
            Expression* expr;
        } UNARY_EXPRESSION;
        struct INT_LIT {
            String value;
        } INT_LIT;
        struct FLOAT_LIT {
            String wholePart;
            String fractPart;
        } FLOAT_LIT;
        struct STRING_LIT {
            String value;
        } STRING_LIT;
        struct BOOL_LIT {
            String value;
        } BOOL_LIT;
        struct SYMBOL {
            String identifier;
        } SYMBOL;
        struct FUNCTION_CALL {
            String identifier;
            Array(ExpressionPtr) args;
        } FUNCTION_CALL;
        struct FUNCTION_LIT {
            TypeInfo* returnType;
            Array(FunctionArg) args;
            Scope* scope;
            bool isExtern;
        } FUNCTION_LIT;
    } expr;
} Expression;

typedef enum ASTNodeType {
    ASTNodeType_NONE,
    
    ASTNodeType_VAR_DECL,
    ASTNodeType_VAR_DECL_ASSIGN,
    ASTNodeType_VAR_REASSIGN,
    ASTNodeType_VAR_CONST,
    ASTNodeType_RET,
    ASTNodeType_IF,
    ASTNodeType_LOOP,
    ASTNodeType_EXPRESSION,
    ASTNodeType_DIRECTIVE,
    
    ASTNodeType_COUNT,
} ASTNodeType;

#pragma GCC diagnostic ignored "-Wunused-variable"
static char* ASTNodeTypeStr[ASTNodeType_COUNT + 1] = {
    [ASTNodeType_NONE]              = "NONE",
    
    [ASTNodeType_VAR_DECL]          = "VAR_DECL",
    [ASTNodeType_VAR_DECL_ASSIGN]   = "VAR_DECL_ASSIGN",
    [ASTNodeType_VAR_REASSIGN]      = "VAR_REASSIGN",
    [ASTNodeType_VAR_CONST]         = "VAR_CONST",
    [ASTNodeType_RET]               = "RET",
    [ASTNodeType_IF]                = "IF",
    [ASTNodeType_LOOP]              = "LOOP",
    [ASTNodeType_EXPRESSION]        = "EXPRESSION",
    [ASTNodeType_DIRECTIVE]         = "DIRECTIVE",

    [ASTNodeType_COUNT]             = "COUNT",
};
#pragma GCC diagnostic pop

typedef struct ASTNode ASTNode;
typedef ASTNode* ASTNodePtr;

defArray(ASTNodePtr);

defHashmapFuncs(String, ExpressionPtr)

typedef struct Scope {
    Arena mem;

    Scope* parent;
    Array(ScopePtr) children; // maybe?
    Array(String) symbols; // symbols defined in this scope
    Array(ASTNodePtr) statements;

    // used for typechecking;
    Hashmap(String, ExpressionPtr) constants;
} Scope;

typedef struct ConditionalBlock {
    Expression* expr;
    Scope* scope;
} ConditionalBlock;

defArray(ConditionalBlock);

// not really an ASTNode anymore, should be renamed to Statement
typedef struct ASTNode {
    ASTNodeType type;
    union Node {
        struct VAR_DECL {
            String identifier;
            TypeInfo* type;
        } VAR_DECL;
        struct VAR_DECL_ASSIGN {
            String identifier;
            TypeInfo* type;
            Expression* expr;
        } VAR_DECL_ASSIGN;
        struct VAR_REASSIGN {
            String identifier;
            Expression* expr;
        } VAR_REASSIGN;
        struct VAR_CONST {
            String identifier;
            Expression* expr;
        } VAR_CONST;
        struct RET {
            Expression* expr;
        } RET;
        struct IF {
            Array(ConditionalBlock) blocks;
            bool hasElse;
            Scope* elze;
        } IF;
        struct LOOP {
            Expression* expr;
            Scope* scope;
        } LOOP;
        struct EXPRESSION {
            Expression* expr;
        } EXPRESSION;
    } node;
} ASTNode;

typedef struct ParseContext {
    Array(Token) tokens;
	u64 index;
    Hashmap(String, LibName) importLibraries;
    String currentImportLibraryName;
    String currentSymbolName; // TODO: scuffed solution, so i dont have to pass the symbol name to `parseExpression()`
    Hashmap(String, FuncInfo) funcInfo;
} ParseContext;

typedef struct Operator {
    TokenType type;
    s64 presedence;
} Operator;

typedef struct ExpressionEvaluationResult {
    u64 result;
    bool isNegative;
} ExpressionEvaluationResult;

typedef struct ParseResult {
    Scope* globalScope;
    Hashmap(String, LibName) importLibraries;
    Hashmap(String, FuncInfo) funcInfo;
} ParseResult;

Expression* parseExpression(ParseContext* ctx, Arena* mem);
Expression* parseDecreasingPresedence(ParseContext* ctx, Arena* mem, s64 minPrec);
Expression* parseLeaf(ParseContext* ctx, Arena* mem);
ParseResult Parse(Array(Token) tokens, Arena* mem);
ExpressionEvaluationResult evaluate_expression(Expression* expr);
ASTNode* parseStatement(ParseContext* ctx, Arena* mem, Scope* parent);
Scope* parseScopeInit(Arena* mem, Scope* parent);
TypeInfo* parseType(ParseContext* ctx, Arena* mem);
bool isFunctionLit(ParseContext* ctx, Token next);

bool parseCheckSemicolon(ParseContext* ctx);
void parseScope2(ParseContext* ctx, Arena* mem, Scope* target);

#if COMP_DEBUG
void ASTPrint(Scope* root);
#endif // COMP_DEBUG

#endif // COMP_PARSER_NEW_H
// TODO: instead of storing symbol information (like function, variable, constant, declaration) in hashmaps
//       store it in arrays and every time there is a `String identifier` in the AST
//       just store the index to that symbol in the "global" table
//       hashmaps are always slower that arrays,
//       this way once parsing is done all symbols are referred to using indecies, which makes typecking and codegen faster
