#ifndef COMP_PARSER_NEW_H
#define COMP_PARSER_NEW_H

#include "types.h"
#include "arena.h"
#include "string.h"
#include "lexer.h"
#include "dataStructuresDefs.h"

typedef enum ASTNodeType {
    ASTNodeType_NONE,
    
    ASTNodeType_FUNCTION_DEF,
    ASTNodeType_FUNCTION_CALL,
    ASTNodeType_VAR_DECL,
    ASTNodeType_VAR_DECL_ASSIGN,
    ASTNodeType_VAR_REASSIGN,
    ASTNodeType_VAR_CONST,
    ASTNodeType_RET,
    ASTNodeType_IF,
    ASTNodeType_LOOP,
    ASTNodeType_BINARY_EXPRESSION,
    ASTNodeType_UNARY_EXPRESSION,
    ASTNodeType_INT_LIT,
    ASTNodeType_FLOAT_LIT,
    ASTNodeType_STRING_LIT,
    ASTNodeType_BOOL_LIT,
    ASTNodeType_SYMBOL,
    ASTNodeType_TYPE,
    ASTNodeType_COMPILER_INST,
    
    ASTNodeType_COUNT,
} ASTNodeType;

#pragma GCC diagnostic ignored "-Wunused-variable"
static char* ASTNodeTypeStr[ASTNodeType_COUNT + 1] = {
    [ASTNodeType_NONE]              = "NONE",
    
    [ASTNodeType_FUNCTION_DEF]      = "FUNCTION_DEF",
    [ASTNodeType_FUNCTION_CALL]     = "FUNCTION_CALL",
    [ASTNodeType_VAR_DECL]          = "VAR_DECL",
    [ASTNodeType_VAR_DECL_ASSIGN]   = "VAR_DECL_ASSIGN",
    [ASTNodeType_VAR_REASSIGN]      = "VAR_REASSIGN",
    [ASTNodeType_VAR_CONST]         = "VAR_CONST",
    [ASTNodeType_RET]               = "RET",
    [ASTNodeType_IF]                = "IF",
    [ASTNodeType_LOOP]              = "LOOP",
    [ASTNodeType_BINARY_EXPRESSION] = "BINARY_EXPRESSION",
    [ASTNodeType_UNARY_EXPRESSION]  = "UNARY_EXPRESSION",
    [ASTNodeType_INT_LIT]           = "INT_LIT",
    [ASTNodeType_FLOAT_LIT]         = "FLOAT_LIT,",
    [ASTNodeType_STRING_LIT]        = "STRING_LIT",
    [ASTNodeType_BOOL_LIT]          = "BOOL_LIT",
    [ASTNodeType_SYMBOL]            = "SYMBOL",
    [ASTNodeType_TYPE]              = "TYPE",
    [ASTNodeType_COMPILER_INST]     = "COMPILER_INST",

    [ASTNodeType_COUNT]             = "COUNT",
};
#pragma GCC diagnostic pop

typedef struct ASTNode ASTNode;
typedef ASTNode* ASTNodePtr;

defArray(ASTNodePtr);
defArray(String);

// used when defining arguments during function declaration
typedef struct FunctionArg {
    String id;
    ASTNode* type; // TODO: needs to be a type, types will be their own struct eventually
    ASTNode* initialValue; // the expression this argument should be initialized with
} FunctionArg;

defArray(FunctionArg);

typedef struct Scope Scope;
typedef Scope* ScopePtr;

defArray(ScopePtr);

typedef struct Scope {
    Arena mem;

    Scope* parent;
    Array(ScopePtr) children; // maybe?
    Array(String) symbols; // symbols defined in this scope
    Array(ASTNodePtr) statements;
} Scope;

typedef enum CompilerInstructionType {
    CompilerInstructionType_NONE,
    CompilerInstructionType_LIB,    // #library "kernel32.dll";
    CompilerInstructionType_EXTERN, // #extern GetStdHandle :: (handle: s64);
    CompilerInstructionType_COUNT,
} CompilerInstructionType;

typedef struct CompilerInstruction {
    CompilerInstructionType type;
    union {
        struct LIB {
            String libName;
        } LIB;
        struct EXTERN {
            String funcName;
        } EXTERN;
    } inst;
} CompilerInstruction;

typedef struct ConditionalBlock {
    ASTNode* expr;
    Scope* scope;
} ConditionalBlock;

defArray(ConditionalBlock);

typedef struct ASTNode {
    ASTNodeType type;
    union Node {
        struct FUNCTION_DEF {
            String identifier;
            ASTNode* type;
            Array(FunctionArg) args;
            Scope* scope;
            bool isExtern;
        } FUNCTION_DEF;
        struct FUNCTION_CALL {
            String identifier;
            Array(ASTNodePtr) args; // binary expressions should be their own struct
        } FUNCTION_CALL;
        struct VAR_DECL {
            String identifier;
            ASTNode* type;
        } VAR_DECL;
        struct VAR_DECL_ASSIGN {
            String identifier;
            ASTNode* type;
            ASTNode* expr;
        } VAR_DECL_ASSIGN;
        struct VAR_REASSIGN {
            String identifier;
            ASTNode* expr;
        } VAR_REASSIGN;
        struct VAR_CONST {
            String identifier;
            ASTNode* expr;
        } VAR_CONST;
        struct RET {
            ASTNode* expr;
        } RET;
        struct BINARY_EXPRESSION {
            String operator;
            ASTNode* rhs;
            ASTNode* lhs;
        } BINARY_EXPRESSION;
        struct UNARY_EXPRESSION {
            String operator;
            ASTNode* expr;
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
        struct IF {
            Array(ConditionalBlock) blocks;
            bool hasElse;
            Scope* elze;
        } IF;
        struct LOOP {
            ASTNode* expr;
            Scope* scope;
        } LOOP;
        struct SYMBOL {
            String identifier;
            ASTNode* type;
        } SYMBOL;
        struct TYPE {
            Type type;

            // NOTE: these maybe required later but for not all the information required about the type is in `Type type`
            //       info about the signedness or the size in case of structs or enums can be queried for,
            //       using functions like `typeSize()` or `typeBehaviour()`
            // u64 size;       // size on the stack in bytes
            // bool isSigned;  // signed or unsigned
            bool isArray;   // single value or array of values
            bool isDynamic; // dynamic or static array
            bool isPointer; // pointer to a type of value specified by this node
            u64 arraySize;  // size of static array
        } TYPE;
        struct COMPILER_INST {
            CompilerInstruction* inst;
        } COMPILER_INST;
    } node;
} ASTNode;

typedef struct ParseContext {
    Array(Token) tokens;
	u64 index;
    Hashmap(String, LibName) importLibraries;
    String currentImportLibraryName;
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

ASTNode* parseExpression(ParseContext* ctx, Arena* mem);
ASTNode* parseDecreasingPresedence(ParseContext* ctx, Arena* mem, s64 minPrec);
ASTNode* parseLeaf(ParseContext* ctx, Arena* mem);
ParseResult Parse(Array(Token) tokens, Arena* mem);
ExpressionEvaluationResult evaluate_expression(ASTNode* expr);
ASTNode* parseStatement(ParseContext* ctx, Arena* mem, Scope* parent);

#if COMP_DEBUG
void ASTPrint(Scope* root);
#endif // COMP_DEBUG

#endif // COMP_PARSER_NEW_H
