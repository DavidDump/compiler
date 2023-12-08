#ifndef COMP_PARSER_NEW_H
#define COMP_PARSER_NEW_H

#include "arena.h"
#include "string.h"
#include "lexer.h"

typedef enum ASTNodeType{
    ASTNodeType_NONE,
    
    ASTNodeType_FUNCTION_DEF,
    ASTNodeType_FUNCTION_CALL,
    ASTNodeType_VAR_DECL,
    ASTNodeType_VAR_DECL_ASSIGN,
    ASTNodeType_VAR_REASSIGN,
    ASTNodeType_VAR_CONST,
    ASTNodeType_RET,
    ASTNodeType_IF,
    ASTNodeType_ELSE,
    ASTNodeType_ELSE_IF,
    ASTNodeType_LOOP,
    ASTNodeType_EXPRESION,
    ASTNodeType_INT_LIT,
    ASTNodeType_SYMBOL_RVALUE,
    ASTNodeType_TYPE,
    
    ASTNodeType_COUNT,
} ASTNodeType;

#pragma GCC diagnostic ignored "-Wunused-variable"
static char* ASTNodeTypeStr[ASTNodeType_COUNT + 1] = {
    [ASTNodeType_NONE]            = "NONE",
    
    [ASTNodeType_FUNCTION_DEF]    = "FUNCTION_DEF",
    [ASTNodeType_FUNCTION_CALL]   = "FUNCTION_CALL",
    [ASTNodeType_VAR_DECL]        = "VAR_DECL",
    [ASTNodeType_VAR_DECL_ASSIGN] = "VAR_DECL_ASSIGN",
    [ASTNodeType_VAR_REASSIGN]    = "VAR_REASSIGN",
    [ASTNodeType_VAR_CONST]       = "VAR_CONST",
    [ASTNodeType_RET]             = "RET",
    [ASTNodeType_IF]              = "IF",
    [ASTNodeType_ELSE]            = "ELSE",
    [ASTNodeType_ELSE_IF]         = "ELSE_IF",
    [ASTNodeType_LOOP]            = "LOOP",
    [ASTNodeType_EXPRESION]       = "EXPRESION",
    [ASTNodeType_INT_LIT]         = "INT_LIT",
    [ASTNodeType_SYMBOL_RVALUE]   = "SYMBOL_RVALUE",
    [ASTNodeType_TYPE]            = "TYPE",

    [ASTNodeType_COUNT]           = "COUNT",
};
#pragma GCC diagnostic pop

typedef struct _ASTNode ASTNode;

typedef struct Args{
    // the type of this node has to be:
    // on FUNCTION_DEF - VAR_DECL
    // on FUNCTION_CALL - INT_LIT or EXPRESION
    ASTNode** args;
    int size;
    int capacity;

    Arena mem;
} Args;

typedef struct StmtList{
    ASTNode** statements;
    int size;
    int capacity;

    Arena mem;
} StmtList;

typedef struct Scope{
    Arena mem;

    String* symbolTable;
    int symbolSize;
    int symbolCapacity;

    struct Scope* parent;

    struct Scope** children;
    int childrenSize;
    int childrenCapacity;

    StmtList stmts;
} Scope;

typedef struct _ASTNode{
    ASTNodeType type;
    union Node{
        struct FUNCTION_DEF {
            String identifier;
            ASTNode* type;
            Args args;
            Scope* scope;
        } FUNCTION_DEF;
        struct FUNCTION_CALL {
            String identifier;
            Args args;
        } FUNCTION_CALL;
        struct VAR_DECL {
            String identifier;
            ASTNode* type;
        } VAR_DECL;
        struct VAR_DECL_ASSIGN {
            String identifier;
            ASTNode* type;
            ASTNode* expresion;
        } VAR_DECL_ASSIGN;
        struct VAR_REASSIGN {
            String identifier;
            ASTNode* expresion;
        } VAR_REASSIGN;
        struct VAR_CONST {
            String identifier;
            String value;
        } VAR_CONST;
        struct RET {
            ASTNode* expresion;
        } RET;
        struct EXPRESION {
            String operator;
            ASTNode* rhs;
            ASTNode* lhs;
        } EXPRESION;
        struct INT_LIT {
            String value;
        } INT_LIT;
        struct IF {
            ASTNode* expresion;
            Scope* scope;
        } IF;
        struct ELSE {
            Scope* scope;
        } ELSE;
        struct ELSE_IF {
            ASTNode* expresion;
            Scope* scope;
        } ELSE_IF;
        struct LOOP {
            ASTNode* expresion;
            Scope* scope;
        } LOOP;
        struct SYMBOL_RVALUE {
            String identifier;
        } SYMBOL_RVALUE;
        struct TYPE {
            String symbol;
            
            bool array;
            int arraySize; // maybe ??
            bool dynamic; // is the array dynamic
        } TYPE;
    } node;
} ASTNode;

typedef struct ParseContext{
    TokenArray tokens;
	int index;
    
    TypeInformation* typeInfo;
    OperatorInformation* opsInfo;
} ParseContext;

void parseAddStatement(StmtList* list, ASTNode* node);
ParseContext ParseContextInit(TokenArray tokens, TypeInformation* typeInfo, OperatorInformation* opsInfo);
ASTNode* NodeInit(Arena* mem);
void ASTNodePrint(ASTNode* node, int indent);
void ASTPrint(Scope* root);
int OpGetPrecedence(ParseContext* ctx, String op);
Token parseConsume(ParseContext* ctx);
Token parsePeek(ParseContext* ctx, int num);
bool parseScopeContainsSymbol(Scope* scope, String symbol);
ASTNode* parseFunctionCall(ParseContext* ctx, Arena* mem, Scope* scope);
ASTNode* parsePrimary(ParseContext* ctx, Arena* mem, Scope* scope);
ASTNode* parseExpression_rec(ParseContext* ctx, Arena* mem, Scope* scope, ASTNode* lhs, int precedence);
ASTNode* parseExpression(ParseContext* ctx, Arena* mem, Scope* scope);
bool parseCheckSemicolon(ParseContext* ctx);
void parseScopeAddChild(Scope* parent, Scope* child);
void parseAddArg(Args* args, ASTNode* node);
Scope* parseScopeInit(Arena* mem, Scope* parent);
void parseScopeAddSymbol(Scope* scope, String symbol);
ASTNode* parseType(ParseContext* ctx, Arena* mem);
ASTNode* parseFunctionCall(ParseContext* ctx, Arena* mem, Scope* scope);
Args parseFunctionDeclArgs(ParseContext* ctx, Scope* scope);
Scope* Parse(ParseContext* ctx, Arena* mem);

#endif // COMP_PARSER_NEW_H