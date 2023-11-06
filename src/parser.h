#ifndef COMP_PARSER_H
#define COMP_PARSER_H

#include "lexer.h"

typedef struct NodeExpresion NodeExpresion;

typedef struct NodeBinExpresion{
    Token* left;
    Token* operator;
    NodeExpresion* right;
} NodeBinExpresion;

typedef enum NodeExpresionType{
    NodeExpresionType_NONE,
    NodeExpresionType_INT_LIT,
    NodeExpresionType_BIN_EXP,
    NodeExpresionType_COUNT,
} NodeExpresionType;

typedef struct NodeExpresion{
    NodeExpresionType type;
    union{
        Token* intLit;
        NodeBinExpresion* binExp;
    };
} NodeExpresion;

typedef struct NodeKeywordRet{
    NodeExpresion* exp;
} NodeKeywordRet;

typedef struct NodeKeywordVarDefAssignment{
    Token* identifier;
    NodeExpresion* exp;
} NodeKeywordVarDefAssignment;

typedef enum NodeKeywordVarType{
    NodeKeywordVarType_NONE,
    NodeKeywordVarType_DEF_ONLY,
    NodeKeywordVarType_DEF_ASSIGNMENT,
    NodeKeywordVarType_COUNT,
} NodeKeywordVarType;

typedef struct NodeKeywordVar{
    NodeKeywordVarType type;
    union{
        Token* defIdentifier;
        NodeKeywordVarDefAssignment* assignment;
    };
} NodeKeywordVar;

typedef enum NodeKeywordType{
    NodeKeywordType_NONE,
    NodeKeywordType_RET,
    NodeKeywordType_VAR,
    NodeKeywordType_COUNT,
} NodeKeywordType;

typedef struct NodeKeyword{
    NodeKeywordType type;
    union{
        NodeKeywordRet* ret;
        NodeKeywordVar* var;
    };
} NodeKeyword;

typedef enum NodeStatementType{
    NodeStatementType_NONE,
    NodeStatementType_KEYWORD,
    NodeStatementType_EXPRESION,
    NodeStatementType_COUNT,
} NodeStatementType;

typedef struct NodeStatement{
    NodeStatementType type;
    union{
        NodeExpresion* exp;
        NodeKeyword* keyword;
    };
} NodeStatement;

typedef struct NodeRoot{
    NodeStatement* stmts;
    int count;
    int capacity;
} NodeRoot;

typedef struct Parser{
    TokenArray tokens;
    int index;
    Arena mem;
    NodeRoot root;
} Parser;

NodeRoot Parse(Parser* parser);

#pragma GCC diagnostic ignored "-Wunused-variable"
static char* NodeExpresionTypeStr[NodeExpresionType_COUNT + 1] = {
    [NodeExpresionType_NONE]    = "NONE",
    [NodeExpresionType_INT_LIT] = "INT_LIT",
    [NodeExpresionType_BIN_EXP] = "BIN_EXP",
    [NodeExpresionType_COUNT]   = "COUNT",
};

static char* NodeKeywordVarTypeStr[NodeKeywordVarType_COUNT + 1] = {
    [NodeKeywordVarType_NONE]           = "NONE",
    [NodeKeywordVarType_DEF_ONLY]       = "DEF_ONLY",
    [NodeKeywordVarType_DEF_ASSIGNMENT] = "DEF_ASSIGNMENT",
    [NodeKeywordVarType_COUNT]          = "COUNT",
};

static char* NodeKeywordTypeStr[NodeKeywordType_COUNT + 1] = {
    [NodeKeywordType_NONE]  = "NONE",
    [NodeKeywordType_RET]   = "RET",
    [NodeKeywordType_VAR]   = "VAR",
    [NodeKeywordType_COUNT] = "COUNT",
};

static char* NodeStatementTypeStr[NodeStatementType_COUNT + 1] = {
    [NodeStatementType_NONE]        = "NONE",
    [NodeStatementType_KEYWORD]     = "KEYWORD",
    [NodeStatementType_EXPRESION]   = "EXPRESION",
    [NodeStatementType_COUNT]       = "COUNT",
};
#pragma GCC diagnostic pop

Token* ParserPeek(Parser* parser, int offset);
Token* ParserConsume(Parser* parser);
NodeBinExpresion* ParseBinExpresion(Parser* parser);
NodeExpresion* ParseExpresion(Parser* parser);
NodeKeywordRet* ParseKeywordRet(Parser* parser);
NodeKeywordVar* ParseKeywordVar(Parser* parser);
void ParserAddStmt(Parser* parser, void* node, NodeStatementType type);

#endif // COMP_PARSER_H