#ifndef COMP_LEXER_H
#define COMP_LEXER_H

#include "structs.h"
#include "string.h"
#include "common.h"

typedef struct Tokenizer{
    String source;
    String filename;
    int index;

    TypeInformation* typeInfo;
    OperatorInformation* opsInfo;
} Tokenizer;

typedef struct Location{
    String filename;
    int line;
    int collum;
} Location;

typedef enum TokenType{
    TokenType_NONE,
    
    TokenType_RETURN,      // return
    TokenType_IF,          // if
    TokenType_ELSE,        // else
    TokenType_TYPE,        // like u8
    TokenType_SEMICOLON,   // ;
    TokenType_COLON,       // :
    TokenType_DOUBLECOLON, // ::
    TokenType_INITIALIZER, // :=
    TokenType_RARROW,      // ->
    TokenType_LPAREN,      // (
    TokenType_RPAREN,      // )
    TokenType_LSCOPE,      // {
    TokenType_RSCOPE,      // }
    TokenType_LBRACKET,    // [
    TokenType_RBRACKET,    // ]
    TokenType_COMMA,       // ,
    TokenType_OPERATOR,    // +
    TokenType_ASSIGNMENT,  // =
    TokenType_COMPARISON,  // ==
    TokenType_IDENTIFIER,  // any string that is not a keyword
    TokenType_INT_LITERAL, // 69
    
    TokenType_COUNT,
} TokenType;

#pragma GCC diagnostic ignored "-Wunused-variable"
static char* TokenTypeStr[TokenType_COUNT + 1] = {
    [TokenType_NONE]        = "NONE",
    
    [TokenType_RETURN]      = "RETURN",
    [TokenType_IF]          = "IF",
    [TokenType_ELSE]        = "ELSE",
    [TokenType_TYPE]        = "TYPE",
    [TokenType_SEMICOLON]   = "SEMICOLON",
    [TokenType_COLON]       = "COLON",
    [TokenType_DOUBLECOLON] = "DOUBLECOLON",
    [TokenType_INITIALIZER] = "INITIALIZER",
    [TokenType_RARROW]      = "RARROW",
    [TokenType_LPAREN]      = "LPAREN",
    [TokenType_RPAREN]      = "RPAREN",
    [TokenType_LSCOPE]      = "LSCOPE",
    [TokenType_RSCOPE]      = "RSCOPE",
    [TokenType_LBRACKET]    = "LBRACKET",
    [TokenType_RBRACKET]    = "RBRACKET",
    [TokenType_COMMA]       = "COMMA",
    [TokenType_OPERATOR]    = "OPERATOR",
    [TokenType_ASSIGNMENT]  = "ASSIGNMENT",
    [TokenType_COMPARISON]  = "COMPARISON",
    [TokenType_IDENTIFIER]  = "IDENTIFIER",
    [TokenType_INT_LITERAL] = "INT_LITERAL",
    
    [TokenType_COUNT]       = "COUNT",
};
#pragma GCC diagnostic pop

typedef struct Token{
    TokenType type;
    String value;
    Location loc;
} Token;

typedef struct TokenArray{
    Token* tokens;
    int size;
    int capacity;
    Arena mem;
} TokenArray;

TokenArray Tokenize(Tokenizer* tokenizer);
void TokensPrint(TokenArray* tokens);
void TokenPrint(Token t);
Tokenizer TokenizerInit(String source, String filename, TypeInformation* typeInfo, OperatorInformation* opsInfo);

char TokenizerPeek(Tokenizer* tokenizer, int offset);
char* TokenizerConsume(Tokenizer* tokenizer);
bool isLetter(char c);
bool isNumber(char c);
bool isWhitespace(char c);
bool isOperator(Tokenizer* tokenizer, char* c, int* len);
void TokenArrayAddToken(TokenArray* arr, String value, TokenType type, String filename, int line, int collum);

#endif // COMP_LEXER_H