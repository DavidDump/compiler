#ifndef COMP_LEXER_H
#define COMP_LEXER_H

#include "types.h"
#include "string.h"
#include "common.h"

typedef struct Location{
    String filename;
    int line;
    int collum;
} Location;

typedef enum TokenType{
    TokenType_NONE,
    
    // keywords
    TokenType_RETURN,        // return
    TokenType_IF,            // if
    TokenType_ELSE,          // else
    TokenType_LOOP,          // loop

    TokenType_TYPE,          // like u8
    TokenType_SEMICOLON,     // ;
    TokenType_COLON,         // :
    TokenType_RARROW,        // ->
    TokenType_LPAREN,        // (
    TokenType_RPAREN,        // )
    TokenType_LSCOPE,        // {
    TokenType_RSCOPE,        // }
    TokenType_LBRACKET,      // [
    TokenType_RBRACKET,      // ]
    TokenType_COMMA,         // ,
    TokenType_DOT,           // .
    TokenType_DOUBLEDOT,     // ..
    TokenType_TRIPLEDOT,     // ...
    TokenType_HASHTAG,       // #
    
    // operators
    TokenType_LESS,          // <
    TokenType_LESS_EQ,       // <=
    TokenType_GREATER,       // >
    TokenType_GREATER_EQ,    // >=
    TokenType_NOT_EQUALS,    // !=
    TokenType_ADD,           // +
    TokenType_SUB,           // -
    TokenType_MUL,           // *
    TokenType_DIV,           // /
    TokenType_COMPARISON,    // ==
    TokenType_ASSIGNMENT,    // =
    TokenType_DOUBLECOLON,   // ::
    TokenType_INITIALIZER,   // :=

    TokenType_IDENTIFIER,    // any string that is not a keyword
    TokenType_INT_LITERAL,   // 69
    TokenType_STRING_LIT,    // "Hello, world!"
    TokenType_BOOL_LITERAL,  // true and false
    
    TokenType_COUNT,
} TokenType;

#pragma GCC diagnostic ignored "-Wunused-variable"
static char* TokenTypeStr[TokenType_COUNT + 1] = {
    [TokenType_NONE]          = "NONE",
    
    [TokenType_RETURN]        = "RETURN",
    [TokenType_IF]            = "IF",
    [TokenType_ELSE]          = "ELSE",
    [TokenType_LOOP]          = "LOOP",
    [TokenType_TYPE]          = "TYPE",
    [TokenType_SEMICOLON]     = "SEMICOLON",
    [TokenType_COLON]         = "COLON",
    [TokenType_DOUBLECOLON]   = "DOUBLECOLON",
    [TokenType_INITIALIZER]   = "INITIALIZER",
    [TokenType_RARROW]        = "RARROW",
    [TokenType_LPAREN]        = "LPAREN",
    [TokenType_RPAREN]        = "RPAREN",
    [TokenType_LSCOPE]        = "LSCOPE",
    [TokenType_RSCOPE]        = "RSCOPE",
    [TokenType_LBRACKET]      = "LBRACKET",
    [TokenType_RBRACKET]      = "RBRACKET",
    [TokenType_COMMA]         = "COMMA",
    [TokenType_DOT]           = "DOT",
    [TokenType_DOUBLEDOT]     = "DOUBLEDOT",
    [TokenType_TRIPLEDOT]     = "TRIPLEDOT",
    [TokenType_HASHTAG]       = "HASHTAG",
    [TokenType_LESS]          = "LESS",
    [TokenType_LESS_EQ]       = "LESS_EQ",
    [TokenType_GREATER]       = "GREATER",
    [TokenType_GREATER_EQ]    = "GREATER_EQ",
    [TokenType_NOT_EQUALS]    = "NOT_EQUALS",
    [TokenType_ADD]           = "ADD",
    [TokenType_SUB]           = "SUB",
    [TokenType_MUL]           = "MUL",
    [TokenType_DIV]           = "DIV",
    [TokenType_ASSIGNMENT]    = "ASSIGNMENT",
    [TokenType_COMPARISON]    = "COMPARISON",
    [TokenType_IDENTIFIER]    = "IDENTIFIER",
    [TokenType_INT_LITERAL]   = "INT_LITERAL",
    [TokenType_STRING_LIT]    = "STRING_LIT",
    [TokenType_BOOL_LITERAL]  = "BOOL_LITERAL",
    
    [TokenType_COUNT]         = "COUNT",
};
#pragma GCC diagnostic pop

typedef struct Token{
    TokenType type;
    String value;
    Location loc;
} Token;

typedef struct TokenArray{
    Token* tokens;
    u64 size;
    u64 capacity;
    Arena mem;
} TokenArray;

TokenArray Tokenize(String source, String filenameCstring);
void TokensPrint(TokenArray* tokens);
void TokenPrint(Token t);

#endif // COMP_LEXER_H