#ifndef COMP_LEXER_H
#define COMP_LEXER_H

#include "string.h"
#include "common.h"
#include "dataStructures.h"

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
    TokenType_STRUCT,        // struct

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
    TokenType_COMPARISON,    // == // TODO: rename to TokenType_EQUALS
    TokenType_ASSIGNMENT,    // =
    TokenType_DOUBLECOLON,   // ::
    TokenType_INITIALIZER,   // :=
    TokenType_AS,            // as

    TokenType_IDENTIFIER,    // any string that is not a keyword
    TokenType_INT_LITERAL,   // 69
    TokenType_STRING_LIT,    // "Hello, world!"
    TokenType_BOOL_LITERAL,  // true and false
    
    TokenType_COUNT,
} TokenType;

extern char* TokenTypeStr[TokenType_COUNT + 1];

typedef struct Token {
    TokenType type;
    String value;
    Location loc;
} Token;

defArray(Token);

Array(Token) Tokenize(Arena* mem, String source, String filenameCstring);

#ifdef COMP_DEBUG
void TokensPrint(Array(Token)* tokens);
void TokenPrint(Token t);
#endif // COMP_DEBUG

#endif // COMP_LEXER_H
