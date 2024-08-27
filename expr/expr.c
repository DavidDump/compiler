#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include "../src/common.h"

#define ARENA_IMPLEMENTATION
#include "../src/arena.h"

typedef struct String {
    u8* str;
    u64 length;
} String;

typedef enum Operand {
    Operand_NONE,
    Operand_RESERVED,
    Operand_PLUS,
    Operand_MINUS,
    Operand_MUL,
    Operand_DIV,
} Operand;

typedef enum TokenType {
    TokenType_NONE,
    TokenType_NUMBER,
    TokenType_PLUS,
    TokenType_MINUS,
    TokenType_MUL,
    TokenType_DIV,
} TokenType;

typedef struct Token {
    TokenType type;
    String value;
} Token;

typedef struct Primary {
    Token token;
} Primary;

typedef enum ExprType {
    ExprType_NONE,
    ExprType_Primary,
    ExprType_Expr,
} ExprType;

typedef struct Expr {
    ExprType type;
    union {
        Primary primary;
        struct {
            Operand op;
            struct Expr* lhs;
            struct Expr* rhs;
        } expr;
    };
} Expr;

void printOp(Operand op) {
    switch(op) {
        case Operand_NONE: break;
        case Operand_PLUS: {
            printf("+");
        } break;
        case Operand_MINUS: {
            printf("-");
        } break;
        case Operand_MUL: {
            printf("*");
        } break;
        case Operand_DIV: {
            printf("/");
        } break;
    }
}

void printExpr(Expr* value) {
    if(value->type == ExprType_Primary) {
        printf("%.*s", value->primary.token.value.length, value->primary.token.value.str);
    } else if(value->type == ExprType_Expr) {
        printExpr(value->expr.lhs);
        printOp(value->expr.op);
        printExpr(value->expr.rhs);
    } else {
        printf("[ERROR] NONE\n");
    }
}

typedef struct ParseContext {
    Token* tokens;
    u64 tokensCount;
    u64 tokensIndex;
    Arena mem;
} ParseContext;

Token parseNext(ParseContext* ctx) {
    if(ctx->tokensIndex + 1 > ctx->tokensCount) return (Token){0};
    return ctx->tokens[ctx->tokensIndex++];
}

Token parsePeek(ParseContext* ctx) {
    if(ctx->tokensIndex >= ctx->tokensCount) return (Token){0};
    return ctx->tokens[ctx->tokensIndex];
}

bool isOperator(TokenType token) {
    if(
        token == TokenType_PLUS ||
        token == TokenType_MINUS ||
        token == TokenType_MUL ||
        token == TokenType_DIV
    ) return TRUE;
    return FALSE;
}

s64 getPresedence(TokenType token) {
    if(token == TokenType_PLUS || token == TokenType_MINUS) return 5;
    if(token == TokenType_MUL || token == TokenType_DIV) return 10;
}

Expr* parsePrimary(ParseContext* ctx) {
    Expr* result = arena_alloc(&ctx->mem, sizeof(Expr));
    
    Token token = parseNext(ctx);
    if(token.type == TokenType_NUMBER) {
        result->type = ExprType_Primary;
        // NOTE: the value is 0 for now
        result->primary = (Primary){.token = token};
    } else {
        result->type = ExprType_NONE;
    }

    return result;
}

Expr* parseExpr_rec(ParseContext* ctx, Expr* lhs, s64 presedence) {
    Token token = parsePeek(ctx);
    while(isOperator(token.type)) {
        s64 tokenPresedence = getPresedence(token.type);
        if(tokenPresedence >= presedence) {
            Token op = token;
            parseNext(ctx);
            Expr* rhs = parsePrimary(ctx);
            token = parsePeek(ctx);
            while(isOperator(token.type)) {
                s64 newPresedence = getPresedence(token.type);
                if(newPresedence >= tokenPresedence) {
                    rhs = parseExpr_rec(ctx, lhs, tokenPresedence + (newPresedence > tokenPresedence ? 1 : 0));
                    token = parsePeek(ctx);
                } else {
                    break;
                }
            }
            Expr* node = arena_alloc(&ctx->mem, sizeof(Expr));
            node->type = ExprType_Expr;
            node->expr.lhs = lhs;
            node->expr.rhs = rhs;
            node->expr.op = op.type;
            lhs = node;
        } else {
            break;
        }
    }
    return lhs;
}

Expr* parseExpr(ParseContext* ctx) {
    Expr* lhs = parsePrimary(ctx);
    return parseExpr_rec(ctx, lhs, 0);
}

bool isNum(u8 c) {
    return ('0' <= c && c <= '9');
}

// bool isWhitespace(u8 c) {
//     return (c == ' ' || c == '\n' || c == '\t' || c == '\r' || c == '\v');
// }

typedef struct TokenizeResult {
    Token* tokens;
    u64 count;
} TokenizeResult;

TokenizeResult tokenize(Arena* mem, u8* str, u64 strLen) {
    TokenizeResult result = {0};
    result.tokens = arena_alloc(mem, sizeof(Token) * 0x100);
    
    for(u64 i = 0; i < strLen; ++i) {
        u8 c = str[i];
        if(c == '+') {
            result.tokens[result.count++] = (Token){.type = TokenType_PLUS, .value = (String){.str = &str[i], .length = 1}};
        } else if(c == '-') {
            result.tokens[result.count++] = (Token){.type = TokenType_MINUS, .value = (String){.str = &str[i], .length = 1}};
        } else if(c == '*') {
            result.tokens[result.count++] = (Token){.type = TokenType_MUL, .value = (String){.str = &str[i], .length = 1}};
        } else if(c == '/') {
            result.tokens[result.count++] = (Token){.type = TokenType_DIV, .value = (String){.str = &str[i], .length = 1}};
        } else if(isNum(c)) {
            u64 startIndex = i;
            while(isNum(c) && i + 1 < strLen) c = str[++i];
            if(i + 1 < strLen) --i;
            u64 endIndex = i;
            u64 len = (endIndex - startIndex) + 1;
            result.tokens[result.count++] = (Token){.type = TokenType_NUMBER, .value = (String){.str = &str[startIndex], .length = len}};
        }
    }

    return result;
}

int main(void) {
    u8* input = "1 + 1 / 2";
    u64 inputLen = strlen(input);
    Arena mem = {0};
    TokenizeResult tokens = tokenize(&mem, input, inputLen);

    ParseContext ctx = {
        .tokens = tokens.tokens,
        .tokensCount = tokens.count,
    };
    Expr* result = parseExpr(&ctx);

    printExpr(result);
    return 0;
}