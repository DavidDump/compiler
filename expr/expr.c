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
    Operand_ADD,
    Operand_SUB,
    Operand_MUL,
    Operand_DIV,
} Operand;

typedef enum TokenType {
    TokenType_NONE,
    TokenType_NUMBER,
    TokenType_ADD,
    TokenType_SUB,
    TokenType_MUL,
    TokenType_DIV,
    TokenType_OPEN_PAREN,
    TokenType_CLOSE_PAREN,
    TokenType_IDENTIFIER,
} TokenType;

u8* TokenTypeStr[] = {
    [TokenType_NUMBER] = "NUMBER",
    [TokenType_ADD] = "ADD",
    [TokenType_SUB] = "SUB",
    [TokenType_MUL] = "MUL",
    [TokenType_DIV] = "DIV",
    [TokenType_OPEN_PAREN] = "OPEN_PAREN",
    [TokenType_CLOSE_PAREN] = "CLOSE_PAREN",
    [TokenType_IDENTIFIER] = "IDENTIFIER",
};

typedef struct Token {
    TokenType type;
    String value;
} Token;

typedef struct Primary {
    Token token;
} Primary;

typedef enum ExprType {
    ExprType_NONE,
    ExprType_Number,
    ExprType_Expr,
    ExprType_Variable,
} ExprType;

typedef struct Expr {
    ExprType type;
    union {
        Primary primary;
        Primary variable;
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
        case Operand_ADD: {
            printf("+");
        } break;
        case Operand_SUB: {
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
    if(value->type == ExprType_Number) {
        printf("%.*s", value->primary.token.value.length, value->primary.token.value.str);
    } else if(value->type == ExprType_Expr) {
        printExpr(value->expr.lhs);
        printOp(value->expr.op);
        printExpr(value->expr.rhs);
    } else {
        printf("[ERROR] NONE\n");
    }
}

void printTree(Expr* value, u64 indent) {
    if(value->type == ExprType_Number) {
        for(int i = 0; i < indent - 1; i++) printf("  ");
        printf("%.*s\n", value->primary.token.value.length, value->primary.token.value.str);
    } else if(value->type == ExprType_Expr) {
        for(int i = 0; i < indent; i++) printf("  ");
        printOp(value->expr.op);
        printf("\n");
        printTree(value->expr.lhs, indent + 1);
        printTree(value->expr.rhs, indent + 1);
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

bool isOperator(Token token) {
    if(
        token.type == TokenType_ADD ||
        token.type == TokenType_SUB ||
        token.type == TokenType_MUL ||
        token.type == TokenType_DIV
    ) return TRUE;
    return FALSE;
}

s64 getPresedence(Token token) {
    if(token.type == TokenType_ADD || token.type == TokenType_SUB) return 5;
    if(token.type == TokenType_MUL || token.type == TokenType_DIV) return 10;
}

Expr* parsePrimary(ParseContext* ctx) {
    Expr* result = arena_alloc(&ctx->mem, sizeof(Expr));
    
    Token token = parsePeek(ctx);
    if(token.type == TokenType_NUMBER) {
        parseNext(ctx);
        result->type = ExprType_Number;
        result->primary = (Primary){.token = token};
    } else {
        result->type = ExprType_NONE;
    }

    return result;
}

Expr* parseExpr_rec(ParseContext* ctx, Expr* lhs, s64 presedence) {
    Token token = parsePeek(ctx);
    while(isOperator(token)) {
        s64 tokenPresedence = getPresedence(token);
        if(tokenPresedence >= presedence) {
            Token op = token;
            parseNext(ctx);
            Expr* rhs = parsePrimary(ctx);
            if(rhs->type == ExprType_NONE) {
                printf("[ERROR] expresion error at: %i \"%.*s\"\n", ctx->tokensIndex, ctx->tokens[ctx->tokensIndex].value.length, ctx->tokens[ctx->tokensIndex].value.str);
                exit(EXIT_FAILURE);
            }
            token = parsePeek(ctx);
            while(isOperator(token)) {
                s64 newPresedence = getPresedence(token);
                if(newPresedence >= tokenPresedence) {
                    rhs = parseExpr_rec(ctx, lhs, tokenPresedence + (newPresedence > tokenPresedence ? 1 : 0));
                    if(rhs->type == ExprType_NONE) {
                        printf("[ERROR] expresion error at: %i \"%.*s\"\n", ctx->tokensIndex, ctx->tokens[ctx->tokensIndex].value.length, ctx->tokens[ctx->tokensIndex].value.str);
                        exit(EXIT_FAILURE);
                    }
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
    if(lhs->type == ExprType_NONE) {
        printf("[ERROR] expresion error at: %i \"%.*s\"\n", ctx->tokensIndex, ctx->tokens[ctx->tokensIndex].value.length, ctx->tokens[ctx->tokensIndex].value.str);
        exit(EXIT_FAILURE);
    }
    return parseExpr_rec(ctx, lhs, 0);
}

bool isNum(u8 c) {
    return ('0' <= c && c <= '9');
}

bool isWhitespace(u8 c) {
    return (c == ' ' || c == '\n' || c == '\t' || c == '\r' || c == '\v');
}

typedef struct TokenizeResult {
    Token* tokens;
    u64 count;
} TokenizeResult;

TokenizeResult tokenize(Arena* mem, u8* str, u64 strLen) {
    TokenizeResult result = {0};
    result.tokens = arena_alloc(mem, sizeof(Token) * 0x200);
    
    for(u64 i = 0; i < strLen; ++i) {
        u8 c = str[i];
        if(c == '+') {
            result.tokens[result.count++] = (Token){.type = TokenType_ADD, .value = (String){.str = &str[i], .length = 1}};
        } else if(c == '-') {
            result.tokens[result.count++] = (Token){.type = TokenType_SUB, .value = (String){.str = &str[i], .length = 1}};
        } else if(c == '*') {
            result.tokens[result.count++] = (Token){.type = TokenType_MUL, .value = (String){.str = &str[i], .length = 1}};
        } else if(c == '/') {
            result.tokens[result.count++] = (Token){.type = TokenType_DIV, .value = (String){.str = &str[i], .length = 1}};
        } else if(c == '(') {
            result.tokens[result.count++] = (Token){.type = TokenType_OPEN_PAREN, .value = (String){.str = &str[i], .length = 1}};
        } else if(c == ')') {
            result.tokens[result.count++] = (Token){.type = TokenType_CLOSE_PAREN, .value = (String){.str = &str[i], .length = 1}};
        } else if(isNum(c)) {
            u64 startIndex = i;
            while(isNum(c) && i + 1 < strLen) c = str[++i];
            if(!isNum(c)) --i;
            u64 endIndex = i;
            u64 len = (endIndex - startIndex) + 1;
            result.tokens[result.count++] = (Token){.type = TokenType_NUMBER, .value = (String){.str = &str[startIndex], .length = len}};
        } else if(isWhitespace(c)) {
            continue;
        } else {
            printf("[ERROR] unhandled char: %c\n", c);
            exit(EXIT_FAILURE);
        }
    }

    return result;
}

void tokensEncloseInParenthesis(TokenizeResult* tokens) {
    for(s64 i = tokens->count; i >= 0; i--) {
        tokens->tokens[i + 1] = tokens->tokens[i];
    }
    tokens->count += 2;
    tokens->tokens[0] = (Token){.type = TokenType_OPEN_PAREN};
    tokens->tokens[tokens->count - 1] = (Token){.type = TokenType_CLOSE_PAREN};
}

Expr* parseDecreasingPresedence(ParseContext* ctx, s64 min_prec);
Expr* parseExpression(ParseContext* ctx);

Operand toOperator(Token token) {
    return (Operand)token.type;
}

Expr* makeBinary(Arena* mem, Expr* left, Operand op, Expr* right) {
    Expr* node = arena_alloc(mem, sizeof(Expr));
    node->type = ExprType_Expr;
    node->expr.lhs = left;
    node->expr.rhs = right;
    node->expr.op = op;
    return node;
}

Expr* makeNumber(ParseContext* ctx, Token token) {
    parseNext(ctx);
    Expr* node = arena_alloc(&ctx->mem, sizeof(Expr));
    node->type = ExprType_Number;
    node->primary = (Primary){.token = token};
    return node;
}

Expr* makeVariable(ParseContext* ctx, Token token) {
    parseNext(ctx);
    Expr* node = arena_alloc(&ctx->mem, sizeof(Expr));
    node->type = ExprType_Variable;
    node->primary = (Primary){.token = token};
    return node;
}

Expr* parseLeaf(ParseContext* ctx) {
    Token next = parsePeek(ctx);
    
    // if is_string_literal(next) return make_string(next)
    if(next.type == TokenType_NUMBER)          return makeNumber(ctx, next);
    if(next.type == TokenType_IDENTIFIER)      return makeVariable(ctx, next);
    if(next.type == TokenType_OPEN_PAREN)      return parseExpression(ctx);
    // if is_function(next)       return make_function()

    printf("[ERROR] Unhandled input\n");
    exit(EXIT_FAILURE);
}

Expr* parseIncreasingPresedence(ParseContext* ctx, Expr* left, s64 min_prec) {
    Token next = parsePeek(ctx);
    if(!isOperator(next)) return left;

    s64 next_prec = getPresedence(next);
    if(next_prec <= min_prec) {
        return left;
    } else {
        parseNext(ctx);
        Expr* right = parseDecreasingPresedence(ctx, next_prec);
        return makeBinary(&ctx->mem, left, toOperator(next), right);
    }
}

Expr* parseDecreasingPresedence(ParseContext* ctx, s64 min_prec) {
    Expr* left = parseLeaf(ctx);
    
    while(TRUE) {
        Expr* node = parseIncreasingPresedence(ctx, left, min_prec);
        if(node == left) break;
        left = node;
    }
    return left;
}

Expr* parseExpression(ParseContext* ctx) {
    Token next = parsePeek(ctx);
    if(next.type == TokenType_OPEN_PAREN) {
        parseNext(ctx);
    }
    Expr* result = parseDecreasingPresedence(ctx, 0);
    next = parsePeek(ctx);
    if(next.type == TokenType_CLOSE_PAREN) {
        parseNext(ctx);
    }
    return result;
}

#include "printTree.c"

int main(void) {
    u8* tests[] = {
        "(8 + 4) * 2 - 6",
        "12 / (3 + 1) + 5",
        "(15 - 3) * (2 + 1)",
        "20 - (4 * 3) + 6 / 2",
        "(10 + 2) / 2 + (5 * 3)",
        "18 - (6 / 2) + (4 * 2)",
        "(7 + 5) * 2 - (9 / 3)",
        "30 / (5 + 5) + (6 * 2)",
        "(9 - 3) * (4 + 2) - 5",
        "25 - (10 / 2) + (3 * 4)",
        "1 / (1 + 2) + 3",
    };
    for(int i = 0; i < ARRAY_SIZE(tests); i++) {
        u8* input = tests[i];
        printf("parsing: %s\n", input);
        u64 inputLen = strlen(input);
        
        Arena mem = {0};
        TokenizeResult tokens = tokenize(&mem, input, inputLen);
        tokensEncloseInParenthesis(&tokens);

        // for(int i = 0; i < tokens.count; i++) {
        //     printf("%s\n", TokenTypeStr[tokens.tokens[i].type]);
        // }

        ParseContext ctx = {
            .tokens = tokens.tokens,
            .tokensCount = tokens.count,
        };
        // Expr* result = parseExpr(&ctx);
        Expr* result = parseExpression(&ctx);

        // printExpr(result);
        // printTree(result, 0);
        bst_print_tree(result);

        arena_free(&mem);
        arena_free(&ctx.mem);
    }
    return 0;
}
