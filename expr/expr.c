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

#define STR(_cstr_) (String){.str = (u8*)(_cstr_), .length = sizeof(_cstr_) - 1}

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
    TokenType_COMMA,
    TokenType_LESS,
    TokenType_LESS_EQ,
    TokenType_GREATER,
    TokenType_GREATER_EQ,
    TokenType_EQUALS,
    TokenType_NOT_EQUALS,
    TokenType_ASSIGN,
} TokenType;

u8* TokenTypeStr[] = {
    [TokenType_NONE] = "NONE",
    [TokenType_NUMBER] = "NUMBER",
    [TokenType_ADD] = "ADD",
    [TokenType_SUB] = "SUB",
    [TokenType_MUL] = "MUL",
    [TokenType_DIV] = "DIV",
    [TokenType_OPEN_PAREN] = "OPEN_PAREN",
    [TokenType_CLOSE_PAREN] = "CLOSE_PAREN",
    [TokenType_IDENTIFIER] = "IDENTIFIER",
    [TokenType_COMMA] = "COMMA",
    [TokenType_LESS] = "LESS",
    [TokenType_LESS_EQ] = "LESS_EQ",
    [TokenType_GREATER] = "GREATER",
    [TokenType_GREATER_EQ] = "GREATER_EQ",
    [TokenType_EQUALS] = "EQUALS",
    [TokenType_NOT_EQUALS] = "NOT_EQUALS",
    [TokenType_ASSIGN] = "ASSIGN",
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
    ExprType_Function,
} ExprType;

typedef struct Expr Expr;

typedef struct Function {
    Token identifier;
    Expr** args;
    u64 argCount;
} Function;

typedef struct Expr {
    ExprType type;
    union {
        Primary primary;   // ExprType_Number
        Primary variable;  // ExprType_Variable
        Function function; // ExprType_Function
        struct {
            Token op;
            struct Expr* lhs;
            struct Expr* rhs;
        } expr;            // ExprType_Expr
    };
} Expr;

typedef struct TokenizeResult {
    Token* tokens;
    u64 count;
} TokenizeResult;

void printOp(Token op) {
    printf("%.*s", op.value.length, op.value.str);
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

void printToken(Token token) {
    printf("%s ", TokenTypeStr[token.type]);
}

void printTokens(TokenizeResult tokens) {
    for(int i = 0; i < tokens.count; ++i) {
        printToken(tokens.tokens[i]);
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

typedef struct Operator {
    TokenType type;
    s64 presedence;
} Operator;

Operator operators[] = {
    {.type = TokenType_LESS,       .presedence = 4},
    {.type = TokenType_LESS_EQ,    .presedence = 4},
    {.type = TokenType_GREATER,    .presedence = 4},
    {.type = TokenType_GREATER_EQ, .presedence = 4},
    {.type = TokenType_EQUALS,     .presedence = 4},
    {.type = TokenType_NOT_EQUALS, .presedence = 4},
    {.type = TokenType_ADD, .presedence = 5},
    {.type = TokenType_ADD, .presedence = 5},
    {.type = TokenType_SUB, .presedence = 5},
    {.type = TokenType_MUL, .presedence = 10},
    {.type = TokenType_DIV, .presedence = 10},
};

bool isOperator(Token token) {
    for(u64 i = 0; i < ARRAY_SIZE(operators); ++i) {
        if(token.type == operators[i].type) return TRUE;
    }
    return FALSE;
}

s64 getPresedence(Token token) {
    for(u64 i = 0; i < ARRAY_SIZE(operators); ++i) {
        if(token.type == operators[i].type) return operators[i].presedence;
    }
    return 0;
}

bool isNum(u8 c) {
    return ('0' <= c && c <= '9');
}

bool isWhitespace(u8 c) {
    return (c == ' ' || c == '\n' || c == '\t' || c == '\r' || c == '\v');
}

bool isLetter(u8 c) {
    return (('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z'));
}

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
        } else if(c == ',') {
            result.tokens[result.count++] = (Token){.type = TokenType_COMMA, .value = (String){.str = &str[i], .length = 1}};
        } else if(c == '<') {
            u64 startIndex = i;
            do c = str[++i]; while(isWhitespace(c));
            u64 endIndex = i;
            u64 len = (endIndex - startIndex) + 1;

            if(c == '=') {
                result.tokens[result.count++] = (Token){.type = TokenType_LESS_EQ, .value = (String){.str = &str[startIndex], .length = len}};
            } else {
                --i;
                result.tokens[result.count++] = (Token){.type = TokenType_LESS, .value = (String){.str = &str[startIndex], .length = 1}};
            }
        } else if(c == '>') {
            u64 startIndex = i;
            do c = str[++i]; while(isWhitespace(c));
            u64 endIndex = i;
            u64 len = (endIndex - startIndex) + 1;

            if(c == '=') {
                result.tokens[result.count++] = (Token){.type = TokenType_GREATER_EQ, .value = (String){.str = &str[startIndex], .length = len}};
            } else {
                --i;
                result.tokens[result.count++] = (Token){.type = TokenType_GREATER, .value = (String){.str = &str[startIndex], .length = 1}};
            }
        } else if(c == '=') {
            u64 startIndex = i;
            do c = str[++i]; while(isWhitespace(c));
            u64 endIndex = i;
            u64 len = (endIndex - startIndex) + 1;

            if(c == '=') {
                result.tokens[result.count++] = (Token){.type = TokenType_EQUALS, .value = (String){.str = &str[startIndex], .length = len}};
            } else {
                --i;
                result.tokens[result.count++] = (Token){.type = TokenType_ASSIGN, .value = (String){.str = &str[startIndex], .length = 1}};
            }
        } else if(c == '!') {
            u64 startIndex = i;
            do c = str[++i]; while(isWhitespace(c));
            u64 endIndex = i;
            u64 len = (endIndex - startIndex) + 1;

            if(c == '=') {
                result.tokens[result.count++] = (Token){.type = TokenType_NOT_EQUALS, .value = (String){.str = &str[startIndex], .length = len}};
            } else {
                printf("[ERROR] ! need to be followed by a =, found: \'%c\'\n", c);
                exit(EXIT_FAILURE);
            }
        } else if(isNum(c)) {
            u64 startIndex = i;
            while(isNum(c) && i + 1 < strLen) c = str[++i];
            if(!isNum(c)) --i;
            u64 endIndex = i;
            u64 len = (endIndex - startIndex) + 1;
            result.tokens[result.count++] = (Token){.type = TokenType_NUMBER, .value = (String){.str = &str[startIndex], .length = len}};
        } else if(isLetter(c)) {
            u64 startIndex = i;
            while(isLetter(c) && i + 1 < strLen) c = str[++i];
            if(!isLetter(c)) --i;
            u64 endIndex = i;
            u64 len = (endIndex - startIndex) + 1;
            result.tokens[result.count++] = (Token){.type = TokenType_IDENTIFIER, .value = (String){.str = &str[startIndex], .length = len}};
        } else if(isWhitespace(c)) {
            continue;
        } else {
            printf("[ERROR] unhandled char: %c\n", c);
            exit(EXIT_FAILURE);
        }
    }

    return result;
}

Expr* parseDecreasingPresedence(ParseContext* ctx, s64 min_prec);
Expr* parseExpression(ParseContext* ctx);

Expr* makeBinary(Arena* mem, Expr* left, Token op, Expr* right) {
    Expr* node = arena_alloc(mem, sizeof(Expr));
    node->type = ExprType_Expr;
    node->expr.lhs = left;
    node->expr.rhs = right;
    node->expr.op = op;
    return node;
}

Expr* makeNumber(ParseContext* ctx, Token token) {
    Expr* node = arena_alloc(&ctx->mem, sizeof(Expr));
    node->type = ExprType_Number;
    node->primary = (Primary){.token = token};
    return node;
}

Expr* makeVariable(ParseContext* ctx, Token token) {
    Expr* node = arena_alloc(&ctx->mem, sizeof(Expr));
    node->type = ExprType_Variable;
    node->primary = (Primary){.token = token};
    return node;
}

Expr* makeFunction(ParseContext* ctx, Token next) {
    parseNext(ctx); // opening parenthesis (

    Expr* result = arena_alloc(&ctx->mem, sizeof(Expr));
    result->type = ExprType_Function;
    result->function = (Function){
        .identifier = next,
        .args = arena_alloc(&ctx->mem, sizeof(Expr*) * 32), // TODO: 32 max args hardcoded here
        .argCount = 0,
    };

    Token token = parsePeek(ctx);
    while(token.type != TokenType_CLOSE_PAREN) {
        Expr* expr = parseExpression(ctx);
        result->function.args[result->function.argCount++] = expr;

        token = parseNext(ctx);
        if(token.type == TokenType_CLOSE_PAREN) break;
        if(token.type != TokenType_COMMA) {
            printf("[ERROR] Failed in function parsing, expected comma or closing parenthesis, got: %s ", TokenTypeStr[token.type]);
            exit(EXIT_FAILURE);
        }
    }

    return result;
}

bool isFunction(ParseContext* ctx, Token next) {
    if(next.type != TokenType_IDENTIFIER) return FALSE;
    Token token = parsePeek(ctx);
    if(token.type != TokenType_OPEN_PAREN) return FALSE;
    return TRUE;
}

Expr* parseLeaf(ParseContext* ctx) {
    Token next = parseNext(ctx);
    
    // if is_string_literal(next) return make_string(next)
    if(isFunction(ctx, next))                  return makeFunction(ctx, next);
    if(next.type == TokenType_NUMBER)          return makeNumber(ctx, next);
    if(next.type == TokenType_IDENTIFIER)      return makeVariable(ctx, next);
    if(next.type == TokenType_OPEN_PAREN) {
        Expr* result = parseExpression(ctx);
        Token token = parseNext(ctx);
        if(token.type != TokenType_CLOSE_PAREN) {
            printf("[ERROR] Expected closing paranthesis, got: %s ", TokenTypeStr[token.type]);
            exit(EXIT_FAILURE);
        }
        return result;
    }

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
        return makeBinary(&ctx->mem, left, next, right);
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
    return parseDecreasingPresedence(ctx, 0);
}

#include "printTree.c"

#define BUFF_SIZE 1024
#define formatf(_result_, _arena_, _format_, ...) do { \
    char buff[BUFF_SIZE] = {0}; \
    u64 len = snprintf(buff, BUFF_SIZE, (_format_), __VA_ARGS__); \
    u8* stringBuffer = arena_alloc((_arena_), len * sizeof(u8)); \
    memcpy(stringBuffer, buff, len); \
    (_result_).str = stringBuffer; \
    (_result_).length = len; \
} while(0)

// result should be in rax
String exprToAsm(Arena* mem, Expr* expr) {
    switch(expr->type) {
        case ExprType_NONE: {
            UNIMPLEMENTED("ExprType_NONE");
        } break;
        case ExprType_Number: {
            // mov rax, number
            Token number = expr->primary.token;

            String result = {0};
            formatf(result, mem, "mov rax, %.*s\n", number.value.length, number.value.str);
            return result;
        } break;
        case ExprType_Expr: {
            Token op = expr->expr.op;
            Expr* lhs = expr->expr.lhs;
            Expr* rhs = expr->expr.rhs;

            String first = {0};
            String firstPost = {0};
            String second = {0};
            String secondPost = {0};
            String extra = {0};

            char buff[BUFF_SIZE] = {0};
            u64 len = 0;
            if (op.type == TokenType_ADD) {
                first = exprToAsm(mem, lhs);
                firstPost = STR("push rax\n");
                second = exprToAsm(mem, rhs);
                secondPost = STR("pop rcx\n");
                len = snprintf(buff, BUFF_SIZE, "add rax, rcx\n");
            } else if (op.type == TokenType_SUB) {
                first = exprToAsm(mem, rhs);
                firstPost = STR("push rax\n");
                second = exprToAsm(mem, lhs);
                secondPost = STR("pop rcx\n");
                len = snprintf(buff, BUFF_SIZE, "sub rax, rcx\n");
            } else if (op.type == TokenType_MUL) {
                first = exprToAsm(mem, lhs);
                firstPost = STR("push rax\n");
                second = exprToAsm(mem, rhs);
                secondPost = STR("pop rcx\n");
                len = snprintf(buff, BUFF_SIZE, "mul rcx\n");
            } else if (op.type == TokenType_DIV) {
                first = exprToAsm(mem, rhs);
                firstPost = STR("push rax\n");
                second = exprToAsm(mem, lhs);
                secondPost = STR("pop rcx\n");
                extra = STR("mov rdx, 0\n");
                len = snprintf(buff, BUFF_SIZE, "div rcx\n");
            }

            // composing the final buffer
            u64 totalLen = first.length + firstPost.length + second.length + secondPost.length + extra.length + len;
            u8* stringBuffer = arena_alloc(mem, totalLen * sizeof(u8));
            u64 at = 0;
            memcpy(&stringBuffer[at], first.str, first.length);           at += first.length;
            memcpy(&stringBuffer[at], firstPost.str, firstPost.length);   at += firstPost.length;
            memcpy(&stringBuffer[at], second.str, second.length);         at += second.length;
            memcpy(&stringBuffer[at], secondPost.str, secondPost.length); at += secondPost.length;
            memcpy(&stringBuffer[at], extra.str, extra.length);           at += extra.length;
            memcpy(&stringBuffer[at], buff, len);                         at += len;

            return (String) {
                .str = stringBuffer,
                .length = totalLen,
            };
        } break;
        case ExprType_Variable: {
            // mov rax, [rbp - ...]
            Token var = expr->variable.token;

            String result = {0};
            formatf(result, mem, "mov rax, [rbp - ...] ; %.*s\n", var.value.length, var.value.str);
            return result;
        } break;
        case ExprType_Function: {
            // call function
            Function fn = expr->function;
            Token id = fn.identifier;

            String result = {0};
            formatf(result, mem, "call %.*s ; (args not finished)\n", id.value.length, id.value.str);
            return result;
        } break;
    }
}

int main(void) {
    u8* tests[] = {
        #if 0
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
        #else
        "asd + foo + dsa",
        "f()",

        "max((8 + 4) * 2, 20) - 6",
        "min(12 / (3 + 1), 5) + 1",
        "(15 - 3) * (2 + 1) > 30",
        "20 - (4 * 3) + 6 / 2 <= 10",
        "(10 + 2) / 2 + (5 * 3) == 21",
        "18 - (6 / 2) + (4 * 2) != 20",
        "(7 + 5) * 2 - (9 / 3) < 20",
        "30 / (5 + 5) + (6 * 2) >= 12",
        "(9 - 3) * (4 + 2) - 5 == 19",
        "25 - (10 / 2) + (3 * 4) > 30",
        #endif
    };
    for(int i = 0; i < ARRAY_SIZE(tests); i++) {
        u8* input = tests[i];
        printf("parsing: %s\n", input);
        u64 inputLen = strlen(input);
        
        Arena mem = {0};
        TokenizeResult tokens = tokenize(&mem, input, inputLen);
        // printTokens(tokens);
        // printf("\n");

        ParseContext ctx = {
            .tokens = tokens.tokens,
            .tokensCount = tokens.count,
        };

        Expr* result = parseExpression(&ctx);
        // bst_print_tree(result);

        String assembly = exprToAsm(&mem, result);
        printf("asm:\n");
        printf("%.*s", assembly.length, assembly.str);

        arena_free(&mem);
        arena_free(&ctx.mem);
    }
    return 0;
}

// TODO: string literals
// TODO: float literals
// TODO: boolean literals
// TODO: unary operators, ex.: minus, not
