#include "parser.h"
#include "common.h"

#include <stdlib.h> // exit(), EXIT_FAILURE
#include <stdio.h> // printf

char* ExpressionTypeStr[] = {
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

char* ASTNodeTypeStr[ASTNodeType_COUNT + 1] = {
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

ASTNode* NodeInit(Arena* mem){
    ASTNode* node = arena_alloc(mem, sizeof(ASTNode));
    assert(node, "Failed to allocate AST node");
    node->type = ASTNodeType_NONE; // NOTE: might not be necessary
    return node;
}

#ifdef COMP_DEBUG
#define genPrintHelper(...) do{for(u64 h = 0; h < indent; ++h) printf("    "); printf(__VA_ARGS__);}while(0)

void ASTNodePrint(ASTNode* node, u64 indent);

void TypePrint(TypeInfo* type, u64 indent) {
    UNUSED(indent);
    // UNIMPLEMENTED("TypePrint");
    
    printf("%s", TypeStr[type->symbolType]);
}

void ExpressionPrint(Expression* expr, u64 indent) {
    switch(expr->type) {
        case ExpressionType_INT_LIT: {
            String val = expr->expr.INT_LIT.value;
            printf(" "STR_FMT, STR_PRINT(val));
        } break;
        case ExpressionType_FLOAT_LIT: {
            String wholePart = expr->expr.FLOAT_LIT.wholePart;
            String fractPart = expr->expr.FLOAT_LIT.fractPart;
            printf(" "STR_FMT"."STR_FMT, STR_PRINT(wholePart), STR_PRINT(fractPart));
        } break;
        case ExpressionType_STRING_LIT: {
            String val = expr->expr.STRING_LIT.value;
            if(val.length > 50) printf(" "STR_FMT"...", 50, val.str);
            else                printf(" "STR_FMT, STR_PRINT(val));
        } break;
        case ExpressionType_BOOL_LIT: {
            String val = expr->expr.STRING_LIT.value;
            printf(" "STR_FMT, STR_PRINT(val));
        } break;
        case ExpressionType_SYMBOL: { // NOTE: identifier
            String id = expr->expr.SYMBOL.identifier;

            printf(" "STR_FMT, STR_PRINT(id));
        } break;        
        case ExpressionType_BINARY_EXPRESSION: {
            String op = expr->expr.BINARY_EXPRESSION.operator.value;
            ExpressionPrint(expr->expr.BINARY_EXPRESSION.lhs, indent + 1);
            printf(" "STR_FMT, STR_PRINT(op));
            ExpressionPrint(expr->expr.BINARY_EXPRESSION.rhs, indent + 1);
        } break;
        case ExpressionType_UNARY_EXPRESSION: {
            String op = expr->expr.UNARY_EXPRESSION.operator.value;
            printf(" "STR_FMT, STR_PRINT(op));
            ExpressionPrint(expr->expr.UNARY_EXPRESSION.expr, indent + 1);
        } break;
        case ExpressionType_FUNCTION_LIT: {
            // String id = expr->expr.FUNCTION_LIT.identifier;
            TypeInfo* retType = expr->expr.FUNCTION_LIT.returnType;
            Array(FunctionArg) args = expr->expr.FUNCTION_LIT.args;
            Scope* scope = expr->expr.FUNCTION_LIT.scope;

            genPrintHelper("FUNCTION_LIT: {\n");
            // genPrintHelper("    id: "STR_FMT",\n", STR_PRINT(id));
            genPrintHelper("    type: ");
            TypePrint(retType, indent);
            printf(",\n");

            // args
            if(args.size > 0) {
                genPrintHelper("    args: [\n");
                for(u64 i = 0; i < args.size; ++i) {
                    String argId = args.data[i].id;
                    TypeInfo* argType = args.data[i].type;

                    genPrintHelper("        {id: "STR_FMT", type: ", STR_PRINT(argId));
                    TypePrint(argType, indent);
                    printf("},\n");
                }
                genPrintHelper("    ],\n");
            } else {
                genPrintHelper("    args: [],\n");
            }
            
            // statements
            if(scope->statements.size > 0) {
                genPrintHelper("    statements: [\n");
                for(u64 i = 0; i < scope->statements.size; ++i) {
                    ASTNodePrint(scope->statements.data[i], indent + 2);
                }
                genPrintHelper("    ],\n");
            } else {
                genPrintHelper("    statements: [],\n");
            }
            genPrintHelper("}\n");
        } break;
        case ExpressionType_FUNCTION_CALL: {
            String id = expr->expr.FUNCTION_CALL.identifier;
            Array(ExpressionPtr) args = expr->expr.FUNCTION_CALL.args;

            genPrintHelper("FUNCTION_CALL: {\n");
            genPrintHelper("    id: "STR_FMT"\n", STR_PRINT(id));

            // args
            if(args.size > 0) {
                genPrintHelper("    args: [\n");
                for(u64 i = 0; i < args.size; ++i) {
                    for(u64 h = 0; h < indent + 2; ++h) printf("    ");
                    ExpressionPrint(args.data[i], indent + 1);
                    printf(",\n");
                }
                genPrintHelper("    ],\n");
            } else {
                genPrintHelper("    args: [],\n");
            }
            genPrintHelper("}\n");
        } break;
    }
}

void ASTNodePrint(ASTNode* node, u64 indent) {
    // TODO: need to fix indentation, everytime a case has a newline it needs to be indented
    switch(node->type) {
        case ASTNodeType_COUNT: break;
        case ASTNodeType_NONE: break;

        case ASTNodeType_VAR_DECL: {
            String id = node->node.VAR_DECL.identifier;
            TypeInfo* type = node->node.VAR_DECL.type;

            genPrintHelper("VAR_DECL: {\n");
            genPrintHelper("    id: "STR_FMT",\n", STR_PRINT(id));
            genPrintHelper("    type: ");
            TypePrint(type, indent);
            printf(",\n");
            genPrintHelper("}\n");
        } break;
        case ASTNodeType_VAR_DECL_ASSIGN: {
            String id = node->node.VAR_DECL_ASSIGN.identifier;
            TypeInfo* type = node->node.VAR_DECL_ASSIGN.type;
            Expression* expr = node->node.VAR_DECL_ASSIGN.expr;

            genPrintHelper("VAR_DECL_ASSIGN: {\n");
            genPrintHelper("    id: "STR_FMT",\n", STR_PRINT(id));
            genPrintHelper("    type: ");
            TypePrint(type, indent);
            printf(",\n");
            genPrintHelper("    expr:");
            ExpressionPrint(expr, indent + 1);
            printf(",\n");
            genPrintHelper("}\n");
        } break;
        case ASTNodeType_VAR_REASSIGN: {
            String id = node->node.VAR_REASSIGN.identifier;
            Expression* expr = node->node.VAR_REASSIGN.expr;

            genPrintHelper("VAR_REASSIGN: {\n");
            genPrintHelper("    id: "STR_FMT",\n", STR_PRINT(id));
            genPrintHelper("    expr:");
            ExpressionPrint(expr, indent + 1);
            printf(",\n");
            genPrintHelper("}\n");
        } break;
        case ASTNodeType_VAR_CONST: {
            String id = node->node.VAR_CONST.identifier;
            Expression* expr = node->node.VAR_CONST.expr;

            genPrintHelper("VAR_CONST: {\n");
            genPrintHelper("    id: "STR_FMT",\n", STR_PRINT(id));
            genPrintHelper("    expr:");
            ExpressionPrint(expr, indent + 1);
            printf(",\n");
            genPrintHelper("}\n");
        } break;
        case ASTNodeType_RET: {
            Expression* expr = node->node.RET.expr;

            genPrintHelper("RET: {\n");
            genPrintHelper("    expr:");
            ExpressionPrint(expr, indent + 1);
            printf(",\n");
            genPrintHelper("}\n");
        } break;
        case ASTNodeType_IF: {
            Array(ConditionalBlock) blocks = node->node.IF.blocks;
            bool hasElse = node->node.IF.hasElse;
            Scope* elze = node->node.IF.elze;

            // if block, always the 0 index
            {
                ConditionalBlock block = blocks.data[0];
                Expression* expr = block.expr;
                Scope* scope = block.scope;

                genPrintHelper("IF: {\n");
                genPrintHelper("    expr:");
                ExpressionPrint(expr, indent + 1);
                printf(",\n");

                // statements
                if(scope->statements.size > 0) {
                    genPrintHelper("    statements: [\n");
                    for(u64 i = 0; i < scope->statements.size; ++i) {
                        ASTNodePrint(scope->statements.data[i], indent + 2);
                    }
                    genPrintHelper("    ],\n");
                } else {
                    genPrintHelper("    statements: [],\n");
                }
                genPrintHelper("}\n");
            }

            // else if blocks
            for(u64 i = 1; i < blocks.size; ++i) {
                ConditionalBlock block = blocks.data[i];
                Expression* expr = block.expr;
                Scope* scope = block.scope;

                genPrintHelper("ELSE IF: {\n");
                genPrintHelper("    expr:");
                ExpressionPrint(expr, indent + 1);
                printf(",\n");

                // statements
                if(scope->statements.size > 0) {
                    genPrintHelper("    statements: [\n");
                    for(u64 h = 0; h < scope->statements.size; ++h) {
                        ASTNodePrint(scope->statements.data[h], indent + 2);
                    }
                    genPrintHelper("    ],\n");
                } else {
                    genPrintHelper("    statements: [],\n");
                }
                genPrintHelper("}\n");
            }

            if(hasElse) {
                Scope* scope = elze;
                // statements
                if(scope->statements.size > 0) {
                    genPrintHelper("    statements: [\n");
                    for(u64 i = 0; i < scope->statements.size; ++i) {
                        ASTNodePrint(scope->statements.data[i], indent + 2);
                    }
                    genPrintHelper("    ],\n");
                } else {
                    genPrintHelper("    statements: [],\n");
                }
                genPrintHelper("}\n");
            }
        } break;
        case ASTNodeType_LOOP: {
            Expression* expr = node->node.LOOP.expr;
            Scope* scope = node->node.LOOP.scope;

            genPrintHelper("LOOP: {\n");
            genPrintHelper("    expr:");
            ExpressionPrint(expr, indent + 1);
            printf(",\n");

            // statements
            if(scope->statements.size > 0) {
                genPrintHelper("    statements: [\n");
                for(u64 i = 0; i < scope->statements.size; ++i) {
                    ASTNodePrint(scope->statements.data[i], indent + 2);
                }
                genPrintHelper("    ],\n");
            } else {
                genPrintHelper("    statements: [],\n");
            }
            genPrintHelper("}\n");
        } break;
        case ASTNodeType_EXPRESSION: {
            Expression* expr = node->node.EXPRESSION.expr;
            ExpressionPrint(expr, indent);
        } break;
        case ASTNodeType_DIRECTIVE: break;
    }
}
#undef genPrintHelper

void ASTPrint(Scope* root){
    for(u64 i = 0; i < root->statements.size; ++i) {
        ASTNodePrint(root->statements.data[i], 0);
    }
}
#endif // COMP_DEBUG

// return what the parse context is pointong to then advance
Token parseConsume(ParseContext* ctx){
	if (ctx->index + 1 > ctx->tokens.size) return (Token){0};
	return ctx->tokens.data[ctx->index++];
}

// return what the parse context is pointon to without advancing
Token parsePeek(ParseContext* ctx, int num){
	if (ctx->index + num > ctx->tokens.size) return (Token){0};
	return ctx->tokens.data[ctx->index + num];
}

bool parseScopeContainsSymbol(Scope* scope, String symbol) {
    Scope* workingScope = scope;
    while(workingScope){
        for(u64 i = 0; i < workingScope->symbols.size; ++i) {
            if(StringEquals(workingScope->symbols.data[i], symbol)) {
                return TRUE;
            }
        }
        workingScope = workingScope->parent;
    }
    return FALSE;
}

Expression* makeBinary(Arena* mem, Expression* left, Token op, Expression* right) {
    Expression* result = arena_alloc(mem, sizeof(Expression));
    result->type = ExpressionType_BINARY_EXPRESSION;
    result->expr.BINARY_EXPRESSION.lhs = left;
    result->expr.BINARY_EXPRESSION.operator = op;
    result->expr.BINARY_EXPRESSION.rhs = right;
    return result;
}

Expression* makeNumber(ParseContext* ctx, Arena* mem, Token tok) {
    Expression* result = arena_alloc(mem, sizeof(Expression));

    if(tok.type == TokenType_INT_LITERAL) {
        Token next = parsePeek(ctx, 0);
        if(next.type == TokenType_DOT) {
            parseConsume(ctx); // DOT
            next = parsePeek(ctx, 0); // factional part
            
            String fractPart = {0};
            if (next.type == TokenType_INT_LITERAL) {
                parseConsume(ctx);
                fractPart = next.value;
            } else {
                fractPart = StringFromCstrLit("0");
            }

            result->type = ExpressionType_FLOAT_LIT;
            result->expr.FLOAT_LIT.wholePart = tok.value;
            result->expr.FLOAT_LIT.fractPart = fractPart;
        } else {
            result->type = ExpressionType_INT_LIT;
            result->expr.INT_LIT.value = tok.value;
        }
    } else if(tok.type == TokenType_DOT) {
        Token next = parsePeek(ctx, 0); // factional part
        
        String fractPart = {0};
        if (next.type == TokenType_INT_LITERAL) {
            parseConsume(ctx);
            fractPart = next.value;
        } else {
            fractPart = StringFromCstrLit("0");
        }

        result->type = ExpressionType_FLOAT_LIT;
        result->expr.FLOAT_LIT.wholePart = StringFromCstrLit("0");
        result->expr.FLOAT_LIT.fractPart = fractPart;
    }

    return result;
}

Expression* makeVariable(Arena* mem, Token identifier) {
    Expression* node = arena_alloc(mem, sizeof(Expression));
    node->type = ExpressionType_SYMBOL;
    node->expr.SYMBOL.identifier = identifier.value;
    return node;
}

Expression* makeFunctionCall(ParseContext* ctx, Arena* mem, Token next) {
    parseConsume(ctx); // opening parenthesis (

    Expression* result = arena_alloc(mem, sizeof(Expression));
    result->type = ExpressionType_FUNCTION_CALL;
    result->expr.FUNCTION_CALL.identifier = next.value;

    Token token = parsePeek(ctx, 0);
    if(token.type == TokenType_RPAREN) {
        parseConsume(ctx); // )
        return result;
    }

    Array(ExpressionPtr) args = {0};
    // NOTE: this could be while(TRUE) { ... }
    while(token.type != TokenType_RPAREN) {
        Expression* expr = parseExpression(ctx, mem);
        ArrayAppend(args, expr);

        token = parseConsume(ctx);
        if(token.type == TokenType_RPAREN) break;
        if(token.type != TokenType_COMMA) {
            printf("[ERROR] Failed in function parsing, expected comma or closing parenthesis, got: \"%s\"\n", TokenTypeStr[token.type]);
            exit(EXIT_FAILURE);
        }
    }
    result->expr.FUNCTION_CALL.args = args;

    return result;
}

Expression* makeString(Arena* mem, Token value) {
    Expression* result = arena_alloc(mem, sizeof(Expression));
    result->type = ExpressionType_STRING_LIT;
    result->expr.STRING_LIT.value = value.value;
    return result;
}

Expression* makeBool(Arena* mem, Token value) {
    Expression* result = arena_alloc(mem, sizeof(Expression));
    result->type = ExpressionType_BOOL_LIT;
    result->expr.BOOL_LIT.value = value.value;
    return result;
}

bool isFunctionCall(ParseContext* ctx, Token next) {
    if(next.type != TokenType_IDENTIFIER) return FALSE;
    Token token = parsePeek(ctx, 0);
    if(token.type != TokenType_LPAREN) return FALSE;
    return TRUE;
}

bool parseIsNumber(Token next) {
    return (next.type == TokenType_INT_LITERAL || next.type == TokenType_DOT);
}

// NOTE: for now the only unary prefix operator is: '-'
bool isUnaryOperator(Token next) {
    return (next.type == TokenType_SUB);
}

// NOTE: untested
Expression* makeUnary(ParseContext* ctx, Arena* mem, Token next) {
    Expression* result = arena_alloc(mem, sizeof(Expression));
    result->type = ExpressionType_UNARY_EXPRESSION;
    result->expr.UNARY_EXPRESSION.operator = next;
    result->expr.UNARY_EXPRESSION.expr = parseLeaf(ctx, mem);
    return result;
}

// ctx points to token after '('
Array(FunctionArg) functionArgs(ParseContext* ctx, Scope* scope) {
    Array(FunctionArg) result = {0};

    Token next = parseConsume(ctx);
    while(next.type != TokenType_RPAREN) {
        if(next.type != TokenType_IDENTIFIER) {
            ERROR(next.loc, "Function argument needs an identifier");
        }
        ArrayAppend(scope->symbols, next.value);

        Token id = next;
        next = parseConsume(ctx);
        if(next.type != TokenType_COLON) {
            ERROR(next.loc, "Identifier name and type have to be separated a colon \":\"");
        }

        FunctionArg arg = {0};
        arg.id = id.value;
        arg.type = parseType(ctx, &scope->mem);
        // arg.initialValue = ; // TODO: currently we dont support value initializer parsing
        ArrayAppend(result, arg);

        next = parseConsume(ctx);
        if(next.type == TokenType_COMMA) {
            next = parseConsume(ctx); // so that the next points at the identifier for the next iteration
            continue;
        } else if(next.type == TokenType_RPAREN) {
            break;
        } else {
            ERROR(next.loc, "Function declaration needs to end with a closing parenthesis \")\"");
        }
    }

    return result;
}

Expression* makeFunctionLit(ParseContext* ctx, Arena* mem, bool isExtern) {
    Expression* result = arena_alloc(mem, sizeof(Expression));
    result->type = ExpressionType_FUNCTION_LIT;

    Scope* functionScope = parseScopeInit(mem, NULL); // TODO: fix parent scope
    result->expr.FUNCTION_LIT.scope = functionScope;
    result->expr.FUNCTION_LIT.isExtern = isExtern;
    result->expr.FUNCTION_LIT.args = functionArgs(ctx, functionScope);

    // ret type
    Token next = parsePeek(ctx, 0);
    if(next.type == TokenType_RARROW) {
        parseConsume(ctx); // ->
        result->expr.FUNCTION_LIT.returnType = parseType(ctx, mem);
    } else {
        result->expr.FUNCTION_LIT.returnType = TypeInitSimple(mem, TYPE_VOID);
    }

    // scope
    if(!isExtern) {
        parseScope2(ctx, mem, functionScope);
    } else {
        // NOTE: this is kind of a hack but if it works it twerks
        parseCheckSemicolon(ctx);
    }

    return result;
}

// these are the compiler instructions that can prefix literals
Expression* makeCompInstructionLeaf(ParseContext* ctx, Arena* mem) {
    Token next = parseConsume(ctx);
    if(StringEqualsCstr(next.value, "extern")) {
        next = parseConsume(ctx);
        if(next.type != TokenType_LPAREN) {
            ERROR(next.loc, "#extern needs to be followed by a function signature.");
        }

        Expression* result = makeFunctionLit(ctx, mem, TRUE);

        LibName lib = {0};
        if(!HashmapGet(String, LibName)(&ctx->importLibraries, ctx->currentImportLibraryName, &lib)) {
            // #library "kernel32.dll"
            UNREACHABLE_VA("library not found: "STR_FMT, STR_PRINT(ctx->currentImportLibraryName));
        }
        FuncName funcName = {0};
        if(!HashmapSet(String, FuncName)(lib.functions, ctx->currentSymbolName, funcName)) {
            UNREACHABLE_VA("failed to insert into hashmap, cap: %llu, count: %llu, key: "STR_FMT, lib.functions->capacity, lib.functions->size, STR_PRINT(ctx->currentSymbolName));
        }

        return result;
    } else {
        ERROR_VA(next.loc, "Unknown compiler instruction: "STR_FMT, STR_PRINT(next.value));
    }

    // NOTE: silencing compiler warning
    return 0;
}

Expression* parseLeaf(ParseContext* ctx, Arena* mem) {
    Token next = parseConsume(ctx);

    if(isFunctionLit(ctx, next))            return makeFunctionLit(ctx, mem, FALSE);
    if(isFunctionCall(ctx, next))           return makeFunctionCall(ctx, mem, next);
    if(parseIsNumber(next))                 return makeNumber(ctx, mem, next);
    if(isUnaryOperator(next))               return makeUnary(ctx, mem, next);
    if(next.type == TokenType_HASHTAG)      return makeCompInstructionLeaf(ctx, mem);
    if(next.type == TokenType_BOOL_LITERAL) return makeBool(mem, next);
    if(next.type == TokenType_STRING_LIT)   return makeString(mem, next);
    if(next.type == TokenType_IDENTIFIER)   return makeVariable(mem, next);
    if(next.type == TokenType_LPAREN) {
        Expression* result = parseExpression(ctx, mem);
        Token token = parseConsume(ctx);
        if(token.type != TokenType_RPAREN) {
            ERROR_VA(token.loc, "Expected closing paranthesis, got: %s", TokenTypeStr[token.type]);
        }
        return result;
    }

    ERROR_VA(next.loc, "Unhandled input: "STR_FMT, STR_PRINT(next.value));
    // NOTE: silencing compiler warning
    return 0;
}

// TODO: move somewhere more sane
Operator operators[] = {
    {.type = TokenType_LESS,       .presedence = 4},
    {.type = TokenType_LESS_EQ,    .presedence = 4},
    {.type = TokenType_GREATER,    .presedence = 4},
    {.type = TokenType_GREATER_EQ, .presedence = 4},
    {.type = TokenType_COMPARISON, .presedence = 4},
    {.type = TokenType_NOT_EQUALS, .presedence = 4},
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

Expression* parseIncreasingPresedence(ParseContext* ctx, Arena* mem, Expression* left, s64 minPrec) {
    Token next = parsePeek(ctx, 0);
    if(!isOperator(next)) return left;

    s64 nextPrec = getPresedence(next);
    if(nextPrec <= minPrec) {
        return left;
    } else {
        parseConsume(ctx);
        Expression* right = parseDecreasingPresedence(ctx, mem, nextPrec);
        return makeBinary(mem, left, next, right);
    }
}

Expression* parseDecreasingPresedence(ParseContext* ctx, Arena* mem, s64 minPrec) {
    Expression* left = parseLeaf(ctx, mem);
    
    while(TRUE) {
        Expression* node = parseIncreasingPresedence(ctx, mem, left, minPrec);
        if(node == left) break;
        left = node;
    }
    return left;
}

Expression* parseExpression(ParseContext* ctx, Arena* mem) {
    return parseDecreasingPresedence(ctx, mem, 0);
}

bool parseCheckSemicolon(ParseContext* ctx) {
    Token next = parseConsume(ctx);
    if(next.type != TokenType_SEMICOLON){
        ERROR_VA(next.loc, "Statement needs to end with ; got: %s", TokenTypeStr[next.type]);
    }
    return TRUE;
}

Scope* parseScopeInit(Arena* mem, Scope* parent) {
    Scope* result = arena_alloc(mem, sizeof(Scope));
    result->parent = parent;
    HashmapInit(result->constants, 0x100); // TODO: a more sane default size

    if(parent) ArrayAppend(parent->children, result);

    return result;
}

TypeInfo* parseType(ParseContext* ctx, Arena* mem) {
    TypeInfo* result = arena_alloc(mem, sizeof(TypeInfo));

    Token tok = parseConsume(ctx);
    if(tok.type != TokenType_TYPE) {
        ERROR_VA(tok.loc, "Invalid type: "STR_FMT, STR_PRINT(tok.value));
    }

    // TODO: this could be a hashmap with key `String` value `Type` and just look up the type there
    Type type = TYPE_NONE;
    if(StringEqualsCstr(tok.value, "u8")) {
        type = TYPE_U8;
    } else if(StringEqualsCstr(tok.value, "u16")) {
        type = TYPE_U16;
    } else if(StringEqualsCstr(tok.value, "u32")) {
        type = TYPE_U32;
    } else if(StringEqualsCstr(tok.value, "u64")) {
        type = TYPE_U64;
    } else if(StringEqualsCstr(tok.value, "s8")) {
        type = TYPE_S8;
    } else if(StringEqualsCstr(tok.value, "s16")) {
        type = TYPE_S16;
    } else if(StringEqualsCstr(tok.value, "s32")) {
        type = TYPE_S32;
    } else if(StringEqualsCstr(tok.value, "s64")) {
        type = TYPE_S64;
    } else if(StringEqualsCstr(tok.value, "f32")) {
        type = TYPE_F32;
    } else if(StringEqualsCstr(tok.value, "f64")) {
        type = TYPE_F64;
    } else if(StringEqualsCstr(tok.value, "string")) {
        type = TYPE_STRING;
    } else if(StringEqualsCstr(tok.value, "bool")) {
        type = TYPE_BOOL;
    } else if(StringEqualsCstr(tok.value, "void")) {
        type = TYPE_VOID;
    } else {
        ERROR_VA(tok.loc, "Unknown type: "STR_FMT, STR_PRINT(tok.value));
    }

    bool isPointer = FALSE;
    Token next = parsePeek(ctx, 0);
    if(next.type == TokenType_MUL) {
        parseConsume(ctx); // *
        isPointer = TRUE;
    }

    tok = parsePeek(ctx, 0);
    if(tok.type != TokenType_LBRACKET) {
        result->symbolType = type;
        result->isPointer = isPointer;
        return result;
    }
    parseConsume(ctx); // '['

    ArrayInfo arrayInfo = {0};
    tok = parseConsume(ctx);
    if(tok.type == TokenType_TRIPLEDOT) {
        arrayInfo.isDynamic = TRUE;
    }else if(tok.type == TokenType_INT_LITERAL) {
        u64 size = StringToU64(tok.value);
        arrayInfo.arraySize = size;
    }else{
        ERROR(tok.loc, "Array needs a fixed size or '...' for dynamic size");
    }

    tok = parseConsume(ctx);
    if(tok.type != TokenType_RBRACKET){
        ERROR(tok.loc, "Expected closing pair to square bracket ']'");
    }

    result->symbolType = TYPE_ARRAY;
    result->arrayInfo = arrayInfo;
    result->isPointer = isPointer;
    return result;
}

// TODO: not staying
ExpressionEvaluationResult evaluate_expression(Expression* expr) {
    ExpressionEvaluationResult result = {0};

    if (expr->type == ExpressionType_INT_LIT) {
        String value = expr->expr.INT_LIT.value;
        result.result = StringToU64(value);
    } else if (expr->type == ExpressionType_FLOAT_LIT) {
        UNIMPLEMENTED("float in const");
    } else if (expr->type == ExpressionType_BOOL_LIT) {
        UNIMPLEMENTED("bool in const");
    } else if (expr->type == ExpressionType_STRING_LIT) {
        UNIMPLEMENTED("string in const");
    } else if (expr->type == ExpressionType_FUNCTION_CALL) {
        UNIMPLEMENTED("function call in const");
    } else if (expr->type == ExpressionType_BINARY_EXPRESSION) {
        String op = expr->expr.BINARY_EXPRESSION.operator.value;
        Expression* lhs = expr->expr.BINARY_EXPRESSION.lhs;
        Expression* rhs = expr->expr.BINARY_EXPRESSION.rhs;
        if(StringEqualsCstr(op, "+")) {
            ExpressionEvaluationResult lhsResult = evaluate_expression(lhs);
            ExpressionEvaluationResult rhsResult = evaluate_expression(rhs);
            // TODO: check if there is no overflow
            result.result = lhsResult.result + rhsResult.result;
        } else if(StringEqualsCstr(op, "-")) {
            ExpressionEvaluationResult lhsResult = evaluate_expression(lhs);
            ExpressionEvaluationResult rhsResult = evaluate_expression(rhs);
            // TODO: check if there is no overflow
            result.result = lhsResult.result - rhsResult.result;
        } else if(StringEqualsCstr(op, "*")) {
            ExpressionEvaluationResult lhsResult = evaluate_expression(lhs);
            ExpressionEvaluationResult rhsResult = evaluate_expression(rhs);
            // TODO: check if there is no overflow
            result.result = lhsResult.result * rhsResult.result;
        } else if(StringEqualsCstr(op, "/")) {
            ExpressionEvaluationResult lhsResult = evaluate_expression(lhs);
            ExpressionEvaluationResult rhsResult = evaluate_expression(rhs);
            // TODO: check if there is no overflow
            result.result = lhsResult.result / rhsResult.result;
        }
    } else if (expr->type == ExpressionType_UNARY_EXPRESSION) {
        String op = expr->expr.UNARY_EXPRESSION.operator.value;
        Expression* subExpr = expr->expr.UNARY_EXPRESSION.expr;
        if(StringEqualsCstr(op, "-")) {
            result = evaluate_expression(subExpr);
            result.isNegative = !result.isNegative;
        } else {
            printf("[ERROR] only unary operator implemented is -\n");
            exit(EXIT_FAILURE);
        }
    } else {
        printf("[ERROR] constant expression needs to be of constrant value\n");
        exit(EXIT_FAILURE);
    }

    return result;
}

// if the context points to a `{` parse multiple statements,
// else only add one statement to the scope
// target is the scope that the statements of the scope will be parsed into
void parseScope2(ParseContext* ctx, Arena* mem, Scope* target) {
    // scope isnt enclosed by {} instead its just a single statement
    Token next = parsePeek(ctx, 0);
    if(next.type != TokenType_LSCOPE) {
        ASTNode* statement = parseStatement(ctx, mem, target);
        if(statement->type == ASTNodeType_VAR_CONST) {
            String id = statement->node.VAR_CONST.identifier;
            Expression* expr = statement->node.VAR_CONST.expr;
            if(!HashmapSet(String, ExpressionPtr)(&target->constants, id, expr)) {
                UNREACHABLE_VA("failed to insert into hashmap, cap: %llu, count: %llu, key: "STR_FMT, target->constants.capacity, target->constants.size, STR_PRINT(id));
            }
        } else {
            ArrayAppend(target->statements, statement);
        }
        return;
    }

    // scope consists of multiple statements enclosed by {}
    parseConsume(ctx); // {
    while(next.type != TokenType_RSCOPE) {
        ASTNode* statement = parseStatement(ctx, mem, target);
        if(statement->type == ASTNodeType_VAR_CONST) {
            String id = statement->node.VAR_CONST.identifier;
            Expression* expr = statement->node.VAR_CONST.expr;
            if(!HashmapSet(String, ExpressionPtr)(&target->constants, id, expr)) {
                UNREACHABLE_VA("failed to insert into hashmap, cap: %llu, count: %llu, key: "STR_FMT, target->constants.capacity, target->constants.size, STR_PRINT(id));
            }
        } else {
            ArrayAppend(target->statements, statement);
        }
        next = parsePeek(ctx, 0);
    }
    parseConsume(ctx); // }

    return;
}

// if the context points to a `{` parse multiple statements,
// else only add one statement to the scope
// a new scope will be created with the parent as a parent, and the statements of the scope will be parsed into it
Scope* parseScope(ParseContext* ctx, Arena* mem, Scope* parent) {
    Scope* result = parseScopeInit(mem, parent);
    parseScope2(ctx, mem, result);
    return result;
}

bool isFunctionLit(ParseContext* ctx, Token next) {
    if(next.type != TokenType_LPAREN) return FALSE;

    Token one = parsePeek(ctx, 0);
    Token two = parsePeek(ctx, 1);
    return (
        (one.type == TokenType_RPAREN) || // foo :: ();
        (one.type == TokenType_IDENTIFIER && two.type == TokenType_COLON) // foo :: (bar: ...)
    );
}

ASTNode* parseStatement(ParseContext* ctx, Arena* mem, Scope* parent) {
    ASTNode* result = NodeInit(mem);
    
    Token t = parseConsume(ctx);
    switch(t.type) {
        case TokenType_RETURN: {
            result->type = ASTNodeType_RET;
            result->node.RET.expr = parseExpression(ctx, mem);

            parseCheckSemicolon(ctx);
        } break;
        case TokenType_IF: {
            result->type = ASTNodeType_IF;

            // NOTE: this is kindof stupid, of the order of the parse functions changes the parsing breaks
            ConditionalBlock ifBlock = {
                .expr = parseExpression(ctx, mem),
                .scope = parseScope(ctx, mem, parent),
            };
            ArrayAppend(result->node.IF.blocks, ifBlock);

            Token next = parsePeek(ctx, 0);
            while(next.type == TokenType_ELSE) {
                parseConsume(ctx); // else
                next = parsePeek(ctx, 0);
                if(next.type == TokenType_IF) {
                    // else if
                    parseConsume(ctx); // if
                    ConditionalBlock elseIfBlock = {
                        .expr = parseExpression(ctx, mem),
                        .scope = parseScope(ctx, mem, parent),
                    };
                    ArrayAppend(result->node.IF.blocks, elseIfBlock);
                } else {
                    // else
                    result->node.IF.hasElse = TRUE;
                    result->node.IF.elze = parseScope(ctx, mem, parent);
                    break;
                }
                next = parsePeek(ctx, 0);
            }
        } break;
        case TokenType_LOOP: {
            result->type = ASTNodeType_LOOP;
            result->node.LOOP.expr = parseExpression(ctx, mem);
            result->node.LOOP.scope = parseScope(ctx, mem, parent);
        } break;
        case TokenType_INT_LITERAL:
        case TokenType_STRING_LIT:
        case TokenType_BOOL_LITERAL:
        case TokenType_SUB:
        case TokenType_LPAREN: {
            result->type = ASTNodeType_EXPRESSION;
            result->node.EXPRESSION.expr = parseExpression(ctx, mem);

            parseCheckSemicolon(ctx);
        } break;
        case TokenType_LSCOPE: {
            UNIMPLEMENTED("TokenType_LSCOPE as a statement begin");
            // TODO: the scope is consumed here which is wrong
            Scope* scope = parseScope(ctx, mem, parent);
            // TODO: scope is not a single statement, but should still be valid, should this be its own ASTNode_Type?
            UNUSED(scope);
        } break;
        case TokenType_HASHTAG: {
            Token next = parseConsume(ctx);
            if(next.type != TokenType_IDENTIFIER) {
                ERROR(next.loc, "# needs to be followed by the name of a valid compiler instruction");
            }

            if(StringEqualsCstr(next.value, "library")) {
                next = parseConsume(ctx);
                if(next.type != TokenType_STRING_LIT) {
                    ERROR(next.loc, "#library needs to be followed by a string");
                }
                String key = {.str = &next.value.str[1], .length = next.value.length - 2}; // remove the `"` around the string

                LibName value = {0};
                value.functions = arena_alloc(mem, sizeof(Hashmap(String, FuncName)));
                // TODO: use arena allocator
                HashmapInit(*value.functions, 0x100);
                if(!HashmapSet(String, LibName)(&ctx->importLibraries, key, value)) {
                    UNREACHABLE_VA("failed to insert into hashmap, cap: %llu, count: %llu, key: "STR_FMT, ctx->importLibraries.capacity, ctx->importLibraries.size, STR_PRINT(key));
                }
                ctx->currentImportLibraryName = key;
            } else {
                ERROR_VA(next.loc, "Unknown compiler instrucion: "STR_FMT, STR_PRINT(next.value));
            }

            result->type = ASTNodeType_DIRECTIVE;
            parseCheckSemicolon(ctx);
        } break;
        case TokenType_IDENTIFIER: {
            ctx->currentSymbolName = t.value;
            Token next = parsePeek(ctx, 0);
            if(next.type == TokenType_LPAREN) {
                // function call
                // NOTE: the identifier is already consumed here so we have to rewind the context
                ctx->index--;
                result->type = ASTNodeType_EXPRESSION;
                result->node.EXPRESSION.expr = parseExpression(ctx, mem);

                parseCheckSemicolon(ctx);
            } else if(next.type == TokenType_ASSIGNMENT) {
                parseConsume(ctx); // =
                result->type = ASTNodeType_VAR_REASSIGN;
                result->node.VAR_REASSIGN.identifier = t.value;
                result->node.VAR_REASSIGN.expr = parseExpression(ctx, mem);

                parseCheckSemicolon(ctx);
            } else if(next.type == TokenType_INITIALIZER) {
                parseConsume(ctx); // :=
                result->type = ASTNodeType_VAR_DECL_ASSIGN;
                result->node.VAR_DECL_ASSIGN.identifier = t.value;
                result->node.VAR_DECL_ASSIGN.expr = parseExpression(ctx, mem);
                // NOTE: parse inferred during typechecking, void assigned here so we  can print the node
                result->node.VAR_DECL_ASSIGN.type = TypeInitSimple(mem, TYPE_VOID);

                // parseScopeAddSymbol(parent, t.value); // TODO: this symbol should go in the current scope not the parent
                parseCheckSemicolon(ctx);
            } else if(next.type == TokenType_COLON) {
                parseConsume(ctx); // :
                String identifier = t.value;
                TypeInfo* type = parseType(ctx, mem);

                next = parsePeek(ctx, 0);
                if(next.type == TokenType_ASSIGNMENT) {
                    parseConsume(ctx); // =
                    result->type = ASTNodeType_VAR_DECL_ASSIGN;
                    result->node.VAR_DECL_ASSIGN.identifier = identifier;
                    result->node.VAR_DECL_ASSIGN.type = type;
                    result->node.VAR_DECL_ASSIGN.expr = parseExpression(ctx, mem);
                } else {
                    result->type = ASTNodeType_VAR_DECL;
                    result->node.VAR_DECL.identifier = identifier;
                    result->node.VAR_DECL.type = type;
                }

                // parseScopeAddSymbol(parent, t.value); // TODO: this symbol should go in the current scope not the parent
                parseCheckSemicolon(ctx);
            } else if(next.type == TokenType_DOUBLECOLON) {
                parseConsume(ctx); // ::
                // TODO: `parseScopeAddSymbol()` should also contain the symbols for consts

                result->type = ASTNodeType_VAR_CONST;
                result->node.VAR_CONST.identifier = t.value;
                result->node.VAR_CONST.expr = parseExpression(ctx, mem);

                // TODO: kinda nasty, some better way to indicate if semicolon needs to be checked
                bool checkSemiColon = (result->node.VAR_CONST.expr->type != ExpressionType_FUNCTION_LIT);
                if(checkSemiColon) parseCheckSemicolon(ctx);
            } else {
                ERROR(next.loc, "identifier can only be followed by one of the following: `:`, `::`, `:=`, `=`");
            }
        } break;
        // NOTE: later when added `using` and `struct` this will be a valid statement begin token
        // case TokenType_DOT: {} break;

        case TokenType_COUNT:
        case TokenType_NONE:
        case TokenType_ELSE:
        case TokenType_TYPE:
        case TokenType_SEMICOLON:
        case TokenType_COLON:
        case TokenType_RARROW:
        case TokenType_RSCOPE:
        case TokenType_RPAREN:
        case TokenType_LBRACKET:
        case TokenType_RBRACKET:
        case TokenType_COMMA:
        case TokenType_DOT:
        case TokenType_DOUBLEDOT:
        case TokenType_TRIPLEDOT:
        case TokenType_LESS:
        case TokenType_LESS_EQ:
        case TokenType_GREATER:
        case TokenType_GREATER_EQ:
        case TokenType_NOT_EQUALS:
        case TokenType_ADD:
        case TokenType_MUL:
        case TokenType_DIV:
        case TokenType_COMPARISON:
        case TokenType_ASSIGNMENT:
        case TokenType_DOUBLECOLON:
        case TokenType_INITIALIZER:
            printf("[ERROR] Unhandled token type: %s at ("STR_FMT":%i:%i)\n", TokenTypeStr[t.type], STR_PRINT(t.loc.filename), t.loc.line, t.loc.collum);
            break;
    }

    return result;
}

Scope* parseGlobalScope(ParseContext* ctx, Arena* mem) {
    Scope* globalScope = parseScopeInit(mem, NULL);
    while(ctx->index < ctx->tokens.size) {
        ASTNode* statement = parseStatement(ctx, mem, globalScope);
        // TODO: VAR_CONST should be added to constants instead of statements
        ArrayAppend(globalScope->statements, statement);
    }
    return globalScope;
}

ParseResult Parse(Array(Token) tokens, Arena* mem) {
    ParseResult result = {0};
    ParseContext ctx2 = {.tokens = tokens};
    ParseContext* ctx = &ctx2;
    // TODO: use arena allocator
    HashmapInit(ctx->importLibraries, 0x100);
    HashmapInit(ctx->funcInfo, 0x100);

    Scope* globalScope = parseGlobalScope(ctx, mem);

    result.globalScope = globalScope;
    result.importLibraries = ctx->importLibraries; // TODO: ExitProcess needs to be imported always
    result.funcInfo = ctx->funcInfo;
    return result;
}

// TODO: check if functions with return type return on all codepaths (typechecking step)
