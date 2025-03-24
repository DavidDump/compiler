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

char* StatementTypeStr[StatementType_COUNT + 1] = {
    [StatementType_NONE]              = "NONE",
    
    [StatementType_VAR_DECL]          = "VAR_DECL",
    [StatementType_VAR_DECL_ASSIGN]   = "VAR_DECL_ASSIGN",
    [StatementType_VAR_REASSIGN]      = "VAR_REASSIGN",
    [StatementType_VAR_CONST]         = "VAR_CONST",
    [StatementType_RET]               = "RET",
    [StatementType_IF]                = "IF",
    [StatementType_LOOP]              = "LOOP",
    [StatementType_EXPRESSION]        = "EXPRESSION",
    [StatementType_DIRECTIVE]         = "DIRECTIVE",

    [StatementType_COUNT]             = "COUNT",
};

Statement* StatementInit(Arena* mem){
    Statement* result = arena_alloc(mem, sizeof(Statement));
    assert(result, "Failed to allocate statement");
    result->type = StatementType_NONE;
    return result;
}

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

void parseScopeStoreConst(Scope scope, String id, Expression* expr) {
    switch(scope.type) {
        case ScopeType_NONE: {
            UNREACHABLE("ScopeType_NONE is invalid: cannot store constant");
        } break;
        case ScopeType_GLOBAL: {
            GlobalScope* target = scope.scope.as_global;
            if(!HashmapSet(String, ExpressionPtr)(&target->constants, id, expr)) {
                UNREACHABLE_VA("failed to insert into hashmap, cap: %llu, count: %llu, key: "STR_FMT, target->constants.capacity, target->constants.size, STR_PRINT(id));
            }
        } break;
        case ScopeType_GENERIC: {
            GenericScope* target = scope.scope.as_generic;
            if(!HashmapSet(String, ExpressionPtr)(&target->constants, id, expr)) {
                UNREACHABLE_VA("failed to insert into hashmap, cap: %llu, count: %llu, key: "STR_FMT, target->constants.capacity, target->constants.size, STR_PRINT(id));
            }
        } break;
    }
}

void parseGlobalScopeStoreVar(GlobalScope* scope, String id, Statement* statement) {
    assertf(statement->type == StatementType_VAR_DECL || statement->type == StatementType_VAR_DECL_ASSIGN, "Expected variable, got: %s", StatementTypeStr[statement->type]);
    if(!HashmapSet(String, StatementPtr)(&scope->variables, id, statement)) {
        UNREACHABLE_VA("failed to insert into hashmap, cap: %llu, count: %llu, key: "STR_FMT, scope->constants.capacity, scope->constants.size, STR_PRINT(id));
    }
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
Array(FunctionArg) functionArgs(ParseContext* ctx, Arena* mem) {
    Array(FunctionArg) result = {0};

    Token next = parseConsume(ctx);
    while(next.type != TokenType_RPAREN) {
        if(next.type != TokenType_IDENTIFIER) {
            ERROR(next.loc, "Function argument needs an identifier");
        }

        Token id = next;
        next = parseConsume(ctx);
        if(next.type != TokenType_COLON) {
            ERROR(next.loc, "Identifier name and type have to be separated a colon \":\"");
        }

        FunctionArg arg = {0};
        arg.id = id.value;
        arg.type = parseExpression(ctx, mem);
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

GenericScope* parseGenericScopeInit(Arena* mem, Scope parent) {
    GenericScope* result = arena_alloc(mem, sizeof(GenericScope));
    result->parent = parent;
    HashmapInit(result->constants, 0x100); // TODO: better default
    return result;
}

GlobalScope* parseGlobalScopeInit(Arena* mem, Scope parent) {
    GlobalScope* result = arena_alloc(mem, sizeof(GlobalScope));
    result->parent = parent;
    HashmapInit(result->constants, 0x100); // TODO: better default
    HashmapInit(result->variables, 0x100); // TODO: better default
    return result;
}

Scope makeScopeFromGeneric(GenericScope* scope) {
    Scope result = {0};
    result.type = ScopeType_GENERIC;
    result.scope.as_generic = scope;
    return result;
}

Scope makeScopeFromGlobal(GlobalScope* scope) {
    Scope result = {0};
    result.type = ScopeType_GLOBAL;
    result.scope.as_global = scope;
    return result;
}

// if the context points to a `{` parse multiple statements,
// else only add one statement to the scope
// target is the scope that the statements of the scope will be parsed into
void parseGenericScopeInto(ParseContext* ctx, Arena* mem, GenericScope* target) {
    Scope s = makeScopeFromGeneric(target);

    // scope isnt enclosed by {} instead its just a single statement
    Token next = parsePeek(ctx, 0);
    if(next.type != TokenType_LSCOPE) {
        Statement* statement = parseStatement(ctx, mem, s);
        if(statement->type == StatementType_VAR_CONST) {
            String id = statement->statement.VAR_CONST.identifier;
            Expression* expr = statement->statement.VAR_CONST.expr;
            parseScopeStoreConst(s, id, expr);
        } else {
            ArrayAppend(target->statements, statement);
        }
        return;
    }

    // scope consists of multiple statements enclosed by {}
    parseConsume(ctx); // {
    while(next.type != TokenType_RSCOPE) {
        Statement* statement = parseStatement(ctx, mem, s);
        if(statement->type == StatementType_VAR_CONST) {
            String id = statement->statement.VAR_CONST.identifier;
            Expression* expr = statement->statement.VAR_CONST.expr;
            parseScopeStoreConst(s, id, expr);
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
GenericScope* parseGenericScope(ParseContext* ctx, Arena* mem, Scope parent) {
    GenericScope* result = parseGenericScopeInit(mem, parent);
    parseGenericScopeInto(ctx, mem, result);
    return result;
}

Expression* ExpressionTypeInitSimple(Arena* mem, Type t) {
    Expression* result = arena_alloc(mem, sizeof(Expression));
    result->type = ExpressionType_TYPE;
    result->expr.TYPE.typeInfo = TypeInitSimple(mem, t);
    return result;
}

Expression* makeFunctionLit(ParseContext* ctx, Arena* mem, bool isExtern) {
    Expression* result = arena_alloc(mem, sizeof(Expression));
    result->type = ExpressionType_FUNCTION_LIT;

    FunctionInfo* funcInfo = arena_alloc(mem, sizeof(FunctionInfo));
    funcInfo->isExternal = isExtern;
    funcInfo->args = functionArgs(ctx, mem); // NOTE: this does the parsing so the order when this gets called is important

    GenericScope* functionScope = parseGenericScopeInit(mem, (Scope){0}); // NOTE: this gets fixed later in parseStatement
    result->expr.FUNCTION_LIT.scope = functionScope;
    result->expr.FUNCTION_LIT.typeInfo = funcInfo;

    // ret type
    Token next = parsePeek(ctx, 0);
    if(next.type == TokenType_RARROW) {
        parseConsume(ctx); // ->
        funcInfo->returnType = parseExpression(ctx, mem);
    } else {
        funcInfo->returnType = ExpressionTypeInitSimple(mem, TYPE_VOID);
    }

    // scope
    if(!isExtern) {
        parseGenericScopeInto(ctx, mem, functionScope);
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

Expression* makeType(ParseContext* ctx, Arena* mem, Token next) {
    Expression* result = arena_alloc(mem, sizeof(Expression));
    result->type = ExpressionType_TYPE;
    TypeInfo* typeInfo = arena_alloc(mem, sizeof(TypeInfo));
    result->expr.TYPE.typeInfo = typeInfo;

    // TODO: this could be a hashmap with key `String` value `Type` and just look up the type there
    Type type = TYPE_NONE;
    if(StringEqualsCstr(next.value, "u8")) {
        type = TYPE_U8;
    } else if(StringEqualsCstr(next.value, "u16")) {
        type = TYPE_U16;
    } else if(StringEqualsCstr(next.value, "u32")) {
        type = TYPE_U32;
    } else if(StringEqualsCstr(next.value, "u64")) {
        type = TYPE_U64;
    } else if(StringEqualsCstr(next.value, "s8")) {
        type = TYPE_S8;
    } else if(StringEqualsCstr(next.value, "s16")) {
        type = TYPE_S16;
    } else if(StringEqualsCstr(next.value, "s32")) {
        type = TYPE_S32;
    } else if(StringEqualsCstr(next.value, "s64")) {
        type = TYPE_S64;
    } else if(StringEqualsCstr(next.value, "f32")) {
        type = TYPE_F32;
    } else if(StringEqualsCstr(next.value, "f64")) {
        type = TYPE_F64;
    } else if(StringEqualsCstr(next.value, "string")) {
        type = TYPE_STRING;
    } else if(StringEqualsCstr(next.value, "bool")) {
        type = TYPE_BOOL;
    } else if(StringEqualsCstr(next.value, "void")) {
        type = TYPE_VOID;
    } else {
        ERROR_VA(next.loc, "Unknown type: "STR_FMT, STR_PRINT(next.value));
    }

    bool isPointer = FALSE;
    next = parsePeek(ctx, 0);
    if(next.type == TokenType_MUL) {
        parseConsume(ctx); // *
        isPointer = TRUE;
    }

    next = parsePeek(ctx, 0);
    if(next.type != TokenType_LBRACKET) {
        typeInfo->symbolType = type;
        typeInfo->isPointer = isPointer;
        return result;
    }
    parseConsume(ctx); // '['

    ArrayInfo arrayInfo = {0};
    next = parseConsume(ctx);
    if(next.type == TokenType_TRIPLEDOT) {
        arrayInfo.isDynamic = TRUE;
    } else if(next.type == TokenType_INT_LITERAL) {
        u64 size = StringToU64(next.value);
        arrayInfo.arraySize = size;
    } else {
        ERROR(next.loc, "Array needs a fixed size or '...' for dynamic size");
    }

    next = parseConsume(ctx);
    if(next.type != TokenType_RBRACKET){
        ERROR(next.loc, "Expected closing pair to square bracket ']'");
    }

    typeInfo->symbolType = TYPE_ARRAY;
    typeInfo->arrayInfo = arrayInfo;
    typeInfo->isPointer = isPointer;
    return result;
}

TypeInfo* parseType(ParseContext* ctx, Arena* mem) {
    u64 indexBeforeParsingType = ctx->index;
    Expression* expr = parseExpression(ctx, mem);
    if(expr->type == ExpressionType_TYPE) {
        return expr->expr.TYPE.typeInfo;
    }

    Location loc = ctx->tokens.data[indexBeforeParsingType].loc;
    ERROR_VA(loc, "Expected type, got expression: %s", ExpressionTypeStr[expr->type]);

    // silence warning
    return 0;
}

Expression* makeStructDef(ParseContext* ctx, Arena* mem) {
    Expression* result = arena_alloc(mem, sizeof(Expression));
    result->type = ExpressionType_STRUCT_DEF;

    GlobalScope* structScope = parseGlobalScopeInit(mem, (Scope){0}); // TODO: fix parent scope
    result->expr.STRUCT_DEF.scope = structScope;
    // result->expr.STRUCT_DEF.args = functionArgs(ctx, structScope);

    Token next = parseConsume(ctx);
    if(next.type != TokenType_LSCOPE) {
        ERROR_VA(next.loc, "Struct keyword needs to be followed by a scope, expected: '{', got: '"STR_FMT"'", STR_PRINT(next.value));
    }

    // scope
    parseGlobalScopeInto(ctx, mem, structScope);

    return result;
}

Expression* parseLeaf(ParseContext* ctx, Arena* mem) {
    Token next = parseConsume(ctx);

    if(isFunctionLit(ctx, next))            return makeFunctionLit(ctx, mem, FALSE);
    if(isFunctionCall(ctx, next))           return makeFunctionCall(ctx, mem, next);
    if(parseIsNumber(next))                 return makeNumber(ctx, mem, next);
    if(isUnaryOperator(next))               return makeUnary(ctx, mem, next);
    if(next.type == TokenType_TYPE)         return makeType(ctx, mem, next);
    if(next.type == TokenType_HASHTAG)      return makeCompInstructionLeaf(ctx, mem);
    if(next.type == TokenType_STRUCT)       return makeStructDef(ctx, mem);
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

    ERROR_VA(next.loc, "Expected expression leaf, got: "STR_FMT, STR_PRINT(next.value));
    // NOTE: silencing compiler warning
    return 0;
}

// TODO: move somewhere more sane
Operator operators[BIN_OPERATORS_COUNT] = {
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
    {.type = TokenType_AS,  .presedence = 15},
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

bool isFunctionLit(ParseContext* ctx, Token next) {
    if(next.type != TokenType_LPAREN) return FALSE;

    Token one = parsePeek(ctx, 0);
    Token two = parsePeek(ctx, 1);
    return (
        (one.type == TokenType_RPAREN) || // foo :: ();
        (one.type == TokenType_IDENTIFIER && two.type == TokenType_COLON) // foo :: (bar: ...)
    );
}

// containingScope is the scope that this statement is inside of, it is used when initializing a new chld scope
Statement* parseStatement(ParseContext* ctx, Arena* mem, Scope containingScope) {
    Statement* result = StatementInit(mem);

    Token t = parseConsume(ctx);
    switch(t.type) {
        case TokenType_RETURN: {
            result->type = StatementType_RET;
            result->statement.RET.expr = parseExpression(ctx, mem);

            parseCheckSemicolon(ctx);
        } break;
        case TokenType_IF: {
            result->type = StatementType_IF;

            // NOTE: this is kindof stupid, of the order of the parse functions changes the parsing breaks
            ConditionalBlock ifBlock = {
                .expr = parseExpression(ctx, mem),
                .scope = parseGenericScope(ctx, mem, containingScope),
            };
            ArrayAppend(result->statement.IF.blocks, ifBlock);

            Token next = parsePeek(ctx, 0);
            while(next.type == TokenType_ELSE) {
                parseConsume(ctx); // else
                next = parsePeek(ctx, 0);
                if(next.type == TokenType_IF) {
                    // else if
                    parseConsume(ctx); // if
                    ConditionalBlock elseIfBlock = {
                        .expr = parseExpression(ctx, mem),
                        .scope = parseGenericScope(ctx, mem, containingScope),
                    };
                    ArrayAppend(result->statement.IF.blocks, elseIfBlock);
                } else {
                    // else
                    result->statement.IF.hasElse = TRUE;
                    result->statement.IF.elze = parseGenericScope(ctx, mem, containingScope);
                    break;
                }
                next = parsePeek(ctx, 0);
            }
        } break;
        case TokenType_LOOP: {
            result->type = StatementType_LOOP;
            result->statement.LOOP.expr = parseExpression(ctx, mem);
            result->statement.LOOP.scope = parseGenericScope(ctx, mem, containingScope);
        } break;
        case TokenType_STRUCT:
        case TokenType_INT_LITERAL:
        case TokenType_STRING_LIT:
        case TokenType_BOOL_LITERAL:
        case TokenType_SUB:
        case TokenType_LPAREN: {
            result->type = StatementType_EXPRESSION;
            result->statement.EXPRESSION.expr = parseExpression(ctx, mem);

            parseCheckSemicolon(ctx);
        } break;
        case TokenType_LSCOPE: {
            UNIMPLEMENTED("TokenType_LSCOPE as a statement begin");
            // TODO: the scope is consumed here which is wrong
            GenericScope* scope = parseGenericScope(ctx, mem, containingScope);
            // TODO: scope is not a single statement, but should still be valid, should this be its own Statement_Type?
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

            result->type = StatementType_DIRECTIVE;
            parseCheckSemicolon(ctx);
        } break;
        case TokenType_IDENTIFIER: {
            ctx->currentSymbolName = t.value;
            Token next = parsePeek(ctx, 0);
            if(next.type == TokenType_LPAREN) {
                // function call
                // NOTE: the identifier is already consumed here so we have to rewind the context
                ctx->index--;
                result->type = StatementType_EXPRESSION;
                result->statement.EXPRESSION.expr = parseExpression(ctx, mem);

                parseCheckSemicolon(ctx);
            } else if(next.type == TokenType_ASSIGNMENT) {
                parseConsume(ctx); // =
                result->type = StatementType_VAR_REASSIGN;
                result->statement.VAR_REASSIGN.identifier = t.value;
                result->statement.VAR_REASSIGN.expr = parseExpression(ctx, mem);

                parseCheckSemicolon(ctx);
            } else if(next.type == TokenType_INITIALIZER) {
                parseConsume(ctx); // :=
                result->type = StatementType_VAR_DECL_ASSIGN;
                result->statement.VAR_DECL_ASSIGN.identifier = t.value;
                result->statement.VAR_DECL_ASSIGN.expr = parseExpression(ctx, mem);
                // NOTE: parse inferred during typechecking, void assigned here so we can print the node
                result->statement.VAR_DECL_ASSIGN.type = ExpressionTypeInitSimple(mem, TYPE_NONE);

                // TODO: kinda nasty, some better way to indicate if semicolon needs to be checked
                bool checkSemiColon = (
                    result->statement.VAR_DECL_ASSIGN.expr->type != ExpressionType_FUNCTION_LIT &&
                    result->statement.VAR_DECL_ASSIGN.expr->type != ExpressionType_STRUCT_DEF
                );
                if(checkSemiColon) parseCheckSemicolon(ctx);
                // NOTE: this is yet another supid fix for a problem,
                // the parent of the function scope never gets set because we dont have acess to it in expression parsing
                // maybe add to context as an easy fix
                if(result->statement.VAR_DECL_ASSIGN.expr->type == ExpressionType_FUNCTION_LIT) result->statement.VAR_DECL_ASSIGN.expr->expr.FUNCTION_LIT.scope->parent = containingScope;
                if(result->statement.VAR_DECL_ASSIGN.expr->type == ExpressionType_STRUCT_DEF) result->statement.VAR_DECL_ASSIGN.expr->expr.STRUCT_DEF.scope->parent = containingScope;
            } else if(next.type == TokenType_COLON) {
                parseConsume(ctx); // :
                String identifier = t.value;
                Expression* type = parseExpression(ctx, mem);

                next = parsePeek(ctx, 0);
                if(next.type == TokenType_ASSIGNMENT) {
                    parseConsume(ctx); // =
                    result->type = StatementType_VAR_DECL_ASSIGN;
                    result->statement.VAR_DECL_ASSIGN.identifier = identifier;
                    result->statement.VAR_DECL_ASSIGN.type = type;
                    result->statement.VAR_DECL_ASSIGN.expr = parseExpression(ctx, mem);
                } else {
                    result->type = StatementType_VAR_DECL;
                    result->statement.VAR_DECL.identifier = identifier;
                    result->statement.VAR_DECL.type = type;
                }

                parseCheckSemicolon(ctx);
            } else if(next.type == TokenType_DOUBLECOLON) {
                parseConsume(ctx); // ::

                result->type = StatementType_VAR_CONST;
                result->statement.VAR_CONST.identifier = t.value;
                result->statement.VAR_CONST.expr = parseExpression(ctx, mem);

                // TODO: kinda nasty, some better way to indicate if semicolon needs to be checked
                bool checkSemiColon = (
                    result->statement.VAR_CONST.expr->type != ExpressionType_FUNCTION_LIT &&
                    result->statement.VAR_CONST.expr->type != ExpressionType_STRUCT_DEF
                );
                if(checkSemiColon) parseCheckSemicolon(ctx);
                // NOTE: this is yet another supid fix for a problem,
                // the parent of the function scope never gets set because we dont have acess to it in expression parsing
                // maybe add to context as an easy fix
                if(result->statement.VAR_CONST.expr->type == ExpressionType_FUNCTION_LIT) result->statement.VAR_CONST.expr->expr.FUNCTION_LIT.scope->parent = containingScope;
                if(result->statement.VAR_CONST.expr->type == ExpressionType_STRUCT_DEF) result->statement.VAR_CONST.expr->expr.STRUCT_DEF.scope->parent = containingScope;
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
        case TokenType_AS:
        case TokenType_DOUBLECOLON:
        case TokenType_INITIALIZER:
            printf("[ERROR] Unhandled token type: %s at ("STR_FMT":%i:%i)\n", TokenTypeStr[t.type], STR_PRINT(t.loc.filename), t.loc.line, t.loc.collum);
            break;
    }

    return result;
}

GlobalScope* parseGlobalScopeInto(ParseContext* ctx, Arena* mem, GlobalScope* globalScope) {
    Scope s = makeScopeFromGlobal(globalScope);

    Token next = parsePeek(ctx, 0);
    while(ctx->index < ctx->tokens.size) {
        Statement* statement = parseStatement(ctx, mem, s);
        if(statement->type == StatementType_VAR_CONST) {
            String id = statement->statement.VAR_CONST.identifier;
            Expression* expr = statement->statement.VAR_CONST.expr;
            parseScopeStoreConst(s, id, expr);
        } else if(statement->type == StatementType_VAR_DECL || statement->type == StatementType_VAR_DECL_ASSIGN) {
            String id = {0};
            if(statement->type == StatementType_VAR_DECL) {
                id = statement->statement.VAR_DECL.identifier;
            } else if(statement->type == StatementType_VAR_DECL_ASSIGN) {
                id = statement->statement.VAR_DECL_ASSIGN.identifier;
            }
            parseGlobalScopeStoreVar(globalScope, id, statement);
        }

        next = parsePeek(ctx, 0);
        if(next.type == TokenType_RSCOPE) {
            parseConsume(ctx);
            break;
        }
    }

    return globalScope;
}

GlobalScope* parseGlobalScope(ParseContext* ctx, Arena* mem) {
    GlobalScope* globalScope = parseGlobalScopeInit(mem, (Scope){0}); // NOTE: global scope has no parent
    return parseGlobalScopeInto(ctx, mem, globalScope);
}

ParseResult Parse(Array(Token) tokens, Arena* mem) {
    ParseResult result = {0};
    ParseContext ctx = {.tokens = tokens};

    // TODO: use arena allocator
    HashmapInit(ctx.importLibraries, 0x100);

    GlobalScope* globalScope = parseGlobalScope(&ctx, mem);

    result.globalScope = globalScope;
    result.importLibraries = ctx.importLibraries; // TODO: ExitProcess needs to be imported always
    return result;
}

// TODO: check if functions with return type return on all codepaths (typechecking step)
