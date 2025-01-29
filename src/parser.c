#include "parser.h"
#include "common.h"

#include <stdlib.h> // exit(), EXIT_FAILURE
#include <stdio.h> // printf

ASTNode* NodeInit(Arena* mem){
    ASTNode* node = arena_alloc(mem, sizeof(ASTNode));
    assert(node, "Failed to allocate AST node");
    node->type = ASTNodeType_NONE; // NOTE: might not be necessary
    return node;
}

CompilerInstruction* CompInstInit(Arena* mem) {
    CompilerInstruction* result = arena_alloc(mem, sizeof(CompilerInstruction));
    assert(result, "Failed to allocate CompilerInstruction");
    result->type = CompilerInstructionType_NONE; // NOTE: might not be necessary
    return result;
}

#ifdef COMP_DEBUG
#define genPrintHelper(...) do{for(u64 h = 0; h < indent; ++h) printf("    "); printf(__VA_ARGS__);}while(0)
void ASTPrintCompInst(CompilerInstruction* inst, u64 indent) {
    switch(inst->type) {
        case CompilerInstructionType_NONE:  break;
        case CompilerInstructionType_COUNT: break;
        
        case CompilerInstructionType_LIB: {
            String libName = inst->inst.LIB.libName;
            genPrintHelper("COMP_INST_LIB: "STR_FMT"\n", STR_PRINT(libName));
        } break;
        case CompilerInstructionType_EXTERN: {
            String funcName = inst->inst.EXTERN.funcName;
            genPrintHelper("COMP_INST_EXTERN: "STR_FMT"\n", STR_PRINT(funcName));
        } break;
    }
}

void ASTNodePrint(ASTNode* node, u64 indent) {
    // TODO: need to fix indentation, everytime a case has a newline it needs to be indented
    switch(node->type) {
        case ASTNodeType_COUNT: break;
        case ASTNodeType_NONE: break;
        
        case ASTNodeType_INT_LIT: {
            String val = node->node.INT_LIT.value;
            printf(" "STR_FMT, STR_PRINT(val));
        } break;
        case ASTNodeType_FLOAT_LIT: {
            String wholePart = node->node.FLOAT_LIT.wholePart;
            String fractPart = node->node.FLOAT_LIT.fractPart;
            printf(" "STR_FMT"."STR_FMT, STR_PRINT(wholePart), STR_PRINT(fractPart));
        } break;
        case ASTNodeType_STRING_LIT: {
            String val = node->node.STRING_LIT.value;
            if(val.length > 50) printf(" "STR_FMT"...", 50, val.str);
            else                printf(" "STR_FMT, STR_PRINT(val));
        } break;
        case ASTNodeType_BOOL_LIT: {
            String val = node->node.STRING_LIT.value;
            printf(" "STR_FMT, STR_PRINT(val));
        } break;
        case ASTNodeType_SYMBOL: { // NOTE: identifier
            String id = node->node.SYMBOL.identifier;

            printf(" "STR_FMT, STR_PRINT(id));
        } break;        
        case ASTNodeType_BINARY_EXPRESSION: {
            String op = node->node.BINARY_EXPRESSION.operator;
            ASTNodePrint(node->node.BINARY_EXPRESSION.lhs, indent + 1);
            printf(" "STR_FMT, STR_PRINT(op));
            ASTNodePrint(node->node.BINARY_EXPRESSION.rhs, indent + 1);
        } break;
        case ASTNodeType_UNARY_EXPRESSION: {
            String op = node->node.UNARY_EXPRESSION.operator;
            printf(" "STR_FMT, STR_PRINT(op));
            ASTNodePrint(node->node.UNARY_EXPRESSION.expr, indent + 1);
        } break;
        case ASTNodeType_FUNCTION_DEF: {
            String id = node->node.FUNCTION_DEF.identifier;
            ASTNode* retType = node->node.FUNCTION_DEF.type;
            Array(FunctionArg) args = node->node.FUNCTION_DEF.args;
            Scope* scope = node->node.FUNCTION_DEF.scope;

            genPrintHelper("FUNCTION_DEF: {\n");
            genPrintHelper("    id: "STR_FMT",\n", STR_PRINT(id));
            genPrintHelper("    type: ");
            ASTNodePrint(retType, indent);
            printf(",\n");

            // args
            if(args.size > 0) {
                genPrintHelper("    args: [\n");
                for(u64 i = 0; i < args.size; ++i) {
                    String argId = args.data[i].id;
                    ASTNode* argType = args.data[i].type;

                    genPrintHelper("        {id: "STR_FMT", type: ", STR_PRINT(argId));
                    ASTNodePrint(argType, indent);
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
        case ASTNodeType_VAR_DECL: {
            String id = node->node.VAR_DECL.identifier;
            ASTNode* type = node->node.VAR_DECL.type;

            genPrintHelper("VAR_DECL: {\n");
            genPrintHelper("    id: "STR_FMT",\n", STR_PRINT(id));
            genPrintHelper("    type: ");
            ASTNodePrint(type, indent);
            printf(",\n");
            genPrintHelper("}\n");
        } break;
        case ASTNodeType_VAR_DECL_ASSIGN: {
            String id = node->node.VAR_DECL_ASSIGN.identifier;
            ASTNode* type = node->node.VAR_DECL_ASSIGN.type;
            ASTNode* expr = node->node.VAR_DECL_ASSIGN.expr;

            genPrintHelper("VAR_DECL_ASSIGN: {\n");
            genPrintHelper("    id: "STR_FMT",\n", STR_PRINT(id));
            genPrintHelper("    type: ");
            ASTNodePrint(type, indent);
            printf(",\n");
            genPrintHelper("    expr:");
            ASTNodePrint(expr, indent + 1);
            printf(",\n");
            genPrintHelper("}\n");
        } break;
        case ASTNodeType_VAR_REASSIGN: {
            String id = node->node.VAR_REASSIGN.identifier;
            ASTNode* expr = node->node.VAR_REASSIGN.expr;

            genPrintHelper("VAR_REASSIGN: {\n");
            genPrintHelper("    id: "STR_FMT",\n", STR_PRINT(id));
            genPrintHelper("    expr:");
            ASTNodePrint(expr, indent + 1);
            printf(",\n");
            genPrintHelper("}\n");
        } break;
        case ASTNodeType_VAR_CONST: {
            String id = node->node.VAR_CONST.identifier;
            ASTNode* expr = node->node.VAR_CONST.expr;

            genPrintHelper("VAR_CONST: {\n");
            genPrintHelper("    id: "STR_FMT",\n", STR_PRINT(id));
            genPrintHelper("    expr:");
            ASTNodePrint(expr, indent + 1);
            printf(",\n");
            genPrintHelper("}\n");
        } break;
        case ASTNodeType_RET: {
            ASTNode* expr = node->node.RET.expr;

            genPrintHelper("RET: {\n");
            genPrintHelper("    expr:");
            ASTNodePrint(expr, indent + 1);
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
                ASTNode* expr = block.expr;
                Scope* scope = block.scope;

                genPrintHelper("IF: {\n");
                genPrintHelper("    expr:");
                ASTNodePrint(expr, indent + 1);
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
                ASTNode* expr = block.expr;
                Scope* scope = block.scope;

                genPrintHelper("ELSE IF: {\n");
                genPrintHelper("    expr:");
                ASTNodePrint(expr, indent + 1);
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
            ASTNode* expr = node->node.LOOP.expr;
            Scope* scope = node->node.LOOP.scope;

            genPrintHelper("LOOP: {\n");
            genPrintHelper("    expr:");
            ASTNodePrint(expr, indent + 1);
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
        case ASTNodeType_FUNCTION_CALL: {
            String id = node->node.FUNCTION_CALL.identifier;
            Array(ASTNodePtr) args = node->node.FUNCTION_CALL.args;

            genPrintHelper("FUNCTION_CALL: {\n");
            genPrintHelper("    id: "STR_FMT"\n", STR_PRINT(id));

            // args
            if(args.size > 0) {
                genPrintHelper("    args: [\n");
                for(u64 i = 0; i < args.size; ++i) {
                    for(u64 h = 0; h < indent + 2; ++h) printf("    ");
                    ASTNodePrint(args.data[i], indent + 1);
                    printf(",\n");
                }
                genPrintHelper("    ],\n");
            } else {
                genPrintHelper("    args: [],\n");
            }
            genPrintHelper("}\n");
        } break;
        case ASTNodeType_TYPE:{
            Type type = node->node.TYPE.type;
            bool isArray = node->node.TYPE.isArray;
            bool isDynamic = node->node.TYPE.isDynamic;
            bool isPointer = node->node.TYPE.isPointer;
            u64 arraySize = node->node.TYPE.arraySize;
            
            printf("%s", TypeStr[type]);
            if(isPointer) printf("*");
            if(isArray) isDynamic ? printf("[...]") : printf("[%llu]", arraySize);
        } break;
        case ASTNodeType_COMPILER_INST: {
            CompilerInstruction* inst = node->node.COMPILER_INST.inst;
            ASTPrintCompInst(inst, indent);
        } break;
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
    result->expr.BINARY_EXPRESSION.operator = op.value;
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

ASTNode* makeFunction(ParseContext* ctx, Arena* mem, Token next) {
    parseConsume(ctx); // opening parenthesis (

    ASTNode* result = NodeInit(mem);
    result->type = ASTNodeType_FUNCTION_CALL;
    result->node.FUNCTION_CALL.identifier = next.value;

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
    result->node.FUNCTION_CALL.args = args;

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

bool isFunction(ParseContext* ctx, Token next) {
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
    result->expr.UNARY_EXPRESSION.operator = next.value;
    result->expr.UNARY_EXPRESSION.expr = parseLeaf(ctx, mem);
    return result;
}

Expression* parseLeaf(ParseContext* ctx, Arena* mem) {
    Token next = parseConsume(ctx);
    
    if(isFunction(ctx, next))               return makeFunction(ctx, mem, next);
    if(parseIsNumber(next))                 return makeNumber(ctx, mem, next);
    if(isUnaryOperator(next))               return makeUnary(ctx, mem, next);
    if(next.type == TokenType_BOOL_LITERAL) return makeBool(mem, next);
    if(next.type == TokenType_STRING_LIT)   return makeString(mem, next);
    if(next.type == TokenType_IDENTIFIER)   return makeVariable(mem, next);
    if(next.type == TokenType_LPAREN) {
        ASTNode* result = parseExpression(ctx, mem);
        Token token = parseConsume(ctx);
        if(token.type != TokenType_RPAREN) {
            printf("[ERROR] "STR_FMT":%i:%i Expected closing paranthesis, got: %s\n", STR_PRINT(token.loc.filename), token.loc.line, token.loc.collum, TokenTypeStr[token.type]);
            exit(EXIT_FAILURE);
        }
        return result;
    }

    Location loc = next.loc;
    printf("[ERROR] Unhandled input: "STR_FMT":%i:%i "STR_FMT"\n", STR_PRINT((loc).filename), (loc).line, (loc).collum, STR_PRINT(next.value));
    exit(EXIT_FAILURE);
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

Expression* parseIncreasingPresedence(ParseContext* ctx, Arena* mem, ASTNode* left, s64 minPrec) {
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

bool parseCheckSemicolon(ParseContext* ctx){
    Token next = parseConsume(ctx);
    if(next.type != TokenType_SEMICOLON){
        ERROR_VA(next.loc, "Statement needs to end with ; got: %s", TokenTypeStr[next.type]);
    }
    return TRUE;
}

Scope* parseScopeInit(Arena* mem, Scope* parent){
    Scope* result = arena_alloc(mem, sizeof(Scope));
    result->parent = parent;

    if(parent) ArrayAppend(parent->children, result);

    return result;
}

TypeInfo typeVoid(Arena* mem) {
    TypeInfo result = {0};
    result.symbolType = TYPE_VOID;
    result.isPointer = FALSE;
    result.isArray = FALSE;
    result.isDynamic = FALSE;
    result.arraySize = 0;
    result.functionInfo = (FunctionInfo){0};
    return result;
}

TypeInfo parseType(ParseContext* ctx, Arena* mem) {
    Token tok = parseConsume(ctx);
    if(tok.type != TokenType_TYPE) {
        ERROR(tok.loc, "Variable declaration needs a valid type");
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
    TypeInfo result = {0};
    result.symbolType = type;
    result.isArray = FALSE;
    result.isDynamic = FALSE;
    result.isPointer = FALSE;
    result.arraySize = 0;

    Token next = parsePeek(ctx, 0);
    if(next.type == TokenType_MUL) {
        parseConsume(ctx); // *
        result.isPointer = TRUE;
    }

    tok = parsePeek(ctx, 0);
    if(tok.type != TokenType_LBRACKET) {
        return result;
    }
    parseConsume(ctx); // '['
    result.isArray = TRUE;

    tok = parseConsume(ctx);
    if(tok.type == TokenType_TRIPLEDOT) {
        result.isDynamic = TRUE;
    }else if(tok.type == TokenType_INT_LITERAL) {
        u64 size = StringToU64(tok.value);
        result.arraySize = size;
    }else{
        ERROR(tok.loc, "Array needs a fixed size or '...' for dynamic size");
    }

    tok = parseConsume(ctx);
    if(tok.type != TokenType_RBRACKET){
        ERROR(tok.loc, "Expected closing pair to square bracket ']'");
    }

    return result;
}

// the ctx needs to point at '('
Array(FunctionArg) parseFunctionDeclArgs(ParseContext* ctx, Scope* scope) {
    Array(FunctionArg) result = {0};
    
    Token next = parseConsume(ctx);
    if(next.type != TokenType_LPAREN) {
        ERROR(next.loc, "Function arguments need to be inside parenthesis");
        exit(EXIT_FAILURE);
    }

    for(u64 i = ctx->index; i < ctx->tokens.size; i++) {
        next = parseConsume(ctx);
        if(next.type == TokenType_RPAREN) break;
        if(next.type != TokenType_IDENTIFIER) {
            ERROR(next.loc, "Function argument needs an identifier");
        }

        ArrayAppend(scope->symbols, next.value);
        Token id = next;

        next = parseConsume(ctx);
        if(next.type != TokenType_COLON){
            ERROR(next.loc, "Identifier name and type have to be separated a colon \":\"");
        }

        FunctionArg arg = {0};
        arg.id = id.value;
        arg.type = parseType(ctx, &scope->mem); // TODO: this function requiring an arena will eventually get removed
        // arg.initialValue = ; // TODO: currently we dont support value initializer parsing
        ArrayAppend(result, arg);

        next = parseConsume(ctx);
        if(next.type == TokenType_COMMA) {
            continue;
        } else if(next.type == TokenType_RPAREN) {
            break;
        } else {
            ERROR(next.loc, "Function declaration needs to end with a closing parenthesis \")\"");
            exit(EXIT_FAILURE);
        }
    }

    return result;
}

// ctx needs to point to the opening parenthesis of the function decl
ASTNode* parseFunctionDecl(ParseContext* ctx, Arena* mem, Scope* currentScope, Token identifier) {
    ASTNode* node = NodeInit(mem);
    node->type = ASTNodeType_FUNCTION_DEF;
    node->node.FUNCTION_DEF.identifier = identifier.value;
    node->node.FUNCTION_DEF.isExtern = FALSE;

    // args
    Scope* functionScope = parseScopeInit(mem, currentScope); // TODO: dont init the scope here
    node->node.FUNCTION_DEF.args = parseFunctionDeclArgs(ctx, functionScope);
    node->node.FUNCTION_DEF.scope = functionScope;

    // ret type
    Token next = parsePeek(ctx, 0);
    if(next.type == TokenType_RARROW) {
        parseConsume(ctx); // ->
        node->node.FUNCTION_DEF.type = parseType(ctx, mem);
    } else {
        node->node.FUNCTION_DEF.type = typeVoid(mem);
    }

    return node;
}

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
    } else if (expr->type == ASTNodeType_FUNCTION_CALL) {
        UNIMPLEMENTED("function call in const");
    } else if (expr->type == ExpressionType_BINARY_EXPRESSION) {
        String op = expr->expr.BINARY_EXPRESSION.operator;
        ASTNode* lhs = expr->expr.BINARY_EXPRESSION.lhs;
        ASTNode* rhs = expr->expr.BINARY_EXPRESSION.rhs;
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
        String op = expr->expr.UNARY_EXPRESSION.operator;
        ASTNode* subExpr = expr->expr.UNARY_EXPRESSION.expr;
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
Scope* parseScope(ParseContext* ctx, Arena* mem, Scope* parent) {
    Scope* result = parseScopeInit(mem, parent);

    // scope isnt enclosed by {} instead its just a single statement
    Token next = parsePeek(ctx, 0);
    if(next.type != TokenType_LSCOPE) {
        ASTNode* statement = parseStatement(ctx, mem, parent);
        ArrayAppend(result->statements, statement);
        return result;
    }

    // scope consists of multiple statements enclosed by {}
    parseConsume(ctx); // {
    while(next.type != TokenType_RSCOPE) {
        ASTNode* statement = parseStatement(ctx, mem, parent);
        ArrayAppend(result->statements, statement);
        next = parsePeek(ctx, 0);
    }
    parseConsume(ctx); // }

    return result;
}

// TODO: temporary
//       the #library instruction is only here until dll parsing
//       the #extern instruction should be after the :: in funton def, not at the begining of the line
ASTNode* parseCompInstruction(ParseContext* ctx, Arena* mem, Scope* parent) {
    ASTNode* result = NodeInit(mem);
    result->type = ASTNodeType_COMPILER_INST;
    
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

        CompilerInstruction* inst = CompInstInit(mem);
        inst->type = CompilerInstructionType_LIB;
        inst->inst.LIB.libName = key;

        result->node.COMPILER_INST.inst = inst;

        LibName value = {0};
        value.functions = arena_alloc(mem, sizeof(Hashmap(String, FuncName)));
        // TODO: use arena allocator
        HashmapInit(*value.functions, 0x100);
        if(!HashmapSet(String, LibName)(&ctx->importLibraries, key, value)) {
            UNREACHABLE("hashmap failed to insert");
        }
        ctx->currentImportLibraryName = key;

        // TODO: this needs to be checked here,
        //       the exter command uses parse statement to parse the function declaration,
        //       which consumes the semicolon, so for consisternt behaviour of the `parseCompInstruction()` function,
        //       all paths need to consume the semicolon
        parseCheckSemicolon(ctx);
    } else if(StringEqualsCstr(next.value, "extern")) {
        next = parsePeek(ctx, 0);
        if(next.type != TokenType_IDENTIFIER) {
            ERROR(next.loc, "#extern needs to be followed by a function declaration");
        }
        ASTNode* func = parseStatement(ctx, mem, parent);
        assert(func->type == ASTNodeType_FUNCTION_DEF, "#extern needs to be followed by a function declaration");
        func->node.FUNCTION_DEF.isExtern = TRUE;
        parseCheckSemicolon(ctx);
        ArrayAppend(parent->statements, func);
        String functionName = func->node.FUNCTION_DEF.identifier;

        CompilerInstruction* inst = CompInstInit(mem);
        inst->type = CompilerInstructionType_EXTERN;
        inst->inst.EXTERN.funcName = functionName;

        result->node.COMPILER_INST.inst = inst;

        LibName value = {0};
        if(!HashmapGet(String, LibName)(&ctx->importLibraries, ctx->currentImportLibraryName, &value)) {
            UNREACHABLE("cant find library to import frunction from, either hashmap ran out of space or no #library specified");
        }
        FuncName value2 = {0};
        if(!HashmapSet(String, FuncName)(value.functions, functionName, value2)) {
            UNREACHABLE("hashmap failed to insert");
        }
    }

    return result;
}

bool isFunctionDef(ParseContext* ctx) {
    Token one = parsePeek(ctx, 0);
    Token two = parsePeek(ctx, 1);
    Token three = parsePeek(ctx, 2);
    return (
        (one.type == TokenType_LPAREN && two.type == TokenType_RPAREN) || // foo :: ();
        (one.type == TokenType_LPAREN && two.type == TokenType_IDENTIFIER && three.type == TokenType_COLON) // foo :: (bar: ...)
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
            // NOTE: technicaly a expression with no effect should be allowed, but it will be ignored during codegen
            result = parseExpression(ctx, mem);

            parseCheckSemicolon(ctx);
        } break;
        case TokenType_LSCOPE: {
            Scope* scope = parseScope(ctx, mem, parent);
            // TODO: scope is not a single statement, but should still be valid, should this be its own ASTNode_Type?
            UNUSED(scope);
        } break;
        case TokenType_HASHTAG: {
            result = parseCompInstruction(ctx, mem, parent);
            
            // NOTE: cheking semicolon is not needed, gets checked in `parseCompInstruction()`
            // parseCheckSemicolon(ctx);
        } break;
        case TokenType_IDENTIFIER: {
            Token next = parsePeek(ctx, 0);
            if(next.type == TokenType_LPAREN) {
                // function call
                // TODO: right now `foo() + 3;` doesnt parse, but it should, it would be possible to make it work but it requires to rewind the context
                result = makeFunction(ctx, mem, t);
                
                parseCheckSemicolon(ctx);
                // TODO: `parseScopeAddSymbol()` should also contain the symbols for functions
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
                result->node.VAR_DECL_ASSIGN.type = typeVoid(mem);

                // parseScopeAddSymbol(parent, t.value); // TODO: this symbol should go in the current scope not the parent
                parseCheckSemicolon(ctx);
            } else if(next.type == TokenType_COLON) {
                parseConsume(ctx); // :
                String identifier = t.value;
                TypeInfo type = parseType(ctx, mem);

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

                if(isFunctionDef(ctx)) {
                    result = parseFunctionDecl(ctx, mem, parent, t);
                    assert(result->node.FUNCTION_DEF.isExtern == FALSE, "`parseFunctionDecl()` should set isExtern to FALSE by default");
                    
                    FuncInfo info = {0};

                    next = parsePeek(ctx, 0);
                    if(next.type == TokenType_SEMICOLON) {
                        // NOTE: dont consume the semicolon
                        //       the only case when there is a semicolon here is when the function is external,
                        //       which means the semicolon will be checked after this returns
                        info.isExtern = TRUE;
                    } else if(next.type == TokenType_LSCOPE) {
                        info.isExtern = FALSE;
                        result->node.FUNCTION_DEF.scope = parseScope(ctx, mem, parent);
                    } else {
                        // error
                        ERROR_VA(next.loc, "function definition needs to have a scope or end with semicolon, got: %s", TokenTypeStr[next.type]);
                    }

                    // NOTE: this might not be necesarry, gets added when returned???
                    if(!HashmapSet(String, FuncInfo)(&ctx->funcInfo, t.value, info)) {
                        UNREACHABLE("failed to set hashmap");
                    }
                } else {
                    result->type = ASTNodeType_VAR_CONST;
                    result->node.VAR_CONST.identifier = t.value;
                    result->node.VAR_CONST.expr = parseExpression(ctx, mem);

                    parseCheckSemicolon(ctx);
                }
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
