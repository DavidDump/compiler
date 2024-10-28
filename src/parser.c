#include "parser.h"
#include "common.h"

#include <stdlib.h> // exit(), EXIT_FAILURE
#include <stdio.h> // printf
#include <assert.h>

void parseAddStatement(StmtList* list, ASTNode* node){
    if(list->size >= list->capacity){
        size_t newCap = list->capacity * 2;
        if(newCap == 0) newCap = 1;
        list->statements = arena_realloc(&list->mem, list->statements, list->capacity * sizeof(list->statements[0]), newCap * sizeof(list->statements[0]));
        list->capacity = newCap;
    }

    list->statements[list->size++] = node;
}

void parseAddArg(Args* args, ASTNode* node){
    // assert(node->type == ASTNodeType_VAR_DECL); // NOTE: read the Args struct for type info
    if(args->size >= args->capacity){
        size_t newCap = args->capacity * 2;
        if(newCap == 0) newCap = 1;
        args->args = arena_realloc(&args->mem, args->args, args->capacity * sizeof(args->args[0]), newCap * sizeof(args->args[0]));
        args->capacity = newCap;
    }

    args->args[args->size++] = node;
}

ASTNode* NodeInit(Arena* mem){
    ASTNode* node = arena_alloc(mem, sizeof(ASTNode));
    assert(node && "Failed to allocate AST node");
    node->type = ASTNodeType_NONE; // NOTE: might not be necessary
    return node;
}

CompilerInstruction* CompInstInit(Arena* mem) {
    CompilerInstruction* result = arena_alloc(mem, sizeof(CompilerInstruction));
    assert(result && "Failed to allocate CompilerInstruction");
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
            Args args = node->node.FUNCTION_DEF.args;
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
                    String argId = args.args[i]->node.VAR_DECL.identifier;
                    ASTNode* argType = args.args[i]->node.VAR_DECL.type;

                    genPrintHelper("        {id: "STR_FMT", type: ", STR_PRINT(argId));
                    ASTNodePrint(argType, indent);
                    printf("},\n");
                }
                genPrintHelper("    ],\n");
            } else {
                genPrintHelper("    args: [],\n");
            }
            
            // statements
            if(scope->stmts.size > 0) {
                genPrintHelper("    statements: [\n");
                for(u64 i = 0; i < scope->stmts.size; ++i) {
                    ASTNodePrint(scope->stmts.statements[i], indent + 2);
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
            ASTNode* expr = node->node.IF.expr;
            Scope* scope = node->node.IF.scope;

            genPrintHelper("IF: {\n");
            genPrintHelper("    expr:");
            ASTNodePrint(expr, indent + 1);
            printf(",\n");

            // statements
            if(scope->stmts.size > 0) {
                genPrintHelper("    statements: [\n");
                for(u64 i = 0; i < scope->stmts.size; ++i) {
                    ASTNodePrint(scope->stmts.statements[i], indent + 2);
                }
                genPrintHelper("    ],\n");
            } else {
                genPrintHelper("    statements: [],\n");
            }
            genPrintHelper("}\n");
        } break;
        case ASTNodeType_ELSE: {
            genPrintHelper("ELSE: {\n");
            Scope* scope = node->node.ELSE.scope;

            // statements
            if(scope->stmts.size > 0) {
                genPrintHelper("    statements: [\n");
                for(u64 i = 0; i < scope->stmts.size; ++i) {
                    ASTNodePrint(scope->stmts.statements[i], indent + 2);
                }
                genPrintHelper("    ],\n");
            } else {
                genPrintHelper("    statements: [],\n");
            }
        } break;
        case ASTNodeType_ELSE_IF: {
            ASTNode* expr = node->node.ELSE_IF.expr;
            Scope* scope = node->node.ELSE_IF.scope;

            genPrintHelper("ELSE_IF: {\n");
            genPrintHelper("    expr:");
            ASTNodePrint(expr, indent + 1);
            printf(",\n");

            // statements
            if(scope->stmts.size > 0) {
                genPrintHelper("    statements: [\n");
                for(u64 i = 0; i < scope->stmts.size; ++i) {
                    ASTNodePrint(scope->stmts.statements[i], indent + 2);
                }
                genPrintHelper("    ],\n");
            } else {
                genPrintHelper("    statements: [],\n");
            }
            genPrintHelper("}\n");
        } break;
        case ASTNodeType_LOOP: {
            ASTNode* expr = node->node.LOOP.expr;
            Scope* scope = node->node.LOOP.scope;

            genPrintHelper("LOOP: {\n");
            genPrintHelper("    expr:");
            ASTNodePrint(expr, indent + 1);
            printf(",\n");

            // statements
            if(scope->stmts.size > 0) {
                genPrintHelper("    statements: [\n");
                for(u64 i = 0; i < scope->stmts.size; ++i) {
                    ASTNodePrint(scope->stmts.statements[i], indent + 2);
                }
                genPrintHelper("    ],\n");
            } else {
                genPrintHelper("    statements: [],\n");
            }
            genPrintHelper("}\n");
        } break;
        case ASTNodeType_FUNCTION_CALL: {
            String id = node->node.FUNCTION_CALL.identifier;
            Args args = node->node.FUNCTION_CALL.args;

            genPrintHelper("FUNCTION_CALL: {\n");
            genPrintHelper("    id: "STR_FMT"\n", STR_PRINT(id));

            // args
            if(args.size > 0) {
                genPrintHelper("    args: [\n");
                for(u64 i = 0; i < args.size; ++i) {
                    for(u64 h = 0; h < indent + 2; ++h) printf("    ");
                    ASTNodePrint(args.args[i], indent + 1);
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
    for(u64 i = 0; i < root->stmts.size; ++i) {
        ASTNodePrint(root->stmts.statements[i], 0);
    }
}
#endif // COMP_DEBUG

// return what the parse context is pointong to then advance
Token parseConsume(ParseContext* ctx){
	if (ctx->index + 1 > ctx->tokens.size) return (Token){0};
	return ctx->tokens.tokens[ctx->index++];
}

// return what the parse context is pointon to without advancing
Token parsePeek(ParseContext* ctx, int num){
	if (ctx->index + num > ctx->tokens.size) return (Token){0};
	return ctx->tokens.tokens[ctx->index + num];
}

bool parseScopeContainsSymbol(Scope* scope, String symbol){
    Scope* workingScope = scope;
    while(workingScope){
        for(int i = 0; i < workingScope->symbolSize; i++){
            if(StringEquals(workingScope->symbolTable[i], symbol)){
                return TRUE;
            }
        }
        workingScope = workingScope->parent;
    }
    return FALSE;
}

ASTNode* makeBinary(Arena* mem, ASTNode* left, Token op, ASTNode* right) {
    ASTNode* node = NodeInit(mem);
    node->type = ASTNodeType_BINARY_EXPRESSION;
    node->node.BINARY_EXPRESSION.lhs = left;
    node->node.BINARY_EXPRESSION.rhs = right;
    node->node.BINARY_EXPRESSION.operator = op.value;
    return node;
}

ASTNode* makeNumber(ParseContext* ctx, Arena* mem, Token tok) {
    ASTNode* result = NodeInit(mem);

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

            result->type = ASTNodeType_FLOAT_LIT;
            result->node.FLOAT_LIT.wholePart = tok.value;
            result->node.FLOAT_LIT.fractPart = fractPart;
        } else {
            result->type = ASTNodeType_INT_LIT;
            result->node.INT_LIT.value = tok.value;
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

        result->type = ASTNodeType_FLOAT_LIT;
        result->node.FLOAT_LIT.wholePart = StringFromCstrLit("0");
        result->node.FLOAT_LIT.fractPart = fractPart;
    }

    return result;
}

ASTNode* makeVariable(Arena* mem, Token identifier) {
    ASTNode* node = NodeInit(mem);
    node->type = ASTNodeType_SYMBOL;
    node->node.SYMBOL.identifier = identifier.value;
    // TODO: set during typechecking phase
    // node->node.SYMBOL.type = ;
    return node;
}

ASTNode* makeFunction(ParseContext* ctx, Arena* mem, Token next) {
    parseConsume(ctx); // opening parenthesis (

    ASTNode* result = NodeInit(mem);
    result->type = ASTNodeType_FUNCTION_CALL;
    result->node.FUNCTION_CALL.identifier = next.value;

    Args args = {0};
    Token token = parsePeek(ctx, 0);
    while(token.type != TokenType_RPAREN) {
        ASTNode* expr = parseExpression(ctx, mem);
        parseAddArg(&args, expr);

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

ASTNode* makeString(Arena* mem, Token value) {
    ASTNode* result = NodeInit(mem);
    result->type = ASTNodeType_STRING_LIT;
    result->node.STRING_LIT.value = value.value;
    return result;
}

ASTNode* makeBool(Arena* mem, Token value) {
    ASTNode* result = NodeInit(mem);
    result->type = ASTNodeType_BOOL_LIT;
    result->node.BOOL_LIT.value = value.value;
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
ASTNode* makeUnary(ParseContext* ctx, Arena* mem, Token next) {
    ASTNode* result = NodeInit(mem);
    result->type = ASTNodeType_UNARY_EXPRESSION;
    result->node.UNARY_EXPRESSION.operator = next.value;
    result->node.UNARY_EXPRESSION.expr = parseLeaf(ctx, mem);
    return result;
}

ASTNode* parseLeaf(ParseContext* ctx, Arena* mem) {
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
            printf("[ERROR] Expected closing paranthesis, got: %s ", TokenTypeStr[token.type]);
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

ASTNode* parseIncreasingPresedence(ParseContext* ctx, Arena* mem, ASTNode* left, s64 minPrec) {
    Token next = parsePeek(ctx, 0);
    if(!isOperator(next)) return left;

    s64 nextPrec = getPresedence(next);
    if(nextPrec <= minPrec) {
        return left;
    } else {
        parseConsume(ctx);
        ASTNode* right = parseDecreasingPresedence(ctx, mem, nextPrec);
        return makeBinary(mem, left, next, right);
    }
}

ASTNode* parseDecreasingPresedence(ParseContext* ctx, Arena* mem, s64 minPrec) {
    ASTNode* left = parseLeaf(ctx, mem);
    
    while(TRUE) {
        ASTNode* node = parseIncreasingPresedence(ctx, mem, left, minPrec);
        if(node == left) break;
        left = node;
    }
    return left;
}

ASTNode* parseExpression(ParseContext* ctx, Arena* mem) {
    return parseDecreasingPresedence(ctx, mem, 0);
}

bool parseCheckSemicolon(ParseContext* ctx){
    Token next = parsePeek(ctx, 0);
    if(next.type != TokenType_SEMICOLON){
        ERROR(next.loc, "Statement needs to end with ;");
        exit(EXIT_FAILURE);
    }
    parseConsume(ctx);
    return TRUE;
}

void parseScopeAddChild(Scope* parent, Scope* child){
    if(parent == NULL) return;
    if(parent->childrenSize >= parent->childrenCapacity){
        size_t newCap = parent->childrenCapacity * 2;
        if(newCap == 0) newCap = 1;
        parent->children = arena_realloc(&parent->mem, parent->children, parent->childrenCapacity * sizeof(parent->children[0]), newCap * sizeof(parent->children[0]));
        parent->childrenCapacity = newCap;
    }

    parent->children[parent->childrenSize++] = child;
}

Scope* parseScopeInit(Arena* mem, Scope* parent){
    Scope* result = arena_alloc(mem, sizeof(Scope));
    result->parent = parent;

    parseScopeAddChild(parent, result);

    return result;
}

void parseScopeAddSymbol(Scope* scope, String symbol){
    if(scope->symbolSize >= scope->symbolCapacity){
        size_t newCap = scope->symbolCapacity * 2;
        if(newCap == 0) newCap = 1;
        scope->symbolTable = arena_realloc(&scope->mem, scope->symbolTable, scope->symbolCapacity * sizeof(scope->symbolTable[0]), newCap * sizeof(scope->symbolTable[0]));
        scope->symbolCapacity = newCap;
    }

    scope->symbolTable[scope->symbolSize++] = symbol;
}

ASTNode* typeVoid(Arena* mem){
    ASTNode* node = NodeInit(mem);
    node->type = ASTNodeType_TYPE;
    node->node.TYPE.type = TYPE_VOID;
    node->node.TYPE.isArray = FALSE;
    node->node.TYPE.isDynamic = FALSE;
    node->node.TYPE.arraySize = 0;
    return node;
}

ASTNode* parseType(ParseContext* ctx, Arena* mem) {
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
        ERROR(tok.loc, "Unknown type");
        // TODO: make this error print better
    }
    ASTNode* node = NodeInit(mem);
    node->type = ASTNodeType_TYPE;
    node->node.TYPE.type = type;
    node->node.TYPE.isArray = FALSE;
    node->node.TYPE.isDynamic = FALSE;
    node->node.TYPE.isPointer = FALSE;
    node->node.TYPE.arraySize = 0;

    Token next = parsePeek(ctx, 0);
    if(next.type == TokenType_MUL) {
        parseConsume(ctx); // *
        node->node.TYPE.isPointer = TRUE;
    }

    tok = parsePeek(ctx, 0);
    if(tok.type != TokenType_LBRACKET) {
        return node;
    }
    parseConsume(ctx); // '['
    node->node.TYPE.isArray = TRUE;

    tok = parseConsume(ctx);
    if(tok.type == TokenType_TRIPLEDOT) {
        node->node.TYPE.isDynamic = TRUE;
    }else if(tok.type == TokenType_INT_LITERAL) {
        u64 size = StringToU64(tok.value);
        node->node.TYPE.arraySize = size;
    }else{
        ERROR(tok.loc, "Array needs a fixed size or '...' for dynamic size");
    }

    tok = parseConsume(ctx);
    if(tok.type != TokenType_RBRACKET){
        ERROR(tok.loc, "Expected closing pair to square bracket ']'");
    }

    return node;
}

// the ctx needs to point at '('
Args parseFunctionDeclArgs(ParseContext* ctx, Scope* scope){
    Args result = {0};
    
    Token next = parseConsume(ctx);
    if(next.type != TokenType_LPAREN){
        ERROR(next.loc, "Function arguments need to be inside parenthesis");
        exit(EXIT_FAILURE);
    }
    for(u64 i = ctx->index; i < ctx->tokens.size; i++){
        next = parsePeek(ctx, 0);
        if(next.type == TokenType_IDENTIFIER){
            parseConsume(ctx);
            parseScopeAddSymbol(scope, next.value);
            Token id = next;

            next = parseConsume(ctx);
            if(next.type != TokenType_COLON){
                ERROR(next.loc, "Identifier name and type have to be separated a colon \":\"");
                exit(EXIT_FAILURE);
            }

            // next = parsePeek(ctx, 0);
            // if(next.type != TokenType_TYPE){
            //     ERROR(next.loc, "Function argument needs a type");
            //     exit(EXIT_FAILURE);
            // }
            // parseConsume(ctx);

            ASTNode* node = NodeInit(&result.mem);
            node->type = ASTNodeType_VAR_DECL;
            node->node.VAR_DECL.identifier = id.value;
            node->node.VAR_DECL.type = parseType(ctx, &result.mem);

            parseAddArg(&result, node);

            next = parsePeek(ctx, 0);
            if(next.type == TokenType_COMMA){
                parseConsume(ctx);
                continue;
            }else if(next.type == TokenType_RPAREN){
                parseConsume(ctx);
                break;
            }else{
                ERROR(next.loc, "Function declaration needs to end with a closing parenthesis \")\"");
                exit(EXIT_FAILURE);
            }
        }else if(next.type == TokenType_RPAREN){
            parseConsume(ctx);
            break;
        }else{
            ERROR(next.loc, "Function argument needs an identifier");
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
    Scope* functionScope = parseScopeInit(mem, currentScope);
    node->node.FUNCTION_DEF.args = parseFunctionDeclArgs(ctx, functionScope);
    node->node.FUNCTION_DEF.scope = functionScope;

    // ret type
    Token next = parseConsume(ctx);
    if(next.type == TokenType_RARROW) {
        node->node.FUNCTION_DEF.type = parseType(ctx, mem);
        next = parseConsume(ctx);
    } else {
        node->node.FUNCTION_DEF.type = typeVoid(mem);
    }

    if(!(next.type == TokenType_LSCOPE || next.type == TokenType_SEMICOLON)) {
        ERROR(next.loc, "Function declaration needs to have a body, or be marked as #extern");
    }

    return node;
}

#if 0
typedef struct ExrpressionTypeResult {
    Type type;
    bool isNegative;
} ExrpressionTypeResult;

ExrpressionTypeResult parseGetTypeOfExpression(ParseContext* ctx, ASTNode* expr) {
    if(expr->type == ASTNodeType_INT_LIT) {
        // ...
    } else if(expr->type == ASTNodeType_FLOAT_LIT) {
        // ...
    } else if(expr->type == ASTNodeType_STRING_LIT) {
        // ...
    } else if(expr->type == ASTNodeType_BOOL_LIT) {
        // ...
    } else if(expr->type == ASTNodeType_SYMBOL) {
        // ...
    } else if(expr->type == ASTNodeType_FUNCTION_CALL) {
        // ...
    } else if(expr->type == ASTNodeType_BINARY_EXPRESSION) {
        // ...
    } else {
        UNREACHABLE("called parseGetTypeOfExpression() on not an expression");
    }
}
#endif

ExpressionEvaluationResult evaluate_expression(ASTNode* expr) {
    ExpressionEvaluationResult result = {0};

    if (expr->type == ASTNodeType_INT_LIT) {
        String value = expr->node.INT_LIT.value;
        result.result = StringToU64(value);
    } else if (expr->type == ASTNodeType_FLOAT_LIT) {
        UNIMPLEMENTED("float in const");
    } else if (expr->type == ASTNodeType_BOOL_LIT) {
        UNIMPLEMENTED("bool in const");
    } else if (expr->type == ASTNodeType_STRING_LIT) {
        UNIMPLEMENTED("string in const");
    } else if (expr->type == ASTNodeType_FUNCTION_CALL) {
        UNIMPLEMENTED("function call in const");
    } else if (expr->type == ASTNodeType_BINARY_EXPRESSION) {
        String op = expr->node.BINARY_EXPRESSION.operator;
        ASTNode* lhs = expr->node.BINARY_EXPRESSION.lhs;
        ASTNode* rhs = expr->node.BINARY_EXPRESSION.rhs;
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
    } else if (expr->type == ASTNodeType_UNARY_EXPRESSION) {
        String op = expr->node.UNARY_EXPRESSION.operator;
        ASTNode* subExpr = expr->node.UNARY_EXPRESSION.expr;
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

ParseResult Parse(TokenArray tokens, Arena* mem) {
    ParseResult result = {0};
    ParseContext ctx2 = {.tokens = tokens};
    ParseContext* ctx = &ctx2;
    ctx->importLibraries = hashmapLibNameInit(mem, 0x100);
    ctx->funcInfo = hashmapFuncInfoInit(mem, 0x100);

    Scope* globalScope = parseScopeInit(mem, NULL);
    Scope* currentScope = globalScope;
    for(Token t = parseConsume(ctx); t.type != TokenType_NONE; t = parseConsume(ctx)){
        switch(t.type){
            case TokenType_COUNT:
            case TokenType_NONE: {
                printf("[ERROR] token type none and count are errors\n");
                exit(EXIT_FAILURE);
            } break;

            case TokenType_RETURN: {
                ASTNode* node = NodeInit(mem);
                node->type = ASTNodeType_RET;
                node->node.RET.expr = parseExpression(ctx, mem);
                
                // Check for semicolon
                if(parseCheckSemicolon(ctx)){
                    parseAddStatement(&currentScope->stmts, node);
                }
            } break;
            case TokenType_IDENTIFIER: {
                Token next = parsePeek(ctx, 0);
                if(next.type == TokenType_INITIALIZER){
                    // init
                    parseConsume(ctx);
                    ASTNode* node = NodeInit(mem);
                    node->type = ASTNodeType_VAR_DECL_ASSIGN;
                    node->node.VAR_DECL_ASSIGN.identifier = t.value;
                    ASTNode* expr = parseExpression(ctx, mem);
                    node->node.VAR_DECL_ASSIGN.expr = expr;

                    // TODO: this function can only be called after the second pass,
                    //       because ASTNodeType_SYMBOL only get its type once all functions are parsed,
                    //       so expressions can also only be typed on the second pass,
                    //       the only variables that can get a type are the ones explicitly given one and not inferred
                    node->node.VAR_DECL_ASSIGN.type = typeVoid(mem);
                    // node->node.VAR_DECL_ASSIGN.type = parseGetTypeOfExpression(ctx, mem, expr);
                    
                    // TODO: add the variable to the symbol table
                    parseScopeAddSymbol(currentScope, t.value);

                    // Check for semicolon
                    if(parseCheckSemicolon(ctx)){
                        parseAddStatement(&currentScope->stmts, node);
                    }
                }else if(next.type == TokenType_COLON){
                    // decl
                    parseConsume(ctx); // :
                    
                    ASTNode* node = NodeInit(mem);
                    Token identifier = t;
                    ASTNode* type = parseType(ctx, mem);
                    // ERROR(next.loc, "Variable declaration without initializer needs a type");

                    // TODO: add the variable to the symbol table
                    parseScopeAddSymbol(currentScope, t.value);
                    
                    // check if the var is initialized to a value
                    next = parsePeek(ctx, 0);
                    if(next.type == TokenType_ASSIGNMENT){
                        parseConsume(ctx); // =
                        
                        node->type = ASTNodeType_VAR_DECL_ASSIGN;
                        node->node.VAR_DECL_ASSIGN.expr = parseExpression(ctx, mem);
                        node->node.VAR_DECL_ASSIGN.identifier = identifier.value;
                        node->node.VAR_DECL_ASSIGN.type = type;
                    }else{
                        node->type = ASTNodeType_VAR_DECL;
                        node->node.VAR_DECL.identifier = identifier.value;
                        node->node.VAR_DECL.type = type;
                    }

                    // Check for semicolon
                    if(parseCheckSemicolon(ctx)){
                        parseAddStatement(&currentScope->stmts, node);
                    }
                }else if(next.type == TokenType_ASSIGNMENT){
                    // reassign
                    parseConsume(ctx);
                    ASTNode* node = NodeInit(mem);
                    node->type = ASTNodeType_VAR_REASSIGN;
                    node->node.VAR_REASSIGN.identifier = t.value;
                    node->node.VAR_REASSIGN.expr = parseExpression(ctx, mem);

                    // TODO: should look up in the symbol table if the symbol is already defined
                    // since functions can be defined after use this can only be done on the second pass
                    
                    // if(!parseScopeContainsSymbol(currentScope, t.value)){
                    //     ERROR(t.loc, "Undefined symbol");
                    //     exit(EXIT_FAILURE);
                    // }

                    // Check for semicolon
                    if(parseCheckSemicolon(ctx)){
                        parseAddStatement(&currentScope->stmts, node);
                    }
                }else if(next.type == TokenType_DOUBLECOLON){
                    // constant
                    parseConsume(ctx); // consume the ::
                    if(parsePeek(ctx, 0).type == TokenType_LPAREN && (parsePeek(ctx, 1).type == TokenType_IDENTIFIER || parsePeek(ctx, 1).type == TokenType_RPAREN)) {
                        // constant is a function
                        ASTNode* function = parseFunctionDecl(ctx, mem, currentScope, t);
                        assert(function->node.FUNCTION_DEF.isExtern == FALSE && "`parseFunctionDecl()` should set isExtern to FALSE by default");

                        FuncInfo info = {.isExtern = FALSE};
                        if(!hashmapFuncInfoSet(&ctx->funcInfo, t.value, info)) {
                            UNREACHABLE("failed to set hashmap");
                        }

                        // TODO: add the function to the symbol table
                        parseScopeAddSymbol(currentScope, t.value);
                        parseAddStatement(&currentScope->stmts, function);
                        currentScope = function->node.FUNCTION_DEF.scope;
                    } else {
                        // constant is expression
                        ASTNode* node = NodeInit(mem);
                        node->type = ASTNodeType_VAR_CONST;
                        node->node.VAR_CONST.identifier = t.value;
                        node->node.VAR_CONST.expr = parseExpression(ctx, mem);

                        // TODO: add the variable to the symbol table
                        parseScopeAddSymbol(currentScope, t.value);

                        // Check for semicolon
                        if(parseCheckSemicolon(ctx)){
                            parseAddStatement(&currentScope->stmts, node);
                        }
                    }
                }else if(next.type == TokenType_LPAREN){
                    // function call
                    Token identifier = t;
                    ASTNode* func = makeFunction(ctx, mem, identifier);

                    if(parseCheckSemicolon(ctx)){
                        parseAddStatement(&currentScope->stmts, func);
                    }
                }else{
                    ERROR(next.loc, "Symbol needs to be a variable/function declaration or function call");
                    exit(EXIT_FAILURE);
                }
            } break;
            case TokenType_RSCOPE: {
                // TODO: is this all here???
                if(!currentScope->parent){
                    ERROR(t.loc, "Closing parenthesis needs an openinig pair");
                    exit(EXIT_FAILURE);
                }
                currentScope = currentScope->parent;
            } break;
            case TokenType_IF: {
                // if block
                ASTNode* node = NodeInit(mem);
                node->type = ASTNodeType_IF;
                node->node.IF.expr = parseExpression(ctx, mem);
                
                Token next = parsePeek(ctx, 0);
                if(next.type != TokenType_LSCOPE){
                    // TODO: later add if condition without curly braces when it only contains one statement
                    ERROR(next.loc, "If condition needs a body");
                    exit(EXIT_FAILURE);
                }
                parseConsume(ctx);
                Scope* newScope = parseScopeInit(mem, currentScope);
                node->node.IF.scope = newScope;

                parseAddStatement(&currentScope->stmts, node);
                currentScope = newScope;
            } break;
            case TokenType_ELSE: {
                Token next = parsePeek(ctx, 0);
                if(next.type == TokenType_IF){
                    // else if
                    parseConsume(ctx);

                    ASTNode* node = NodeInit(mem);
                    node->type = ASTNodeType_ELSE_IF;
                    node->node.ELSE_IF.expr = parseExpression(ctx, mem);
                    
                    Token next = parsePeek(ctx, 0);
                    if(next.type != TokenType_LSCOPE){
                        // TODO: later add if condition without curly braces when it only contains one statement
                        ERROR(next.loc, "If condition needs a body");
                    }
                    parseConsume(ctx);

                    Scope* newScope = parseScopeInit(mem, currentScope);
                    node->node.ELSE_IF.scope = newScope;

                    parseAddStatement(&currentScope->stmts, node);
                    currentScope = newScope;
                }else if(next.type == TokenType_LSCOPE){
                    // else
                    #if 0
                    ASTNode* node = NodeInit(mem);
                    node->type = ASTNodeType_ELSE;

                    // next = parsePeek(ctx, 0);
                    if(next.type != TokenType_LSCOPE){
                        // TODO: later add else block without curly braces when it only contains one statement
                        ERROR(next.loc, "Else branch needs a body");
                        exit(EXIT_FAILURE);
                    }
                    parseConsume(ctx);
                    Scope* newScope = parseScopeInit(mem, currentScope);
                    node->node.ELSE.scope = newScope;

                    parseAddStatement(&currentScope->stmts, node);
                    currentScope = newScope;
                    #else
                    parseConsume(ctx);

                    ASTNode* node = NodeInit(mem);
                    node->type = ASTNodeType_ELSE;
                    Scope* newScope = parseScopeInit(mem, currentScope);
                    node->node.ELSE.scope = newScope;

                    parseAddStatement(&currentScope->stmts, node);
                    currentScope = newScope;
                    #endif
                }else{
                    ERROR(next.loc, "Else needs to be followed by if condition or a scope");
                }
            } break;
            case TokenType_LOOP: {
                ASTNode* node = NodeInit(mem);
                node->type = ASTNodeType_LOOP;
                node->node.LOOP.expr = parseExpression(ctx, mem);

                Token next = parsePeek(ctx, 0);
                if(next.type != TokenType_LSCOPE){
                    // TODO: later add loop condition without curly braces when it only contains one statement
                    ERROR(next.loc, "Loop needs a body");
                }
                parseConsume(ctx);

                Scope* newScope = parseScopeInit(mem, currentScope);
                node->node.LOOP.scope = newScope;

                parseAddStatement(&currentScope->stmts, node);
                currentScope = newScope;
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
                    String key = {.str = &next.value.str[1], .length = next.value.length - 2}; // remove the " around the string

                    CompilerInstruction* inst = CompInstInit(mem);
                    inst->type = CompilerInstructionType_LIB;
                    inst->inst.LIB.libName = key;

                    ASTNode* node = NodeInit(mem);
                    node->type = ASTNodeType_COMPILER_INST;
                    node->node.COMPILER_INST.inst = inst;

                    LibName value = {.functions = hashmapFuncNameInit(mem, 0x100)};
                    if(!hashmapLibNameSet(&ctx->importLibraries, key, value)) {
                        UNREACHABLE("hashmap failed to insert");
                    }
                    ctx->currentImportLibraryName = key;

                    if(parseCheckSemicolon(ctx)) {
                        parseAddStatement(&currentScope->stmts, node);
                    }
                } else if(StringEqualsCstr(next.value, "extern")) {
                    next = parseConsume(ctx);
                    if(next.type != TokenType_IDENTIFIER) {
                        ERROR(next.loc, "#extern needs to be followed by a function declaration");
                    }
                    Token functionName = next;

                    next = parseConsume(ctx);
                    if(next.type != TokenType_DOUBLECOLON) {
                        ERROR(next.loc, "#extern needs to be followed by a function declaration");
                    }

                    // NOTE: `parseFunctionDecl()` consumes the semicolon at the end of the line
                    // in case of #extern functions, no need to check here
                    ASTNode* function = parseFunctionDecl(ctx, mem, currentScope, functionName);
                    function->node.FUNCTION_DEF.isExtern = TRUE;
                    parseAddStatement(&currentScope->stmts, function);

                    CompilerInstruction* inst = CompInstInit(mem);
                    inst->type = CompilerInstructionType_EXTERN;
                    inst->inst.EXTERN.funcName = functionName.value;

                    ASTNode* node = NodeInit(mem);
                    node->type = ASTNodeType_COMPILER_INST;
                    node->node.COMPILER_INST.inst = inst;

                    LibName value = {0};
                    if(!hashmapLibNameGet(&ctx->importLibraries, ctx->currentImportLibraryName, &value)) {
                        UNREACHABLE("cant find library to import frunction from, either hashmap ran out of space or no #library specified");
                    }
                    FuncName value2 = {0};
                    if(!hashmapFuncNameSet(&value.functions, functionName.value, value2)) {
                        UNREACHABLE("hashmap failed to insert");
                    }
                    FuncInfo info = {.isExtern = TRUE};
                    if(!hashmapFuncInfoSet(&ctx->funcInfo, functionName.value, info)) {
                        UNREACHABLE("failed to set hashmap");
                    }

                    parseAddStatement(&currentScope->stmts, node);
                } else {
                    ERROR(next.loc, "Unknown compiler instruction");
                }
            } break;

            case TokenType_TYPE:
            case TokenType_SEMICOLON:
            case TokenType_COLON:
            case TokenType_DOUBLECOLON:
            case TokenType_INITIALIZER:
            case TokenType_RARROW:
            case TokenType_LPAREN:
            case TokenType_RPAREN:
            case TokenType_LSCOPE:
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
            case TokenType_SUB:
            case TokenType_MUL:
            case TokenType_DIV:
            case TokenType_ASSIGNMENT:
            case TokenType_COMPARISON:
            case TokenType_INT_LITERAL:
            case TokenType_STRING_LIT:
            case TokenType_BOOL_LITERAL:
                printf("[ERROR] Unhandled token type: %s at ("STR_FMT":%i:%i)\n", TokenTypeStr[t.type], STR_PRINT(t.loc.filename), t.loc.line, t.loc.collum);
            break;
        }
    }

    result.globalScope = globalScope;
    result.importLibraries = ctx->importLibraries; // TODO: ExitProcess need to be imported always
    result.funcInfo = ctx->funcInfo;
    return result;
}

// TODO: check if functions with return type return on all codepaths (typechecking step)
// TODO: all the keywords that are followed by a scope, should have the option to ommit the scope and use a single statement instead
// TODO: make statement parsing better, handle single instruction statements, handle globalScope and currentScope in a better way
