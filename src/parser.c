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

ASTNode* NodeInit(Arena* mem){
    ASTNode* node = arena_alloc(mem, sizeof(ASTNode));
    assert(node && "Failed to allocate AST node");
    node->type = ASTNodeType_NONE;
    return node;
}

#ifdef COMP_DEBUG
void ASTNodePrint(ASTNode* node, int indent){
    // TODO: need to fix indentation, everytime a case has a newline it needs to be indented
    for(int h = 0; h < indent; h++) printf("  ");
    switch(node->type){
        case ASTNodeType_COUNT: break;
        case ASTNodeType_NONE: break;
        
        case ASTNodeType_INT_LIT: {
            String val = node->node.INT_LIT.value;
            printf("INT LIT: %.*s\n", val.length, val.str);
        } break;
        case ASTNodeType_FLOAT_LIT: {
            String wholePart = node->node.FLOAT_LIT.wholePart;
            String fractPart = node->node.FLOAT_LIT.fractPart;
            printf("FLOAT LIT: %.*s.%.*s\n", wholePart.length, wholePart.str, fractPart.length, fractPart.str);
        } break;
        case ASTNodeType_STRING_LIT: {
            String val = node->node.STRING_LIT.value;
            if(val.length > 50) printf("STRING LIT: %.*s...\n", 50, val.str);
            else                printf("STRING LIT: %.*s\n", val.length, val.str);
        } break;
        case ASTNodeType_BOOL_LIT: {
            String val = node->node.STRING_LIT.value;
            printf("BOOL LIT: %.*s\n", val.length, val.str);
        } break;
        case ASTNodeType_EXPRESION: {
            String val = node->node.EXPRESION.operator;
            printf("EXP: %.*s\n", val.length, val.str);
            ASTNodePrint(node->node.EXPRESION.lhs, indent + 1);
            ASTNodePrint(node->node.EXPRESION.rhs, indent + 1);
        } break;
        case ASTNodeType_FUNCTION_DEF: {
            printf("FUNCTION DEF:\n");
            for(int h = 0; h < indent; h++) printf("  ");
            String id = node->node.FUNCTION_DEF.identifier;
            ASTNode* retType = node->node.FUNCTION_DEF.type;
            Args args = node->node.FUNCTION_DEF.args;
            Scope* scope = node->node.FUNCTION_DEF.scope;
            printf("id: %.*s\n", id.length, id.str);
            ASTNodePrint(retType, indent);
            for(int h = 0; h < indent; h++) printf("  ");

            // args
            for(int i = 0; i < args.size; i++){
                printf("arg%i:\n", i + 1);
                for(int h = 0; h < indent; h++) printf("  ");
                String argId = args.args[i]->node.VAR_DECL.identifier;
                ASTNode* argType = args.args[i]->node.VAR_DECL.type;
                printf(" id: %.*s\n", argId.length, argId.str);
                printf(" ");
                ASTNodePrint(argType, indent);
            }
            
            // statements
            for(int i = 0; i < scope->stmts.size; i++){
                for(int h = 0; h < indent + 1; h++) printf("  ");
                printf("Statement %i\n", i + 1);
                ASTNodePrint(scope->stmts.statements[i], indent + 1);
            }
        } break;
        case ASTNodeType_VAR_DECL: {
            printf("VAR DECL:\n");
            for(int h = 0; h < indent; h++) printf("  ");
            String id = node->node.VAR_DECL.identifier;
            ASTNode* type = node->node.VAR_DECL.type;
            printf("id: %.*s\n", id.length, id.str);
            ASTNodePrint(type, indent);
            for(int h = 0; h < indent; h++) printf("  ");
        } break;
        case ASTNodeType_VAR_DECL_ASSIGN: {
            printf("VAR DECL ASSIGN:\n");
            for(int h = 0; h < indent; h++) printf("  ");
            String id = node->node.VAR_DECL_ASSIGN.identifier;
            ASTNode* type = node->node.VAR_DECL_ASSIGN.type;
            ASTNode* expr = node->node.VAR_DECL_ASSIGN.expresion;
            printf("id: %.*s\n", id.length, id.str);
            ASTNodePrint(type, indent);
            for(int h = 0; h < indent; h++) printf("  ");
            printf("expr: \n");
            ASTNodePrint(expr, indent + 1);
        } break;
        case ASTNodeType_VAR_REASSIGN: {
            printf("VAR REASSIGN:\n");
            for(int h = 0; h < indent; h++) printf("  ");
            String id = node->node.VAR_REASSIGN.identifier;
            ASTNode* expr = node->node.VAR_REASSIGN.expresion;
            printf("id: %.*s\n", id.length, id.str);
            for(int h = 0; h < indent; h++) printf("  ");
            printf("expr: \n");
            ASTNodePrint(expr, indent + 1);
        } break;
        case ASTNodeType_VAR_CONST: {
            printf("VAR CONST:\n");
            for(int h = 0; h < indent; h++) printf("  ");
            String id = node->node.VAR_CONST.identifier;
            String value = node->node.VAR_CONST.value;
            printf("id: %.*s\n", id.length, id.str);
            for(int h = 0; h < indent; h++) printf("  ");
            printf("value: %.*s\n", value.length, value.str);
        } break;
        case ASTNodeType_RET: {
            printf("RET:\n");
            ASTNode* expr = node->node.RET.expresion;
            ASTNodePrint(expr, indent + 1);
        } break;
        case ASTNodeType_IF: {
            printf("IF:\n");
            for(int h = 0; h < indent; h++) printf("  ");
            ASTNode* expr = node->node.IF.expresion;
            Scope* scope = node->node.IF.scope;
            printf("expr:\n");
            ASTNodePrint(expr, indent + 1);

            // statements
            for(int i = 0; i < scope->stmts.size; i++){
                for(int h = 0; h < indent + 1; h++) printf("  ");
                printf("Statement %i\n", i + 1);
                ASTNodePrint(scope->stmts.statements[i], indent + 1);
            }
        } break;
        case ASTNodeType_ELSE: {
            printf("ELSE:\n");
            Scope* scope = node->node.ELSE.scope;

            // statements
            for(int i = 0; i < scope->stmts.size; i++){
                for(int h = 0; h < indent + 1; h++) printf("  ");
                printf("Statement %i\n", i + 1);
                ASTNodePrint(scope->stmts.statements[i], indent + 1);
            }
        } break;
        case ASTNodeType_ELSE_IF: {
            printf("ELSE_IF:\n");
            for(int h = 0; h < indent; h++) printf("  ");
            ASTNode* expr = node->node.ELSE_IF.expresion;
            Scope* scope = node->node.ELSE_IF.scope;
            printf("expr:\n");
            ASTNodePrint(expr, indent + 1);

            // statements
            for(int i = 0; i < scope->stmts.size; i++){
                for(int h = 0; h < indent + 1; h++) printf("  ");
                printf("Statement %i\n", i + 1);
                ASTNodePrint(scope->stmts.statements[i], indent + 1);
            }
        } break;
        case ASTNodeType_LOOP: {
            printf("LOOP:\n");
            for(int h = 0; h < indent; h++) printf("  ");
            ASTNode* expr = node->node.LOOP.expresion;
            Scope* scope = node->node.LOOP.scope;
            printf("expr:\n");
            ASTNodePrint(expr, indent + 1);

            // statements
            for(int i = 0; i < scope->stmts.size; i++){
                for(int h = 0; h < indent + 1; h++) printf("  ");
                printf("Statement %i\n", i + 1);
                ASTNodePrint(scope->stmts.statements[i], indent + 1);
            }
        } break;
        case ASTNodeType_SYMBOL: {
            String id = node->node.SYMBOL.identifier;
            printf("RVALUE: %.*s\n", id.length, id.str);
        } break;
        case ASTNodeType_FUNCTION_CALL: {
            printf("FUNCTION CALL\n");
            for(int h = 0; h < indent + 1; h++) printf("  ");
            String id = node->node.FUNCTION_CALL.identifier;
            Args args = node->node.FUNCTION_CALL.args;
            printf("id: %.*s\n", id.length, id.str);

            // args
            for(int i = 0; i < args.size; i++){
                for(int h = 0; h < indent + 1; h++) printf("  ");
                printf("arg%i:\n", i + 1);
                ASTNodePrint(args.args[i], indent + 1);
            }
        } break;
        case ASTNodeType_TYPE:{
            printf("TYPE: ");
            Type type = node->node.TYPE.type;
            bool array = node->node.TYPE.array;
            int arraySize = node->node.TYPE.arraySize;
            bool dynamic = node->node.TYPE.dynamic;
            printf("%s", TypeStr[type]);
            if(array){
                if(dynamic){
                    printf("[]");
                }else{
                    printf("[%i]", arraySize);
                }
            }
            printf("\n");
        } break;
    }
}

void ASTPrint(Scope* root){
    for(int i = 0; i < root->stmts.size; i++){
        printf("Statement %i\n", i + 1);
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
    node->type = ASTNodeType_EXPRESION;
    node->node.EXPRESION.lhs = left;
    node->node.EXPRESION.rhs = right;
    node->node.EXPRESION.operator = op.value;
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

ASTNode* parseLeaf(ParseContext* ctx, Arena* mem) {
    Token next = parseConsume(ctx);
    
    if(isFunction(ctx, next))               return makeFunction(ctx, mem, next);
    if(parseIsNumber(next))                 return makeNumber(ctx, mem, next);
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

    printf("[ERROR] Unhandled input\n");
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
    node->node.TYPE.array = FALSE;
    node->node.TYPE.arraySize = 0;
    node->node.TYPE.dynamic = FALSE;
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

ASTNode* parseType(ParseContext* ctx, Arena* mem) {
    UNUSED(ctx);
    UNUSED(mem);
    UNIMPLEMENTED("parseType in not implemented yet");
}

Scope* Parse(TokenArray tokens, Arena* mem) {
    ParseContext ctx2 = {.tokens = tokens};
    ParseContext* ctx = &ctx2;

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
                node->node.RET.expresion = parseExpression(ctx, mem);
                
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
                    node->node.VAR_DECL_ASSIGN.expresion = expr;

                    // TODO: do this in the typechecking phase
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
                        node->node.VAR_DECL_ASSIGN.expresion = parseExpression(ctx, mem);
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
                    node->node.VAR_REASSIGN.expresion = parseExpression(ctx, mem);

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
                    next = parsePeek(ctx, 0);
                    if(next.type == TokenType_INT_LITERAL){
                        // constant is a compile time value
                        ASTNode* node = NodeInit(mem);
                        node->type = ASTNodeType_VAR_CONST;
                        node->node.VAR_CONST.identifier = t.value;
                        // TODO: this should be an expresion but only if it is evaluatable during compile time
                        parseConsume(ctx);
                        node->node.VAR_CONST.value = next.value;

                        // TODO: add the variable to the symbol table
                        parseScopeAddSymbol(currentScope, t.value);

                        // Check for semicolon
                        if(parseCheckSemicolon(ctx)){
                            parseAddStatement(&currentScope->stmts, node);
                        }
                    }else if(next.type == TokenType_LPAREN){
                        Scope* scopeBackup = currentScope;
                        // constant is a function
                        ASTNode* node = NodeInit(mem);
                        node->type = ASTNodeType_FUNCTION_DEF;
                        node->node.FUNCTION_DEF.identifier = t.value;

                        // TODO: add the function to the symbol table
                        parseScopeAddSymbol(currentScope, t.value);

                        // args
                        Scope* functionScope = parseScopeInit(mem, currentScope);
                        node->node.FUNCTION_DEF.args = parseFunctionDeclArgs(ctx, functionScope);
                        currentScope = functionScope;

                        // ret type
                        next = parsePeek(ctx, 0);
                        if(next.type == TokenType_RARROW){
                            parseConsume(ctx);
                            node->node.FUNCTION_DEF.type = parseType(ctx, mem);

                            next = parseConsume(ctx);
                            if(next.type != TokenType_LSCOPE){
                                ERROR(next.loc, "Function needs to have a scope");
                                exit(EXIT_FAILURE);
                            }
                        }else if(next.type == TokenType_LSCOPE){
                            parseConsume(ctx);
                            node->node.FUNCTION_DEF.type = typeVoid(mem);
                        }else{
                            ERROR(next.loc, "Function args need to be followed by the return type or the function body");
                            exit(EXIT_FAILURE);
                        }

                        node->node.FUNCTION_DEF.scope = functionScope;
                        parseAddStatement(&scopeBackup->stmts, node);
                    }else{
                        ERROR(next.loc, "Constant needs to be a value known at compile time or a function declaration");
                        exit(EXIT_FAILURE);
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
                node->node.IF.expresion = parseExpression(ctx, mem);
                
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
                    node->node.ELSE_IF.expresion = parseExpression(ctx, mem);
                    
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
                node->node.LOOP.expresion = parseExpression(ctx, mem);

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
                printf("[ERROR] Unhandled token type: %s at (%.*s:%i:%i)\n", TokenTypeStr[t.type], t.loc.filename.length, t.loc.filename.str, t.loc.line, t.loc.collum);
            break;
        }
    }

    return globalScope;
}

// TODO: check if functions with return type return on all codepaths (typechecking step)
// TODO: all the keywords that are followed by a scope, should have the option to ommit the scope and use a single statement instead