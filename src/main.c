#include <stdio.h>  // fopen(), fseek(), ftell(), fread(), fclose(), printf(), 
#include <string.h> // strlen(), memcpy()
#include <assert.h> // assert()

#include "structs.h"
#include "codegen.h"
#include "parser.h"
#include "lexer.h"
#include "string.h"
#include "common.h"

#define ARENA_IMPLEMENTATION
#include "arena.h"

#define STB_DS_IMPLEMENTATION
#include "stb_ds.h"

// TODO: remove debug mode
// #define COMP_DEBUG

String EntireFileRead(Arena* mem, const char* filePath){
    FILE* f = fopen(filePath, "rb");
    
    if(f){
        fseek(f, 0, SEEK_END);
        int fileSize = ftell(f);
        fseek(f, 0, SEEK_SET);
        
        char* fileBuffer = arena_alloc(mem, fileSize * sizeof(char));
        fread(fileBuffer, sizeof(char), fileSize, f);
        fclose(f);

        String str = {
            .str = fileBuffer,
            .length = fileSize,
        };
        return str;
    }else{
        printf("[ERROR] Failed to open file: %s\n", filePath);
        return (String){0};
    }
}

bool EntireFileWrite(const char* filePath, StringChain data){
    FILE* f = fopen(filePath, "wb");

    if(f){
        StringNode* current = data.first;
        while(current != NULL){
            fprintf(f, "%.*s", current->str.length, current->str.str);
            current = current->next;
        }
        fclose(f);
        return TRUE;
    }else{
        printf("[ERROR] Failed to open file: %s\n", filePath);
        return FALSE;
    }
}

// 
// New Parser
// 

typedef enum ASTNodeType{
    ASTNodeType_NONE,
    
    ASTNodeType_FUNCTION_DEF,
    ASTNodeType_FUNCTION_CALL,
    ASTNodeType_VAR_DECL,
    ASTNodeType_VAR_DECL_ASSIGN,
    ASTNodeType_VAR_REASSIGN,
    ASTNodeType_VAR_CONST,
    ASTNodeType_RET,
    ASTNodeType_IF,
    ASTNodeType_ELSE,
    ASTNodeType_EXPRESION,
    ASTNodeType_INT_LIT,
    ASTNodeType_SYMBOL_RVALUE,
    
    ASTNodeType_COUNT,
} ASTNodeType;

typedef struct _ASTNode ASTNode;

typedef struct Args{
    // the type of this node has to be:
    // on FUNCTION_DEF - VAR_DECL
    // on FUNCTION_CALL - INT_LIT or EXPRESION
    ASTNode** args;
    int size;
    int capacity;

    Arena mem;
} Args;

typedef struct StmtList{
    ASTNode** statements;
    int size;
    int capacity;

    Arena mem;
} StmtList;

typedef struct Scope{
    Arena mem;

    String* symbolTable;
    int symbolSize;
    int symbolCapacity;

    struct Scope* parent;

    struct Scope** children;
    int childrenSize;
    int childrenCapacity;

    StmtList stmts;
} Scope;

typedef struct _ASTNode{
    ASTNodeType type;
    union Node{
        struct FUNCTION_DEF {
            String identifier;
            String type;
            Args args;
            Scope* scope;
        } FUNCTION_DEF;
        struct FUNCTION_CALL {
            String identifier;
            Args args;
        } FUNCTION_CALL;
        struct VAR_DECL {
            String identifier;
            String type;
        } VAR_DECL;
        struct VAR_DECL_ASSIGN {
            String identifier;
            String type;
            ASTNode* expresion;
        } VAR_DECL_ASSIGN;
        struct VAR_REASSIGN {
            String identifier;
            ASTNode* expresion;
        } VAR_REASSIGN;
        struct VAR_CONST {
            String identifier;
            String value;
        } VAR_CONST;
        struct RET {
            ASTNode* expresion;
        } RET;
        struct EXPRESION {
            String operator;
            ASTNode* rhs;
            ASTNode* lhs;
        } EXPRESION;
        struct INT_LIT {
            String value;
        } INT_LIT;
        struct IF {
            ASTNode* expresion;
            Scope* scope;
        } IF;
        struct ELSE {
            Scope* scope;
        } ELSE;
        struct SYMBOL_RVALUE {
            String identifier;
        } SYMBOL_RVALUE;
    } node;
} ASTNode;

void parseAddStatement(StmtList* list, ASTNode* node){
    if(list->size >= list->capacity){
        size_t newCap = list->capacity * 2;
        if(newCap == 0) newCap = 1;
        list->statements = arena_realloc(&list->mem, list->statements, list->capacity * sizeof(list->statements[0]), newCap * sizeof(list->statements[0]));
        list->capacity = newCap;
    }

    list->statements[list->size++] = node;
}

typedef struct ParseContext{
    TokenArray tokens;
	int index;
    
    TypeInformation* typeInfo;
    OperatorInformation* opsInfo;
} ParseContext;

ParseContext ParseContextInit(TokenArray tokens, TypeInformation* typeInfo, OperatorInformation* opsInfo){
    ParseContext ctx = {
        .tokens = tokens,
        .typeInfo = typeInfo,
        .opsInfo = opsInfo,
    };
    return ctx;
}

TypeDefinition TypeDefinitionInit(String symbol, int byteSize){
    TypeDefinition type = {
        .symbol = symbol,
        .byteSize = byteSize,
    };
    return type;
}

OperatorDefinition OperatorDefinitionInit(String symbol, int precedence, TypeDefinition ret, TypeDefinition lhs, TypeDefinition rhs){
    OperatorDefinition op = {
        .symbol = symbol,
        .precedence = precedence,
        .retType = ret,
        .lhsType = lhs,
        .rhsType = rhs,
    };
    return op;
}

ASTNode* NodeInit2(Arena* mem){
    ASTNode* node = arena_alloc(mem, sizeof(ASTNode));
    assert(node && "Failed to allocate AST node");
    node->type = ASTNodeType_NONE;
    return node;
}

void ASTNodePrint2(ASTNode* node, int indent){
    // TODO: need to fix indentation, everytime a case has a newline it needs to be indented
    for(int h = 0; h < indent; h++) printf("  ");
    switch(node->type){
        case ASTNodeType_COUNT: break;
        case ASTNodeType_NONE: break;
        
        case ASTNodeType_INT_LIT: {
            String val = node->node.INT_LIT.value;
            printf("INT LIT: %.*s\n", val.length, val.str);
        } break;
        case ASTNodeType_EXPRESION: {
            String val = node->node.EXPRESION.operator;
            printf("EXP: %.*s\n", val.length, val.str);
            ASTNodePrint2(node->node.EXPRESION.lhs, indent + 1);
            ASTNodePrint2(node->node.EXPRESION.rhs, indent + 1);
        } break;
        case ASTNodeType_FUNCTION_DEF: {
            printf("FUNCTION DEF:\n");
            for(int h = 0; h < indent; h++) printf("  ");
            String id = node->node.FUNCTION_DEF.identifier;
            String retType = node->node.FUNCTION_DEF.type;
            Args args = node->node.FUNCTION_DEF.args;
            Scope* scope = node->node.FUNCTION_DEF.scope;
            printf("id: %.*s\n", id.length, id.str);
            for(int h = 0; h < indent; h++) printf("  ");
            printf("ret: %.*s\n", retType.length, retType.str);
            for(int h = 0; h < indent; h++) printf("  ");

            // args
            for(int i = 0; i < args.size; i++){
                printf("arg%i:\n", i + 1);
                for(int h = 0; h < indent; h++) printf("  ");
                String argId = args.args[i]->node.VAR_DECL.identifier;
                String argType = args.args[i]->node.VAR_DECL.type;
                printf(" id: %.*s\n", argId.length, argId.str);
                for(int h = 0; h < indent; h++) printf("  ");
                printf(" type: %.*s\n", argType.length, argType.str);
            }
            
            // statements
            for(int i = 0; i < scope->stmts.size; i++){
                for(int h = 0; h < indent + 1; h++) printf("  ");
                printf("Statement %i\n", i + 1);
                ASTNodePrint2(scope->stmts.statements[i], indent + 1);
            }
        } break;
        case ASTNodeType_VAR_DECL: {
            printf("VAR DECL:\n");
            for(int h = 0; h < indent; h++) printf("  ");
            String id = node->node.VAR_DECL.identifier;
            String type = node->node.VAR_DECL.type;
            printf("id: %.*s\n", id.length, id.str);
            for(int h = 0; h < indent; h++) printf("  ");
            printf("type: %.*s\n", type.length, type.str);
            for(int h = 0; h < indent; h++) printf("  ");
        } break;
        case ASTNodeType_VAR_DECL_ASSIGN: {
            printf("VAR DECL ASSIGN:\n");
            for(int h = 0; h < indent; h++) printf("  ");
            String id = node->node.VAR_DECL_ASSIGN.identifier;
            String type = node->node.VAR_DECL_ASSIGN.type;
            ASTNode* expr = node->node.VAR_DECL_ASSIGN.expresion;
            printf("id: %.*s\n", id.length, id.str);
            for(int h = 0; h < indent; h++) printf("  ");
            printf("type: %.*s\n", type.length, type.str);
            for(int h = 0; h < indent; h++) printf("  ");
            printf("expr: \n");
            ASTNodePrint2(expr, indent + 1);
        } break;
        case ASTNodeType_VAR_REASSIGN: {
            printf("VAR REASSIGN:\n");
            for(int h = 0; h < indent; h++) printf("  ");
            String id = node->node.VAR_REASSIGN.identifier;
            ASTNode* expr = node->node.VAR_REASSIGN.expresion;
            printf("id: %.*s\n", id.length, id.str);
            for(int h = 0; h < indent; h++) printf("  ");
            printf("expr: \n");
            ASTNodePrint2(expr, indent + 1);
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
            ASTNodePrint2(expr, indent + 1);
        } break;
        case ASTNodeType_IF: {
            printf("IF:\n");
            for(int h = 0; h < indent; h++) printf("  ");
            ASTNode* expr = node->node.IF.expresion;
            Scope* scope = node->node.IF.scope;
            printf("expr:\n");
            ASTNodePrint2(expr, indent + 1);

            // statements
            for(int i = 0; i < scope->stmts.size; i++){
                for(int h = 0; h < indent + 1; h++) printf("  ");
                printf("Statement %i\n", i + 1);
                ASTNodePrint2(scope->stmts.statements[i], indent + 1);
            }
        } break;
        case ASTNodeType_ELSE: {
            printf("ELSE:\n");
            Scope* scope = node->node.ELSE.scope;

            // statements
            for(int i = 0; i < scope->stmts.size; i++){
                for(int h = 0; h < indent + 1; h++) printf("  ");
                printf("Statement %i\n", i + 1);
                ASTNodePrint2(scope->stmts.statements[i], indent + 1);
            }
        } break;
        case ASTNodeType_SYMBOL_RVALUE: {
            String id = node->node.SYMBOL_RVALUE.identifier;
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
                ASTNodePrint2(args.args[i], indent + 1);
            }
        } break;
    }
}

void ASTPrint2(Scope* root){
    for(int i = 0; i < root->stmts.size; i++){
        printf("Statement %i\n", i + 1);
        ASTNodePrint2(root->stmts.statements[i], 0);
    }
}

int OpGetPrecedence2(ParseContext* ctx, String op){
    for(int i = 0; i < ctx->opsInfo->size; i++){
        if(StringEquals(ctx->opsInfo->ops[i].symbol, op)){
            return ctx->opsInfo->ops[i].precedence;
        }
    }
    return -1;
}

Token parseConsume2(ParseContext* ctx){
	if (ctx->index + 1 > ctx->tokens.size) return (Token){0};
	return ctx->tokens.tokens[ctx->index++];
}

Token parsePeek2(ParseContext* ctx, int num){
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

ASTNode* parseFunctionCall(ParseContext* ctx, Arena* mem, Scope* scope);

ASTNode* parsePrimary2(ParseContext* ctx, Arena* mem, Scope* scope){
	Token t = parseConsume2(ctx);
	if(t.type == TokenType_INT_LITERAL){
		ASTNode* node = NodeInit2(mem);
		node->type = ASTNodeType_INT_LIT;
        node->node.INT_LIT.value = t.value;
		return node;
	}else if(t.type == TokenType_IDENTIFIER){
		ASTNode* node = NodeInit2(mem);
		node->type = ASTNodeType_SYMBOL_RVALUE;
        node->node.SYMBOL_RVALUE.identifier = t.value;
        // TODO: should look up in the symbol table if the symbol is already defined
        // since functions can be defined after use this can only be done on the second pass
        
        UNUSED(scope);
        // if(!parseScopeContainsSymbol(scope, t.value)){
        //     ERROR(t.loc, "Undefined symbol");
        //     exit(EXIT_FAILURE);
        // }

		return node;
	}else{
        // ERROR(t.loc, "Malformed expresion, exprected int literal or variable");
		// exit(EXIT_FAILURE);
        return NULL;
	}
}

// TODO: make expresion parsing not recursive
// TODO: make parenthesis reset precedece so correct AST gets generated
// NOTE: Implementation from: https://en.wikipedia.org/wiki/Operator-precedence_parser
ASTNode* parseExpression2_rec(ParseContext* ctx, Arena* mem, Scope* scope, ASTNode* lhs, int precedence){
	Token next = parsePeek2(ctx, 0);
	while(next.type == TokenType_OPERATOR){
		int tokenPrecedence = OpGetPrecedence2(ctx, next.value);
		if(tokenPrecedence >= precedence){
			Token op = next;
			parseConsume2(ctx);
            ASTNode* rhs = parseFunctionCall(ctx, mem, scope);
            if(!rhs) rhs = parsePrimary2(ctx, mem, scope);
            if(!rhs) return rhs;
			next = parsePeek2(ctx, 0);
			while(next.type == TokenType_OPERATOR){
				int newPrecedence = OpGetPrecedence2(ctx, next.value);
				// the associativity of the operator can be set here
				if(newPrecedence >= tokenPrecedence){
					rhs = parseExpression2_rec(ctx, mem, scope, rhs, tokenPrecedence + (newPrecedence > tokenPrecedence ? 1 : 0));
					next = parsePeek2(ctx, 0);
				}else{
                    break;
                }
			}
			ASTNode* node = NodeInit2(mem);
			node->type = ASTNodeType_EXPRESION;
			node->node.EXPRESION.operator = op.value;
            node->node.EXPRESION.lhs = lhs;
			node->node.EXPRESION.rhs = rhs;
            lhs = node;
		}else{
            break;
        }
	}
	return lhs;
}

ASTNode* parseExpression2(ParseContext* ctx, Arena* mem, Scope* scope){
    ASTNode* func = parseFunctionCall(ctx, mem, scope);
    if(func){
        return parseExpression2_rec(ctx, mem, scope, func, 0);
    }else{
        ASTNode* intLit = parsePrimary2(ctx, mem, scope);
        return parseExpression2_rec(ctx, mem, scope, intLit, 0);
    }
}

bool parseCheckSemicolon(ParseContext* ctx){
    Token next = parsePeek2(ctx, 0);
    if(next.type == TokenType_SEMICOLON){
        parseConsume2(ctx);
        return TRUE;
    }else{
        ERROR(next.loc, "Statement needs to end with ;");
        exit(EXIT_FAILURE);
    }
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

void parseType(ParseContext* ctx){
    UNUSED(ctx);
    UNIMPLEMENTED("parseType");
}

// NOTE: parseFunctionCall and parseExpresion should return a custom error type that stores the error if one happened.
// ctx should point to the function id token
ASTNode* parseFunctionCall(ParseContext* ctx, Arena* mem, Scope* scope){
    Args args = {0};
    ASTNode* node = NodeInit2(mem);
    node->type = ASTNodeType_FUNCTION_CALL;

    Token next = parsePeek2(ctx, 0);
    if(!(next.type == TokenType_IDENTIFIER)){
        // error
        return NULL;
    }
    node->node.FUNCTION_CALL.identifier = next.value;
    
    next = parsePeek2(ctx, 1);
    if(!(next.type == TokenType_LPAREN)){
        // error
        return NULL;
    }
    // only consume after function is confirmed
    parseConsume2(ctx);
    parseConsume2(ctx);
    for(int i = ctx->index; i < ctx->tokens.size; i++){
        // TODO: maybe do a check for no arguments here
        next = parsePeek2(ctx, 0);
        if(next.type == TokenType_RPAREN){
            parseConsume2(ctx);
            break;
        }
        
        ASTNode* expr = parseExpression2(ctx, mem, scope);
        parseAddArg(&args, expr);
        
        next = parsePeek2(ctx, 0);
        if(next.type == TokenType_COMMA){
            parseConsume2(ctx);
            continue;
        }else if(next.type == TokenType_RPAREN){
            // break
            parseConsume2(ctx);
            break;
        }else{
            // error
            return NULL;
        }
    }
    node->node.FUNCTION_CALL.args = args;
    // parseAddStatement(&scope->stmts, node);
    return node;
}

// the ctx needs to point at '('
Args parseFunctionDeclArgs(ParseContext* ctx, Scope* scope){
    Args result = {0};
    
    Token next = parseConsume2(ctx);
    if(!(next.type == TokenType_LPAREN)){
        ERROR(next.loc, "Function arguments need to be inside parenthesis");
        exit(EXIT_FAILURE);
    }
    for(int i = ctx->index; i < ctx->tokens.size; i++){
        next = parsePeek2(ctx, 0);
        if(next.type == TokenType_IDENTIFIER){
            parseConsume2(ctx);
            parseScopeAddSymbol(scope, next.value);
            Token id = next;

            next = parsePeek2(ctx, 0);
            if(!(next.type == TokenType_COLON)){
                ERROR(next.loc, "Identifier name and type have to be separated a colon \":\"");
                exit(EXIT_FAILURE);
            }
            parseConsume2(ctx);
            next = parsePeek2(ctx, 0);
            if(!(next.type == TokenType_TYPE)){
                ERROR(next.loc, "Function argument needs a type");
                exit(EXIT_FAILURE);
            }
            parseConsume2(ctx);
            ASTNode* node = NodeInit2(&result.mem);
            node->type = ASTNodeType_VAR_DECL;
            node->node.VAR_DECL.identifier = id.value;
            node->node.VAR_DECL.type = next.value;

            parseAddArg(&result, node);

            next = parsePeek2(ctx, 0);
            if(next.type == TokenType_COMMA){
                parseConsume2(ctx);
                continue;
            }else if(next.type == TokenType_RPAREN){
                parseConsume2(ctx);
                break;
            }else{
                ERROR(next.loc, "Function declaration needs to end with a closing parenthesis \")\"");
                exit(EXIT_FAILURE);
            }
        }else if(next.type == TokenType_RPAREN){
            parseConsume2(ctx);
            break;
        }else{
            ERROR(next.loc, "Function argument needs an identifier");
            exit(EXIT_FAILURE);
        }
    }

    return result;
}

Scope* Parse2(ParseContext* ctx, Arena* mem){
    Scope* globalScope = parseScopeInit(mem, NULL);
    Scope* currentScope = globalScope;
    for(Token t = parseConsume2(ctx); t.type != TokenType_NONE; t = parseConsume2(ctx)){
        switch(t.type){
            case TokenType_COUNT:
            case TokenType_NONE: {
                printf("[ERROR] token type none and count are errors\n");
                exit(EXIT_FAILURE);
            } break;

            case TokenType_RETURN: {
                ASTNode* node = NodeInit2(mem);
                node->type = ASTNodeType_RET;
                ASTNode* expr = parseExpression2(ctx, mem, currentScope);
                if(!expr){
                    ERROR(t.loc, "Return keyword needs a valid expresion to return");
                    exit(EXIT_FAILURE);
                }
                node->node.RET.expresion = expr;
                
                // Check for semicolon
                if(parseCheckSemicolon(ctx)){
                    parseAddStatement(&currentScope->stmts, node);
                }
            } break;
            case TokenType_IDENTIFIER: {
                Token next = parsePeek2(ctx, 0);
                if(next.type == TokenType_INITIALIZER){
                    // init
                    parseConsume2(ctx);
                    ASTNode* node = NodeInit2(mem);
                    node->type = ASTNodeType_VAR_DECL_ASSIGN;
                    node->node.VAR_DECL_ASSIGN.identifier = t.value;
                    ASTNode* expr = parseExpression2(ctx, mem, currentScope);
                    if(!expr){
                        ERROR(next.loc, "Variable needs a valid expresion to initialize");
                        exit(EXIT_FAILURE);
                    }
                    node->node.VAR_DECL_ASSIGN.expresion = expr;
                    // TODO: add types, figure out type here
                    // node->node.VAR_DECL_ASSIGN.type = ;
                    
                    // TODO: add the variable to the symbol table
                    parseScopeAddSymbol(currentScope, t.value);

                    // Check for semicolon
                    if(parseCheckSemicolon(ctx)){
                        parseAddStatement(&currentScope->stmts, node);
                    }
                }else if(next.type == TokenType_COLON){
                    // decl
                    parseConsume2(ctx);
                    next = parsePeek2(ctx, 0);
                    if(!(next.type == TokenType_TYPE)){
                        ERROR(next.loc, "Variable declaration without initializer needs a type");
                        exit(EXIT_FAILURE);
                    }
                    parseConsume2(ctx);
                    ASTNode* node = NodeInit2(mem);
                    node->type = ASTNodeType_VAR_DECL;
                    node->node.VAR_DECL.identifier = t.value;
                    node->node.VAR_DECL.type = next.value;

                    // TODO: add the variable to the symbol table
                    parseScopeAddSymbol(currentScope, t.value);
                    
                    // Check for semicolon
                    if(parseCheckSemicolon(ctx)){
                        parseAddStatement(&currentScope->stmts, node);
                    }
                }else if(next.type == TokenType_ASSIGNMENT){
                    // reassign
                    parseConsume2(ctx);
                    ASTNode* node = NodeInit2(mem);
                    node->type = ASTNodeType_VAR_REASSIGN;
                    node->node.VAR_REASSIGN.identifier = t.value;
                    ASTNode* expr = parseExpression2(ctx, mem, currentScope);
                    if(!expr){
                        ERROR(next.loc, "Variable assignment needs a valid expresion");
                        exit(EXIT_FAILURE);
                    }
                    node->node.VAR_REASSIGN.expresion = expr;

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
                    parseConsume2(ctx); // consume the ::
                    next = parsePeek2(ctx, 0);
                    if(next.type == TokenType_INT_LITERAL){
                        // constant is a compile time value
                        ASTNode* node = NodeInit2(mem);
                        node->type = ASTNodeType_VAR_CONST;
                        node->node.VAR_CONST.identifier = t.value;
                        // TODO: this should be an expresion but only if it is evaluatable during compile time
                        parseConsume2(ctx);
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
                        ASTNode* node = NodeInit2(mem);
                        node->type = ASTNodeType_FUNCTION_DEF;
                        node->node.FUNCTION_DEF.identifier = t.value;

                        // TODO: add the function to the symbol table
                        parseScopeAddSymbol(currentScope, t.value);

                        // args
                        Scope* functionScope = parseScopeInit(mem, currentScope);
                        node->node.FUNCTION_DEF.args = parseFunctionDeclArgs(ctx, functionScope);
                        currentScope = functionScope;

                        // ret type
                        next = parsePeek2(ctx, 0);
                        if(!(next.type == TokenType_RARROW)){
                            ERROR(next.loc, "Function arguments need to be followed by the return type bikeshedder \"->\"");
                            exit(EXIT_FAILURE);
                        }
                        parseConsume2(ctx);
                        next = parsePeek2(ctx, 0);
                        if(!(next.type == TokenType_TYPE)){
                            ERROR(next.loc, "Function needs a return type");
                            exit(EXIT_FAILURE);
                        }
                        parseConsume2(ctx);
                        node->node.FUNCTION_DEF.type = next.value;
                        next = parsePeek2(ctx, 0);
                        if(!(next.type == TokenType_LSCOPE)){
                            ERROR(next.loc, "Function needs to have a scope");
                            exit(EXIT_FAILURE);
                        }
                        parseConsume2(ctx);

                        node->node.FUNCTION_DEF.scope = functionScope;
                        parseAddStatement(&scopeBackup->stmts, node);
                    }else{
                        ERROR(next.loc, "Constant needs to be a value known at compile time or a function declaration");
                        exit(EXIT_FAILURE);
                    }
                }else{
                    ERROR(next.loc, "Symbol creation needs to be a variable, constant or function");
                    exit(EXIT_FAILURE);
                }
            } break;
            case TokenType_RSCOPE: {
                // TODO: is this all here???
                if(!currentScope->parent){
                    ERROR(t.loc, "Closing parenthesis needs a pair");
                    exit(EXIT_FAILURE);
                }
                currentScope = currentScope->parent;
            } break;
            case TokenType_IF: {
                // if block
                // parseConsume2(ctx);
                ASTNode* node = NodeInit2(mem);
                node->type = ASTNodeType_IF;
                ASTNode* expr = parseExpression2(ctx, mem, currentScope);
                if(!expr){
                    ERROR(t.loc, "Invalid expresion in if condition");
                    exit(EXIT_FAILURE);
                }
                node->node.IF.expresion = expr;
                
                Token next = parsePeek2(ctx, 0);
                if(!(next.type == TokenType_LSCOPE)){
                    // TODO: later add if condition without curly braces when it only contains one statement
                    ERROR(next.loc, "If condition needs a body");
                    exit(EXIT_FAILURE);
                }
                parseConsume2(ctx);
                Scope* newScope = parseScopeInit(mem, currentScope);
                node->node.IF.scope = newScope;

                parseAddStatement(&currentScope->stmts, node);
                currentScope = newScope;
            } break;
            case TokenType_ELSE: {
                ASTNode* node = NodeInit2(mem);
                node->type = ASTNodeType_ELSE;

                Token next = parsePeek2(ctx, 0);
                if(!(next.type == TokenType_LSCOPE)){
                    // TODO: later add else block without curly braces when it only contains one statement
                    ERROR(next.loc, "Else branch needs a body");
                    exit(EXIT_FAILURE);
                }
                parseConsume2(ctx);
                Scope* newScope = parseScopeInit(mem, currentScope);
                node->node.ELSE.scope = newScope;

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
            case TokenType_OPERATOR:
            case TokenType_ASSIGNMENT:
            case TokenType_COMPARISON:
            case TokenType_INT_LITERAL:
                printf("[ERROR] Unhandled token type: %s\n", TokenTypeStr[t.type]);
            break;
        }
    }

    return globalScope;
}

// 
// Main entry point
// 

int main(int argc, char** argv){
    if(argc < 2){
        printf("[ERROR] Source file not provided\n");
        exit(EXIT_FAILURE);
    }

    // parse args
    bool printTokens = FALSE;
    bool printAST = FALSE;
    char* filepath;
    for(int i = 0; i < argc; i++){
        if(strcmp(argv[i], "--tokens") == 0){
            printTokens = TRUE;
        }else if(strcmp(argv[i], "--ast") == 0){
            printAST = TRUE;
        }else{
            if(argv[i][0] == '-' && argv[i][1] == '-'){
                printf("[ERROR] Argument \"%s\" not supported, supported args:\n", argv[i]);
                printf("[ERROR]   --tokens\n");
                printf("[ERROR]   --ast\n");
                exit(EXIT_FAILURE);
            }else{
                filepath = argv[i];
            }
        }
    }

    // types
    TypeInformation typeInfo = {0};
    addType(&typeInfo, TypeDefinitionInit(StringFromCstrLit("u8"),  1)); // NOTE: for now the [0] item is what all binaty operators operate on
    addType(&typeInfo, TypeDefinitionInit(StringFromCstrLit("u16"), 2));
    addType(&typeInfo, TypeDefinitionInit(StringFromCstrLit("u32"), 4));
    addType(&typeInfo, TypeDefinitionInit(StringFromCstrLit("u64"), 8));
    addType(&typeInfo, TypeDefinitionInit(StringFromCstrLit("s8"),  1));
    addType(&typeInfo, TypeDefinitionInit(StringFromCstrLit("s16"), 2));
    addType(&typeInfo, TypeDefinitionInit(StringFromCstrLit("s32"), 4));
    addType(&typeInfo, TypeDefinitionInit(StringFromCstrLit("s64"), 8));
    addType(&typeInfo, TypeDefinitionInit(StringFromCstrLit("string"), 0)); // TODO: figure out string byteSize
    addType(&typeInfo, TypeDefinitionInit(StringFromCstrLit("bool"), 1));

    // oeperators
    OperatorInformation opInfo = {0};
    // TODO: figure out how to group all the int-like types so biary operators can be generated easily
    addOperator(&opInfo, OperatorDefinitionInit(StringFromCstrLit("=="), 4, typeInfo.types[0], typeInfo.types[9], typeInfo.types[0]));
    addOperator(&opInfo, OperatorDefinitionInit(StringFromCstrLit("+"), 5, typeInfo.types[0], typeInfo.types[0], typeInfo.types[0]));
    addOperator(&opInfo, OperatorDefinitionInit(StringFromCstrLit("-"), 5, typeInfo.types[0], typeInfo.types[0], typeInfo.types[0]));
    addOperator(&opInfo, OperatorDefinitionInit(StringFromCstrLit("*"), 10, typeInfo.types[0], typeInfo.types[0], typeInfo.types[0]));
    addOperator(&opInfo, OperatorDefinitionInit(StringFromCstrLit("/"), 10, typeInfo.types[0], typeInfo.types[0], typeInfo.types[0]));

    Arena readFileMem = {0}; // source file is stored in here
    String sourceRaw = EntireFileRead(&readFileMem, filepath);

    int filenameLen = strlen(filepath);
    String filename = {.str = filepath, .length = filenameLen};
    Tokenizer tokenizer = TokenizerInit(sourceRaw, filename, &typeInfo, &opInfo);
    TokenArray tokens = Tokenize(&tokenizer);
    if(printTokens) TokensPrint(&tokens);
    
    ParseContext ctx = ParseContextInit(tokens, &typeInfo, &opInfo);

    Scope* globalScope = Parse2(&ctx, &readFileMem);
    if(printAST) ASTPrint2(globalScope);

    #if 0
    Generator gen = {0}; // uses memory arena
    StringChain outputString = Generate(&gen, &ast);

    bool success = EntireFileWrite("output.asm", outputString);
    if(!success){
        printf("[ERROR] Failed to write to file.\n");
        exit(EXIT_FAILURE);
    }
    #endif

    // TODO: do proper CreateProcess() calls here, but this will do for now
    // system("nasm -fwin64 output.asm");
    // system("ld -o output.exe output.obj");

    // NOTE: for using kernel functions build like this
    // system("nasm -fwin32 output.asm");
    // system("C:\\MinGW\\bin\\gcc.exe -m32 -o output.exe output.obj -lkernel32");

    // arena_free(&parser.mem);
    // arena_free(&gen.mem);
    
    arena_free(&readFileMem);
    exit(EXIT_SUCCESS);
}

// TODO: remove arenas from scope struct, all ast nodes and scope data should be allocated in one arena
// TODO: better error messeges when failing to parse an expresion
// TODO: instead of specifying operator left hand type and right hand type,
//       use type properties so we dont have to add operators multiple times for different types