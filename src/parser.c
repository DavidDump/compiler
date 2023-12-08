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

ParseContext ParseContextInit(TokenArray tokens, TypeInformation* typeInfo, OperatorInformation* opsInfo){
    ParseContext ctx = {
        .tokens = tokens,
        .typeInfo = typeInfo,
        .opsInfo = opsInfo,
    };
    return ctx;
}

ASTNode* NodeInit(Arena* mem){
    ASTNode* node = arena_alloc(mem, sizeof(ASTNode));
    assert(node && "Failed to allocate AST node");
    node->type = ASTNodeType_NONE;
    return node;
}

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
                ASTNodePrint(args.args[i], indent + 1);
            }
        } break;
        case ASTNodeType_TYPE:{
            printf("TYPE: ");
            String symbol = node->node.TYPE.symbol;
            bool array = node->node.TYPE.array;
            printf("%.*s", symbol.length, symbol.str);
            if(array) printf("[]");
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

int OpGetPrecedence(ParseContext* ctx, String op){
    for(int i = 0; i < ctx->opsInfo->size; i++){
        if(StringEquals(ctx->opsInfo->ops[i].symbol, op)){
            return ctx->opsInfo->ops[i].precedence;
        }
    }
    return -1;
}

Token parseConsume(ParseContext* ctx){
	if (ctx->index + 1 > ctx->tokens.size) return (Token){0};
	return ctx->tokens.tokens[ctx->index++];
}

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

ASTNode* parsePrimary(ParseContext* ctx, Arena* mem, Scope* scope){
	Token t = parseConsume(ctx);
	if(t.type == TokenType_INT_LITERAL){
		ASTNode* node = NodeInit(mem);
		node->type = ASTNodeType_INT_LIT;
        node->node.INT_LIT.value = t.value;
		return node;
	}else if(t.type == TokenType_IDENTIFIER){
		ASTNode* node = NodeInit(mem);
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
ASTNode* parseExpression_rec(ParseContext* ctx, Arena* mem, Scope* scope, ASTNode* lhs, int precedence){
	Token next = parsePeek(ctx, 0);
	while(next.type == TokenType_OPERATOR){
		int tokenPrecedence = OpGetPrecedence(ctx, next.value);
		if(tokenPrecedence >= precedence){
			Token op = next;
			parseConsume(ctx);
            ASTNode* rhs = parseFunctionCall(ctx, mem, scope);
            if(!rhs) rhs = parsePrimary(ctx, mem, scope);
            if(!rhs) return rhs;
			next = parsePeek(ctx, 0);
			while(next.type == TokenType_OPERATOR){
				int newPrecedence = OpGetPrecedence(ctx, next.value);
				// the associativity of the operator can be set here
				if(newPrecedence >= tokenPrecedence){
					rhs = parseExpression_rec(ctx, mem, scope, rhs, tokenPrecedence + (newPrecedence > tokenPrecedence ? 1 : 0));
					next = parsePeek(ctx, 0);
				}else{
                    break;
                }
			}
			ASTNode* node = NodeInit(mem);
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

ASTNode* parseExpression(ParseContext* ctx, Arena* mem, Scope* scope){
    ASTNode* func = parseFunctionCall(ctx, mem, scope);
    if(func){
        return parseExpression_rec(ctx, mem, scope, func, 0);
    }else{
        ASTNode* intLit = parsePrimary(ctx, mem, scope);
        return parseExpression_rec(ctx, mem, scope, intLit, 0);
    }
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

// context should point to the first token of the type
ASTNode* parseType(ParseContext* ctx, Arena* mem){
    Token next = parseConsume(ctx);
    if(next.type != TokenType_TYPE){
        ERROR(next.loc, "Token should be a type");
    }

    ASTNode* node = NodeInit(mem);
    node->type = ASTNodeType_TYPE;
    node->node.TYPE.symbol = next.value;
    node->node.TYPE.array = FALSE;
    node->node.TYPE.arraySize = 0;
    node->node.TYPE.dynamic = FALSE;

    next = parsePeek(ctx, 0);
    if(next.type != TokenType_LBRACKET){
        return node;
    }
    parseConsume(ctx); // '['

    next = parseConsume(ctx);
    if(next.type == TokenType_TRIPLEDOT){
        node->node.TYPE.dynamic = TRUE;
    }else if(next.type == TokenType_INT_LITERAL){
        int num = StringToInt(next.value);
        node->node.TYPE.arraySize = num;
    }else{
        ERROR(next.loc, "Array needs a fixed size or '...' for dynamic size");
    }

    next = parseConsume(ctx);
    if(next.type != TokenType_RBRACKET){
        ERROR(next.loc, "Expected closing pair to square bracket ']'");
    }
    node->node.TYPE.array = TRUE;
    return node;
}

// NOTE: parseFunctionCall and parseExpresion should return a custom error type that stores the error if one happened.
// ctx should point to the function id token
ASTNode* parseFunctionCall(ParseContext* ctx, Arena* mem, Scope* scope){
    Args args = {0};
    ASTNode* node = NodeInit(mem);
    node->type = ASTNodeType_FUNCTION_CALL;

    Token next = parsePeek(ctx, 0);
    if(next.type != TokenType_IDENTIFIER){
        // error
        return NULL;
    }
    node->node.FUNCTION_CALL.identifier = next.value;
    
    next = parsePeek(ctx, 1);
    if(next.type != TokenType_LPAREN){
        // error
        return NULL;
    }
    // only consume after function is confirmed
    parseConsume(ctx);
    parseConsume(ctx);
    for(int i = ctx->index; i < ctx->tokens.size; i++){
        // TODO: maybe do a check for no arguments here
        next = parsePeek(ctx, 0);
        if(next.type == TokenType_RPAREN){
            parseConsume(ctx);
            break;
        }
        
        ASTNode* expr = parseExpression(ctx, mem, scope);
        parseAddArg(&args, expr);
        
        next = parsePeek(ctx, 0);
        if(next.type == TokenType_COMMA){
            parseConsume(ctx);
            continue;
        }else if(next.type == TokenType_RPAREN){
            // break
            parseConsume(ctx);
            break;
        }else{
            // error
            return NULL;
        }
    }
    node->node.FUNCTION_CALL.args = args;
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
    for(int i = ctx->index; i < ctx->tokens.size; i++){
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

Scope* Parse(ParseContext* ctx, Arena* mem){
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
                ASTNode* expr = parseExpression(ctx, mem, currentScope);
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
                Token next = parsePeek(ctx, 0);
                if(next.type == TokenType_INITIALIZER){
                    // init
                    parseConsume(ctx);
                    ASTNode* node = NodeInit(mem);
                    node->type = ASTNodeType_VAR_DECL_ASSIGN;
                    node->node.VAR_DECL_ASSIGN.identifier = t.value;
                    ASTNode* expr = parseExpression(ctx, mem, currentScope);
                    if(!expr){
                        ERROR(next.loc, "Variable needs a valid expresion to initialize");
                        exit(EXIT_FAILURE);
                    }
                    node->node.VAR_DECL_ASSIGN.expresion = expr;
                    // TODO: add types, figure out type here
                    ASTNode* tmp = NodeInit(mem);
                    tmp->type = ASTNodeType_NONE; // TODO: FIX ASAP
                    node->node.VAR_DECL_ASSIGN.type = tmp;
                    
                    // TODO: add the variable to the symbol table
                    parseScopeAddSymbol(currentScope, t.value);

                    // Check for semicolon
                    if(parseCheckSemicolon(ctx)){
                        parseAddStatement(&currentScope->stmts, node);
                    }
                }else if(next.type == TokenType_COLON){
                    // decl
                    parseConsume(ctx);

                    // next = parsePeek(ctx, 0);
                    // if(next.type != TokenType_TYPE){
                    //     ERROR(next.loc, "Variable declaration without initializer needs a type");
                    //     exit(EXIT_FAILURE);
                    // }
                    // parseConsume(ctx);
                    
                    ASTNode* node = NodeInit(mem);
                    node->type = ASTNodeType_VAR_DECL;
                    node->node.VAR_DECL.identifier = t.value;
                    node->node.VAR_DECL.type = parseType(ctx, mem);

                    // TODO: add the variable to the symbol table
                    parseScopeAddSymbol(currentScope, t.value);
                    
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
                    ASTNode* expr = parseExpression(ctx, mem, currentScope);
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
                        if(next.type != TokenType_RARROW){
                            ERROR(next.loc, "Function arguments need to be followed by the return type bikeshedder \"->\"");
                            exit(EXIT_FAILURE);
                        }
                        parseConsume(ctx);

                        // next = parsePeek(ctx, 0);
                        // if(next.type != TokenType_TYPE){
                        //     ERROR(next.loc, "Function needs a return type");
                        //     exit(EXIT_FAILURE);
                        // }
                        // parseConsume(ctx);
                        
                        node->node.FUNCTION_DEF.type = parseType(ctx, mem);
                        
                        next = parsePeek(ctx, 0);
                        if(next.type != TokenType_LSCOPE){
                            ERROR(next.loc, "Function needs to have a scope");
                            exit(EXIT_FAILURE);
                        }
                        parseConsume(ctx);

                        node->node.FUNCTION_DEF.scope = functionScope;
                        parseAddStatement(&scopeBackup->stmts, node);
                    }else{
                        ERROR(next.loc, "Constant needs to be a value known at compile time or a function declaration");
                        exit(EXIT_FAILURE);
                    }
                }else if(next.type == TokenType_LPAREN){
                    // function call
                    ASTNode* func = parseFunctionCall(ctx, mem, currentScope);

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
                ASTNode* expr = parseExpression(ctx, mem, currentScope);
                if(!expr){
                    ERROR(t.loc, "Invalid expresion in if condition");
                    exit(EXIT_FAILURE);
                }
                node->node.IF.expresion = expr;
                
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

                    ASTNode* expr = parseExpression(ctx, mem, currentScope);
                    if(!expr){
                        ERROR(next.loc, "Invalid expresion in if condition");
                    }
                    node->node.ELSE_IF.expresion = expr;
                    
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
                
                ASTNode* expr = parseExpression(ctx, mem, currentScope);
                if(!expr){
                    ERROR(t.loc, "Loop needs an expresion");
                }
                node->node.LOOP.expresion = expr;

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
            case TokenType_OPERATOR:
            case TokenType_ASSIGNMENT:
            case TokenType_COMPARISON:
            case TokenType_INT_LITERAL:
                printf("[ERROR] Unhandled token type: %s at (%.*s:%i:%i)\n", TokenTypeStr[t.type], t.loc.filename.length, t.loc.filename.str, t.loc.line, t.loc.collum);
            break;
        }
    }

    return globalScope;
}

// TODO: check if functions with return type return on all codepaths