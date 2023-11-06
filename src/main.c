#include <stdio.h>  // fopen(), fseek(), ftell(), fread(), fclose(), printf(), 
#include <string.h> // strlen(), memcpy()
#include <assert.h> // assert()

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
    ASTNodeType_VAR_DECL,
    ASTNodeType_VAR_DECL_ASSIGN,
    ASTNodeType_VAR_REASSIGN,
    ASTNodeType_VAR_CONST,
    ASTNodeType_RET,
    ASTNodeType_EXPRESION,
    ASTNodeType_INT_LIT,
    
    ASTNodeType_COUNT,
} ASTNodeType;

typedef struct ASTNode{
    ASTNodeType type;
    union Node{
        struct FUNCTION_DEF {
            String identifier;
            String type;
            struct ASTNode* statements;
        } FUNCTION_DEF;
        struct VAR_DECL {
            String identifier;
            String type;
        } VAR_DECL;
        struct VAR_DECL_ASSIGN {
            String identifier;
            String type;
            struct ASTNode* expresion;
        } VAR_DECL_ASSIGN;
        struct VAR_REASSIGN {
            String identifier;
            struct ASTNode* expresion;
        } VAR_REASSIGN;
        struct VAR_CONST {
            String identifier;
            String value;
        } VAR_CONST;
        struct RET {
            struct ASTNode* expresion;
        } RET;
        struct EXPRESION{
            String operator;
            struct ASTNode* rhs;
            struct ASTNode* lhs;
        } EXPRESION;
        struct INT_LIT{
            String value;
        } INT_LIT;
    } node;
} ASTNode;

typedef struct ASTRoot{
    ASTNode* statements;
    int size;
    int capacity;
    
    ASTNode* children; // ???
} ASTRoot;

typedef struct TypeDefinition{
    String symbol;
    int byteSize;
} TypeDefinition;

typedef struct OperatorDefinition{
    String symbol;
    int precedence;
    TypeDefinition retType;
    TypeDefinition rhsType;
    TypeDefinition lhsType;
} OperatorDefinition;

typedef struct ParseContext{
    TokenArray tokens;
	int index;
    
    OperatorDefinition ops[4]; // TODO: later make dynamic
    int opsCount;
} ParseContext;

ParseContext ParseContextInit(TokenArray tokens){
    ParseContext ctx = {
        .tokens = tokens,
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
    for(int i = 0; i < indent; i++){
        printf("  ");
    }
    switch(node->type){
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
    }
}

int OpGetPrecedence2(ParseContext* ctx, String op){
    for(int i = 0; i < ctx->opsCount; i++){
        if(StringEquals(ctx->ops[i].symbol, op)){
            return ctx->ops[i].precedence;
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

ASTNode* parsePrimary2(ParseContext* ctx, Arena* mem){
	Token t = parseConsume2(ctx);
	if(t.type == TokenType_INT_LITERAL){
		ASTNode* node = NodeInit2(mem);
		node->type = ASTNodeType_INT_LIT;
        node->node.INT_LIT.value = t.value;
		return node;
	}else{
		printf("[ERROR] malformed expresion\n");
		exit(EXIT_FAILURE);
	}
}

// TODO: make expresion parsing not recursive
// TODO: make parenthesis reset precedece so correct AST gets generated
// NOTE: Implementation from: https://en.wikipedia.org/wiki/Operator-precedence_parser
ASTNode* parseExpression2_rec(ParseContext* ctx, Arena* mem, ASTNode* lhs, int precedence){
	Token next = parsePeek2(ctx, 0);
	while(next.type == TokenType_OPERATOR){
		int tokenPrecedence = OpGetPrecedence2(ctx, next.value);
		if(tokenPrecedence >= precedence){
			Token op = next;
			parseConsume2(ctx);
			ASTNode* rhs = parsePrimary2(ctx, mem);
			next = parsePeek2(ctx, 0);
			while(next.type == TokenType_OPERATOR){
				int newPrecedence = OpGetPrecedence2(ctx, next.value);
				// the associativity of the operator can be set here
				if(newPrecedence >= tokenPrecedence){
					rhs = parseExpression2_rec(ctx, mem, rhs, tokenPrecedence + (newPrecedence > tokenPrecedence ? 1 : 0));
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
			// return node;
		}else{
            break;
        }
	}
	return lhs;
}

ASTNode* parseExpression2(ParseContext* ctx, Arena* mem){
    ASTNode* intLit = parsePrimary2(ctx, mem);
	return parseExpression2_rec(ctx, mem, intLit, 5);
}

// ASTRoot* Parse2(){

// }

// 
// Main entry point
// 

int main(int argc, char** argv){
    if(argc < 2){
        printf("[ERROR] File not found: %s\n", argv[1]);
        exit(EXIT_FAILURE);
    }
    Arena readFileMem = {0}; // source file is stored in here
    String sourceRaw = EntireFileRead(&readFileMem, argv[1]);

    int filenameLen = strlen(argv[1]);
    String filename = {.str = argv[1], .length = filenameLen};
    Tokenizer tokenizer = TokenizerInit(sourceRaw, filename);
    TokenArray tokens = Tokenize(&tokenizer);
    TokensPrint(&tokens);

    char* intType = "int";
    String intSymbol = {.str = intType, .length = 4};
    TypeDefinition type = TypeDefinitionInit(intSymbol, 4);

    char* opsSymbol = "+-*/";
    String op1Symbol = {.str = opsSymbol + 0, .length = 1};
    String op2Symbol = {.str = opsSymbol + 1, .length = 1};
    String op3Symbol = {.str = opsSymbol + 2, .length = 1};
    String op4Symbol = {.str = opsSymbol + 3, .length = 1};
    
    ParseContext ctx = ParseContextInit(tokens);
    ctx.ops[0] = OperatorDefinitionInit(op1Symbol, 5, type, type, type);
    ctx.ops[1] = OperatorDefinitionInit(op2Symbol, 5, type, type, type);
    ctx.ops[2] = OperatorDefinitionInit(op3Symbol, 10, type, type, type);
    ctx.ops[3] = OperatorDefinitionInit(op4Symbol, 10, type, type, type);
    ctx.opsCount = 4;

    ASTNode* node = parseExpression2(&ctx, &readFileMem);
    ASTNodePrint2(node, 0);

    #if 0
    Parser parser = {.tokens = tokens}; // uses memory arena
    NodeRoot ast = Parse(&parser);

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