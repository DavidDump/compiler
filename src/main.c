#include <stdio.h>  // fopen(), fseek(), ftell(), fread(), fclose(), printf(), 
#include <string.h> // strlen(), memcpy()
#include <assert.h> // assert()

#include "structs.h"
#include "parser.h"
#include "lexer.h"
#include "string.h"
#include "common.h"

#define ARENA_IMPLEMENTATION
#include "arena.h"

#define STB_DS_IMPLEMENTATION
#include "stb_ds.h"

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
// Generator
// 

typedef struct GenContext {
    Arena mem;
} GenContext;

StringChain gen_x86_64_nasm_primary(GenContext* ctx, ASTNode* expr){
    StringChain result = {0};
    
    if(expr->type == ASTNodeType_INT_LIT){
        String s1 = StringFromCstr(&ctx->mem, "    mov rax, ");
        String s2 = expr->node.INT_LIT.value;
        String s3 = StringFromCstr(&ctx->mem, "\n");

        StringChainAppend(&result, &ctx->mem, s1);
        StringChainAppend(&result, &ctx->mem, s2);
        StringChainAppend(&result, &ctx->mem, s3);
    }else if(expr->type == ASTNodeType_FUNCTION_CALL){
        // call function and put the ret value in rax, should be defount behaviour
    }else{
        printf("error in primary\n");
        exit(EXIT_FAILURE);
    }

    return result;
}

StringChain gen_x86_64_nasm_expresion(GenContext* ctx, ASTNode* expr){
    StringChain result = {0};
    // TODO: for now hardcode + operator
    if(expr->type == ASTNodeType_INT_LIT){
        String s1 = StringFromCstr(&ctx->mem, "    mov rax, ");
        String s2 = expr->node.INT_LIT.value;
        String s3 = StringFromCstr(&ctx->mem, "\n");

        StringChainAppend(&result, &ctx->mem, s1);
        StringChainAppend(&result, &ctx->mem, s2);
        StringChainAppend(&result, &ctx->mem, s3);
    }else if(expr->type == ASTNodeType_FUNCTION_CALL){
        // TODO: call function and put the ret value in rax, should be defount behaviour
    }else if(StringEqualsCstr(expr->node.EXPRESION.operator, "+")){
        StringChain lhs = gen_x86_64_nasm_expresion(ctx, expr->node.EXPRESION.lhs);
        String s1 = StringFromCstr(&ctx->mem, "    push rax\n");
        StringChain rhs = gen_x86_64_nasm_expresion(ctx, expr->node.EXPRESION.rhs);
        String s2 = StringFromCstrLit("    pop rcx\n");
        String s3 = StringFromCstrLit("    add rax, rcx\n");

        StringChainAppendChain(&result, &ctx->mem, lhs);
        StringChainAppend(&result, &ctx->mem, s1);
        StringChainAppendChain(&result, &ctx->mem, rhs);
        StringChainAppend(&result, &ctx->mem, s2);
        StringChainAppend(&result, &ctx->mem, s3);
    }else if(StringEqualsCstr(expr->node.EXPRESION.operator, "-")){
        StringChain lhs = gen_x86_64_nasm_expresion(ctx, expr->node.EXPRESION.lhs);
        String s1 = StringFromCstr(&ctx->mem, "    push rax\n");
        StringChain rhs = gen_x86_64_nasm_expresion(ctx, expr->node.EXPRESION.rhs);
        String s2 = StringFromCstrLit("    pop rcx\n");
        String s3 = StringFromCstrLit("    sub rax, rcx\n");

        StringChainAppendChain(&result, &ctx->mem, rhs);
        StringChainAppend(&result, &ctx->mem, s1);
        StringChainAppendChain(&result, &ctx->mem, lhs);
        StringChainAppend(&result, &ctx->mem, s2);
        StringChainAppend(&result, &ctx->mem, s3);
    }else if(StringEqualsCstr(expr->node.EXPRESION.operator, "*")){
        // NOTE: mul rcx means rax = rax * rcx
        StringChain lhs = gen_x86_64_nasm_expresion(ctx, expr->node.EXPRESION.lhs);
        String s1 = StringFromCstr(&ctx->mem, "    push rax\n");
        StringChain rhs = gen_x86_64_nasm_expresion(ctx, expr->node.EXPRESION.rhs);
        String s2 = StringFromCstrLit("    pop rcx\n");
        String s3 = StringFromCstrLit("    mul rcx\n");

        StringChainAppendChain(&result, &ctx->mem, lhs);
        StringChainAppend(&result, &ctx->mem, s1);
        StringChainAppendChain(&result, &ctx->mem, rhs);
        StringChainAppend(&result, &ctx->mem, s2);
        StringChainAppend(&result, &ctx->mem, s3);
    }else if(StringEqualsCstr(expr->node.EXPRESION.operator, "/")){
        // NOTE: http://stackoverflow.com/questions/45506439/ddg#45508617
        // div rcx means rax = rax / rcx remainder is rdx
        StringChain lhs = gen_x86_64_nasm_expresion(ctx, expr->node.EXPRESION.lhs);
        String s1 = StringFromCstr(&ctx->mem, "    push rax\n");
        StringChain rhs = gen_x86_64_nasm_expresion(ctx, expr->node.EXPRESION.rhs);
        String s4 = StringFromCstrLit("    mov rdx, 0\n");
        String s2 = StringFromCstrLit("    pop rcx\n");
        String s3 = StringFromCstrLit("    div rcx\n");

        StringChainAppendChain(&result, &ctx->mem, rhs); // 2
        StringChainAppend(&result, &ctx->mem, s1);
        StringChainAppendChain(&result, &ctx->mem, lhs); // 8
        StringChainAppend(&result, &ctx->mem, s4);
        StringChainAppend(&result, &ctx->mem, s2);
        StringChainAppend(&result, &ctx->mem, s3);
    }else{
        printf("[ERROR] Unknown operator: %.*s\n", expr->node.EXPRESION.operator.length, expr->node.EXPRESION.operator.str);
        exit(EXIT_FAILURE);
    }

    return result;
}

StringChain generate_x86_64_nasm(GenContext* ctx, Scope* globalScope){
    StringChain result = {0};
    
    // header
    StringChainAppend(&result, &ctx->mem, StringFromCstrLit("global _main\n"));
    StringChainAppend(&result, &ctx->mem, StringFromCstrLit("segment .text\n"));
    StringChainAppend(&result, &ctx->mem, StringFromCstrLit("_main:\n"));

    for(int i = 0; i < globalScope->stmts.size; i++){
        ASTNode* node = globalScope->stmts.statements[i];

        switch(node->type){
            case ASTNodeType_NONE:
            case ASTNodeType_COUNT: {
                printf("[ERROR] ast node none and count are errors\n");
                exit(EXIT_FAILURE);
            } break;

            case ASTNodeType_VAR_DECL_ASSIGN: {
                StringChain chain1 = gen_x86_64_nasm_expresion(ctx, node->node.VAR_DECL_ASSIGN.expresion);
                StringChainAppendChain(&result, &ctx->mem, chain1);

                // get stack location
                // store location in a hashmap with the symbol identifier as a key
                // push variable to stack
            } break;

            case ASTNodeType_FUNCTION_DEF:
            case ASTNodeType_FUNCTION_CALL:
            case ASTNodeType_VAR_DECL:
            case ASTNodeType_VAR_REASSIGN:
            case ASTNodeType_VAR_CONST:
            case ASTNodeType_RET:
            case ASTNodeType_IF:
            case ASTNodeType_ELSE:
            case ASTNodeType_EXPRESION:
            case ASTNodeType_INT_LIT:
            case ASTNodeType_SYMBOL_RVALUE:
            case ASTNodeType_TYPE:
                printf("[ERROR] Unhandled AST Node type: %s\n", ASTNodeTypeStr[node->type]);
                break;
        }
    }

    // TODO: this is temporary footer
    StringChainAppend(&result, &ctx->mem, StringFromCstrLit("    ret\n"));


    return result;
}

StringChain Generate(GenContext* ctx, Scope* globalScope){
    return generate_x86_64_nasm(ctx, globalScope);
}

// 
// Main
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
    
    ParseContext parseContext = ParseContextInit(tokens, &typeInfo, &opInfo);

    Scope* globalScope = Parse(&parseContext, &readFileMem);
    if(printAST) ASTPrint(globalScope);

    GenContext genContext = {0};
    StringChain outRaw = Generate(&genContext, globalScope);

    char* outFilePath = "output.asm";
    bool success = EntireFileWrite(outFilePath, outRaw);
    if(!success){
        printf("[ERROR] Failed to write output asm file\n");
        exit(EXIT_FAILURE);
    }

    arena_free(&readFileMem);
    arena_free(&genContext.mem);
    exit(EXIT_SUCCESS);
}

// TODO: remove arenas from scope struct, all ast nodes and scope data should be allocated in one arena
// TODO: better error messeges when failing to parse an expresion
// TODO: instead of specifying operator left hand type and right hand type,
//       use type properties so we dont have to add operators multiple times for different types