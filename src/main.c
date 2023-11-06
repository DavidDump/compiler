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
    
    ASTNodeType_COUNT,
} ASTNodeType;


typedef struct ASTNode{
    ASTNodeType type;
} ASTNode;

// ASTNode* Parse2(){

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