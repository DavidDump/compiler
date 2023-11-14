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

    Scope* globalScope = Parse(&ctx, &readFileMem);
    if(printAST) ASTPrint(globalScope);

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