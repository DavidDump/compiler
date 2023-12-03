#include <stdio.h>  // fopen(), fseek(), ftell(), fread(), fclose(), printf(), 
#include <string.h> // strlen(), memcpy()
#include <assert.h> // assert()
#include <stdarg.h> // va_list, va_start, va_end, va_arg

#include "structs.h"
#include "parser.h"
#include "lexer.h"
#include "string.h"
#include "common.h"

#define ARENA_IMPLEMENTATION
#include "arena.h"

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
// NOTE: using intel asm syntax
// 

typedef struct IdentifierLocations{
    struct Item {
        String key;
        int value;
    }* items;
    int size;
    int capacity;
    Arena mem;
} IdentifierLocations;

// NOTE: i dont know if the stack pointer get saved any other time other than function calls,
// but SAVED_STACK_SIZE is basically the callstack depth
#define SAVED_STACK_SIZE 255
typedef struct GenContext {
    int stack;
    int savedStack[SAVED_STACK_SIZE];
    int savedStackPointer;
    int intSize; // 8 for 64-bit, 4 for 32-bit
    IdentifierLocations idLoc;
    Arena mem;
} GenContext;

void appendIdLoc(IdentifierLocations* idLoc, String key, int value){
    if(idLoc->size >= idLoc->capacity){
        size_t newCap = idLoc->capacity * 2;
        if(newCap == 0) newCap = 1;
        idLoc->items = arena_realloc(&idLoc->mem, idLoc->items, idLoc->capacity * sizeof(idLoc->items[0]), newCap * sizeof(idLoc->items[0]));
        idLoc->capacity = newCap;
    }

    idLoc->items[idLoc->size].key = key;
    idLoc->items[idLoc->size].value = value;
    idLoc->size++;
}

int findIdLoc(IdentifierLocations* idLoc, String key){
    for(int i = 0; i < idLoc->size; i++){
        if(StringEquals(idLoc->items[i].key, key)){
            return idLoc->items[i].value;
        }
    }

    return -1;
}

// helper for counting the digits of and int
int digitsCount(int value){
    int l = !value;
    while(value){
        l++;
        value/=10;
    }
    return l;
}

// %s in the format string means String type instead of regular cstring
void genChainPrintf(StringChain* result, Arena* mem, const char* format, ...){
    va_list args;
    va_start(args, format);

    String workingStr = {.str = format, .length = 0};
    bool wasPercent = FALSE;
    while(*format != '\0'){
        if(wasPercent == FALSE && *format == '%'){
            wasPercent = TRUE;
        }else if(wasPercent == TRUE && *format == 's'){
            wasPercent = FALSE;
            String arg = va_arg(args, String);
            
            StringChainAppend(result, mem, workingStr);
            StringChainAppend(result, mem, arg);
            
            workingStr.str = format + 1;
            workingStr.length = 0;
        }else if(wasPercent == TRUE && *format == 'i'){
            wasPercent = FALSE;
            int arg = va_arg(args, int);

            StringChainAppend(result, mem, workingStr);

            int intLen = digitsCount(arg) + 1; // NOTE: snprintf only works if the buffer has enough space for a \0 terminator
            char* buffer = arena_alloc(mem, intLen * sizeof(char));
            snprintf(buffer, intLen, "%i", arg);
            StringChainAppend(result, mem, (String){.str = buffer, .length = intLen});

            workingStr.str = format + 1;
            workingStr.length = 0;
        }else{
            wasPercent = FALSE;
            workingStr.length++;
        }
        format++;
    }
    // add the string between the last `%s` and `\0`
    if(workingStr.length > 0) StringChainAppend(result, mem, workingStr);

    va_end(args);
}

void gen_win_x86_64_nasm_push(GenContext* ctx, StringChain* result, const char* reg){
    ctx->stack++;
    genChainPrintf(result, &ctx->mem, "    push %s\n", (String){.str = reg, .length = strlen(reg)});
}

void gen_win_x86_64_nasm_pop(GenContext* ctx, StringChain* result, const char* reg){
    ctx->stack--;
    genChainPrintf(result, &ctx->mem, "    pop %s\n", (String){.str = reg, .length = strlen(reg)});
}

void genSaveStack(GenContext* ctx){
    if(ctx->savedStackPointer + 1 >= SAVED_STACK_SIZE){
        printf("[ERROR] Call stack overflow\n");
        exit(EXIT_FAILURE);
    }
    ctx->savedStack[ctx->savedStackPointer++] = ctx->stack;
}

void genRestoreStack(GenContext* ctx){
    if(ctx->savedStackPointer - 1 < 0){
        printf("[ERROR] Call stack underflow\n");
        exit(EXIT_FAILURE);
    }
    ctx->stack = ctx->savedStack[--ctx->savedStackPointer];
}

#if 0
StringChain gen_win_x86_64_nasm_primary(GenContext* ctx, ASTNode* expr){
    StringChain result = {0};
    
    if(expr->type == ASTNodeType_INT_LIT){
        genChainPrintf(&result, &ctx->mem, "    mov rax, %s\n", expr->node.INT_LIT.value);
    }else if(expr->type == ASTNodeType_FUNCTION_CALL){
        // call function and put the ret value in rax, should be defount behaviour
    }else{
        printf("error in primary\n");
        exit(EXIT_FAILURE);
    }

    return result;
}
#endif

StringChain gen_win_x86_64_nasm_expresion(GenContext* ctx, ASTNode* expr){
    StringChain result = {0};
    // TODO: for now hardcode + operator
    if(expr->type == ASTNodeType_INT_LIT){
        genChainPrintf(&result, &ctx->mem, "    mov rax, %s\n", expr->node.INT_LIT.value);
    }else if(expr->type == ASTNodeType_FUNCTION_CALL){
        // TODO: call function and put the ret value in rax, should be default behaviour
        UNIMPLEMENTED("gen_win_x86_64_nasm_expresion: ASTNodeType_FUNCTION_CALL");
    }else if(expr->type == ASTNodeType_SYMBOL_RVALUE){
        String id = expr->node.SYMBOL_RVALUE.identifier;
        int loc = findIdLoc(&ctx->idLoc, id);
        if(loc == -1){
            genChainPrintf(&result, &ctx->mem, "    mov rax, [%s]\n", id);
        }else{
            genChainPrintf(&result, &ctx->mem, "    mov rax, [rbp - %i * %i]\n", loc, ctx->intSize);
        }
    }else if(StringEqualsCstr(expr->node.EXPRESION.operator, "+")){
        StringChain lhs = gen_win_x86_64_nasm_expresion(ctx, expr->node.EXPRESION.lhs);
        StringChain rhs = gen_win_x86_64_nasm_expresion(ctx, expr->node.EXPRESION.rhs);

        StringChainAppendChain(&result, &ctx->mem, lhs);       // expresion ends up in rax
        gen_win_x86_64_nasm_push(ctx, &result, "rax");
        StringChainAppendChain(&result, &ctx->mem, rhs);       // expresion ends up in rax
        gen_win_x86_64_nasm_pop(ctx, &result, "rcx");
        genChainPrintf(&result, &ctx->mem, "    add rax, rcx\n");
    }else if(StringEqualsCstr(expr->node.EXPRESION.operator, "-")){
        StringChain lhs = gen_win_x86_64_nasm_expresion(ctx, expr->node.EXPRESION.lhs);
        StringChain rhs = gen_win_x86_64_nasm_expresion(ctx, expr->node.EXPRESION.rhs);

        StringChainAppendChain(&result, &ctx->mem, rhs);
        gen_win_x86_64_nasm_push(ctx, &result, "rax");
        StringChainAppendChain(&result, &ctx->mem, lhs);
        gen_win_x86_64_nasm_pop(ctx, &result, "rcx");
        genChainPrintf(&result, &ctx->mem, "    sub rax, rcx\n");
    }else if(StringEqualsCstr(expr->node.EXPRESION.operator, "*")){
        // NOTE: mul rcx means rax = rax * rcx
        StringChain lhs = gen_win_x86_64_nasm_expresion(ctx, expr->node.EXPRESION.lhs);
        StringChain rhs = gen_win_x86_64_nasm_expresion(ctx, expr->node.EXPRESION.rhs);

        StringChainAppendChain(&result, &ctx->mem, lhs);
        gen_win_x86_64_nasm_push(ctx, &result, "rax");
        StringChainAppendChain(&result, &ctx->mem, rhs);
        gen_win_x86_64_nasm_pop(ctx, &result, "rcx");
        genChainPrintf(&result, &ctx->mem, "    mul rcx\n");
    }else if(StringEqualsCstr(expr->node.EXPRESION.operator, "/")){
        // NOTE: http://stackoverflow.com/questions/45506439/ddg#45508617
        // div rcx means rax = rax / rcx remainder is rdx
        StringChain lhs = gen_win_x86_64_nasm_expresion(ctx, expr->node.EXPRESION.lhs);
        StringChain rhs = gen_win_x86_64_nasm_expresion(ctx, expr->node.EXPRESION.rhs);

        StringChainAppendChain(&result, &ctx->mem, rhs);
        gen_win_x86_64_nasm_push(ctx, &result, "rax");
        StringChainAppendChain(&result, &ctx->mem, lhs);
        genChainPrintf(&result, &ctx->mem, "    mov rdx, 0\n");
        gen_win_x86_64_nasm_pop(ctx, &result, "rcx");
        genChainPrintf(&result, &ctx->mem, "    div rcx\n");
    }else{
        printf("[ERROR] Unknown operator: %.*s\n", expr->node.EXPRESION.operator.length, expr->node.EXPRESION.operator.str);
        exit(EXIT_FAILURE);
    }

    return result;
}

StringChain generate_win_x86_64_nasm_scope(GenContext* ctx, Scope* globalScope, StringChain* dataSection){
    StringChain result = {0};
    for(int i = 0; i < globalScope->stmts.size; i++){
        ASTNode* node = globalScope->stmts.statements[i];

        switch(node->type){
            case ASTNodeType_NONE:
            case ASTNodeType_COUNT: {
                printf("[ERROR] ast node none and count are errors\n");
                exit(EXIT_FAILURE);
            } break;

            case ASTNodeType_VAR_DECL_ASSIGN: {
                ASTNode* exprNode = node->node.VAR_DECL_ASSIGN.expresion;
                String id = node->node.VAR_DECL_ASSIGN.identifier;
                ASTNode* type = node->node.VAR_DECL_ASSIGN.type;
                
                StringChain expr = gen_win_x86_64_nasm_expresion(ctx, exprNode);
                StringChainAppendChain(&result, &ctx->mem, expr);

                // store location in a hashmap with the symbol identifier as a key
                appendIdLoc(&ctx->idLoc, id, ctx->stack);
                // push variable to stack
                gen_win_x86_64_nasm_push(ctx, &result, "rax");
            } break;
            case ASTNodeType_VAR_CONST: {
                String id = node->node.VAR_CONST.identifier;
                String value = node->node.VAR_CONST.value;

                // TODO: dont hardcode dq size but somehow use space more efficiently
                genChainPrintf(dataSection, &ctx->mem, "    %s dq %s\n", id, value);
            } break;
            case ASTNodeType_VAR_DECL: {
                String id = node->node.VAR_DECL.identifier;
                ASTNode* type = node->node.VAR_DECL.type;

                // store location in a hashmap with the symbol identifier as a key
                appendIdLoc(&ctx->idLoc, id, ctx->stack);
                // increase the stack
                genChainPrintf(&result, &ctx->mem, "    sub rsp, %i\n", ctx->intSize);
                ctx->stack++;
            } break;
            case ASTNodeType_VAR_REASSIGN: {
                String id = node->node.VAR_REASSIGN.identifier;
                ASTNode* exprNode = node->node.VAR_REASSIGN.expresion;

                StringChain expr = gen_win_x86_64_nasm_expresion(ctx, exprNode);
                StringChainAppendChain(&result, &ctx->mem, expr);

                int loc = findIdLoc(&ctx->idLoc, id);
                if(loc == -1){
                    printf("[ERROR] Symbol access not found\n");
                    exit(EXIT_FAILURE);
                }
                int savedStack = ctx->stack;
                genChainPrintf(&result, &ctx->mem, "    lea rsp, [rbp - %i * %i]\n", loc - 1, ctx->intSize);
                genChainPrintf(&result, &ctx->mem, "    push rax\n");
                genChainPrintf(&result, &ctx->mem, "    lea rsp, [rbp - %i * %i]\n", savedStack - 1, ctx->intSize);
            } break;
            case ASTNodeType_RET: {
                ASTNode* exprNode = node->node.RET.expresion;
                
                StringChain expr = gen_win_x86_64_nasm_expresion(ctx, exprNode);
                StringChainAppendChain(&result, &ctx->mem, expr);

                genChainPrintf(&result, &ctx->mem, "    mov rsp, rbp\n");
                genRestoreStack(ctx);
                gen_win_x86_64_nasm_pop(ctx, &result, "rbp");
                genChainPrintf(&result, &ctx->mem, "    ret\n");
            } break;
            case ASTNodeType_FUNCTION_DEF: {
                String id = node->node.FUNCTION_DEF.identifier;
                ASTNode* type = node->node.FUNCTION_DEF.type;
                Scope* scope = node->node.FUNCTION_DEF.scope;
                Args args = node->node.FUNCTION_DEF.args;

                // function header
                genChainPrintf(&result, &ctx->mem, "jmp after_%s\n", id);
                genChainPrintf(&result, &ctx->mem, "%s:\n", id);
                genChainPrintf(&result, &ctx->mem, "    push rbp\n");
                genChainPrintf(&result, &ctx->mem, "    mov rbp, rsp\n");
                genSaveStack(ctx);
                
                // get args
                for(int i = 0; i < args.size; i++){
                    String argId = args.args[i]->node.VAR_DECL.identifier;
                    ASTNode* argType = args.args[i]->node.VAR_DECL.type;

                    if(i == 0){
                        // first arg
                        appendIdLoc(&ctx->idLoc, argId, ctx->stack);
                        gen_win_x86_64_nasm_push(ctx, &result, "rcx");
                    }else if(i == 1){
                        // second arg
                        appendIdLoc(&ctx->idLoc, argId, ctx->stack);
                        gen_win_x86_64_nasm_push(ctx, &result, "rdx");
                    }else if(i == 2){
                        // third arg
                        appendIdLoc(&ctx->idLoc, argId, ctx->stack);
                        gen_win_x86_64_nasm_push(ctx, &result, "r8");
                    }else if(i == 3){
                        // fourth arg
                        appendIdLoc(&ctx->idLoc, argId, ctx->stack);
                        gen_win_x86_64_nasm_push(ctx, &result, "r9");
                    }else{
                        // fifth+ arg
                        appendIdLoc(&ctx->idLoc, argId, ctx->stack);
                        genChainPrintf(&result, &ctx->mem, "    mov rax, [rbp + %i * %i]\n", i - 3, ctx->intSize);
                        gen_win_x86_64_nasm_push(ctx, &result, "rax");
                        // NOTE: maybe this isnt nessecary and we can just save a negative stack value and store half the args above rbp and the other half below
                    }
                }

                // generate body
                StringChain body = generate_win_x86_64_nasm_scope(ctx, scope, dataSection);
                StringChainAppendChain(&result, &ctx->mem, body);

                // function footer
                // NOTE: return is handled by the return keyword,
                // dont know if there is a situation it need to be generated here
                genChainPrintf(&result, &ctx->mem, "after_%s:\n", id);
            } break;

            case ASTNodeType_FUNCTION_CALL:
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

    return result;
}

StringChain Generate(GenContext* ctx, Scope* globalScope){
    ctx->intSize = 8;

    // header
    StringChain result = {0};
    genChainPrintf(&result, &ctx->mem, "bits 64\n");
    genChainPrintf(&result, &ctx->mem, "default rel\n");
    genChainPrintf(&result, &ctx->mem, "section .text\n");

    // data
    StringChain dataSection = {0};
    genChainPrintf(&dataSection, &ctx->mem, "section .data\n");

    StringChain scope = generate_win_x86_64_nasm_scope(ctx, globalScope, &dataSection);
    StringChainAppendChain(&result, &ctx->mem, scope);

    // data section at the end
    StringChainAppendChain(&result, &ctx->mem, dataSection);

    return result;
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
                printf("          --tokens\n");
                printf("          --ast\n");
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