#ifndef COMP_CODEGEN_H
#define COMP_CODEGEN_H

#include <stdarg.h> // va_list(), va_start(), va_end(), va_arg()
#include <stdlib.h> // exit(), EXIT_FAILURE, EXIT_SUCCESS
#include <stdio.h> // snprintf()
#include <string.h> // strlen()

#include "string.h"
#include "types.h"
#include "parser.h"

typedef struct IdentifierLocations {
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
    int labelCounter;
    int intSize; // 8 for 64-bit, 4 for 32-bit
    IdentifierLocations idLoc;
    
    TypeMapping* typeMappings;
    int typeMappingsSize;
    OperatorInfo* opInfo;
    int opInfoSize;
    
    Arena mem;
} GenContext;

GenContext GenContextInit(TypeMapping* typeMappings, int typeMappingsSize, OperatorInfo* opInfo, int opInfoSize);
void appendIdLoc(IdentifierLocations* idLoc, String key, int value);
int findIdLoc(IdentifierLocations* idLoc, String key);
int digitsCount(int value);
void genChainPrintf(StringChain* result, Arena* mem, char* format, ...);
void gen_win_x86_64_nasm_push(GenContext* ctx, StringChain* result, char* reg);
void gen_win_x86_64_nasm_pop(GenContext* ctx, StringChain* result, char* reg);
void genSaveStack(GenContext* ctx);
void genRestoreStack(GenContext* ctx);
StringChain gen_win_x86_64_nasm_expresion(GenContext* ctx, ASTNode* expr);
StringChain gen_win_x86_64_nasm_func_call(GenContext* ctx, String id, Args args);
StringChain generate_win_x86_64_nasm_condition(GenContext* ctx, ASTNode* expr, int label);
StringChain generate_win_x86_64_nasm_scope(GenContext* ctx, Scope* globalScope, StringChain* dataSection);
StringChain Generate(GenContext* ctx, Scope* globalScope);

#endif // COMP_CODEGEN_H