#if 0
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
typedef struct AsmGenContext {
    int stack;
    int savedStack[SAVED_STACK_SIZE];
    int savedStackPointer;
    int labelCounter;
    int intSize; // 8 for 64-bit, 4 for 32-bit
    IdentifierLocations idLoc;
    
    Arena mem;
} AsmGenContext;

StringChain Generate(AsmGenContext* ctx, Scope* globalScope);

#endif // COMP_CODEGEN_H
#endif
