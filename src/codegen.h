#ifndef COMP_CODEGEN_H
#define COMP_CODEGEN_H

#include "arena.h"
#include "string.h"
#include "parser.h"

typedef struct Generator{
    Arena mem;
    StringChain outputAsm;
    int stackPointer;
    // NodeRoot root; // NOTE: maybe add ast here for consistency
} Generator;

void GeneratorPushStack(Generator* gen, const char* target);
void GeneratorPopStack(Generator* gen, const char* target);
void GenerateExpresion(Generator* gen, NodeExpresion* node);
void GenerateReturn(Generator* gen, NodeKeywordRet* node);
void GenerateVar(Generator* gen, NodeKeywordVar* node);
void GenerateKeyword(Generator* gen, NodeKeyword* node);
StringChain Generate(Generator* gen, NodeRoot* root);

#endif // COMP_CODEGEN_H