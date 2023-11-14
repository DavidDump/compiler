#include <stdlib.h>
#include <stdio.h>

#include "codegen.h"

void GeneratorPushStack(Generator* gen, const char* target){
    StringChainAppend(&gen->outputAsm, &gen->mem, StringFromCstr(&gen->mem, "    push "));
    StringChainAppend(&gen->outputAsm, &gen->mem, StringFromCstr(&gen->mem, target));
    StringChainAppend(&gen->outputAsm, &gen->mem, StringFromCstr(&gen->mem, "\n"));
    // TODO: figure out what the stack size is to check for stack overflows
    // if(gen->stackPointer - 1 < 0){
    //     printf("[ERROR] Stack underflow\n");
    //     exit(EXIT_FAILURE);
    // }
    gen->stackPointer++;
}

void GeneratorPopStack(Generator* gen, const char* target){
    StringChainAppend(&gen->outputAsm, &gen->mem, StringFromCstr(&gen->mem, "    pop "));
    StringChainAppend(&gen->outputAsm, &gen->mem, StringFromCstr(&gen->mem, target));
    StringChainAppend(&gen->outputAsm, &gen->mem, StringFromCstr(&gen->mem, "\n"));
    if(gen->stackPointer - 1 < 0){
        printf("[ERROR] Stack underflow\n");
        exit(EXIT_FAILURE);
    }
    gen->stackPointer--;
}

// pushes int literal to rax
void GenerateExpresion(Generator* gen, NodeExpresion* node){
    switch(node->type){
        case NodeExpresionType_INT_LIT: {
            StringChainAppend(&gen->outputAsm, &gen->mem, StringFromCstr(&gen->mem, "    mov rax, "));
            StringChainAppend(&gen->outputAsm, &gen->mem, node->intLit->value);
            StringChainAppend(&gen->outputAsm, &gen->mem, StringFromCstr(&gen->mem, "\n"));
            GeneratorPushStack(gen, "rax"); // TODO: do you need to push here?
        } break;
        case NodeExpresionType_BIN_EXP: {
            // generate subexpresion
            // if literal will end up in rax
            // if binary expresion result will end up in rax
            GenerateExpresion(gen, node->binExp->right);

            StringChainAppend(&gen->outputAsm, &gen->mem, StringFromCstr(&gen->mem, "    mov rbx, "));
            StringChainAppend(&gen->outputAsm, &gen->mem, node->binExp->left->value);
            StringChainAppend(&gen->outputAsm, &gen->mem, StringFromCstr(&gen->mem, "\n"));
            // TODO: add other operators
            if(StringEqualsCstr(node->binExp->operator->value, "+")){
                StringChainAppend(&gen->outputAsm, &gen->mem, StringFromCstr(&gen->mem, "    add rax, rbx\n"));
            }else{
                printf("[ERROR] Operator \'%.*s\' not implemented\n", node->binExp->operator->value.length, node->binExp->operator->value.str);
                exit(EXIT_FAILURE);
            }

            GeneratorPushStack(gen, "rax");
        } break;
        default: {
            printf("[ERROR] Unhandled expresion case: %s\n", NodeExpresionTypeStr[node->type]);
        } break;
    }
}

void GenerateReturn(Generator* gen, NodeKeywordRet* node){
    // NOTE: the comment only handles int literals for now
    StringChainAppend(&gen->outputAsm, &gen->mem, StringFromCstr(&gen->mem, "; return "));
    StringChainAppend(&gen->outputAsm, &gen->mem, node->exp->intLit->value);
    StringChainAppend(&gen->outputAsm, &gen->mem, StringFromCstr(&gen->mem, "\n"));
    
    GenerateExpresion(gen, node->exp);
    GeneratorPopStack(gen, "rax");
    StringChainAppend(&gen->outputAsm, &gen->mem, StringFromCstr(&gen->mem, "    ret\n"));
}

void GenerateVar(Generator* gen, NodeKeywordVar* node){
    UNUSED(gen);
    switch(node->type){
        case NodeKeywordVarType_DEF_ONLY: {
            // node->defIdentifier;
            
            // increase stack pointer
            // register identifier in symbol table
            assert(FALSE && "Unimplemented");
        } break;
        case NodeKeywordVarType_DEF_ASSIGNMENT: {
            assert(FALSE && "Unimplemented");
        } break;
        default: {
            printf("[ERROR] Unhandled var case: %s\n", NodeKeywordVarTypeStr[node->type]);
        } break;
    }
}

void GenerateKeyword(Generator* gen, NodeKeyword* node){
    switch(node->type){
        case NodeKeywordType_RET: {
            GenerateReturn(gen, node->ret);
        } break;
        case NodeKeywordType_VAR: {
            GenerateVar(gen, node->var);
        } break;
        default: {
            printf("[ERROR] Unhandled keyword case: %s\n", NodeKeywordTypeStr[node->type]);
        } break;
    }
}

StringChain Generate(Generator* gen, NodeRoot* root){
    // preamble
    StringChainAppend(&gen->outputAsm, &gen->mem, StringFromCstr(&gen->mem, "global _start\n"));
    StringChainAppend(&gen->outputAsm, &gen->mem, StringFromCstr(&gen->mem, "_start:\n"));

    for(int i = 0; i < root->count; i++){
        switch(root->stmts[i].type){
            case NodeStatementType_KEYWORD: {
                GenerateKeyword(gen, root->stmts[i].keyword);
            } break;
            case NodeStatementType_EXPRESION: {
                GenerateExpresion(gen, root->stmts[i].exp);
            } break;
            default: {
                printf("[ERROR] Unhandled statement case: %s\n", NodeStatementTypeStr[root->stmts[i].type]);
            } break;
        }
    }

    return gen->outputAsm;
}
