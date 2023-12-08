#include "structs.h"

bool containsOp(OperatorInformation opInfo, String op, OperatorDefinition* result){
    for(int i = 0; i < opInfo.size; i++){
        if(StringEquals(opInfo.ops[i].symbol, op)){
            *result = opInfo.ops[i];
            return TRUE;
        }
    }
    result = NULL;
    return FALSE;
}

void addType(TypeInformation* info, TypeDefinition def){
    if(info->size >= info->capacity){
        size_t newCap = info->capacity * 2;
        if(newCap == 0) newCap = 1;
        info->types = arena_realloc(&info->mem, info->types, info->capacity * sizeof(info->types[0]), newCap * sizeof(info->types[0]));
        info->capacity = newCap;
    }

    info->types[info->size++] = def;
}

void addOperator(OperatorInformation* info, OperatorDefinition def){
    if(info->size >= info->capacity){
        size_t newCap = info->capacity * 2;
        if(newCap == 0) newCap = 1;
        info->ops = arena_realloc(&info->mem, info->ops, info->capacity * sizeof(info->ops[0]), newCap * sizeof(info->ops[0]));
        info->capacity = newCap;
    }

    info->ops[info->size++] = def;
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
