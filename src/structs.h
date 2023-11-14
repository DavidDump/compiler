#ifndef COMP_STRUCTS_H
#define COMP_STRUCTS_H

#include "string.h"
#include "arena.h"

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

typedef struct TypeInformation{
    TypeDefinition* types;
    int size;
    int capacity;
    Arena mem;
} TypeInformation;

typedef struct OperatorInformation{
    OperatorDefinition* ops;
    int size;
    int capacity;
    Arena mem;
} OperatorInformation;

void addType(TypeInformation* info, TypeDefinition def);
void addOperator(OperatorInformation* info, OperatorDefinition def);
TypeDefinition TypeDefinitionInit(String symbol, int byteSize);
OperatorDefinition OperatorDefinitionInit(String symbol, int precedence, TypeDefinition ret, TypeDefinition lhs, TypeDefinition rhs);

#endif // COMP_STRUCTS_H

// TODO: will probably want to split this file into two later when there is enough differece between types and operators