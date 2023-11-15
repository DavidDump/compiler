#ifndef COMP_STRUCTS_H
#define COMP_STRUCTS_H

#include "string.h"
#include "arena.h"

// NOTE: this is and idea on how type properties could be solved,
// maybe this is overkill and its actually enough to just hardcode all the built in types
// assert that ((unsigned long long) TypeFlags_COUNT) <= ((unsigned long long) (1 << 64))
typedef enum TypeFlags {
    TypeFlags_NONE = 0,
    
    // meaning the type is an integer, but the byte size may differ
    TypeFlags_INTEGER = (1 << 0),
    // meaning the type can be interpreted as a bool
    TypeFlags_BOOLLIKE = (1 << 1),
    // the type can be defined as and array
    TypeFlags_ARRAYABLE = (1 << 2),
    
    TypeFlags_COUNT,
} TypeFlags;

typedef struct TypeDefinition {
    String symbol;
    int byteSize;
} TypeDefinition;

typedef struct OperatorDefinition {
    String symbol;
    int precedence;
    TypeDefinition retType;
    TypeDefinition rhsType;
    TypeDefinition lhsType;
} OperatorDefinition;

typedef struct TypeInformation {
    TypeDefinition* types;
    int size;
    int capacity;
    Arena mem;
} TypeInformation;

typedef struct OperatorInformation {
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