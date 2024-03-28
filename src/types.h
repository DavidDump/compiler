#ifndef COMP_TYPES_H
#define COMP_TYPES_H

#include "string.h"
#include "arena.h"

typedef enum Type {
    TYPE_NONE,
    
    TYPE_U8,
    TYPE_U16,
    TYPE_U32,
    TYPE_U64,
    TYPE_S8,
    TYPE_S16,
    TYPE_S32,
    TYPE_S64,
    TYPE_F32,
    TYPE_F64,
    TYPE_STRING,
    TYPE_BOOL,
    TYPE_VOID,
    // TYPE_ANY,
    // TYPE_TYPE,
    
    TYPE_COUNT,
} Type;

#pragma GCC diagnostic ignored "-Wunused-variable"
static char* TypeStr[TYPE_COUNT + 1] = {
    [TYPE_NONE]   = "NONE",
    
    [TYPE_U8]     = "U8",
    [TYPE_U16]    = "U16",
    [TYPE_U32]    = "U32",
    [TYPE_U64]    = "U64",
    [TYPE_S8]     = "S8",
    [TYPE_S16]    = "S16",
    [TYPE_S32]    = "S32",
    [TYPE_S64]    = "S64",
    [TYPE_F32]    = "F32",
    [TYPE_F64]    = "F64",
    [TYPE_STRING] = "STRING",
    [TYPE_BOOL]   = "BOOL",
    [TYPE_VOID]   = "VOID",
    
    [TYPE_COUNT]  = "COUNT",
};
#pragma GCC diagnostic pop

typedef struct TypeMapping {
    Type type;
    String symbol;
    // int byteSize; // NOTE: maybe later
} TypeMapping;

typedef enum OperatorType {
    OP_TYPE_NONE,
    
    // arithmetic operators take the same type inputs as lhs and rhs, and return the same type aswell: u8 + u8 = u8
    OP_TYPE_ARITHMETIC,
    // logical operators take two int, float, string or bool and always return bool: u8 < u8 = bool
    OP_TYPE_LOGICAL,
    
    OP_TYPE_COUNT,
} OperatorType;

typedef struct OperatorInfo {
    String symbol;
    int precedence;
    OperatorType behaviour;
} OperatorInfo;

// void addType(TypeInformation* info, TypeDefinition def);
// void addOperator(OperatorInformation* info, OperatorDefinition def);
// TypeDefinition TypeDefinitionInit(String symbol, int byteSize);
// OperatorDefinition OperatorDefinitionInit(String symbol, int precedence, TypeDefinition ret, TypeDefinition lhs, TypeDefinition rhs);
// bool containsOp(OperatorInformation opInfo, String op, OperatorDefinition* result);

Type mapFromSymbolToType(TypeMapping* mappings, int mappingsSize, String symbol);
OperatorType getOperatorBehaviourType(OperatorInfo* opInfo, int opInfoSize, String symbol);

#endif // COMP_TYPES_H

// TODO: will probably want to split this file into two later when there is enough differece between types and operators