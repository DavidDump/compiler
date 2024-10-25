#ifndef COMP_TYPES_H
#define COMP_TYPES_H

#include "common.h"

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

// Type mapFromSymbolToType(TypeMapping* mappings, int mappingsSize, String symbol);
// OperatorType getOperatorBehaviourType(OperatorInfo* opInfo, int opInfoSize, String symbol);
u64 typeSize(Type type); // assert TYPE_COUNT in these types of functions

#endif // COMP_TYPES_H

// TODO: will probably want to split this file into two later when there is enough differece between types and operators
