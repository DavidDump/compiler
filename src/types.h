#ifndef COMP_TYPES_H
#define COMP_TYPES_H

#include "common.h"
#include "dataStructures.h"

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
    TYPE_FUNCTION,
    // TYPE_ANY,
    // TYPE_TYPE,
    // TYPE_STRUCT,
    // TYPE_ENUM,
    // TYPE_SCOPE,

    TYPE_COUNT,
} Type;

#pragma GCC diagnostic ignored "-Wunused-variable"
static char* TypeStr[TYPE_COUNT + 1] = {
    [TYPE_NONE]     = "NONE",

    [TYPE_U8]       = "U8",
    [TYPE_U16]      = "U16",
    [TYPE_U32]      = "U32",
    [TYPE_U64]      = "U64",
    [TYPE_S8]       = "S8",
    [TYPE_S16]      = "S16",
    [TYPE_S32]      = "S32",
    [TYPE_S64]      = "S64",
    [TYPE_F32]      = "F32",
    [TYPE_F64]      = "F64",
    [TYPE_STRING]   = "STRING",
    [TYPE_BOOL]     = "BOOL",
    [TYPE_VOID]     = "VOID",
    [TYPE_FUNCTION] = "FUNCTION",

    [TYPE_COUNT]    = "COUNT",
};
#pragma GCC diagnostic pop

typedef struct TypeInfo TypeInfo;
defArray(TypeInfo);

typedef struct FunctionInfo {
    Array(TypeInfo) argTypes;
    TypeInfo returnType;
    bool isExternal;
} FunctionInfo;

typedef struct TypeInfo {
    Type symbolType;
    bool isPointer; // flag if the type is a pointer to symbolType type
    bool isArray;   // flag if the type is an array of symbolTypes
    bool isDynamic; // if array, flag if array is dynamic
    u64 arraySize;  // if array, size of array is this
    FunctionInfo functionInfo; // if fuction, contains information about the function
} TypeInfo;

#endif // COMP_TYPES_H
