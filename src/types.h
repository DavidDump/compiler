#ifndef COMP_TYPES_H
#define COMP_TYPES_H

#include "common.h"
#include "dataStructures.h"
#include "string.h"

typedef enum Type {
    TYPE_NONE,

    // NOTE: the order cannot change
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
    TYPE_ARRAY,
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
    [TYPE_ARRAY]    = "ARRAY",

    [TYPE_COUNT]    = "COUNT",
};
#pragma GCC diagnostic pop

typedef struct TypeInfo TypeInfo;
typedef TypeInfo* TypeInfoPtr;
defArray(TypeInfoPtr);

typedef struct StringAndType {
    String id;
    TypeInfo* type;
} StringAndType;

defArray(StringAndType);

typedef struct FunctionInfo {
    Array(StringAndType) args;
    TypeInfo* returnType;
    bool isExternal;
} FunctionInfo;

typedef struct ArrayInfo {
    bool isDynamic;
    u64 arraySize;
    TypeInfo* elementType;
} ArrayInfo;

typedef struct TypeInfo {
    Type symbolType;
    bool isPointer;  // flag if the type is a pointer to symbolType type
    bool isConstant; // flag if all the leaves of the expression are constant values, example.: 1 + 1
    ArrayInfo arrayInfo;
    FunctionInfo functionInfo; // if fuction, contains information about the function
} TypeInfo;

TypeInfo* TypeInitSimple(Arena* mem, Type t);
String TypeToString(Arena* mem, TypeInfo* typeInfo);
u64 TypeToByteSize(TypeInfo* type);
bool TypeIsSigned(TypeInfo* type);
bool TypeIsUnsigned(TypeInfo* type);
bool TypeIsFloat(TypeInfo* type);
bool TypeIsInt(TypeInfo* type);
bool TypeIsNumber(TypeInfo* type);
bool TypeIsBool(TypeInfo* type);
bool TypeMatch(TypeInfo* type1, TypeInfo* type2);
Type TypeDefaultInt();
Type TypeDefaultFloat();

#endif // COMP_TYPES_H

// TODO: make isPointer into a u64 pointerDepth, with indicates the level of indirecion to a type
//       ex.: int foo;   (pointerDepth = 0)
//            int* foo;  (pointerDepth = 1)
//            int** foo; (pointerDepth = 2)
