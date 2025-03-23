#ifndef COMP_TYPES_H
#define COMP_TYPES_H

typedef struct TypeInfo TypeInfo;
typedef TypeInfo* TypeInfoPtr;

typedef struct FunctionInfo FunctionInfo;
typedef struct ArrayInfo ArrayInfo;

typedef struct FunctionArg FunctionArg;

#include "dataStructures.h"

defArray(TypeInfoPtr);
defArray(FunctionArg);

#include "common.h"
#include "string.h"
#include "parser.h"

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
    TYPE_ARRAY,
    TYPE_TYPE,
    // TYPE_ANY,
    // TYPE_STRUCT,
    // TYPE_ENUM,
    // TYPE_SCOPE,

    TYPE_COUNT,
} Type;

extern char* TypeStr[TYPE_COUNT + 1];

// used when defining arguments during function declaration
typedef struct FunctionArg {
    String id;
    Expression* type;
    // Expression* initialValue; // the expression this argument should be initialized with
} FunctionArg;

typedef struct FunctionInfo {
    Array(FunctionArg) args;
    Expression* returnType;
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
    FunctionInfo* functionInfo;
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
bool TypeIsType(TypeInfo* type);
bool TypeMatch(TypeInfo* type1, TypeInfo* type2);
Type TypeDefaultInt();
Type TypeDefaultFloat();

#endif // COMP_TYPES_H

// TODO: make isPointer into a u64 pointerDepth, with indicates the level of indirecion to a type
//       ex.: int foo;   (pointerDepth = 0)
//            int* foo;  (pointerDepth = 1)
//            int** foo; (pointerDepth = 2)
