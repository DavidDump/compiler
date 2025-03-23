#include <string.h>
#include "types.h"

char* TypeStr[TYPE_COUNT + 1] = {
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
    [TYPE_TYPE]     = "TYPE",

    [TYPE_COUNT]    = "COUNT",
};

TypeInfo* TypeExpressionToType(Expression* expr) {
    assert(expr->type == ExpressionType_TYPE, "Can only convert type Expressions to TypeInfo");
    return expr->expr.TYPE.typeInfo;
}

String TypeToString(Arena* mem, TypeInfo* typeInfo) {
    String result = {0};

    switch(typeInfo->symbolType) {
        case TYPE_NONE:
        case TYPE_COUNT: {
            UNREACHABLE("invalid case");
        } break;

        case TYPE_U8:
        case TYPE_U16:
        case TYPE_U32:
        case TYPE_U64:
        case TYPE_S8:
        case TYPE_S16:
        case TYPE_S32:
        case TYPE_S64:
        case TYPE_F32:
        case TYPE_F64:
        case TYPE_STRING:
        case TYPE_BOOL:
        case TYPE_VOID: {
            result.str = (u8*)TypeStr[typeInfo->symbolType];
            result.length = strlen(TypeStr[typeInfo->symbolType]);
        } break;
        case TYPE_FUNCTION: {
            Array(FunctionArg) args = typeInfo->functionInfo->args;
            TypeInfo* returnType = TypeExpressionToType(typeInfo->functionInfo->returnType);
            bool isExternal = typeInfo->functionInfo->isExternal;
            UNUSED(isExternal);

            // (arg, arg, arg) -> VOID

            Array(String) argTypes = {0};
            u64 totalLen = 2; // NOTE: start from 2 for the opening and closing parenthesis
            for(u64 i = 0; i < args.size; ++i) {
                FunctionArg arg = args.data[i];
                String argStr = TypeToString(mem, TypeExpressionToType(arg.type));
                totalLen += argStr.length;
                ArrayAppend(argTypes, argStr);
            }

            if(totalLen != 0) {
                // NOTE: one comma and one space per inbetween each argument
                totalLen += (args.size - 1) * 2;
            }

            String returnStr = TypeToString(mem, returnType);
            if(returnStr.length != 0) {
                totalLen += 4; // NOTE: ' -> ' delimiter between arguments and return type, 4 characters
                totalLen += returnStr.length;
            }

            u8* buffer = arena_alloc(mem, totalLen);
            result.length = totalLen;
            u64 at = 0;
            buffer[at++] = '(';
            for(u64 i = 0; i < argTypes.size; ++i) {
                String type = argTypes.data[i];
                memcpy(&buffer[at], type.str, type.length);
                at += type.length;
                if(i + 1 < argTypes.size) {
                    buffer[at++] = ',';
                    buffer[at++] = ' ';
                }
            }
            buffer[at++] = ')';

            if(returnStr.length != 0) {
                buffer[at++] = ' ';
                buffer[at++] = '-';
                buffer[at++] = '>';
                buffer[at++] = ' ';
                memcpy(&buffer[at], returnStr.str, returnStr.length);
            }
            result.str = buffer;
            free(argTypes.data);
        } break;
        case TYPE_TYPE: {
            // Type(...)
            String subType = TypeToString(mem, typeInfo->typeInfo);
            u8* buffer = arena_alloc(mem, subType.length + 6); // +6 for the `Type()` string
            result.str = buffer;
            result.length = subType.length + 6;

            u64 at = 0;
            buffer[at++] = 'T';
            buffer[at++] = 'y';
            buffer[at++] = 'p';
            buffer[at++] = 'e';
            buffer[at++] = '(';
            memcpy(&buffer[at], subType.str, subType.length);
            at += subType.length;
            buffer[at++] = ')';
        } break;
        case TYPE_ARRAY: {
            u64 arraySize = typeInfo->arrayInfo.arraySize;
            TypeInfo* elementType = typeInfo->arrayInfo.elementType;
            bool isDynamic = typeInfo->arrayInfo.isDynamic;

            // u64[12]
            // u64[...]

            String elementStr = TypeToString(mem, elementType);
            String arraySizeStr = {0};
            u64 totalLen = 2; // NOTE: opening and closing square bracket chracters
            if(isDynamic) {
                totalLen += 3; // NOTE: three dots, '...' characters
            } else {
                arraySizeStr = StringFromU64(mem, arraySize);
                totalLen += arraySizeStr.length;
            }

            result.str = arena_alloc(mem, totalLen);
            result.length = totalLen;

            u64 at = 0;
            memcpy(&result.str[at], elementStr.str, elementStr.length);
            at += elementStr.length;
            result.str[at++] = '[';
            if(isDynamic) {
                result.str[at++] = '.';
                result.str[at++] = '.';
                result.str[at++] = '.';
            } else {
                memcpy(&result.str[at], arraySizeStr.str, arraySizeStr.length);
                at += arraySizeStr.length;
            }
            result.str[at++] = ']';
        } break;
    }

    return result;
}

TypeInfo* TypeInitSimple(Arena* mem, Type t) {
    TypeInfo* result = arena_alloc(mem, sizeof(TypeInfo));
    result->symbolType = t;
    return result;
}

u64 TypeToByteSize(TypeInfo* type) {
    switch(type->symbolType) {
        case TYPE_NONE:
        case TYPE_COUNT: {
            UNREACHABLE("This is an invalid chase");
        } break;

        // TODO: right now a string is just a pointer to the .data section
        // once struct are added this will be the size of the string struct
        case TYPE_STRING: {
            return 8;
        } break;

        // TODO: type will later have a struct that its stored in if its a value
        case TYPE_TYPE:
        // NOTE: these are stored in a special way not directly on the stack
        case TYPE_FUNCTION:
        case TYPE_VOID: {
            return 0;
        } break;

        case TYPE_BOOL:
        case TYPE_S8:
        case TYPE_U8: {
            return 1;
        } break;

        case TYPE_S16:
        case TYPE_U16: {
            return 2;
        } break;

        case TYPE_F32:
        case TYPE_S32:
        case TYPE_U32: {
            return 4;
        } break;

        case TYPE_F64:
        case TYPE_S64:
        case TYPE_U64: {
            return 8;
        } break;

        case TYPE_ARRAY: {
            u64 elementSize = TypeToByteSize(type->arrayInfo.elementType);
            return type->arrayInfo.arraySize * elementSize;
        } break;
    }

    UNREACHABLE("not all cases are handled in TypeToByteSize");
    return 0; // NOTE: silencing the warning
}

bool TypeIsSigned(TypeInfo* type) {
    if(type == NULL) return FALSE;
    return (
        type->symbolType == TYPE_S8  ||
        type->symbolType == TYPE_S16 ||
        type->symbolType == TYPE_S32 ||
        type->symbolType == TYPE_S64
    );
}

bool TypeIsUnsigned(TypeInfo* type) {
    if(type == NULL) return FALSE;
    return (
        type->symbolType == TYPE_U8  ||
        type->symbolType == TYPE_U16 ||
        type->symbolType == TYPE_U32 ||
        type->symbolType == TYPE_U64
    );
}

bool TypeIsFloat(TypeInfo* type) {
    if(type == NULL) return FALSE;
    return (
        type->symbolType == TYPE_F32 ||
        type->symbolType == TYPE_F64
    );
}

bool TypeIsInt(TypeInfo* type) {
    if(type == NULL) return FALSE;
    return TypeIsSigned(type) || TypeIsUnsigned(type);
}

bool TypeIsNumber(TypeInfo* type) {
    if(type == NULL) return FALSE;
    return TypeIsInt(type) || TypeIsFloat(type);
}

bool TypeIsBool(TypeInfo* type) {
    if(type == NULL) return FALSE;
    return type->symbolType == TYPE_BOOL;
}

bool TypeIsType(TypeInfo* type) {
    if(type == NULL) return FALSE;
    return type->symbolType == TYPE_TYPE;
}

// TODO: pointers need to be compared
bool TypeMatch(TypeInfo* type1, TypeInfo* type2) {
    bool result = FALSE;

    result = type1->symbolType == type2->symbolType;

    if(type1->symbolType == TYPE_ARRAY) {
        result = result && TypeMatch(type1->arrayInfo.elementType, type2->arrayInfo.elementType);
        result = result && type1->arrayInfo.arraySize == type2->arrayInfo.arraySize;
        result = result && type1->arrayInfo.isDynamic == type2->arrayInfo.isDynamic;
    } else if(type1->symbolType == TYPE_FUNCTION) {
        result = result && TypeMatch(TypeExpressionToType(type1->functionInfo->returnType), TypeExpressionToType(type2->functionInfo->returnType));
        result = result && type1->functionInfo->isExternal == type2->functionInfo->isExternal;
        result = result && type1->functionInfo->args.size == type2->functionInfo->args.size;
        for(u64 i = 0; i < type1->functionInfo->args.size; ++i) {
            FunctionArg info1 = type1->functionInfo->args.data[i];
            FunctionArg info2 = type2->functionInfo->args.data[i];
            result = result && TypeMatch(TypeExpressionToType(info1.type), TypeExpressionToType(info2.type));
        }
    }

    return result;
}

Type TypeDefaultInt() {
    return TYPE_S64;
}

Type TypeDefaultFloat() {
    return TYPE_F64;
}
