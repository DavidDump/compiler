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

TypeInfo* TypeExpressionToType(ParsedType* type) {
    assert(type->type == ParsedTypeType_SIMPLE, "Can only convert ParsedTypeType_SIMPLE to TypeInfo");
    return type->as_simple.typeInfo;
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
        case TYPE_TYPE:
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
        case TYPE_STRUCT_DEF: {
            // struct {fieldName: type; ...}
            // struct {  // 8 characters
            // }         // 1 character
            // ': '      // 2 characters per field
            // '; '      // 2 characters per field - 1
            // fieldName // length characters
            // type      // length characters
            Array(TypecheckedField) fields = typeInfo->structInfo.fields;

            u64 totalCharCount = 8 + 1 + (2 * fields.size) + 2 * (fields.size - 1);

            Array(String) typeStrs = {0};
            for(u64 i = 0; i < fields.size; ++i) {
                TypecheckedField field = fields.data[i];

                String typeStr = TypeToString(mem, field.type);
                ArrayAppend(typeStrs, typeStr);

                totalCharCount += field.id.length;
                totalCharCount += typeStr.length;
            }

            u8* buffer = arena_alloc(mem, totalCharCount);
            u64 at = 0;
            char* tmpStrDat = "struct {";
            u64 tmpStrLen = strlen(tmpStrDat);
            memcpy(&buffer[at], tmpStrDat, tmpStrLen);
            at += tmpStrLen;

            for(u64 i = 0; i < fields.size; ++i) {
                TypecheckedField field = fields.data[i];
                String typeStr = typeStrs.data[i];

                memcpy(&buffer[at], field.id.str, field.id.length);
                at += field.id.length;

                buffer[at++] = ':';
                buffer[at++] = ' ';

                memcpy(&buffer[at], typeStr.str, typeStr.length);
                at += typeStr.length;

                if(i + 1 < fields.size) {
                    buffer[at++] = ';';
                    buffer[at++] = ' ';
                }
            }

            result.str = buffer;
            result.length = totalCharCount;

            free(typeStrs.data);
        } break;
        case TYPE_STRUCT_LIT: {
            UNREACHABLE("TYPE_STRUCT_LIT is not really a valid type, i need to refactor some things");
            // name{..., ...} 'name' is 4 + '{}' is 2 + ', ' is 2 per initializer count - 1
            // name{.name = expr, .name = expr} 'name' is 4 + '{}' is 2 + '. = ' is 4 per initializer count + ', ' is 2 per initializer count - 1
        } break;
        case TYPE_ARRAY: {
            u64 arraySize = typeInfo->arrayInfo.arraySize;
            TypeInfo* elementType = typeInfo->arrayInfo.elementType;
            bool isDynamic = typeInfo->arrayInfo.isDynamic;

            // u64[12] elementStr.length + 2 + sizeDigitsCount
            // u64[...]

            String elementStr = TypeToString(mem, elementType);
            String arraySizeStr = {0};
            u64 totalLen = elementStr.length + 2; // NOTE: opening and closing square bracket characters, plus elementLenght
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

        case TYPE_STRUCT_DEF: {
            Array(TypecheckedField) fields = type->structInfo.fields;
            u64 sizeInBytes = 0;
            for(u64 i = 0; i < fields.size; ++i) {
                TypecheckedField field = fields.data[i];
                sizeInBytes += TypeToByteSize(field.type);
            }
            return sizeInBytes;
        } break;
        case TYPE_STRUCT_LIT: {
            UNREACHABLE("TYPE_STRUCT_LIT is not really a valid type, i need to refactor some things");
        } break;

        case TYPE_BOOL:
        case TYPE_S8:
        case TYPE_U8: {
            // return 1;
            return 8;
            // TODO: tmp, the codegen can only generate 64 bit reads and writes, so all variables are 64 bit sized for now
        } break;

        case TYPE_S16:
        case TYPE_U16: {
            // return 2;
            return 8;
            // TODO: tmp, the codegen can only generate 64 bit reads and writes, so all variables are 64 bit sized for now
        } break;

        case TYPE_F32:
        case TYPE_S32:
        case TYPE_U32: {
            // return 4;
            return 8;
            // TODO: tmp, the codegen can only generate 64 bit reads and writes, so all variables are 64 bit sized for now
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

bool TypeIsStructDef(TypeInfo* type) {
    if(type == NULL) return FALSE;
    return type->symbolType == TYPE_STRUCT_DEF;
}

// TODO: pointers need to be compared
bool TypeMatch(TypeInfo* type1, TypeInfo* type2) {
    if(type1 == NULL || type2 == NULL) return FALSE;
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
