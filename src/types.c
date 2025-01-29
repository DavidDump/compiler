#include "types.h"

char* strAlloc = "TODO: type";
String typeToString(TypeInfo typeInfo) {
    return (String){.str = strAlloc, .length = 10};
    // TODO: implement the function
}

TypeInfo* typeVoid(Arena* mem) {
    TypeInfo* result = arena_alloc(mem, sizeof(TypeInfo));
    result->symbolType = TYPE_VOID;
    result->isPointer = FALSE;
    result->arrayInfo = (ArrayInfo){0};
    result->functionInfo = (FunctionInfo){0};
    return result;
}
