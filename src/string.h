#ifndef COMP_STRING_H
#define COMP_STRING_H

#include "common.h"
#include "arena.h"
#include "dataStructures.h"
#include "commonTypes.h"

// used for printing String type, usage:
//     String foo = STR("test string");
//     printf("some text in the format string: "STR_FMT"\n", STR_PRINT(foo));
#define STR_FMT "%.*s"
#define STR_PRINT(_str_) (int)(_str_).length, (_str_).str

// #define STR(_cstr_) StringFromCstrLit(_cstr_)
#define STR(_cstr_) (String){.str = (u8*)(_cstr_), .length = sizeof(_cstr_) - 1}

typedef struct String {
    u8* str;
    u64 length;
} String;
defArray(String);

typedef struct StringNode StringNode;

typedef struct StringNode {
    String str;
    StringNode* next;
    StringNode* prev;
} StringNode;

typedef struct StringChain {
    StringNode* first;
    StringNode* last;
    int nodeCount; // dont know if need
} StringChain;

void StringChainAppend(StringChain* chain, Arena* mem, String str);
void StringChainPrepend(StringChain* chain, Arena* mem, String str);
void StringChainAppendChain(StringChain* dest, Arena* mem, StringChain src);
void StringChainPrependChain(StringChain* dest, Arena* mem, StringChain src);
String StringFromCstr(Arena* mem, char* cstr);
String StringFromCstrLit(char* cstr);
String StringFromArray(Arena* mem, char* arr, int size);
bool StringEquals(String str1, String str2);
bool StringEqualsCstr(String str1, char* cstr);
bool StringContains(String str1, char* cstr);
int StringToInt(String str);
u64 StringToU64(String value);
String StringFromU64(Arena* mem, u64 value);
String StringFromS64(Arena* mem, s64 value);
String StringFromF64(Arena* mem, f64 value);
u32 StringToU32(String value);
s64 StringToS64(String value);
bool StringEndsWith(String str, String end);
s64 StringLastIndexOf(String str, u8 c);
u64 StringHash(String str);

#endif // COMP_STRING_H
