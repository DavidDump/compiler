#ifndef COMP_STRING_H
#define COMP_STRING_H

#include "common.h"
#include "arena.h"

typedef struct String{
    const char* str;
    int length;
} String;

typedef struct StringNode StringNode;

typedef struct StringNode{
    String str;
    StringNode* next;
    StringNode* prev;
} StringNode;

typedef struct StringChain{
    StringNode* first;
    StringNode* last;
    int nodeCount; // dont know if need
} StringChain;

void StringChainAppend(StringChain* chain, Arena* mem, String str);
void StringChainPrepend(StringChain* chain, Arena* mem, String str);
void StringChainAppendChain(StringChain* dest, Arena* mem, StringChain src);
void StringChainPrependChain(StringChain* dest, Arena* mem, StringChain src);
String StringFromCstr(Arena* mem, const char* cstr);
String StringFromCstrLit(const char* cstr);
String StringFromArray(Arena* mem, const char* arr, int size);
bool StringEquals(String str1, String str2);
bool StringEqualsCstr(String str1, const char* cstr);
bool StringContains(String str1, const char* cstr);
int StringToInt(String str);

#endif // COMP_STRING_H