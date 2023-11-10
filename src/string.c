#include "string.h"
#include "common.h"

#include <string.h>

void StringChainAppend(StringChain* chain, Arena* mem, String str){
    StringNode* node = arena_alloc(mem, sizeof(StringNode));
    node->str = str;
    node->prev = chain->last;
    if(chain->last) chain->last->next = node;
    chain->last = node;
    if(!chain->first) chain->first = node;
    chain->nodeCount++;
}

String StringFromCstr(Arena* mem, const char* cstr){
    String result = {0};
    int len = strlen(cstr);
    result.str = arena_alloc(mem, len * sizeof(char));
    memcpy((char*)result.str, cstr, len);
    result.length = len;
    return result;
}

String StringFromCstrLit(const char* cstr){
    String result = {0};
    int len = strlen(cstr);
    result.str = cstr;
    result.length = len;
    return result;
}

String StringFromArray(Arena* mem, const char* arr, int size){
    String result = {0};
    result.str = arena_alloc(mem, size * sizeof(char));
    memcpy((char*)result.str, arr, size);
    result.length = size;
    return result;
}

bool StringEquals(String str1, String str2){
    if(str1.length != str2.length) return FALSE;
    for(int i = 0; i < str1.length; i++){
        if(str1.str[i] != str2.str[i]) return FALSE;
    }
    return TRUE;
}

bool StringEqualsCstr(String str1, const char* cstr){
    if(str1.length != (int)strlen(cstr)) return FALSE;
    for(int i = 0; i < str1.length; i++){
        if(str1.str[i] != cstr[i]) return FALSE;
    }
    return TRUE;
}

bool StringContains(String str1, const char* cstr){
    if(str1.length > (int)strlen(cstr)) return FALSE;
    for(int i = 0; i < str1.length; i++){
        if(str1.str[i] != cstr[i]) return FALSE;
    }
    return TRUE;
}
