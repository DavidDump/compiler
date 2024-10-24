#include "string.h"
#include "common.h"

#include <string.h> // strlen(), memcpy()

int StringToInt(String str){
    int result = 0;
    for(int i = str.length; i > 0; i--){
        char c = str.str[i];
        if('0' < c && c < '9' && c == '-'){
            int num = c - '0';
            result *= 10;
            result += num;
        }else{
            break;
        }
    }
    return result;
}

void StringChainAppend(StringChain* chain, Arena* mem, String str){
    StringNode* node = arena_alloc(mem, sizeof(StringNode));
    node->str = str;
    node->prev = chain->last;
    if(chain->last) chain->last->next = node;
    chain->last = node;
    if(!chain->first) chain->first = node;
    chain->nodeCount++;
}

void StringChainPrepend(StringChain* chain, Arena* mem, String str){
    StringNode* node = arena_alloc(mem, sizeof(StringNode));
    node->str = str;
    node->next = chain->first;
    if(chain->first) chain->first->prev = node;
    chain->first = node;
    if(!chain->last) chain->last = node;
    chain->nodeCount++;
}

void StringChainAppendChain(StringChain* dest, Arena* mem, StringChain src){
    StringNode* current = src.first;
    for(int i = 0; i < src.nodeCount; i++){
        StringChainAppend(dest, mem, current->str);
        current = current->next;
    }
}

void StringChainPrependChain(StringChain* dest, Arena* mem, StringChain src){
    StringNode* current = src.last;
    for(int i = src.nodeCount; i > 0; i--){
        StringChainPrepend(dest, mem, current->str);
        current = current->prev;
    }
}

String StringFromCstr(Arena* mem, char* cstr){
    String result = {0};
    int len = strlen(cstr);
    result.str = arena_alloc(mem, len * sizeof(char));
    memcpy((char*)result.str, cstr, len);
    result.length = len;
    return result;
}

String StringFromCstrLit(char* cstr){
    String result = {0};
    int len = strlen(cstr);
    result.str = (u8*)cstr;
    result.length = len;
    return result;
}

String StringFromArray(Arena* mem, char* arr, int size){
    String result = {0};
    result.str = arena_alloc(mem, size * sizeof(char));
    memcpy((char*)result.str, arr, size);
    result.length = size;
    return result;
}

bool StringEquals(String str1, String str2){
    if(str1.length != str2.length) return FALSE;
    for(u64 i = 0; i < str1.length; ++i) {
        if(str1.str[i] != str2.str[i]) return FALSE;
    }
    return TRUE;
}

bool StringEqualsCstr(String str1, char* cstr){
    if(str1.length != strlen(cstr)) return FALSE;
    for(u64 i = 0; i < str1.length; ++i) {
        if(str1.str[i] != cstr[i]) return FALSE;
    }
    return TRUE;
}

bool StringContains(String str1, char* cstr){
    if(str1.length > strlen(cstr)) return FALSE;
    for(u64 i = 0; i < str1.length; ++i) {
        if(str1.str[i] != cstr[i]) return FALSE;
    }
    return TRUE;
}

u64 StringToU64(String value) {
    u64 result = 0;

    // TODO: cast
    for(u64 i = 0; i < (u64)value.length; ++i) {
        u8 c = value.str[i];
        assert('0' <= c && c <= '9' && "can only convert numbers");
        // if(result > UINT64_MAX / 10); // overflow
        result *= 10;
        // if(result > UINT64_MAX - (c - '0')); // overflow
        result += c - '0';
    }

    return result;
}

u32 StringToU32(String value) {
    u32 result = 0;

    for(u64 i = 0; i < value.length; ++i) {
        u8 c = value.str[i];
        assert('0' <= c && c <= '9' && "can only convert numbers");
        // if(result > UINT32_MAX / 10); // overflow
        result *= 10;
        // if(result > UINT32_MAX - (c - '0')); // overflow
        result += c - '0';
    }

    return result;
}

bool StringEndsWith(String str, String end) {
    if(str.length < end.length) return FALSE;
    if(StringEquals((String){.str = str.str + (str.length - end.length), .length = end.length}, end)) return TRUE;
    return FALSE;
}

s64 StringLastIndexOf(String str, u8 c) {
    for(s64 i = (s64)str.length; i >= 0; --c) {
        if(str.str[i] == c) return i;
    }
    return -1;
}
