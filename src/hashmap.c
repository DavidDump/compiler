#include "hashmap.h"

#define DJB2_INIT 5381
u64 djb2(u64 hash, u8* buffer, u64 size){
    for(size_t i = 0; i < size; ++i) {
        hash = hash * 33 + buffer[i];
    }
    return hash;
}

u64 hash(String str) {
    return djb2(DJB2_INIT, str.str, str.length);
}

Hashmap hashmapInit(Arena* mem, u64 capacity) {
    Hashmap result = {0};
    
    result.pair = arena_alloc(mem, capacity * sizeof(*result.pair));
    result.capacity = capacity;
    
    return result;
}

bool hashmapSet(Hashmap* hs, String key, s64 value) {
    if(hs->size >= hs->capacity) return FALSE;
    u64 index = hash(key) % hs->capacity;

    while(hs->pair[index].key.length != 0) index = (index + 1) % hs->capacity;

    KVPair_SI pair = {.key = key, .value = value};
    hs->pair[index] = pair;
    hs->size++;
    return TRUE;
}

bool hashmapGet(Hashmap* hs, String key, s64* value) {
    u64 index = hash(key) % hs->capacity;

    while(hs->pair[index].key.length != 0 && !StringEquals(hs->pair[index].key, key)) index = (index + 1) % hs->capacity;

    if(StringEquals(hs->pair[index].key, key)) {
        KVPair_SI pair = hs->pair[index];
        *value = pair.value;
        return TRUE;
    }
    return FALSE;
}

HashmapData hashmapDataInit(Arena* mem, u64 capacity) {
    HashmapData result = {0};
    
    result.pair = arena_alloc(mem, capacity * sizeof(*result.pair));
    result.capacity = capacity;
    
    return result;
}

bool hashmapDataSet(HashmapData* hs, String key, UserDataEntry value) {
    if(hs->size >= hs->capacity) return FALSE;
    u64 index = hash(key) % hs->capacity;

    while(hs->pair[index].key.length != 0) index = (index + 1) % hs->capacity;

    KVPair_SD pair = {.key = key, .value = value};
    hs->pair[index] = pair;
    hs->size++;
    return TRUE;
}

bool hashmapDataGet(HashmapData* hs, String key, UserDataEntry* value) {
    u64 index = hash(key) % hs->capacity;

    while(hs->pair[index].key.length != 0 && !StringEquals(hs->pair[index].key, key)) index = (index + 1) % hs->capacity;

    if(StringEquals(hs->pair[index].key, key)) {
        KVPair_SD pair = hs->pair[index];
        *value = pair.value;
        return TRUE;
    }
    return FALSE;
}

HashmapFuncName hashmapFuncNameInit(Arena* mem, u64 capacity) {
    HashmapFuncName result = {0};
    
    result.pair = arena_alloc(mem, capacity * sizeof(*result.pair));
    result.capacity = capacity;
    
    return result;
}

bool hashmapFuncNameSet(HashmapFuncName* hs, String key, FuncName value) {
    if(hs->size >= hs->capacity) return FALSE;
    u64 index = hash(key) % hs->capacity;

    while(hs->pair[index].key.length != 0) index = (index + 1) % hs->capacity;

    KVPair_FuncName pair = {.key = key, .value = value};
    hs->pair[index] = pair;
    hs->size++;
    return TRUE;
}

bool hashmapFuncNameGet(HashmapFuncName* hs, String key, FuncName* value) {
    u64 index = hash(key) % hs->capacity;

    while(hs->pair[index].key.length != 0 && !StringEquals(hs->pair[index].key, key)) index = (index + 1) % hs->capacity;

    if(StringEquals(hs->pair[index].key, key)) {
        KVPair_FuncName pair = hs->pair[index];
        *value = pair.value;
        return TRUE;
    }
    return FALSE;
}

HashmapLibName hashmapLibNameInit(Arena* mem, u64 capacity) {
    HashmapLibName result = {0};
    
    result.pair = arena_alloc(mem, capacity * sizeof(*result.pair));
    result.capacity = capacity;
    
    return result;
}

bool hashmapLibNameSet(HashmapLibName* hs, String key, LibName value) {
    if(hs->size >= hs->capacity) return FALSE;
    u64 index = hash(key) % hs->capacity;

    while(hs->pair[index].key.length != 0) index = (index + 1) % hs->capacity;

    KVPair_LibName pair = {.key = key, .value = value};
    hs->pair[index] = pair;
    hs->size++;
    return TRUE;
}

bool hashmapLibNameGet(HashmapLibName* hs, String key, LibName* value) {
    u64 index = hash(key) % hs->capacity;

    while(hs->pair[index].key.length != 0 && !StringEquals(hs->pair[index].key, key)) index = (index + 1) % hs->capacity;

    if(StringEquals(hs->pair[index].key, key)) {
        KVPair_LibName pair = hs->pair[index];
        *value = pair.value;
        return TRUE;
    }
    return FALSE;
}

// TODO: add a linked list of filled entries in hashmap for quick iteration when iterating all filled entries
// TODO: handle the case when a key is not in the hashmap
