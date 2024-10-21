#ifndef COMP_HASHMAP_H
#define COMP_HASHMAP_H

#include "string.h"

typedef struct KVPair_SI {
    String key;
    s64 value;
} KVPair_SI;

typedef struct Hashmap {
    KVPair_SI* pair;
    u64 capacity;
    u64 size;
} Hashmap;

typedef struct UserDataEntry {
    u8* data;
    u64 dataLen;

    // Known after the data is writen to the file
    u64 dataRVA;
} UserDataEntry;

typedef struct KVPair_SD {
    String key;
    UserDataEntry value;
} KVPair_SD;

typedef struct HashmapData {
    KVPair_SD* pair;
    u64 capacity;
    u64 size;
} HashmapData;

Hashmap hashmapInit(Arena* mem, u64 capacity);
bool hashmapSet(Hashmap* hs, String key, s64 value);
bool hashmapGet(Hashmap* hs, String key, s64* value);

HashmapData hashmapDataInit(Arena* mem, u64 capacity);
bool hashmapDataSet(HashmapData* hs, String key, UserDataEntry value);
bool hashmapDataGet(HashmapData* hs, String key, UserDataEntry* value);

#endif //  COMP_HASHMAP_H