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

typedef struct FuncName {
    u64 nameRva;
    u64 iatRva;
} FuncName;

typedef struct KVPair_FuncName {
    String key;
    FuncName value;
} KVPair_FuncName;

typedef struct HashmapFuncName {
    KVPair_FuncName* pair;
    u64 capacity;
    u64 size;
} HashmapFuncName;

typedef struct LibName {
    u64 nameRva;
    u64 iatRva;
    u64 imageThunkRva;
    HashmapFuncName* functions;
} LibName;

typedef struct KVPair_LibName {
    String key;
    LibName value;
} KVPair_LibName;

typedef struct HashmapLibName {
    KVPair_LibName* pair;
    u64 capacity;
    u64 size;
} HashmapLibName;

Hashmap hashmapInit(Arena* mem, u64 capacity);
bool hashmapSet(Hashmap* hs, String key, s64 value);
bool hashmapGet(Hashmap* hs, String key, s64* value);

HashmapData hashmapDataInit(Arena* mem, u64 capacity);
bool hashmapDataSet(HashmapData* hs, String key, UserDataEntry value);
bool hashmapDataGet(HashmapData* hs, String key, UserDataEntry* value);

HashmapFuncName hashmapFuncNameInit(Arena* mem, u64 capacity);
bool hashmapFuncNameSet(HashmapFuncName* hs, String key, FuncName value);
bool hashmapFuncNameGet(HashmapFuncName* hs, String key, FuncName* value);

HashmapLibName hashmapLibNameInit(Arena* mem, u64 capacity);
bool hashmapLibNameSet(HashmapLibName* hs, String key, LibName value);
bool hashmapLibNameGet(HashmapLibName* hs, String key, LibName* value);

#endif //  COMP_HASHMAP_H