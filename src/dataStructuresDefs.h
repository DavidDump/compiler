#ifndef COMP_DATA_STRUCT_DEFS_H
#define COMP_DATA_STRUCT_DEFS_H

#include "string.h"
#include "dataStructures.h"

// TODO: move to a proper location
typedef struct UserDataEntry {
    u8* data;
    u64 dataLen;

    // Known after the data is writen to the file
    u64 dataRVA;
} UserDataEntry;

typedef struct FuncName {
    u64 nameRva;
    u64 iatRva;
} FuncName;

#define HashmapHashString StringHash
#define HashmapCmpString StringEquals

defHashmapFuncs(String, s64)
defHashmapFuncs(String, UserDataEntry)
defHashmapFuncs(String, FuncName)

typedef struct LibName {
    u64 nameRva;
    u64 iatRva;
    u64 imageThunkRva;
    Hashmap(String, FuncName)* functions;
} LibName;

defHashmapFuncs(String, LibName)

#endif // COMP_DATA_STRUCT_DEFS_H
