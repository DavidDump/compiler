#ifndef DATA_STRUCTS_H
#define DATA_STRUCTS_H

// Common data structures v1
//   Hashmap
//   Dynamic array

// Usage:
// TODO

// TODO: somehow configure this
#if 1
#include "common.h"
#else
typedef int bool;
#define TRUE 1
#define FALSE 0

typedef int8_t  s8;
typedef int16_t s16;
typedef int32_t s32;
typedef int64_t s64;

typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
#endif

// 
// Hashmaps
// 

#define DJB2_INIT 5381
u64 djb2(u64 hash, u8* buffer, u64 size);

#define KVPair(_keyType_, _valType_) KVPair##_keyType_##_valType_
#define defKVPair(_keyType_, _valType_) typedef struct KVPair(_keyType_, _valType_) { \
        _keyType_ key; \
        _valType_ value; \
        bool occupied; \
        struct KVPair(_keyType_, _valType_)* next; \
        struct KVPair(_keyType_, _valType_)* prev; \
    } KVPair(_keyType_, _valType_)

#define Hashmap(_keyType_, _valType_) Hashmap##_keyType_##_valType_
#define defHashmap(_keyType_, _valType_) typedef struct Hashmap(_keyType_, _valType_) { \
    KVPair(_keyType_, _valType_)* pairs;       \
    u64 capacity;                                \
    u64 size;                                    \
    /* linked list of all the elements with a valid value, used for iterating all the elements */ \
    KVPair(_keyType_, _valType_)* first;       \
    KVPair(_keyType_, _valType_)* last;        \
} Hashmap(_keyType_, _valType_)

// TODO: make allocator configurable
#define HashmapInit(_hashmap_, _capacity_) do { \
    (_hashmap_).pairs = calloc(_capacity_, sizeof(*(_hashmap_).pairs)); \
    (_hashmap_).capacity = _capacity_; \
} while(0)

#define HashmapCmpFunc(_keyType_) (HashmapCmp##_keyType_)
#define HashmapHashFunc(_keyType_) (HashmapHash##_keyType_)

// HashmapSet
#define HashmapSet(_keyType_, _valType_) (HashmapSet##_keyType_##_valType_)
#ifdef DATA_STRUCT_IMPLEMENTATION
#define defHashmapSet(_keyType_, _valType_) \
bool (HashmapSet##_keyType_##_valType_)(Hashmap(_keyType_, _valType_)* hs, _keyType_ key, _valType_ value) { \
    assert(hs->capacity != 0, "Trying to set a value in an uninitialized hashmap"); \
    if(hs->size >= hs->capacity) return FALSE; \
    u64 index = HashmapHashFunc(_keyType_)(key) % hs->capacity; \
    while(hs->pairs[index].occupied && !HashmapCmpFunc(_keyType_)(hs->pairs[index].key, key)) index = (index + 1) % hs->capacity; \
    bool wasOccupied = hs->pairs[index].occupied; \
    hs->pairs[index].key = key; \
    hs->pairs[index].value = value; \
    hs->pairs[index].occupied = TRUE; \
    hs->pairs[index].next = NULL; \
    hs->pairs[index].prev = hs->last; \
    if(hs->last) { \
        if(!wasOccupied) hs->last->next = &hs->pairs[index]; \
    } else { \
        hs->first = &hs->pairs[index]; \
    } \
    hs->last = &hs->pairs[index]; \
    hs->size++; \
    return TRUE; \
}
#else
#define defHashmapSet(_keyType_, _valType_) \
bool (HashmapSet##_keyType_##_valType_)(Hashmap(_keyType_, _valType_)* hs, _keyType_ key, _valType_ value);
#endif // DATA_STRUCT_IMPLEMENTATION

// HashmapGet
#define HashmapGet(_keyType_, _valType_) (hashmapGet##_keyType_##_valType_)
#ifdef DATA_STRUCT_IMPLEMENTATION
#define defHashmapGet(_keyType_, _valType_) \
bool (hashmapGet##_keyType_##_valType_)(Hashmap(_keyType_, _valType_)* hs, _keyType_ key, _valType_* value) { \
    assert(hs->capacity != 0, "Trying to get a value from an uninitialized hashmap"); \
    if(hs->size == 0) return FALSE; \
    u64 index = HashmapHashFunc(_keyType_)(key) % hs->capacity; \
    u64 checkedCount = 0; \
    while(checkedCount++ < hs->size && hs->pairs[index].occupied && !HashmapCmpFunc(_keyType_)(hs->pairs[index].key, key)) index = (index + 1) % hs->capacity; \
    if(HashmapCmpFunc(_keyType_)(hs->pairs[index].key, key)) { \
        *value = hs->pairs[index].value; \
        return TRUE; \
    } \
    return FALSE; \
}
#else
#define defHashmapGet(_keyType_, _valType_) \
bool (hashmapGet##_keyType_##_valType_)(Hashmap(_keyType_, _valType_)* hs, _keyType_ key, _valType_* value);
#endif // DATA_STRUCT_IMPLEMENTATION

#define defHashmapFuncs(_keyType_, _valType_) \
    defKVPair(_keyType_, _valType_);          \
    defHashmap(_keyType_, _valType_);         \
    defHashmapSet(_keyType_, _valType_)       \
    defHashmapGet(_keyType_, _valType_)

#define HashmapFor(_keyType_, _valType_, _itName_, _hashmap_) \
    for(KVPair(_keyType_, _valType_)* (_itName_) = (_hashmap_)->first; (_itName_) != NULL; (_itName_) = (_itName_)->next)

// TODO: additional operations HashmapRemove, HashmapResize
// TODO: allow for custom allocators in HashmapInit

// 
// Dynamic Arrays
// 

#define Array(_type_) Array##_type_
#define defArray(_type_) typedef struct Array(_type_) { \
    _type_* data; \
    u64 capacity; \
    u64 size; \
} Array(_type_)

// TODO: make allocator configurable
#define ArrayInitialCapacity 1
#define ArrayAppend(_array_, _value_) do { \
    if((_array_).size >= (_array_).capacity) { \
        size_t newCap = (_array_).capacity * 2; \
        if(newCap == 0) newCap = ArrayInitialCapacity; \
        (_array_).data = realloc((_array_).data, newCap * sizeof(*(_array_).data)); \
        (_array_).capacity = newCap; \
    } \
    (_array_).data[(_array_).size] = (_value_); \
    (_array_).size++; \
} while(0)

#define ArrayRemoveUnordered(_array_, _index_) do { \
    assert(0 <= (_index_) && (_index_) < (_array_).size, "Index out of bounds"); \
    (_array_).data[(_index_)] = (_array_).data[(_array_).size - 1]; \
    (_array_).size--; \
} while(0)

#endif // DATA_STRUCTS_H

#ifdef DATA_STRUCT_IMPLEMENTATION

u64 djb2(u64 hash, u8* buffer, u64 size) {
    for(size_t i = 0; i < size; ++i) {
        hash = hash * 33 + buffer[i];
    }
    return hash;
}

#endif // DATA_STRUCT_IMPLEMENTATION
