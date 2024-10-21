#include "buffer.h"

#include <assert.h>
#include <windows.h>

Buffer make_buffer(u64 capacity, u32 permission_flags) {
    Buffer result = {0};
    result.mem = VirtualAlloc(0, capacity, MEM_COMMIT | MEM_RESERVE, permission_flags);
    result.capacity = capacity;
    return result;
}

void* buffer_allocate_size(Buffer* buffer, u64 byte_size) {
    assert(buffer->size + byte_size <= buffer->capacity && "Buffer ran out of memory");
    void *target = buffer->mem + buffer->size;
    buffer->size += byte_size;
    return target;
}

#define define_buffer_append(_type_)                             \
    _type_ *buffer_append_##_type_(Buffer *buffer, _type_ value) \
    {                                                            \
        _type_ *target = buffer_allocate(buffer, _type_);        \
        *target = value;                                         \
        return target;                                           \
    }

define_buffer_append(s8)
define_buffer_append(s16)
define_buffer_append(s32)
define_buffer_append(s64)

define_buffer_append(u8)
define_buffer_append(u16)
define_buffer_append(u32)
define_buffer_append(u64)

#undef define_buffer_append
