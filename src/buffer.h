#ifndef COMP_BUFFER_H
#define COMP_BUFFER_H

#include "common.h"

// NOTE: redefining this here because i dont want to include windows headers everywhere
#define PAGE_NOACCESS           0x01
#define PAGE_READONLY           0x02
#define PAGE_READWRITE          0x04
#define PAGE_WRITECOPY          0x08
#define PAGE_EXECUTE            0x10
#define PAGE_EXECUTE_READ       0x20
#define PAGE_EXECUTE_READWRITE  0x40
#define PAGE_EXECUTE_WRITECOPY  0x80
#define PAGE_GUARD             0x100

typedef struct Buffer {
    u8* mem;
    u64 size;
    u64 capacity;
} Buffer;

#define buffer_allocate(_buffer_, _type_)  (_type_ *)buffer_allocate_size((_buffer_), sizeof(_type_))

Buffer make_buffer(u64 capacity, u32 permission_flags);
void* buffer_allocate_size(Buffer* buffer, u64 byte_size);
u8* buffer_append_u8(Buffer *buffer, u8 value);
u16* buffer_append_u16(Buffer *buffer, u16 value);
u32* buffer_append_u32(Buffer *buffer, u32 value);
u64* buffer_append_u64(Buffer *buffer, u64 value);
s8* buffer_append_s8(Buffer *buffer, s8 value);
s16* buffer_append_s16(Buffer *buffer, s16 value);
s32* buffer_append_s32(Buffer *buffer, s32 value);
s64* buffer_append_s64(Buffer *buffer, s64 value);

#endif // COMP_BUFFER_H
