#include "common.h"

#include <assert.h>
#include <windows.h>
#include <winnt.h>
#include <stdbool.h>
#include <math.h>
#include <time.h>

#define ARRAY_SIZE(x) (sizeof(x)/sizeof(*x))

typedef struct Buffer {
    u8* mem;
    u64 size;
    u64 capacity;
} Buffer;

typedef struct NamesToPatch {
    u8* name;
    u64 len;
    u64 offset;
} NamesToPatch;

Buffer make_buffer(u64 capacity, u32 permission_flags) {
    Buffer result = {0};
    result.mem = VirtualAlloc(0, capacity, MEM_COMMIT | MEM_RESERVE, permission_flags);
    result.capacity = capacity;
    return result;
}

#define buffer_allocate(_buffer_, _type_)  (_type_ *)buffer_allocate_size((_buffer_), sizeof(_type_))

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

s32 align(s32 number, s32 alignment) {
  return (s32)(ceil((double)number / alignment) * alignment);
}

typedef struct {
    const char *name;
    u32 name_rva;
    u32 iat_rva;
} Import_Name_To_Rva;

typedef struct {
    Import_Name_To_Rva dll;
    Import_Name_To_Rva *functions;
    u32 image_thunk_rva;
    s32 function_count;
} Import_Library;

u32 getFunctionRVA(Import_Library* libs, u64 libsCount, u8* name, u64 nameLen) {
    for (u64 i = 0; i < libsCount; ++i) {
        Import_Library *lib = &libs[i];
        for (s32 i = 0; i < lib->function_count; ++i) {
            Import_Name_To_Rva *fn = &lib->functions[i];
            // TODO: remove strcmp and use sized strings
            if (strcmp(fn->name, name) == 0) return fn->iat_rva;
        }
    }
    assert(false && "Unreachable: a function was not imported");
}

Buffer write_executable(Import_Library* libs, u64 libsCount, Buffer code, Buffer names) {
    u32 fileAlignment = 0x200;
    
    Buffer exe_buffer = make_buffer(1024 * 1024, PAGE_READWRITE);
    IMAGE_DOS_HEADER *dos_header = buffer_allocate(&exe_buffer, IMAGE_DOS_HEADER);

    *dos_header = (IMAGE_DOS_HEADER){
        .e_magic = IMAGE_DOS_SIGNATURE,
        .e_lfanew = sizeof(IMAGE_DOS_HEADER),
    };
    buffer_append_s32(&exe_buffer, IMAGE_NT_SIGNATURE);

    IMAGE_FILE_HEADER *file_header = buffer_allocate(&exe_buffer, IMAGE_FILE_HEADER);

    *file_header = (IMAGE_FILE_HEADER){
        .Machine = IMAGE_FILE_MACHINE_AMD64,
        .NumberOfSections = 2,       // TODO: this can be hardcoded but depends on the writer architecture
        .TimeDateStamp = time(NULL),
        .SizeOfOptionalHeader = sizeof(IMAGE_OPTIONAL_HEADER64),
        .Characteristics = IMAGE_FILE_EXECUTABLE_IMAGE | IMAGE_FILE_LARGE_ADDRESS_AWARE,
    };

    IMAGE_OPTIONAL_HEADER64 *optional_header = buffer_allocate(&exe_buffer, IMAGE_OPTIONAL_HEADER64);

    enum {
        EXPORT_DIRECTORY_INDEX,
        IMPORT_DIRECTORY_INDEX,
        RESOURCE_DIRECTORY_INDEX,
        EXCEPTION_DIRECTORY_INDEX,
        SECURITY_DIRECTORY_INDEX,
        RELOCATION_DIRECTORY_INDEX,
        DEBUG_DIRECTORY_INDEX,
        ARCHITECTURE_DIRECTORY_INDEX,
        GLOBAL_PTR_DIRECTORY_INDEX,
        TLS_DIRECTORY_INDEX,
        LOAD_CONFIG_DIRECTORY_INDEX,
        BOUND_IMPORT_DIRECTORY_INDEX,
        IAT_DIRECTORY_INDEX,
        DELAY_IMPORT_DIRECTORY_INDEX,
        CLR_DIRECTORY_INDEX,
    };

    *optional_header = (IMAGE_OPTIONAL_HEADER64){
        .Magic = IMAGE_NT_OPTIONAL_HDR64_MAGIC,
        .SizeOfCode = 0x200,             // TODO: should be the same as the size (misc) of the .text section
        .SizeOfInitializedData = 0x200,  // TODO: calculate based on the amount of global data
        .AddressOfEntryPoint = 0x2000,   // TODO: resolve to the entry point in the machine code
        .BaseOfCode = 0x2000,            // TODO: resolve to the right section containing code
        .ImageBase = 0,                  // NOTE: Does not matter as we are using dynamic base
        .SectionAlignment = 0x1000,
        .FileAlignment = fileAlignment,
        .MajorOperatingSystemVersion = 6,
        .MinorOperatingSystemVersion = 0,
        .MajorSubsystemVersion = 6,
        .MinorSubsystemVersion = 0,
        .SizeOfImage = 0x3000,            // FIXME: calculate based on the sizes of the sections
        .SizeOfHeaders = 0,
        .Subsystem = IMAGE_SUBSYSTEM_WINDOWS_CUI, // TODO: allow user to specify this
        .DllCharacteristics =
            IMAGE_DLLCHARACTERISTICS_HIGH_ENTROPY_VA |
            IMAGE_DLLCHARACTERISTICS_NX_COMPAT | // TODO: figure out what NX is
            IMAGE_DLLCHARACTERISTICS_DYNAMIC_BASE |
            IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE,
        .SizeOfStackReserve = 0x100000,
        .SizeOfStackCommit = 0x1000,
        .SizeOfHeapReserve = 0x100000,
        .SizeOfHeapCommit = 0x1000,
        .NumberOfRvaAndSizes = IMAGE_NUMBEROF_DIRECTORY_ENTRIES,
        .DataDirectory = {0},
    };

    // .rdata section
    IMAGE_SECTION_HEADER *rdata_section_header = buffer_allocate(&exe_buffer, IMAGE_SECTION_HEADER);
    *rdata_section_header = (IMAGE_SECTION_HEADER){
        .Name = ".rdata",
        .Misc = 0x14C,            // FIXME: size of machine code in bytes
        .VirtualAddress = 0x1000, // FIXME: calculate this
        .SizeOfRawData = 0x200,   // FIXME: calculate this
        .PointerToRawData = 0,
        .Characteristics = IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ,
    };

    // .text section
    IMAGE_SECTION_HEADER *text_section_header = buffer_allocate(&exe_buffer, IMAGE_SECTION_HEADER);
    *text_section_header = (IMAGE_SECTION_HEADER){
        .Name = ".text",
        .Misc = 0x10, // FIXME: size of machine code in bytes
        .VirtualAddress = optional_header->BaseOfCode,
        .SizeOfRawData = optional_header->SizeOfCode,
        .PointerToRawData = 0,
        .Characteristics = IMAGE_SCN_CNT_CODE | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_EXECUTE,
    };

    // NULL header telling that the list is done
    *buffer_allocate(&exe_buffer, IMAGE_SECTION_HEADER) = (IMAGE_SECTION_HEADER){0};

    optional_header->SizeOfHeaders = align((s32)exe_buffer.size, optional_header->FileAlignment);
    exe_buffer.size = optional_header->SizeOfHeaders;

    IMAGE_SECTION_HEADER *sections[] = {
        rdata_section_header,
        text_section_header,
    };
    s32 section_offset = optional_header->SizeOfHeaders;
    for (u32 i = 0; i < ARRAY_SIZE(sections); ++i) {
        sections[i]->PointerToRawData = section_offset;
        section_offset += sections[i]->SizeOfRawData;
    }

#define INVALID_ADDRESS 0xCAFEBABE
#define file_offset_to_rva(_section_header_) \
    ((s32)exe_buffer.size -                  \
     (_section_header_)->PointerToRawData +  \
     (_section_header_)->VirtualAddress)

    // .rdata segment

    // IAT
    exe_buffer.size = rdata_section_header->PointerToRawData;

    for (u64 i = 0; i < libsCount; ++i) {
        Import_Library *lib = &libs[i];
        for (s32 i = 0; i < lib->function_count; ++i) {
            lib->functions[i].name_rva = file_offset_to_rva(rdata_section_header);
            buffer_append_s16(&exe_buffer, 0); // Ordinal Hint, value not required
            size_t name_size = strlen(lib->functions[i].name) + 1;
            s32 aligned_name_size = align((s32)name_size, 2);
            memcpy(
                buffer_allocate_size(&exe_buffer, aligned_name_size),
                lib->functions[i].name,
                name_size
            );
        }
    }

    optional_header->DataDirectory[IAT_DIRECTORY_INDEX].VirtualAddress = file_offset_to_rva(rdata_section_header);
    
    for (u64 i = 0; i < libsCount; ++i) {
        Import_Library *lib = &libs[i];
        lib->dll.iat_rva = file_offset_to_rva(rdata_section_header);

        for (s32 i = 0; i < lib->function_count; ++i) {
            Import_Name_To_Rva* fn = &lib->functions[i];
            fn->iat_rva = file_offset_to_rva(rdata_section_header);
            buffer_append_u64(&exe_buffer, fn->name_rva);
        }
        // End of IAT list
        buffer_append_u64(&exe_buffer, 0);
    }
    optional_header->DataDirectory[IAT_DIRECTORY_INDEX].Size =
        (s32)(exe_buffer.size - rdata_section_header->PointerToRawData);

    // Image thunks
    for (u64 i = 0; i < libsCount; ++i) {
        Import_Library *lib = &libs[i];
        lib->image_thunk_rva = file_offset_to_rva(rdata_section_header);

        for (s32 i = 0; i < lib->function_count; ++i) {
            buffer_append_u64(&exe_buffer, lib->functions[i].name_rva);
        }
        // End of IAT list
        buffer_append_u64(&exe_buffer, 0);
    }

    buffer_append_s64(&exe_buffer, 0);

    // Library Names

    for (u64 i = 0; i < libsCount; ++i) {
        Import_Library *lib = &libs[i];
        lib->dll.name_rva = file_offset_to_rva(rdata_section_header);
        size_t name_size = strlen(lib->dll.name) + 1;
        s32 aligned_name_size = align((s32)name_size, 2);
        memcpy(
            buffer_allocate_size(&exe_buffer, aligned_name_size),
            lib->dll.name,
            name_size
        );
    }

    // Import Directory
    s32 import_directory_rva = file_offset_to_rva(rdata_section_header);
    optional_header->DataDirectory[IMPORT_DIRECTORY_INDEX].VirtualAddress = import_directory_rva;

    for (u64 i = 0; i < libsCount; ++i) {
        Import_Library *lib = &libs[i];

        IMAGE_IMPORT_DESCRIPTOR *image_import_descriptor =
            buffer_allocate(&exe_buffer, IMAGE_IMPORT_DESCRIPTOR);
        *image_import_descriptor = (IMAGE_IMPORT_DESCRIPTOR){
            .OriginalFirstThunk = lib->image_thunk_rva,
            .Name = lib->dll.name_rva,
            .FirstThunk = lib->dll.iat_rva,
        };
    }

    optional_header->DataDirectory[IMPORT_DIRECTORY_INDEX].Size =
        file_offset_to_rva(rdata_section_header) - import_directory_rva;

    // End of IMAGE_IMPORT_DESCRIPTOR list
    *buffer_allocate(&exe_buffer, IMAGE_IMPORT_DESCRIPTOR) = (IMAGE_IMPORT_DESCRIPTOR){0};

    exe_buffer.size =
        rdata_section_header->PointerToRawData + rdata_section_header->SizeOfRawData;

    // .text segment
    exe_buffer.size = text_section_header->PointerToRawData;

    u32 begingIndex = exe_buffer.size;
    u8* beginnigOfCode = buffer_allocate_size(&exe_buffer, code.size);
    memcpy(beginnigOfCode, code.mem, code.size);

    // patch the funcion call locations
    for(int i = 0; i < names.size/sizeof(NamesToPatch); ++i) {
        NamesToPatch* castNames = (NamesToPatch*)names.mem;
        u8* addrLoc = &beginnigOfCode[castNames[i].offset];
        u32 nextInstructonAddr = (begingIndex + castNames[i].offset + 4) - text_section_header->PointerToRawData + text_section_header->VirtualAddress;
        u32 funcRVA = getFunctionRVA(libs, libsCount, castNames[i].name, castNames[i].len);
        
        addrLoc[0] = ((funcRVA - nextInstructonAddr) >> (8 * 0)) & 0xff;
        addrLoc[1] = ((funcRVA - nextInstructonAddr) >> (8 * 1)) & 0xff;
        addrLoc[2] = ((funcRVA - nextInstructonAddr) >> (8 * 2)) & 0xff;
        addrLoc[3] = ((funcRVA - nextInstructonAddr) >> (8 * 3)) & 0xff;
    }
    
    exe_buffer.size = text_section_header->PointerToRawData + text_section_header->SizeOfRawData;

    /////////
    return exe_buffer;
}

#ifndef LIB_ONLY
int main(void) {
    Import_Name_To_Rva kernel32_functions[] = {
        {.name = "ExitProcess", .name_rva = INVALID_ADDRESS, .iat_rva = INVALID_ADDRESS},

        {.name = "GetStdHandle", .name_rva = INVALID_ADDRESS, .iat_rva = INVALID_ADDRESS},
        {.name = "ReadConsoleA", .name_rva = INVALID_ADDRESS, .iat_rva = INVALID_ADDRESS},
    };

    Import_Name_To_Rva user32_functions[] = {
        {.name = "ShowWindow", .name_rva = INVALID_ADDRESS, .iat_rva = INVALID_ADDRESS},
    };

    Import_Library import_libraries[] = {
        {
            .dll = {.name = "kernel32.dll", .name_rva = INVALID_ADDRESS, .iat_rva = INVALID_ADDRESS},
            .image_thunk_rva = INVALID_ADDRESS,
            .functions = kernel32_functions,
            .function_count = ARRAY_SIZE(kernel32_functions),
        },
        {
            .dll = {.name = "user32.dll", .name_rva = INVALID_ADDRESS, .iat_rva = INVALID_ADDRESS},
            .image_thunk_rva = INVALID_ADDRESS,
            .functions = user32_functions,
            .function_count = ARRAY_SIZE(user32_functions),
        },
    };

    Buffer code = make_buffer(1024, PAGE_READWRITE);
    Buffer names = make_buffer(1024, PAGE_READWRITE);

    buffer_append_s8(&code, 0x48); // sub rsp 28
    buffer_append_s8(&code, 0x83);
    buffer_append_s8(&code, 0xEC);
    buffer_append_s8(&code, 0x28);

    buffer_append_s8(&code, 0xB9); // mov rcx, 42
    buffer_append_s8(&code, 0x2A);
    buffer_append_s8(&code, 0x00);
    buffer_append_s8(&code, 0x00);
    buffer_append_s8(&code, 0x00);

    buffer_append_s8(&code, 0xFF); // call ExitProcess
    buffer_append_s8(&code, 0x15);
    NamesToPatch* foo = buffer_allocate(&names, NamesToPatch);
    foo->name = "ExitProcess";
    foo->len = strlen("ExitProcess");
    foo->offset = code.size;
    buffer_append_s32(&code, 0);

    buffer_append_s8(&code, 0xCC); // int3

    Buffer outBuff = write_executable(import_libraries, ARRAY_SIZE(import_libraries), code, names);
    HANDLE file = CreateFile(
        "testExecutable.exe",  // name of the write
        GENERIC_WRITE,         // open for writing
        0,                     // do not share
        0,                     // default security
        CREATE_ALWAYS,         // create new file only
        FILE_ATTRIBUTE_NORMAL, // normal file
        0                      // no attr. template
    );
    assert(file != INVALID_HANDLE_VALUE);

    DWORD bytesWritten;
    WriteFile(
        file,                   // open file handle
        outBuff.mem,          // start of data to write
        (DWORD)outBuff.size,  // number of bytes to write
        &bytesWritten,                   // number of bytes that were written
        NULL
    );

    CloseHandle(file);
    return 0;
}
#endif // LIB_ONLY