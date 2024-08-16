#include "common.h"

#include <assert.h>
#include <windows.h>
#include <winnt.h>
#include <stdbool.h>
#include <math.h>
#include <time.h>

#define PE32_FILE_ALIGNMENT 0x200
#define PE32_SECTION_ALIGNMENT 0x1000
#define PE32_MIN_WIN_VERSION 6

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

u64 align_u64(u64 number, u64 alignment) {
  return (u64)(ceil((double)number / alignment) * alignment);
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

u32 getFunctionRVA(Import_Library* libs, u64 libsCount, u8* name, u64 nameLen) {
    // TODO: instead of iterating all the import use a hashmap
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

typedef struct ParsedDataSection {
    Buffer buffer;
    s32 iat_rva;
    s32 iat_size;
    s32 import_directory_rva;
    s32 import_directory_size;
} ParsedDataSection;

ParsedDataSection parseDataSection(Import_Library* libs, u64 libsCount, IMAGE_SECTION_HEADER *header) {
    #define get_rva() (s32)(header->VirtualAddress + buffer->size)

    // NOTE: is it better to precalculate this or just use a dynamic array?
    u64 expected_encoded_size = 0;
    for (s64 i = 0; i < libsCount; ++i) {
        Import_Library *lib = &libs[i];
        // Aligned to 2 bytes c string of library name
        expected_encoded_size += align((s32)strlen(lib->dll.name) + 1, 2);
        for (s32 i = 0; i < lib->function_count; ++i) {
            Import_Name_To_Rva *fn = &lib->functions[i];

            // Ordinal Hint, value not required
            expected_encoded_size += sizeof(s16);
            // Aligned to 2 bytes c string of symbol name
            expected_encoded_size += align((s32)strlen(fn->name) + 1, 2);
            // IAT placeholder for symbol pointer
            expected_encoded_size += sizeof(u64);
            // Image Thunk
            expected_encoded_size += sizeof(u64);
        }
        // NOTE: in the extimation code this was done once per imported function,
        // but in the actuall emmiter we only add one descriptor per imported library, it still works
        // Import Directory
        expected_encoded_size += sizeof(IMAGE_IMPORT_DESCRIPTOR);

        // IAT zero-termination
        expected_encoded_size += sizeof(u64);
        // Import Directory zero-termination
        expected_encoded_size += sizeof(u64);
        // Image Thunk zero-termination
        expected_encoded_size += sizeof(IMAGE_IMPORT_DESCRIPTOR);
    }

    ParsedDataSection result = {
        // FIXME dynamically resize the buffer
        .buffer = make_buffer(expected_encoded_size, PAGE_READWRITE),
    };

    Buffer* buffer = &result.buffer;
    // Function names
    for (s64 i = 0; i < libsCount; ++i) {
        Import_Library *lib = &libs[i];
        for (s32 i = 0; i < lib->function_count; ++i) {
            Import_Name_To_Rva *function = &lib->functions[i];
            function->name_rva = get_rva();
            buffer_append_s16(buffer, 0); // Ordinal Hint, value not required
            size_t name_size = strlen(function->name) + 1;
            s32 aligned_name_size = align((s32)name_size, 2);
            memcpy(
                buffer_allocate_size(buffer, aligned_name_size),
                function->name,
                name_size
            );
        }
    }

    // IAT
    result.iat_rva = get_rva();
    for (s64 i = 0; i < libsCount; ++i) {
        Import_Library *lib = &libs[i];
        lib->dll.iat_rva = get_rva();
        for (s32 i = 0; i < lib->function_count; ++i) {
            Import_Name_To_Rva *fn = &lib->functions[i];
            fn->iat_rva = get_rva();
            buffer_append_u64(buffer, fn->name_rva);
        }
        // End of IAT list
        buffer_append_u64(buffer, 0);
    }
    result.iat_size = (s32)buffer->size;

    // Image Thunks
    for (s64 i = 0; i < libsCount; ++i) {
        Import_Library *lib = &libs[i];
        lib->image_thunk_rva = get_rva();
        for (s32 i = 0; i < lib->function_count; ++i) {
            Import_Name_To_Rva *fn = &lib->functions[i];
            buffer_append_u64(buffer, fn->name_rva);
        }
        // End of IAT list
        buffer_append_u64(buffer, 0);
    }

    // Library Names
    for (s64 i = 0; i < libsCount; ++i) {
        Import_Library *lib = &libs[i];
        lib->dll.name_rva = get_rva();
        size_t name_size = strlen(lib->dll.name) + 1;
        s32 aligned_name_size = align((s32)name_size, 2);
        memcpy(
            buffer_allocate_size(buffer, aligned_name_size),
            lib->dll.name,
            name_size
        );
    }

    // Import Directory
    result.import_directory_rva = get_rva();
    for (s64 i = 0; i < libsCount; ++i) {
        Import_Library *lib = &libs[i];

        IMAGE_IMPORT_DESCRIPTOR *image_import_descriptor = buffer_allocate(buffer, IMAGE_IMPORT_DESCRIPTOR);
        *image_import_descriptor = (IMAGE_IMPORT_DESCRIPTOR){
            .OriginalFirstThunk = lib->image_thunk_rva,
            .Name = lib->dll.name_rva,
            .FirstThunk = lib->dll.iat_rva,
        };
    }
    result.import_directory_size = get_rva() - result.import_directory_rva;
    *buffer_allocate(buffer, IMAGE_IMPORT_DESCRIPTOR) = (IMAGE_IMPORT_DESCRIPTOR) {0};

    assert(buffer->size == expected_encoded_size);
    assert(buffer->size < MAXINT32);

    header->Misc.VirtualSize = (s32)buffer->size;
    header->SizeOfRawData = (s32)align_u64(buffer->size, PE32_FILE_ALIGNMENT);

    #undef get_rva
    return result;
}

Buffer write_executable(Import_Library* libs, u64 libsCount, Buffer code, Buffer names) {
    // Sections
    IMAGE_SECTION_HEADER sections[] = {
        {
            .Name = ".rdata",
            .Misc = {0},
            .VirtualAddress = 0,
            .SizeOfRawData = 0,
            .PointerToRawData = 0,
            .Characteristics = IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ,
        },
        {
            .Name = ".text",
            .Misc = {0},
            .VirtualAddress = 0,
            .SizeOfRawData = 0,
            .PointerToRawData = 0,
            .Characteristics = IMAGE_SCN_CNT_CODE | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_EXECUTE,
        },
        {0}
    };

    IMAGE_SECTION_HEADER *rdata_section_header = &sections[0];
    IMAGE_SECTION_HEADER *text_section_header  = &sections[1];

    s32 file_size_of_headers =
        sizeof(IMAGE_DOS_HEADER) +
        sizeof(s32) + // IMAGE_NT_SIGNATURE
        sizeof(IMAGE_FILE_HEADER) +
        sizeof(IMAGE_OPTIONAL_HEADER64) +
        sizeof(sections);
    file_size_of_headers = align(file_size_of_headers, PE32_FILE_ALIGNMENT);
    
    // prepare .rdata
    s32 virtual_size_of_headers = align(file_size_of_headers, PE32_SECTION_ALIGNMENT);
    rdata_section_header->PointerToRawData = file_size_of_headers;
    rdata_section_header->VirtualAddress = virtual_size_of_headers;
    ParsedDataSection rdata_section = parseDataSection(libs, libsCount, rdata_section_header);

    // prepare .text
    u32 codeSizeAligned = align(code.size, PE32_FILE_ALIGNMENT);
    text_section_header->PointerToRawData = rdata_section_header->PointerToRawData + rdata_section_header->SizeOfRawData;
    text_section_header->VirtualAddress = rdata_section_header->VirtualAddress + align(rdata_section_header->SizeOfRawData, PE32_SECTION_ALIGNMENT);
    text_section_header->Misc.VirtualSize = code.size;
    text_section_header->SizeOfRawData = codeSizeAligned;

    // total size of image
    s32 virtualSizeOfImage = text_section_header->VirtualAddress + align(text_section_header->SizeOfRawData, PE32_SECTION_ALIGNMENT);
    u64 max_exe_buffer = file_size_of_headers + rdata_section_header->SizeOfRawData + text_section_header->SizeOfRawData;
    Buffer exe_buffer = make_buffer(max_exe_buffer, PAGE_READWRITE);

    IMAGE_DOS_HEADER *dos_header = buffer_allocate(&exe_buffer, IMAGE_DOS_HEADER);
    *dos_header = (IMAGE_DOS_HEADER){
        .e_magic = IMAGE_DOS_SIGNATURE,
        .e_lfanew = sizeof(IMAGE_DOS_HEADER),
    };

    buffer_append_s32(&exe_buffer, IMAGE_NT_SIGNATURE);
    IMAGE_FILE_HEADER *file_header = buffer_allocate(&exe_buffer, IMAGE_FILE_HEADER);
    *file_header = (IMAGE_FILE_HEADER){
        .Machine = IMAGE_FILE_MACHINE_AMD64,
        .NumberOfSections = ARRAY_SIZE(sections) - 1,
        .TimeDateStamp = time(NULL),
        .SizeOfOptionalHeader = sizeof(IMAGE_OPTIONAL_HEADER64),
        .Characteristics = IMAGE_FILE_EXECUTABLE_IMAGE | IMAGE_FILE_LARGE_ADDRESS_AWARE,
    };

    IMAGE_OPTIONAL_HEADER64 *optional_header = buffer_allocate(&exe_buffer, IMAGE_OPTIONAL_HEADER64);
    *optional_header = (IMAGE_OPTIONAL_HEADER64) {
        .Magic = IMAGE_NT_OPTIONAL_HDR64_MAGIC,
        .SizeOfCode = text_section_header->SizeOfRawData,
        .SizeOfInitializedData = rdata_section_header->SizeOfRawData,
        .AddressOfEntryPoint = 0,
        .BaseOfCode = text_section_header->VirtualAddress,
        .ImageBase = 0,                  // NOTE: Does not matter as we are using dynamic base
        .SectionAlignment = PE32_SECTION_ALIGNMENT,
        .FileAlignment = PE32_FILE_ALIGNMENT,
        .MajorOperatingSystemVersion = PE32_MIN_WIN_VERSION,
        .MinorOperatingSystemVersion = 0,
        .MajorSubsystemVersion = PE32_MIN_WIN_VERSION,
        .MinorSubsystemVersion = 0,
        .SizeOfImage = virtualSizeOfImage,
        .SizeOfHeaders = file_size_of_headers,
        .Subsystem = IMAGE_SUBSYSTEM_WINDOWS_CUI, // TODO: allow user to specify this
        .DllCharacteristics =
            IMAGE_DLLCHARACTERISTICS_HIGH_ENTROPY_VA |
            IMAGE_DLLCHARACTERISTICS_NX_COMPAT |
            IMAGE_DLLCHARACTERISTICS_DYNAMIC_BASE |
            IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE,
        .SizeOfStackReserve = 0x100000,
        .SizeOfStackCommit = 0x1000,
        .SizeOfHeapReserve = 0x100000,
        .SizeOfHeapCommit = 0x1000,
        .NumberOfRvaAndSizes = IMAGE_NUMBEROF_DIRECTORY_ENTRIES,
        .DataDirectory = {0},
    };
    optional_header->DataDirectory[IAT_DIRECTORY_INDEX].VirtualAddress    = rdata_section.iat_rva;
    optional_header->DataDirectory[IAT_DIRECTORY_INDEX].Size              = rdata_section.iat_size;
    optional_header->DataDirectory[IMPORT_DIRECTORY_INDEX].VirtualAddress = rdata_section.import_directory_rva;
    optional_header->DataDirectory[IMPORT_DIRECTORY_INDEX].Size           = rdata_section.import_directory_size;

#define INVALID_ADDRESS 0xCAFEBABE

    // write IMAGE_SECTION_HEADERs
    for(u32 i = 0; i < ARRAY_SIZE(sections); ++i) {
        *buffer_allocate(&exe_buffer, IMAGE_SECTION_HEADER) = sections[i];
    }
    
    // .rdata section
    exe_buffer.size = rdata_section_header->PointerToRawData;
    u8* mem = buffer_allocate_size(&exe_buffer, rdata_section.buffer.size);
    memcpy(mem, rdata_section.buffer.mem, rdata_section.buffer.size);
    exe_buffer.size = rdata_section_header->PointerToRawData + rdata_section_header->SizeOfRawData;

    // .text segment
    exe_buffer.size = text_section_header->PointerToRawData;

    u32 begingIndex = exe_buffer.size;
    u8* beginnigOfCode = buffer_allocate_size(&exe_buffer, codeSizeAligned);
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
    
    // patch entry point
    optional_header->AddressOfEntryPoint = text_section_header->VirtualAddress;

    exe_buffer.size = text_section_header->PointerToRawData + text_section_header->SizeOfRawData;

    /////////
    // TODO: free buffers
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