#include "peWriter.h"
#include "bytecode_exe_common.h"

#include <windows.h>
#include <winnt.h>
#include <math.h>
#include <time.h>
#include <stdio.h>

#define true TRUE
#define false FALSE

s32 align(s32 number, s32 alignment) {
    return (s32)(ceil((double)number / alignment) * alignment);
}

u64 align_u64(u64 number, u64 alignment) {
    return (u64)(ceil((double)number / alignment) * alignment);
}

// TODO: test if this function stil works, slightly changed during hashmap refactor
u32 getFunctionRVA(Hashmap(String, LibName)* libs, String name) {
    HashmapFor(String, LibName, lib, libs) {
        FuncName result = {0};
        if(HashmapGet(String, FuncName)(lib->value.functions, name, &result)) {
            return result.iatRva;
        }
    }
    UNREACHABLE("function could not be found in imported libraries");
    return 0; // NOTE: just to silence a warning
}

u32 genDataRVA(Hashmap(String, UserDataEntry)* userData, String name) {
    UserDataEntry value;
    if(HashmapGet(String, UserDataEntry)(userData, name, &value)) {
        return value.dataRVA;
    }
    UNREACHABLE("a data entry could not be found");
    return 0; // NOTE: just to silence a warning
}

ParsedDataSection parseDataSection(Hashmap(String, LibName)* libs, Hashmap(String, UserDataEntry)* userData, IMAGE_SECTION_HEADER *header) {
    #define get_rva() (s32)(header->VirtualAddress + buffer->size)

    // NOTE: is it better to precalculate this or just use a dynamic array?
    u64 expected_encoded_size = 0;
    HashmapFor(String, LibName, lib, libs) {
        // Aligned to 2 bytes c string of library name
        expected_encoded_size += align((s32)lib->key.length + 1, 2);
        HashmapFor(String, FuncName, fn, lib->value.functions) {
            // Ordinal Hint, value not required
            expected_encoded_size += sizeof(s16);
            // Aligned to 2 bytes c string of symbol name
            expected_encoded_size += align((s32)fn->key.length + 1, 2);
            // IAT placeholder for symbol pointer
            expected_encoded_size += sizeof(u64);
            // Image Thunk
            expected_encoded_size += sizeof(u64);
        }
        // NOTE: in the example code this was done once per imported function,
        // but in the actual emiter we only add one descriptor per imported library, it still works
        // Import Directory
        expected_encoded_size += sizeof(IMAGE_IMPORT_DESCRIPTOR);

        // IAT zero-termination
        expected_encoded_size += sizeof(u64);
        // Import Directory zero-termination
        expected_encoded_size += sizeof(u64);
        // Image Thunk zero-termination
        expected_encoded_size += sizeof(IMAGE_IMPORT_DESCRIPTOR);
    }
    // Data section
    if(userData->size != 0) { // TODO: find out why the memory is corrupted
        for(KVPair(String, UserDataEntry)* pair = userData->first; pair != NULL; pair = pair->next) {
            expected_encoded_size += sizeof(pair->value.dataLen);
            expected_encoded_size += pair->value.dataLen;
        }
    }

    ParsedDataSection result = {
        // FIXME dynamically resize the buffer
        .buffer = make_buffer(expected_encoded_size, PAGE_READWRITE),
    };

    Buffer* buffer = &result.buffer;
    // Function names
    HashmapFor(String, LibName, lib, libs) {
        HashmapFor(String, FuncName, fn, lib->value.functions) {
            fn->value.nameRva = get_rva();
            buffer_append_s16(buffer, 0); // Ordinal Hint, value not required
            size_t name_size = fn->key.length + 1;
            s32 aligned_name_size = align((s32)name_size, 2);
            u8* buf = buffer_allocate_size(buffer, aligned_name_size);
            memcpy(buf, fn->key.str, name_size);
            buf[name_size - 1] = 0; // TODO: i dont think the +1 for len is nessecary, but test before change
        }
    }

    // IAT
    result.iatRva = get_rva();
    HashmapFor(String, LibName, lib, libs) {
        lib->value.iatRva = get_rva();
        HashmapFor(String, FuncName, fn, lib->value.functions) {
            fn->value.iatRva = get_rva();
            buffer_append_u64(buffer, fn->value.nameRva);
        }
        // End of IAT list
        buffer_append_u64(buffer, 0);
    }
    result.iatSize = (s32)buffer->size;

    // Image Thunks
    HashmapFor(String, LibName, lib, libs) {
        lib->value.imageThunkRva = get_rva();
        HashmapFor(String, FuncName, fn, lib->value.functions) {
            buffer_append_u64(buffer, fn->value.nameRva);
        }
        // End of IAT list
        buffer_append_u64(buffer, 0);
    }

    // Library Names
    HashmapFor(String, LibName, lib, libs) {
        lib->value.nameRva= get_rva();
        size_t name_size = lib->key.length + 1;
        s32 aligned_name_size = align((s32)name_size, 2);
        u8* buf = buffer_allocate_size(buffer, aligned_name_size);
        memcpy(buf, lib->key.str, name_size);
        buf[name_size - 1] = 0; // TODO: i dont think the +1 for len is nessecary, but test before change
    }

    // Import Directory
    result.importDirectoryRva = get_rva();
    HashmapFor(String, LibName, lib, libs) {
        IMAGE_IMPORT_DESCRIPTOR *image_import_descriptor = buffer_allocate(buffer, IMAGE_IMPORT_DESCRIPTOR);
        *image_import_descriptor = (IMAGE_IMPORT_DESCRIPTOR){
            .OriginalFirstThunk = lib->value.imageThunkRva,
            .Name = lib->value.nameRva,
            .FirstThunk = lib->value.iatRva,
        };
    }
    result.importDirectorySize = get_rva() - result.importDirectoryRva;
    if(libs->size > 0) *buffer_allocate(buffer, IMAGE_IMPORT_DESCRIPTOR) = (IMAGE_IMPORT_DESCRIPTOR) {0};

    // Data Section
    if(userData->size != 0) { // TODO: find out why the memory is corrupted
        HashmapFor(String, UserDataEntry, data, userData) {
            data->value.dataRVA = get_rva();
            buffer_append_u64(buffer, data->value.dataLen);
            u8* bufferMem = buffer_allocate_size(buffer, data->value.dataLen);
            memcpy(bufferMem, data->value.data, data->value.dataLen);
        }
    }
    
    assert(buffer->size == expected_encoded_size, "");
    assert(buffer->size < UINT32_MAX, "");

    header->Misc.VirtualSize = (s32)buffer->size;
    header->SizeOfRawData = (s32)align_u64(buffer->size, PE32_FILE_ALIGNMENT);

    #undef get_rva
    return result;
}

// names is the locations where imported functions were used, used for patching
// dataToPatch is the locations where user defined data was refferenced, used for patching
Buffer genExecutable(Hashmap(String, LibName)* libs, Buffer bytecode, Buffer names, Hashmap(String, UserDataEntry)* userData, Buffer dataToPatch, Hashmap(String, s64)* funcCalls, Buffer funcsToPatch, u64 entryPointOffset) {
    // Sections
    IMAGE_SECTION_HEADER sections[3] = {
        [0] = {
            .Name = ".rdata",
            .Misc = {0},
            .VirtualAddress = 0,
            .SizeOfRawData = 0,
            .PointerToRawData = 0,
            .Characteristics = IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ,
        },
        [1] = {
            .Name = ".text",
            .Misc = {0},
            .VirtualAddress = 0,
            .SizeOfRawData = 0,
            .PointerToRawData = 0,
            .Characteristics = IMAGE_SCN_CNT_CODE | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_EXECUTE,
        },
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
    ParsedDataSection rdata_section = parseDataSection(libs, userData, rdata_section_header);

    // prepare .text
    u32 codeSizeAligned = align(bytecode.size, PE32_FILE_ALIGNMENT);
    text_section_header->PointerToRawData = rdata_section_header->PointerToRawData + rdata_section_header->SizeOfRawData;
    text_section_header->VirtualAddress = rdata_section_header->VirtualAddress + align(rdata_section_header->SizeOfRawData, PE32_SECTION_ALIGNMENT);
    text_section_header->Misc.VirtualSize = bytecode.size;
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
        .ImageBase = 0, // NOTE: Does not matter as we are using dynamic base
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
        .DataDirectory = {{0}},
    };
    optional_header->DataDirectory[IAT_DIRECTORY_INDEX].VirtualAddress    = rdata_section.iatRva;
    optional_header->DataDirectory[IAT_DIRECTORY_INDEX].Size              = rdata_section.iatSize;
    optional_header->DataDirectory[IMPORT_DIRECTORY_INDEX].VirtualAddress = rdata_section.importDirectoryRva;
    optional_header->DataDirectory[IMPORT_DIRECTORY_INDEX].Size           = rdata_section.importDirectorySize;

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

    u32 beginingIndex = exe_buffer.size;
    u8* beginingOfCode = buffer_allocate_size(&exe_buffer, codeSizeAligned);
    memcpy(beginingOfCode, bytecode.mem, bytecode.size);

    // patch the external funcion call locations
    for(u64 i = 0; i < names.size/sizeof(AddrToPatch); ++i) {
        AddrToPatch* castNames = (AddrToPatch*)names.mem;
        u8* addrLoc = &beginingOfCode[castNames[i].offset];
        u32 nextInstructonAddr = (beginingIndex + castNames[i].offset + 4) - text_section_header->PointerToRawData + text_section_header->VirtualAddress;
        u32 funcRVA = getFunctionRVA(libs, castNames[i].name);
        
        addrLoc[0] = ((funcRVA - nextInstructonAddr) >> (8 * 0)) & 0xff;
        addrLoc[1] = ((funcRVA - nextInstructonAddr) >> (8 * 1)) & 0xff;
        addrLoc[2] = ((funcRVA - nextInstructonAddr) >> (8 * 2)) & 0xff;
        addrLoc[3] = ((funcRVA - nextInstructonAddr) >> (8 * 3)) & 0xff;
    }
    
    // patch the internal function call locations
    for(u64 i = 0; i < funcsToPatch.size/sizeof(AddrToPatch); ++i) {
        AddrToPatch* castNames = (AddrToPatch*)funcsToPatch.mem;
        u64 offset = castNames[i].offset;
        String name = castNames[i].name;
        u8* addrLoc = &beginingOfCode[offset];
        u32 nextInstructonAddr = (beginingIndex + offset + 4) - text_section_header->PointerToRawData + text_section_header->VirtualAddress;
        s64 addr = 0;
        if(!HashmapGet(String, s64)(funcCalls, name, &addr)) {
            assertf(FALSE, "[UNREACHABLE] Failed to get the location of a function during pe writing: "STR_FMT"\n", STR_PRINT(name));
        }
        u32 funcRVA = (beginingIndex + addr) - text_section_header->PointerToRawData + text_section_header->VirtualAddress;

        addrLoc[0] = ((funcRVA - nextInstructonAddr) >> (8 * 0)) & 0xff;
        addrLoc[1] = ((funcRVA - nextInstructonAddr) >> (8 * 1)) & 0xff;
        addrLoc[2] = ((funcRVA - nextInstructonAddr) >> (8 * 2)) & 0xff;
        addrLoc[3] = ((funcRVA - nextInstructonAddr) >> (8 * 3)) & 0xff;
    }

    // patch the data reference locations
    for(u64 i = 0; i < dataToPatch.size/sizeof(AddrToPatch); ++i) {
        AddrToPatch* castData = (AddrToPatch*)dataToPatch.mem;
        u64 offset = castData[i].offset;
        String name = castData[i].name;
        
        u8* addrLoc = &beginingOfCode[offset];
        u32 nextInstructonAddr = (beginingIndex + offset + 4) - text_section_header->PointerToRawData + text_section_header->VirtualAddress;
        u32 dataRVA = genDataRVA(userData, name);

        *(u32*)addrLoc += dataRVA - nextInstructonAddr;
    }

    // patch entry point
    optional_header->AddressOfEntryPoint = text_section_header->VirtualAddress + entryPointOffset;

    exe_buffer.size = text_section_header->PointerToRawData + text_section_header->SizeOfRawData;

    return exe_buffer;
    // TODO: free buffers
}
