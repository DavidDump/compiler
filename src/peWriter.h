#ifndef COMP_PE_WRITER_H
#define COMP_PE_WRITER_H

#include "common.h"
#include "buffer.h"
#include "hashmap.h"

// NOTE: all the information about the PE32 format is from the MSDN documentation page
// https://learn.microsoft.com/en-us/windows/win32/debug/pe-format

// NOTE: all parameters of the PE32 file, should be configurable
#define PE32_FILE_ALIGNMENT 0x200
#define PE32_SECTION_ALIGNMENT 0x1000
#define PE32_MIN_WIN_VERSION 6

typedef struct ImportNameToRva {
    String name;
    u32 nameRva;
    u32 iatRva;
} ImportNameToRva;

typedef struct ImportLibrary {
    ImportNameToRva dll;
    ImportNameToRva *functions;
    s32 functionCount; // TODO: why is this signed?
    u32 imageThunkRva;
} ImportLibrary;

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

typedef struct ParsedDataSection {
    Buffer buffer;
    s32 iatRva;
    s32 iatSize;
    s32 importDirectoryRva;
    s32 importDirectorySize;
} ParsedDataSection;

Buffer genExecutable(ImportLibrary* libs, u64 libsCount, Buffer bytecode, Buffer names, HashmapData* userData, Buffer dataToPatch);

#endif // COMP_PE_WRITER_H