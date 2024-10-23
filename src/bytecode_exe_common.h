#ifndef COMP_BYTECODE_EXE_COMMON_H
#define COMP_BYTECODE_EXE_COMMON_H

#include "common.h"
#include "string.h"

// symbols or functions imported from other libs, and data defined in the data section is referenced using RIP addressing,
// the address is only knowns once the text section and the data section have been written to the file,
// so they need to be patched later
typedef struct AddrToPatch {
    String name;
    u64 offset;
} AddrToPatch;

#endif // COMP_BYTECODE_EXE_COMMON_H