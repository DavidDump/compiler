#ifndef COMP_BYTECODE_EXE_COMMON_H
#define COMP_BYTECODE_EXE_COMMON_H

// NOTE: the existance of this file is solely the reason of microsoft
//       one of the windows.h headers has a type named TokenType
//       if all the headers are included in the peWriter.c compilation unit
//       if shits out errors, because of this the peWriter.c compilation unit
//       need to be as small as possible, quarantined from the rest of the world

#include "common.h"
#include "string.h"
#include "dataStructures.h"

// symbols or functions imported from other libs, and data defined in the data section is referenced using RIP addressing,
// the address is only knowns once the text section and the data section have been written to the file,
// so they need to be patched later
typedef struct AddrToPatch {
    String name;
    u64 offset;
} AddrToPatch;

defArray(u8);
defArray(AddrToPatch);

#endif // COMP_BYTECODE_EXE_COMMON_H
