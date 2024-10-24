#ifndef COMP_BYTECODE_H
#define COMP_BYTECODE_H

#include "common.h"
#include "arena.h"
#include "buffer.h"
#include "instructions_x86.h"
#include "hashmap.h"
#include "parser.h"

// Register values used in the Mod/RM byte
typedef enum Register {
    RAX = 0,
    RCX = 1,
    RDX = 2,
    RBX = 3,
    RSP = 4,
    RBP = 5,
    RSI = 6,
    RDI = 7,
    R8  = 8,
    R9  = 9,
    R10 = 10,
    R11 = 11,
    R12 = 12,
    R13 = 13,
    R14 = 14,
    R15 = 15,
} Register;

typedef enum AddressingMode {
    INDIRECT_NO_DISPLACE    = 0, // [rax]          used when target address is stored in a register
    INDIRECT_08_DISPLACE    = 1, // [rax + 0x12]   used when target address is stored in a register and adding a single byte displacement
    INDIRECT_32_DISPLACE    = 2, // [rax + 0x1234] used when target address is stored in a register and adding a 4/8? byte displacement
    DIRECT                  = 3, // rax            used when addressing registers directly
} AddressingMode;

typedef enum Scale {
    X0 = 0,
    X2 = 1,
    X4 = 2,
    X8 = 3,
} Scale;

#include "bytecode_exe_common.h"

#define INVALID_ADDRESS 0xCAFEBABE
#define SAVED_STACK_SIZE 255
typedef struct GenContext {
    Arena mem;
    Buffer code;

    // TODO: this is not good, me not like, make better
    Buffer symbolsToPatch;   // AddrToPatch, external function call, variable or other symbol
    Buffer functionsToPatch; // AddrToPatch, internal function call
    Buffer dataToPatch;      // AddrToPatch, data defined in the .data section

    // u32 freeRegisterMask;
    s64 stackPointer;     // keeping track of the stack pointer for variables
    u64 entryPointOffset; // the offset from the begining of the code buffer to the entry point

    Hashmap variables; // value is the stack offset to the variable
    Hashmap functions; // value is the offset to the begining of the function from the begining of the code buffer
    HashmapData data;  // value is a UserDataEntry
} GenContext;

typedef enum OperandType {
    OPERAND_NONE,

    OPERAND_Register,          // rax
    OPERAND_AddrInReg,         // [rax]
    OPERAND_AddrInRegOffset8,  // [rax + 0x12]
    OPERAND_AddrInRegOffset32, // [rax + 0x1234]
    OPERAND_SIB,               // [rax + X0 * regB]
    OPERAND_SIBOffset8,        // [rax + X0 * regB + 0x12]
    OPERAND_SIBOffset32,       // [rax + X0 * regB + 0x1234]
    OPERAND_RIP,               // [RIP + 0x1234]
    OPERAND_AbsoluteAddr,      // [0x1234]
    OPERAND_Immediate8,        // 0x12
    OPERAND_Immediate32,       // 0x12345678

    // NOTE: dont know if these are valid
    OPERAND_Immediate16,
    OPERAND_Immediate64,
    
    OPERAND_COUNT,
} OperandType;

#pragma GCC diagnostic ignored "-Wunused-variable"
static char* OperandTypeStr[OPERAND_COUNT] = {
    [OPERAND_NONE]              = "NONE",
    [OPERAND_Register]          = "r",
    [OPERAND_AddrInReg]         = "r/m",
    [OPERAND_AddrInRegOffset8]  = "r/m",
    [OPERAND_AddrInRegOffset32] = "r/m",
    [OPERAND_SIB]               = "r/m",
    [OPERAND_SIBOffset8]        = "r/m",
    [OPERAND_SIBOffset32]       = "r/m",
    [OPERAND_RIP]               = "[RIP + addr]",
    [OPERAND_AbsoluteAddr]      = "[addr]",
    [OPERAND_Immediate8]        = "imm8",
    [OPERAND_Immediate32]       = "imm32",
};
#pragma GCC diagnostic pop

typedef struct Operand {
    OperandType type;
    Register reg;
    u32 displacement;
    Register base;
    Scale scale;
    Register index;
    u32 immediate;

    // NOTE: maybe having isReg and isIndirect can be conbined?
    bool isReg;
    bool isIndirect;
    bool isSIB;
    bool isImm;
    bool isRIPorAbs;
} Operand;

typedef struct Instruction {
    Mnemonic name;
    Operand ops[INSTRUCTION_MAX_OPERANDS];
} Instruction;

#define OP_REG(_register_) \
    (Operand){.type = OPERAND_Register, .reg = (_register_), .isReg = TRUE}
#define OP_INDIRECT(_register_) \
    (Operand){.type = OPERAND_AddrInReg, .reg = (_register_), .isIndirect = TRUE}
#define OP_INDIRECT_OFFSET8(_register_, _displacement_) \
    (Operand){.type = OPERAND_AddrInRegOffset8, .reg = (_register_), .displacement = (_displacement_), .isIndirect = TRUE}
#define OP_INDIRECT_OFFSET32(_register_, _displacement_) \
    (Operand){.type = OPERAND_AddrInRegOffset32, .reg = (_register_), .displacement = (_displacement_), .isIndirect = TRUE}
#define OP_INDIRECT_SIB(_base_, _scale_, _index_) \
    (Operand){.type = OPERAND_SIB, .base = (_base_), .scale = (_scale_), .index = (_index_), .isSIB = TRUE}
#define OP_INDIRECT_SIB_OFFSET8(_base_, _scale_, _index_, _displacement_) \
    (Operand){.type = OPERAND_SIBOffset8, .base = (_base_), .scale = (_scale_), .index = (_index_), .displacement = (_displacement_), .isSIB = TRUE}
#define OP_INDIRECT_SIB_OFFSET32(_base_, _scale_, _index_, _displacement_) \
    (Operand){.type = OPERAND_SIBOffset32, .base = (_base_), .scale = (_scale_), .index = (_index_), .displacement = (_displacement_), .isSIB = TRUE}
#define OP_RIP(_displacement_) \
    (Operand){.type = OPERAND_RIP, .displacement = (_displacement_), .isRIPorAbs = TRUE}
#define OP_ABSOLUTE(_displacement_) \
    (Operand){.type = OPERAND_AbsoluteAddr, .displacement = (_displacement_), .isRIPorAbs = TRUE}
#define OP_IMM8(_immediate_) \
    (Operand){.type = OPERAND_Immediate8, .immediate = (_immediate_), .isImm = TRUE}
#define OP_IMM32(_immediate_) \
    (Operand){.type = OPERAND_Immediate32, .immediate = (_immediate_), .isImm = TRUE}

#define INST(_mnemonic_, ...) (Instruction){.name = _mnemonic_##_, .ops = {__VA_ARGS__}}

void gen_x86_64_expression(GenContext* ctx, ASTNode* expr);
GenContext gen_x86_64_bytecode(Scope* globalScope);

#endif // COMP_BYTECODE_H

// TODO: make a OP_IMM macro that dispaches to a specific sized version based on the value provided
