#ifndef COMP_INSTRUCTIONS_H
#define COMP_INSTRUCTIONS_H

#include "common.h"

typedef enum RexByteType {
    RexByte_NONE, // incidates that no REX byte shold be generated
    RexByte_W,    // inditaces that the REX byte generated should have the W bit set
    RexByte_NoW,  // indicated that the REX byte generated should not have the W bit set
} RexByteType;

typedef enum InstructionType {
    InstructionType_NONE,
    InstructionType_8BIT,
    InstructionType_16BIT,
    InstructionType_32BIT,
    InstructionType_64BIT,
} InstructionType;

typedef enum ModRMType {
    ModRMType_NONE,
    ModRMType_REG, // indicates that the ModR/M byte contains a register operand and a R/M operand
    ModRMType_EXT, // indicates that the ModR/M byte contains an opcode extension in the reg field
} ModRMType;

typedef enum OpType {
    OpType_NONE,
    OpType_REG,
    OpType_RM,
    OpType_IMM8,
    OpType_IMM16,
    OpType_IMM32,
    OpType_IMM64,
} OpType;

#define INSTRUCTION_MAX_OPERANDS 2
typedef struct InstructionEncoding {
    InstructionType type;
    RexByteType rexType;
    // opcodes can be up to 64bits?, most are only 8bits, generetor will not emit null bytes in case of only 8bit opcodes
    u64 opcode;
    bool regInOpcode; // the target register is stored in the lower 3bits of the opcode
    ModRMType modRMType;
    u8 opcodeExtension;
    OpType opTypes[INSTRUCTION_MAX_OPERANDS];

    // NOTE: could add mode fields to indicate what CPU mode this instruction is valid in,
    // would encode the 64-Bit Mode and Compat/Leg Mode columns,
    // but thats only useful if generating kernel level code, in userspace its not required
} InstructionEncoding;

typedef enum Mnemonic {
    push_,
    pop_,
    call_,
    ret_,
    mov_,
    add_,
    sub_,
    mul_,
    div_,
    cmp_,
    lea_,
    inc_,
    dec_,
    // jmp_,
    // NOTE: less and greater are used for signed comparison
    //       below and above are used for unsigned comparison
    je_,
    jne_,
    jl_,
    jg_,
    jle_,
    jge_,
    Mnemonic_COUNT,
} Mnemonic;

#pragma GCC diagnostic ignored "-Wunused-variable"
extern const char* MnemonicStr[Mnemonic_COUNT];

#define MAX_ENCODING_FOR_INSTRUCTION 255
extern InstructionEncoding encodings[][MAX_ENCODING_FOR_INSTRUCTION];
#pragma GCC diagnostic pop

#endif // COMP_INSTRUCTIONS_H
