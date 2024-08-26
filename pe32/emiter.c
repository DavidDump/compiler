// Code generator test for x86_64 intel bytecode

#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <stdlib.h>
#include <windows.h>

// #define ARENA_IMPLEMENTATION
// #include "../src/arena.h"

#include "common.h"
#include "instructions.c"

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

#define KiB(x)    ((x) * 1024)
#define MiB(x) (KiB(x) * 1024)
#define GiB(x) (MiB(x) * 1024)

#define KB(x)   ((x) * 1000)
#define MB(x) (KB(x) * 1000)
#define GB(x) (MB(x) * 1000)

#define LIB_ONLY 1
#include "write.c"

typedef struct EmiterContext {
    Buffer code;
    Buffer symbolsToPatch; // AddrToPatch
    Buffer dataToPatch;    // AddrToPatch

    u32 freeRegisterMask;
} EmiterContext;

void Emit8(EmiterContext* ctx, u8 data) {
    buffer_append_u8(&ctx->code, data);
}

void Emit16(EmiterContext* ctx, u16 data) {
    Emit8(ctx, (data >> 0) & 0xFF);
    Emit8(ctx, (data >> 8) & 0xFF);
}

void Emit32(EmiterContext* ctx, u32 data) {
    Emit8(ctx, (data >> 0)  & 0xFF);
    Emit8(ctx, (data >> 8)  & 0xFF);
    Emit8(ctx, (data >> 16) & 0xFF);
    Emit8(ctx, (data >> 24) & 0xFF);
}

void Emit64(EmiterContext* ctx, u64 data) {
    Emit8(ctx, (data >> 0)  & 0xFF);
    Emit8(ctx, (data >> 8)  & 0xFF);
    Emit8(ctx, (data >> 16) & 0xFF);
    Emit8(ctx, (data >> 24) & 0xFF);
    Emit8(ctx, (data >> 32) & 0xFF);
    Emit8(ctx, (data >> 40) & 0xFF);
    Emit8(ctx, (data >> 48) & 0xFF);
    Emit8(ctx, (data >> 56) & 0xFF);
}

// Mod/RM byte:
// mode: addressing mode
// reg:  registers/op code extension
// rm:   register, used based on addressing mode
//     mode            reg               rm
// ------------|-----------------|------------------
// |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |
// ------------|-----------------|------------------

// the mod value selects the address encoding mode for the operands
// the reg value always encodes a register, except when the opcode only uses one operand, then the reg value can be used as a opcode extension
// the rm value is also a register but it is used based on the addressing mode, if rm = RSP(4) and mode is indirect, a SIB must follow, if mod = 0 and rm = RBP(5) it means absolute address?? [0x1234]
// in DIRECT addressing mode, if both operands are registers: reg = dst, rm = src, add reg rm
void EmitModRMByte(EmiterContext* ctx, AddressingMode mod, Register reg, Register rm) {
    assert(mod < 4);  // 2 bit number
    assert(reg < 16); // 3 bit number, but 16 registers, the top bit is encoded in REX byte
    assert(rm < 16);  // 3 bit number, but 16 registers, the top bit is encoded in REX byte
    Emit8(ctx, ((mod & 3) << 6) | ((reg & 7) << 3) | ((rm & 7) << 0));
}

// SIB byte:
// [base + scale * index]
//     scale          index             base
// ------------|-----------------|------------------
// |  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0  |
// ------------|-----------------|------------------

// scale is the scale to apply to the index
// index is a register that stores the index to use, rsp means no index, it cant be used as an index
// base is a register that stores the base to use, base = RBP(5) is a special case, if in the ModR/M byte mod = 0 base is ignored
void EmitSIBByte(EmiterContext* ctx, Scale scale, Register index, Register base) {
    assert(scale < 4);  // 2 bit number
    assert(index < 16); // 3 bit number, but 16 registers, the top bit is encoded in REX byte
    assert(base < 16);  // 3 bit number, but 16 registers, the top bit is encoded in REX byte
    Emit8(ctx, ((scale & 3) << 6) | ((index & 7) << 3) | ((base & 7) << 0));
}

// REX byte:
// the top half is always set to 0b0100
// in 64 bit mode the W bit is always set, so its safe to consider the base to be 0x48
// if W = 1, the RXB bits are used to extend the ModR/M and SIB bytes
// if W = 0, code segment is used to determine if the RXB bits are used
// R bit is used to extend the reg field of the ModR/M byte
// X bit is used to extend the index field of the SIB byte
// B bit is used to extend the rm field of the ModR/M, base field of the SIB byte or reg field of the ModR/M byte if it is used as an opcode
// ------------------------|-----|-----|-----|------
// |  0  |  1  |  0  |  0  |  W  |  R  |  X  |  B  |
// ------------------------|-----|-----|-----|------

// r is the register in the reg field of the ModR/M byte
// x is the register in the index field SIB byte
// b can be the register in the rm field of the ModR/M, base field of the SIB byte or reg field of the ModR/M byte if it is used as an opcode
void EmitRexByte(EmiterContext* ctx, u8 w, Register r, Register x, Register b) {
    assert(w == 0 || w == 1);
    assert(r < 16); // 1 bit number, but 16 registers, only the top bit is extracted
    assert(x < 16); // 1 bit number, but 16 registers, only the top bit is extracted
    assert(b < 16); // 1 bit number, but 16 registers, only the top bit is extracted
    Emit8(ctx, 0x40 | (w << 3) | ((r >> 3) << 2) | ((x >> 3) << 1) | ((b >> 3) << 0));
}

// ModR/M and SIB byte emiters

// op rax, rcx
// reg = RAX, rm = RCX
void EmitDirect(EmiterContext* ctx, Register reg, Register rm) {
    EmitModRMByte(ctx, DIRECT, reg, rm);
}

// op rax, [rcx]
// reg = RAX, rm = RCX
void EmitIndirect(EmiterContext* ctx, Register reg, Register rm) {
    assert((reg & 7) != RSP);
    assert((reg & 7) != RBP);
    EmitModRMByte(ctx, INDIRECT_NO_DISPLACE, reg, rm);
}

// op rax, [rcx + 0x12]
// reg = RAX, rm = RCX, displacement = 0x12
void EmitIndirectDisplaced8(EmiterContext* ctx, Register reg, Register rm, u8 displacement) {
    assert((rm & 7) != RSP);
    EmitModRMByte(ctx, INDIRECT_08_DISPLACE, reg, rm);
    Emit8(ctx, displacement);
}

// op rax, [rcx + 0x12345678]
// reg = RAX, rm = RCX, displacement = 0x12345678
void EmitIndirectDisplaced32(EmiterContext* ctx, u8 reg, Register rm, u32 displacement) {
    assert((rm & 7) != RSP);
    EmitModRMByte(ctx, INDIRECT_32_DISPLACE, reg, rm);
    Emit32(ctx, displacement);
}

// op rax, [rcx + 4*rdx]
// reg = RAX, rm = RCX, index = RDX, scale = X4
void EmitIndirectSIB(EmiterContext* ctx, u8 reg, Register rm, Register index, Scale scale) {
    assert((rm & 7) != RBP);
    EmitModRMByte(ctx, INDIRECT_NO_DISPLACE, reg, RSP);
    EmitSIBByte(ctx, scale, index, rm);
}

// op rax, [rcx + 4*rdx + 0x12]
// reg = RAX, rm = RCX, index = RDX, scale = X4, displacement = 0x12
void EmitIndirectDisplaced8SIB(EmiterContext* ctx, u8 reg, Register rm, Register index, Scale scale, u8 displacement) {
    EmitModRMByte(ctx, INDIRECT_08_DISPLACE, reg, RSP);
    EmitSIBByte(ctx, scale, index, rm);
    Emit8(ctx, displacement);
}

// op rax, [rcx + 4*rdx + 0x12345678]
// reg = RAX, rm = RCX, index = RDX, scale = X4, displacement = 0x12345678
void EmitIndirectDisplaced32SIB(EmiterContext* ctx, u8 reg, Register rm, Register index, Scale scale, u32 displacement) {
    EmitModRMByte(ctx, INDIRECT_32_DISPLACE, reg, RSP);
    EmitSIBByte(ctx, scale, index, rm);
    Emit32(ctx, displacement);
}

// op rax, [rip + 0x12345678]
// reg = RAX, displacement = 0x12345678
void EmitIndirectDisplacedRip(EmiterContext* ctx, u8 reg, u32 displacement) {
    EmitModRMByte(ctx, INDIRECT_NO_DISPLACE, reg, RBP);
    Emit32(ctx, displacement);
}

// op rax, [0x12345678]
// reg = RAX, displacement = 0x12345678
void EmitIndirectAbsolute(EmiterContext* ctx, u8 reg, u32 displacement) {
    EmitModRMByte(ctx, INDIRECT_NO_DISPLACE, reg, RSP);
    EmitSIBByte(ctx, X0, RSP, RBP);
    Emit32(ctx, displacement);
}

// --------------------------------------------------

typedef enum OperandType {
    OPERAND_NONE,
    // regA
    OPERAND_Register,
    // [regA]
    OPERAND_AddrInReg,
    // [regA + 0x12]
    OPERAND_AddrInRegOffset8,
    // [regA + 0x1234]
    OPERAND_AddrInRegOffset32,
    // [regA + X0 * regB]
    OPERAND_SIB,
    // [regA + X0 * regB + 0x12]
    OPERAND_SIBOffset8,
    // [regA + X0 * regB + 0x1234]
    OPERAND_SIBOffset32,
    // [RIP + 0x1234]
    OPERAND_RIP,
    // [0x1234]
    OPERAND_AbsoluteAddr,
    // 0x12
    OPERAND_Immediate8,
    // 0x1234
    OPERAND_Immediate32,

    // NOTE: dont know if these are valid
    OPERAND_Immediate16,
    OPERAND_Immediate64,
} OperandType;

const u8* OperandTypeStr[] = {
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

bool isZeroStruct(InstructionEncoding* enc) {
    u8* testValBuffer = (u8*)&(InstructionEncoding){0};
    u64 size = sizeof(*enc);
    u8* encBuff = (u8*)enc;
    return memcmp(encBuff, testValBuffer, size) == 0;
}

bool checkTypesMatch(OperandType operandType, OpType encodingOperandType) {
    switch(encodingOperandType) {
        case OpType_NONE: {
            if(operandType != OPERAND_NONE) return false;
        } break;
        case OpType_REG: {
            if(operandType != OPERAND_Register) return false;
        } break;
        case OpType_RM: {
            if(!(
                operandType == OPERAND_Register ||
                operandType == OPERAND_AddrInReg ||
                operandType == OPERAND_AddrInRegOffset8 ||
                operandType == OPERAND_AddrInRegOffset32 ||
                operandType == OPERAND_SIB ||
                operandType == OPERAND_SIBOffset8 ||
                operandType == OPERAND_SIBOffset32 ||
                operandType == OPERAND_RIP ||
                operandType == OPERAND_AbsoluteAddr
            )) return false;
        } break;
        case OpType_IMM8: {
            if(operandType != OPERAND_Immediate8) return false;
        } break;
        case OpType_IMM16: {
            if(!(
                operandType == OPERAND_Immediate8 ||
                operandType == OPERAND_Immediate16
            )) return false;
        } break;
        case OpType_IMM32: {
            if(!(
                operandType == OPERAND_Immediate8 ||
                operandType == OPERAND_Immediate16 ||
                operandType == OPERAND_Immediate32
            )) return false;
        } break;
        case OpType_IMM64: {
            if(!(
                operandType == OPERAND_Immediate8 ||
                operandType == OPERAND_Immediate16 ||
                operandType == OPERAND_Immediate32 ||
                operandType == OPERAND_Immediate64
            )) return false;
        } break;
    }
    return true;
}

void genInstruction(EmiterContext* ctx, Instruction inst) {
    InstructionEncoding* instructionEncodings =  encodings[inst.name];
    for(u64 i = 0; i < MAX_ENCODING_FOR_INSTRUCTION; i++) {
        InstructionEncoding encoding = instructionEncodings[i];
        if(isZeroStruct(&encoding)) break;
        if(!checkTypesMatch(inst.ops[0].type, encoding.opTypes[0]) || !checkTypesMatch(inst.ops[1].type, encoding.opTypes[1])) continue;

        // NOTE: what about instructions with no operands?
        u8 rexR = 0;
        u8 rexX = 0;
        u8 rexB = 0;
        Register dstReg = 0; // NOTE: maybe need better invalid here
        OperandType emitType = OPERAND_Register;
        u8 immOpIndex = 255; // NOTE: better invalid
        u32 disspl = 0;
        Register base = 0; // NOTE: maybe need better invalid here
        Scale scale = X0;
        Register index = 0; // NOTE: maybe need better invalid here
        Register rmReg = 0; // NOTE: maybe need better invalid here
        u8 regOrExt = 0;
        for(u64 h = 0; h < INSTRUCTION_MAX_OPERANDS; h++) {
            Operand op = inst.ops[h];
            OpType encodingOp = encoding.opTypes[h];

            switch(encodingOp) {
                case OpType_NONE: break;
                case OpType_REG: {
                    // emitType = OPERAND_Register;
                    if(encoding.regInOpcode) {
                        rexB = op.reg;
                    } else {
                        rexR = op.reg;
                    }
                    dstReg = op.reg;
                    // NOTE: probably redundant if
                    if(encoding.modRMType == ModRMType_REG) {
                        regOrExt = op.reg;
                    }
                } break;
                case OpType_RM: {
                    switch(op.type) {
                        case OPERAND_Register:
                        case OPERAND_AddrInReg: {
                            emitType = op.type;
                            rmReg = op.reg;
                            rexB = op.reg;
                        } break;
                        case OPERAND_AddrInRegOffset8:
                        case OPERAND_AddrInRegOffset32: {
                            emitType = op.type;
                            rmReg = op.reg;
                            disspl = op.displacement;
                            rexB = op.reg;
                        } break;
                        case OPERAND_SIB: {
                            emitType = op.type;
                            base = op.base;
                            scale = op.scale;
                            index = op.index;
                            rexB = op.base;
                            rexX = op.index;
                        } break;
                        case OPERAND_SIBOffset8:
                        case OPERAND_SIBOffset32: {
                            emitType = op.type;
                            base = op.base;
                            scale = op.scale;
                            index = op.index;
                            disspl = op.displacement;
                            rexB = op.base;
                            rexX = op.index;
                        } break;
                        case OPERAND_RIP:
                        case OPERAND_AbsoluteAddr: {
                            emitType = op.type;
                            disspl = op.displacement;
                        } break;
                    }
                } break;
                case OpType_IMM8:
                case OpType_IMM16:
                case OpType_IMM32:
                case OpType_IMM64: {
                    immOpIndex = h;
                } break;
            }
        }

        if(encoding.modRMType == ModRMType_EXT) {
            regOrExt = encoding.opcodeExtension;
        }

        // Rex byte
        // TODO: support for 16 and 8 bit prefixes
        u8 rexW = 0;
        if(encoding.rexType == RexByte_W) rexW = 1;
        if(encoding.rexType != RexByte_NONE && (rexW || (rexR & 8) || (rexX & 8) || (rexB & 8))) EmitRexByte(ctx, rexW, rexR, rexX, rexB);

        // Opcode
        if(encoding.regInOpcode) Emit8(ctx, encoding.opcode | (dstReg & 7));
        else Emit8(ctx, encoding.opcode);

        // ModR/M and SIB bytes and dissplacement
        if(encoding.modRMType != ModRMType_NONE) {
            switch(emitType) {
                case OPERAND_Register: {
                    EmitDirect(ctx, regOrExt, rmReg);
                } break;
                case OPERAND_AddrInReg: {
                    EmitIndirect(ctx, regOrExt, rmReg);
                } break;
                case OPERAND_AddrInRegOffset8: {
                    EmitIndirectDisplaced8(ctx, regOrExt, rmReg, (u8)disspl);
                } break;
                case OPERAND_AddrInRegOffset32: {
                    EmitIndirectDisplaced32(ctx, regOrExt, rmReg, disspl);
                } break;
                case OPERAND_SIB: {
                    EmitIndirectSIB(ctx, regOrExt, base, index, scale);
                } break;
                case OPERAND_SIBOffset8: {
                    EmitIndirectDisplaced8SIB(ctx, regOrExt, base, index, scale, (u8)disspl);
                } break;
                case OPERAND_SIBOffset32: {
                    EmitIndirectDisplaced32SIB(ctx, regOrExt, base, index, scale, disspl);
                } break;
                case OPERAND_RIP: {
                    EmitIndirectDisplacedRip(ctx, regOrExt, disspl);
                } break;
                case OPERAND_AbsoluteAddr: {
                    EmitIndirectAbsolute(ctx, regOrExt, disspl);
                } break;
            }
        }

        // Immediates
        if(immOpIndex < INSTRUCTION_MAX_OPERANDS) {
            switch(encoding.opTypes[immOpIndex]) {
                case OpType_IMM8:  Emit8(ctx, inst.ops[immOpIndex].immediate); break;
                case OpType_IMM16: Emit16(ctx, inst.ops[immOpIndex].immediate); break;
                case OpType_IMM32: Emit32(ctx, inst.ops[immOpIndex].immediate); break;
                case OpType_IMM64: Emit64(ctx, inst.ops[immOpIndex].immediate); break;
            }
        }
        
        return;
    }
    printf("[ERROR] Could not find encoding for instruction: %s %s %s\n", MnemonicStr[inst.name], OperandTypeStr[inst.ops[0].type], OperandTypeStr[inst.ops[1].type]);
    assert(false);
}

// -------------------------------------------

void InitializeFreeRegisters(EmiterContext* ctx) {
    Register available_registers[] = {RCX, RBX, RSI, RDI, R8, R9, R10, R11, R12, R13, R14, R15};
    for (size_t i = 0; i < sizeof(available_registers) / sizeof(*available_registers); i++) {
        ctx->freeRegisterMask |= 1 << available_registers[i];
    }
}

u32 GetSetBits(u64 mask) {
    u32 i;
    for(i = 0; i < 64; i++){
        if (mask & (1 << i) == 1) break;
    }
    return i;
}

Register AllocateRegister(EmiterContext* ctx) {
    assert(ctx->freeRegisterMask != 0);
    u32 free_register = GetSetBits(ctx->freeRegisterMask);
    ctx->freeRegisterMask &= ~(1 << free_register);
    return (Register)free_register;
}

void FreeRegister(EmiterContext* ctx, Register registerToFree) {
    assert((ctx->freeRegisterMask & (1 << registerToFree)) == 0);
    ctx->freeRegisterMask |= 1 << registerToFree;
}

#define OP_REG(_register_) \
    (Operand){.type = OPERAND_Register, .reg = (_register_), .isReg = true}
#define OP_INDIRECT(_register_) \
    (Operand){.type = OPERAND_AddrInReg, .reg = (_register_), .isIndirect = true}
#define OP_INDIRECT_OFFSET8(_register_, _displacement_) \
    (Operand){.type = OPERAND_AddrInRegOffset8, .reg = (_register_), .displacement = (_displacement_), .isIndirect = true}
#define OP_INDIRECT_OFFSET32(_register_, _displacement_) \
    (Operand){.type = OPERAND_AddrInRegOffset32, .reg = (_register_), .displacement = (_displacement_), .isIndirect = true}
#define OP_INDIRECT_SIB(_base_, _scale_, _index_) \
    (Operand){.type = OPERAND_SIB, .base = (_base_), .scale = (_scale_), .index = (_index_), .isSIB = true}
#define OP_INDIRECT_SIB_OFFSET8(_base_, _scale_, _index_, _displacement_) \
    (Operand){.type = OPERAND_SIBOffset8, .base = (_base_), .scale = (_scale_), .index = (_index_), .displacement = (_displacement_), .isSIB = true}
#define OP_INDIRECT_SIB_OFFSET32(_base_, _scale_, _index_, _displacement_) \
    (Operand){.type = OPERAND_SIBOffset32, .base = (_base_), .scale = (_scale_), .index = (_index_), .displacement = (_displacement_), .isSIB = true}
#define OP_RIP(_displacement_) \
    (Operand){.type = OPERAND_RIP, .displacement = (_displacement_), .isRIPorAbs = true}
#define OP_ABSOLUTE(_displacement_) \
    (Operand){.type = OPERAND_AbsoluteAddr, .displacement = (_displacement_), .isRIPorAbs = true}
#define OP_IMM8(_immediate_) \
    (Operand){.type = OPERAND_Immediate8, .immediate = (_immediate_), .isImm = true}
#define OP_IMM32(_immediate_) \
    (Operand){.type = OPERAND_Immediate32, .immediate = (_immediate_), .isImm = true}

#define INST(_mnemonic_, ...) (Instruction){.name = _mnemonic_##_, .ops = {__VA_ARGS__}}

#define gen_callExtern(_ctx_, _name_) _gen_callExtern(_ctx_, _name_, strlen(_name_))
void _gen_callExtern(EmiterContext* ctx, u8* name, u64 size) {
    // 8 + 8 + 32 bits pushed to the ctx, last 32 are the address
    genInstruction(ctx, INST(call, OP_RIP(0xDEADBEEF)));
    int offset = ctx->code.size - 4;
    *buffer_allocate(&ctx->symbolsToPatch, AddrToPatch) = (AddrToPatch){
        .name = name,
        .len = size,
        .offset = offset,
    };
}

// lea rax, [rip + 0xdeadbeef + sizeof(u64)] ; load data
// mov rax, [rip + 0xdeadbeef]               ; load size
#define gen_DataInReg(_ctx_, _reg_, _name_) _gen_DataInReg(_ctx_, _reg_, _name_, strlen(_name_))
void _gen_DataInReg(EmiterContext* ctx, Register reg, u8* name, u64 nameLen) {
    // NOTE: the rip address get added to the patched address,
    // so adding the size of u64 offsets the address by 8,
    // meaning instead of reading from the data begining, where the size is stored,
    // instead it read the data stored in the entry
    genInstruction(ctx, INST(lea, OP_REG(reg), OP_RIP(sizeof(u64))));
    int offset = ctx->code.size - 4;
    *buffer_allocate(&ctx->dataToPatch, AddrToPatch) = (AddrToPatch){
        .name = name,
        .len = nameLen,
        .offset = offset,
    };
}

#define gen_SizeInReg(_ctx_, _reg_, _name_) _gen_SizeInReg(_ctx_, _reg_, _name_, strlen(_name_))
void _gen_SizeInReg(EmiterContext* ctx, Register reg, u8* name, u64 nameLen) {
    // NOTE: the rip address get added to the patched address,
    // the address gets added to 0 so in this case we read the size field of the entry
    genInstruction(ctx, INST(mov, OP_REG(reg), OP_RIP(0)));
    int offset = ctx->code.size - 4;
    *buffer_allocate(&ctx->dataToPatch, AddrToPatch) = (AddrToPatch){
        .name = name,
        .len = nameLen,
        .offset = offset,
    };
}

#define TEST_IN_MEM_EXECUTION 0
#define TEST_EXE_GENERATION 0
#define TEST_INSTRUCION_ENCODING 0
#define TEST_DATA 1

#include "tests.c"

int main(int argc, char** argv) {
    EmiterContext ctx = {0};
    ctx.code = make_buffer(0x200, PAGE_READWRITE);
    ctx.symbolsToPatch = make_buffer(0x200, PAGE_READWRITE);
    ctx.dataToPatch = make_buffer(0x200, PAGE_READWRITE);

    #if TEST_DATA
    // Data section
    // NOTE: the data is layed out the following way:
    // the size of data (8 bytes), followed by the data (byte count given by the size field)
    // the size does not include its own 8 bytes, meaning the total size of the data entry is size + 8 number of bytes
    u8* foo_string = "Hello World\n";
    u64 dataLen = strlen(foo_string);
    UserDataEntry dataSection[] = {
        {.name = "msg", .data = foo_string, .dataLen = dataLen, .dataRVA = INVALID_ADDRESS},
    };
    
    // Import section
    Import_Name_To_Rva kernel32_functions[] = {
        {.name = "ExitProcess", .name_rva = INVALID_ADDRESS, .iat_rva = INVALID_ADDRESS},
        {.name = "GetStdHandle", .name_rva = INVALID_ADDRESS, .iat_rva = INVALID_ADDRESS},
        {.name = "WriteFile", .name_rva = INVALID_ADDRESS, .iat_rva = INVALID_ADDRESS},
    };
    Import_Library import_libraries[] = {
        {
            .dll = {.name = "kernel32.dll", .name_rva = INVALID_ADDRESS, .iat_rva = INVALID_ADDRESS},
            .image_thunk_rva = INVALID_ADDRESS,
            .functions = kernel32_functions,
            .function_count = ARRAY_SIZE(kernel32_functions),
        },
    };

    // Function prolog
    genInstruction(&ctx, INST(push, OP_REG(RBP)));
    genInstruction(&ctx, INST(mov, OP_REG(RBP), OP_REG(RSP)));

    // HANDLE stack[0] = GetStdHandle(STD_OUTPUT_HANDLE)
    genInstruction(&ctx, INST(mov, OP_REG(RCX), OP_IMM32(STD_OUTPUT_HANDLE)));
    gen_callExtern(&ctx, "GetStdHandle");
    genInstruction(&ctx, INST(push, OP_REG(RAX))); // store GetStdHandle return handle

    // WriteFile(stack[0], [msg], msg.len, 0, 0)
    genInstruction(&ctx, INST(pop, OP_REG(RCX))); // pop to rcx, first arg
    gen_DataInReg(&ctx, RDX, "msg"); // load the address into rdx, second arg
    gen_SizeInReg(&ctx, R8, "msg"); // load the value of the len into r8, third arg
    genInstruction(&ctx, INST(mov, OP_REG(R9), OP_IMM8(0))); // NULL for bytesWrittenPtr, fourth arg
    genInstruction(&ctx, INST(push, OP_IMM8(0))); // NULL, fifth arg
    gen_callExtern(&ctx, "WriteFile");

    // Function epilog
    genInstruction(&ctx, INST(mov, OP_REG(RSP), OP_REG(RBP)));
    genInstruction(&ctx, INST(pop, OP_REG(RBP)));
    // ExitProcess(0);
    genInstruction(&ctx, INST(mov, OP_REG(RCX), OP_IMM8(0)));
    gen_callExtern(&ctx, "ExitProcess");

    write_executable("smallExe.exe", import_libraries, ARRAY_SIZE(import_libraries), ctx.code, ctx.symbolsToPatch, dataSection, ARRAY_SIZE(dataSection), ctx.dataToPatch);
    #endif // TEST_DATA

    #if TEST_IN_MEM_EXECUTION
    ctx.code = make_buffer(MiB(1), PAGE_EXECUTE_READWRITE);
    
    genInstruction(&ctx, INST(push, OP_REG(RBP)));
    genInstruction(&ctx, INST(mov, OP_REG(RBP), OP_REG(RSP)));
    
    genInstruction(&ctx, INST(mov, OP_REG(RAX), OP_REG(RCX)));
    // genInstruction(&ctx, INST(add, OP_REG(RAX), OP_REG(RDX)));

    genInstruction(&ctx, INST(mov, OP_REG(RSP), OP_REG(RBP)));
    genInstruction(&ctx, INST(pop, OP_REG(RBP)));
    genInstruction(&ctx, INST(ret, 0));

    if(argc < 2){
        printf("[ERROR] Needs at least one argument\n");
        exit(1);
    }

    u64 (*func)(u64) = (u64(*)(u64))ctx.code.mem;
    u64 foo = func(argv[1][0]);
    printf("ret: %i\n", foo);
    
    FILE* f = fopen("test.bin", "wb");
    fwrite(ctx.code.mem, sizeof(*ctx.code.mem), ctx.code.size, f);
    fclose(f);
    #endif // TEST_IN_MEM_EXECUTION

    #if TEST_EXE_GENERATION
    // Function prolog
    genInstruction(&ctx, INST(push, OP_REG(RBP)));
    genInstruction(&ctx, INST(mov, OP_REG(RBP), OP_REG(RSP)));
    
    // STR stack[0] = GetCommandLineA()
    gen_callExtern(&ctx, "GetCommandLineA");
    genInstruction(&ctx, INST(push, OP_REG(RAX))); // store GetCommandLineA return address
    
    // HANDLE stack[1] = GetStdHandle(STD_OUTPUT_HANDLE)
    genInstruction(&ctx, INST(mov, OP_REG(RCX), OP_IMM32(STD_OUTPUT_HANDLE)));
    gen_callExtern(&ctx, "GetStdHandle");
    genInstruction(&ctx, INST(push, OP_REG(RAX))); // store GetStdHandle return handle
    
    // WriteFile(stack[1], stack[0], 40, 0, 0)
    genInstruction(&ctx, INST(pop, OP_REG(RCX))); // pop to rcx, first arg
    genInstruction(&ctx, INST(pop, OP_REG(RDX))); // pop to rdx, second arg
    genInstruction(&ctx, INST(mov, OP_REG(R8), OP_IMM8(40))); // str len 40, third arg
    genInstruction(&ctx, INST(mov, OP_REG(R9), OP_IMM8(0))); // NULL for bytesWrittenPtr, fourth arg
    genInstruction(&ctx, INST(push, OP_IMM8(0))); // NULL, fifth arg
    gen_callExtern(&ctx, "WriteFile");
    
    // Function epilog
    genInstruction(&ctx, INST(mov, OP_REG(RSP), OP_REG(RBP)));
    genInstruction(&ctx, INST(pop, OP_REG(RBP)));
    
    // ExitProcess(0);
    genInstruction(&ctx, INST(mov, OP_REG(RCX), OP_IMM8(0)));
    gen_callExtern(&ctx, "ExitProcess");
    
    // int3
    buffer_append_u8(&ctx.code, 0xCC);

    Import_Name_To_Rva kernel32_functions[] = {
        {.name = "ExitProcess", .name_rva = INVALID_ADDRESS, .iat_rva = INVALID_ADDRESS},
        {.name = "GetStdHandle", .name_rva = INVALID_ADDRESS, .iat_rva = INVALID_ADDRESS},
        {.name = "WriteFile", .name_rva = INVALID_ADDRESS, .iat_rva = INVALID_ADDRESS},
        {.name = "GetCommandLineA", .name_rva = INVALID_ADDRESS, .iat_rva = INVALID_ADDRESS},
    };
    Import_Library import_libraries[] = {
        {
            .dll = {.name = "kernel32.dll", .name_rva = INVALID_ADDRESS, .iat_rva = INVALID_ADDRESS},
            .image_thunk_rva = INVALID_ADDRESS,
            .functions = kernel32_functions,
            .function_count = ARRAY_SIZE(kernel32_functions),
        },
    };
    printf("Finished generating bytecode.\n");
    write_executable("smallExe.exe", import_libraries, ARRAY_SIZE(import_libraries), ctx.code, ctx.names);
    #endif // TEST_EXE_GENERATION

    #if TEST_INSTRUCION_ENCODING
    #define GEN_TEST(_name_)                                               \
        do{                                                                \
            EmiterContext ctx = {0};                                       \
            ctx.code = make_buffer(MiB(1), PAGE_READWRITE);                \
            ctx.names = make_buffer(0x200, PAGE_READWRITE);                \
            test##_name_(&ctx);                                            \
            FILE *f = fopen("dump" #_name_ ".bin", "wb");                  \
            fwrite(ctx.code.mem, sizeof(*ctx.code.mem), ctx.code.size, f); \
            fclose(f);                                                     \
            printf("Done generating dump" #_name_ ".bin\n");               \
            VirtualFree(ctx.code.mem, 0, MEM_RELEASE);                     \
            VirtualFree(ctx.names.mem, 0, MEM_RELEASE);                    \
        }while(0)

    GEN_TEST(Add);
    GEN_TEST(Mov);
    GEN_TEST(Push);
    GEN_TEST(Pop);
    GEN_TEST(Call);
    #endif // TEST_INSTRUCION_ENCODING

    return 0;
}

// NOTE: delayed register allocations
