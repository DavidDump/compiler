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
    Buffer names;

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
    union {
        // Used for OPERAND_Register and OPERAND_AddrInReg
        struct {
            Register reg;
        } REGISTER;
        struct {
            Register reg;
            u8 offset;
        } REGISTER_OFFSET8;
        struct {
            Register reg;
            u32 offset;
        } REGISTER_OFFSET32;
        struct {
            Register base;
            Register index;
            Scale scale;
        } SIB;
        struct {
            Register base;
            Register index;
            Scale scale;
            u8 offset;
        } SIB_OFFSET8;
        struct {
            Register base;
            Register index;
            Scale scale;
            u32 offset;
        } SIB_OFFSET32;
        struct {
            u32 offset;
        } RIP;
        struct {
            u32 addr;
        } ABSOLUTE_ADDR;
        struct {
            u8 immediate;
        } IMMEDIATE8;
        struct {
            u32 immediate;
        } IMMEDIATE32;
    };
} Operand;

// NOTE: These functions might be stupid if the encoding for individual instructions is simple,
// we basically have a case for every operand type during encoding and then decoding,
// which might be double work
void gen_add(EmiterContext* ctx, Operand dest, Operand source) {
    assert(dest.type != OPERAND_Immediate8 && "This case is not allowed");
    assert(dest.type != OPERAND_Immediate32 && "This case is not allowed");

    u8 opcode = 0x03;
    if(source.type == OPERAND_Register && dest.type != OPERAND_Register) {
        Operand tmp = dest;
        dest = source;
        source = tmp;
        opcode = 0x01;
    }

    switch(source.type) {
        case OPERAND_Register: {
            assert(dest.type == OPERAND_Register);
            EmitRexByte(ctx, 1, dest.REGISTER.reg, 0, source.REGISTER.reg);
            Emit8(ctx, opcode);
            EmitDirect(ctx, dest.REGISTER.reg, source.REGISTER.reg);
        } break;
        case OPERAND_AddrInReg: {
            assert(dest.type == OPERAND_Register);
            EmitRexByte(ctx, 1, dest.REGISTER.reg, 0, source.REGISTER.reg);
            Emit8(ctx, opcode);
            EmitIndirect(ctx, dest.REGISTER.reg, source.REGISTER.reg);
        } break;
        case OPERAND_AddrInRegOffset8: {
            assert(dest.type == OPERAND_Register);
            EmitRexByte(ctx, 1, dest.REGISTER.reg, 0, source.REGISTER_OFFSET8.reg);
            Emit8(ctx, opcode);
            EmitIndirectDisplaced8(ctx, dest.REGISTER.reg, source.REGISTER_OFFSET8.reg, source.REGISTER_OFFSET8.offset);
        } break;
        case OPERAND_AddrInRegOffset32: {
            assert(dest.type == OPERAND_Register);
            EmitRexByte(ctx, 1, dest.REGISTER.reg, 0, source.REGISTER_OFFSET32.reg);
            Emit8(ctx, opcode);
            EmitIndirectDisplaced32(ctx, dest.REGISTER.reg, source.REGISTER_OFFSET32.reg, source.REGISTER_OFFSET32.offset);
        } break;
        case OPERAND_SIB: {
            assert(dest.type == OPERAND_Register);
            EmitRexByte(ctx, 1, dest.REGISTER.reg, source.SIB.index, source.SIB.base);
            Emit8(ctx, opcode);
            EmitIndirectSIB(ctx, dest.REGISTER.reg, source.SIB.base, source.SIB.index, source.SIB.scale);
        } break;
        case OPERAND_SIBOffset8: {
            assert(dest.type == OPERAND_Register);
            EmitRexByte(ctx, 1, dest.REGISTER.reg, source.SIB_OFFSET8.index, source.SIB_OFFSET8.base);
            Emit8(ctx, opcode);
            EmitIndirectDisplaced8SIB(ctx, dest.REGISTER.reg, source.SIB_OFFSET8.base, source.SIB_OFFSET8.index, source.SIB_OFFSET8.scale, source.SIB_OFFSET8.offset);
        } break;
        case OPERAND_SIBOffset32: {
            assert(dest.type == OPERAND_Register);
            EmitRexByte(ctx, 1, dest.REGISTER.reg, source.SIB_OFFSET32.index, source.SIB_OFFSET32.base);
            Emit8(ctx, opcode);
            EmitIndirectDisplaced32SIB(ctx, dest.REGISTER.reg, source.SIB_OFFSET32.base, source.SIB_OFFSET32.index, source.SIB_OFFSET32.scale, source.SIB_OFFSET32.offset);
        } break;
        case OPERAND_RIP: {
            assert(dest.type == OPERAND_Register);
            EmitRexByte(ctx, 1, dest.REGISTER.reg, 0, 0);
            Emit8(ctx, opcode);
            EmitIndirectDisplacedRip(ctx, dest.REGISTER.reg, source.RIP.offset);
        } break;
        case OPERAND_AbsoluteAddr: {
            assert(dest.type == OPERAND_Register);
            EmitRexByte(ctx, 1, dest.REGISTER.reg, 0, 0);
            Emit8(ctx, opcode);
            EmitIndirectAbsolute(ctx, dest.REGISTER.reg, source.ABSOLUTE_ADDR.addr);
        } break;
        case OPERAND_Immediate8:
        case OPERAND_Immediate32: {
            u8 opcode = source.type == OPERAND_Immediate8 ? 0x83 : 0x81;
            
            switch(dest.type) {
                case OPERAND_Register: {
                    EmitRexByte(ctx, 1, 0, 0, dest.REGISTER.reg);
                    Emit8(ctx, opcode);
                    EmitDirect(ctx, 0, dest.REGISTER.reg);
                } break;
                case OPERAND_AddrInReg: {
                    EmitRexByte(ctx, 1, 0, 0, dest.REGISTER.reg);
                    Emit8(ctx, opcode);
                    EmitIndirect(ctx, 0, dest.REGISTER.reg);
                } break;
                case OPERAND_AddrInRegOffset8: {
                    EmitRexByte(ctx, 1, 0, 0, dest.REGISTER_OFFSET8.reg);
                    Emit8(ctx, opcode);
                    EmitIndirectDisplaced8(ctx, 0, dest.REGISTER_OFFSET8.reg, dest.REGISTER_OFFSET8.offset);
                } break;
                case OPERAND_AddrInRegOffset32: {
                    EmitRexByte(ctx, 1, 0, 0, dest.REGISTER_OFFSET32.reg);
                    Emit8(ctx, opcode);
                    EmitIndirectDisplaced32(ctx, 0, dest.REGISTER_OFFSET32.reg, dest.REGISTER_OFFSET32.offset);
                } break;
                case OPERAND_SIB: {
                    EmitRexByte(ctx, 1, 0, dest.SIB.index, dest.SIB.base);
                    Emit8(ctx, opcode);
                    EmitIndirectSIB(ctx, 0, dest.SIB.base, dest.SIB.index, dest.SIB.scale);
                } break;
                case OPERAND_SIBOffset8: {
                    EmitRexByte(ctx, 1, 0, dest.SIB_OFFSET8.index, dest.SIB_OFFSET8.base);
                    Emit8(ctx, opcode);
                    EmitIndirectDisplaced8SIB(ctx, 0, dest.SIB_OFFSET8.base, dest.SIB_OFFSET8.index, dest.SIB_OFFSET8.scale, dest.SIB_OFFSET8.offset);
                } break;
                case OPERAND_SIBOffset32: {
                    EmitRexByte(ctx, 1, 0, dest.SIB_OFFSET32.index, dest.SIB_OFFSET32.base);
                    Emit8(ctx, opcode);
                    EmitIndirectDisplaced32SIB(ctx, 0, dest.SIB_OFFSET32.base, dest.SIB_OFFSET32.index, dest.SIB_OFFSET32.scale, dest.SIB_OFFSET32.offset);
                } break;
                case OPERAND_RIP: {
                    EmitRexByte(ctx, 1, 0, 0, 0);
                    Emit8(ctx, opcode);
                    EmitIndirectDisplacedRip(ctx, 0, dest.RIP.offset);
                } break;
                case OPERAND_AbsoluteAddr: {
                    EmitRexByte(ctx, 1, 0, 0, 0);
                    Emit8(ctx, opcode);
                    EmitIndirectAbsolute(ctx, 0, dest.ABSOLUTE_ADDR.addr);
                } break;
            }
            
            if(source.type == OPERAND_Immediate8) Emit8(ctx, source.IMMEDIATE8.immediate);
            else Emit32(ctx, source.IMMEDIATE32.immediate);
        } break;
    }
}

void gen_mov(EmiterContext* ctx, Operand dest, Operand source) {
    assert(dest.type != OPERAND_Immediate8 && "This case is not allowed");
    assert(dest.type != OPERAND_Immediate32 && "This case is not allowed");

    u8 opcode = 0x8B;
    if(source.type == OPERAND_Register && dest.type != OPERAND_Register) {
        Operand tmp = dest;
        dest = source;
        source = tmp;
        opcode = 0x89;
    }

    switch(source.type) {
        case OPERAND_Register: {
            assert(dest.type == OPERAND_Register);
            EmitRexByte(ctx, 1, dest.REGISTER.reg, 0, source.REGISTER.reg);
            Emit8(ctx, opcode);
            EmitDirect(ctx, dest.REGISTER.reg, source.REGISTER.reg);
        } break;
        case OPERAND_AddrInReg: {
            assert(dest.type == OPERAND_Register);
            EmitRexByte(ctx, 1, dest.REGISTER.reg, 0, source.REGISTER.reg);
            Emit8(ctx, opcode);
            EmitIndirect(ctx, dest.REGISTER.reg, source.REGISTER.reg);
        } break;
        case OPERAND_AddrInRegOffset8: {
            assert(dest.type == OPERAND_Register);
            EmitRexByte(ctx, 1, dest.REGISTER.reg, 0, source.REGISTER_OFFSET8.reg);
            Emit8(ctx, opcode);
            EmitIndirectDisplaced8(ctx, dest.REGISTER.reg, source.REGISTER_OFFSET8.reg, source.REGISTER_OFFSET8.offset);
        } break;
        case OPERAND_AddrInRegOffset32: {
            assert(dest.type == OPERAND_Register);
            EmitRexByte(ctx, 1, dest.REGISTER.reg, 0, source.REGISTER_OFFSET32.reg);
            Emit8(ctx, opcode);
            EmitIndirectDisplaced32(ctx, dest.REGISTER.reg, source.REGISTER_OFFSET32.reg, source.REGISTER_OFFSET32.offset);
        } break;
        case OPERAND_SIB: {
            assert(dest.type == OPERAND_Register);
            EmitRexByte(ctx, 1, dest.REGISTER.reg, source.SIB.index, source.SIB.base);
            Emit8(ctx, opcode);
            EmitIndirectSIB(ctx, dest.REGISTER.reg, source.SIB.base, source.SIB.index, source.SIB.scale);
        } break;
        case OPERAND_SIBOffset8: {
            assert(dest.type == OPERAND_Register);
            EmitRexByte(ctx, 1, dest.REGISTER.reg, source.SIB_OFFSET8.index, source.SIB_OFFSET8.base);
            Emit8(ctx, opcode);
            EmitIndirectDisplaced8SIB(ctx, dest.REGISTER.reg, source.SIB_OFFSET8.base, source.SIB_OFFSET8.index, source.SIB_OFFSET8.scale, source.SIB_OFFSET8.offset);
        } break;
        case OPERAND_SIBOffset32: {
            assert(dest.type == OPERAND_Register);
            EmitRexByte(ctx, 1, dest.REGISTER.reg, source.SIB_OFFSET32.index, source.SIB_OFFSET32.base);
            Emit8(ctx, opcode);
            EmitIndirectDisplaced32SIB(ctx, dest.REGISTER.reg, source.SIB_OFFSET32.base, source.SIB_OFFSET32.index, source.SIB_OFFSET32.scale, source.SIB_OFFSET32.offset);
        } break;
        case OPERAND_RIP: {
            assert(dest.type == OPERAND_Register);
            EmitRexByte(ctx, 1, dest.REGISTER.reg, 0, 0);
            Emit8(ctx, opcode);
            EmitIndirectDisplacedRip(ctx, dest.REGISTER.reg, source.RIP.offset);
        } break;
        case OPERAND_AbsoluteAddr: {
            assert(dest.type == OPERAND_Register);
            EmitRexByte(ctx, 1, dest.REGISTER.reg, 0, 0);
            Emit8(ctx, opcode);
            EmitIndirectAbsolute(ctx, dest.REGISTER.reg, source.ABSOLUTE_ADDR.addr);
        } break;
        case OPERAND_Immediate8:
        case OPERAND_Immediate32: {
            // u8 opcode = source.type == OPERAND_Immediate8 ? 0xC6 : 0xC7;
            u8 opcode = 0xC7;
            
            switch(dest.type) {
                case OPERAND_Register: {
                    EmitRexByte(ctx, 1, 0, 0, dest.REGISTER.reg);
                    Emit8(ctx, opcode);
                    EmitDirect(ctx, 0, dest.REGISTER.reg);
                } break;
                case OPERAND_AddrInReg: {
                    EmitRexByte(ctx, 1, 0, 0, dest.REGISTER.reg);
                    Emit8(ctx, opcode);
                    EmitIndirect(ctx, 0, dest.REGISTER.reg);
                } break;
                case OPERAND_AddrInRegOffset8: {
                    EmitRexByte(ctx, 1, 0, 0, dest.REGISTER_OFFSET8.reg);
                    Emit8(ctx, opcode);
                    EmitIndirectDisplaced8(ctx, 0, dest.REGISTER_OFFSET8.reg, dest.REGISTER_OFFSET8.offset);
                } break;
                case OPERAND_AddrInRegOffset32: {
                    EmitRexByte(ctx, 1, 0, 0, dest.REGISTER_OFFSET32.reg);
                    Emit8(ctx, opcode);
                    EmitIndirectDisplaced32(ctx, 0, dest.REGISTER_OFFSET32.reg, dest.REGISTER_OFFSET32.offset);
                } break;
                case OPERAND_SIB: {
                    EmitRexByte(ctx, 1, 0, dest.SIB.index, dest.SIB.base);
                    Emit8(ctx, opcode);
                    EmitIndirectSIB(ctx, 0, dest.SIB.base, dest.SIB.index, dest.SIB.scale);
                } break;
                case OPERAND_SIBOffset8: {
                    EmitRexByte(ctx, 1, 0, dest.SIB_OFFSET8.index, dest.SIB_OFFSET8.base);
                    Emit8(ctx, opcode);
                    EmitIndirectDisplaced8SIB(ctx, 0, dest.SIB_OFFSET8.base, dest.SIB_OFFSET8.index, dest.SIB_OFFSET8.scale, dest.SIB_OFFSET8.offset);
                } break;
                case OPERAND_SIBOffset32: {
                    EmitRexByte(ctx, 1, 0, dest.SIB_OFFSET32.index, dest.SIB_OFFSET32.base);
                    Emit8(ctx, opcode);
                    EmitIndirectDisplaced32SIB(ctx, 0, dest.SIB_OFFSET32.base, dest.SIB_OFFSET32.index, dest.SIB_OFFSET32.scale, dest.SIB_OFFSET32.offset);
                } break;
                case OPERAND_RIP: {
                    EmitRexByte(ctx, 1, 0, 0, 0);
                    Emit8(ctx, opcode);
                    EmitIndirectDisplacedRip(ctx, 0, dest.RIP.offset);
                } break;
                case OPERAND_AbsoluteAddr: {
                    EmitRexByte(ctx, 1, 0, 0, 0);
                    Emit8(ctx, opcode);
                    EmitIndirectAbsolute(ctx, 0, dest.ABSOLUTE_ADDR.addr);
                } break;
            }

            if(source.type == OPERAND_Immediate8) Emit32(ctx, source.IMMEDIATE8.immediate);
            else Emit32(ctx, source.IMMEDIATE32.immediate);
        } break;
    }
}

void gen_push(EmiterContext* ctx, Operand op) {
    switch(op.type) {
        case OPERAND_Register: {
            if(op.REGISTER.reg & 8) EmitRexByte(ctx, 0, 0, 0, op.REGISTER.reg);
            Emit8(ctx, 0x50 | (op.REGISTER.reg & 7));
        } break;
        case OPERAND_AddrInReg: {
            if(op.REGISTER.reg & 8) EmitRexByte(ctx, 0, 0, 0, op.REGISTER.reg);
            Emit8(ctx, 0xFF);
            EmitIndirect(ctx, 6, op.REGISTER.reg);
        } break;
        case OPERAND_AddrInRegOffset8: {
            if(op.REGISTER_OFFSET8.reg & 8) EmitRexByte(ctx, 0, 0, 0, op.REGISTER_OFFSET8.reg);
            Emit8(ctx, 0xFF);
            EmitIndirectDisplaced8(ctx, 6, op.REGISTER_OFFSET8.reg, op.REGISTER_OFFSET8.offset);
        } break;
        case OPERAND_AddrInRegOffset32: {
            if(op.REGISTER_OFFSET32.reg & 8) EmitRexByte(ctx, 0, 0, 0, op.REGISTER_OFFSET32.reg);
            Emit8(ctx, 0xFF);
            EmitIndirectDisplaced32(ctx, 6, op.REGISTER_OFFSET32.reg, op.REGISTER_OFFSET32.offset);
        } break;
        case OPERAND_SIB: {
            if(op.SIB.index & 8 || op.SIB.base & 8) EmitRexByte(ctx, 0, 0, op.SIB.index, op.SIB.base);
            Emit8(ctx, 0xFF);
            EmitIndirectSIB(ctx, 6, op.SIB.base, op.SIB.index, op.SIB.scale);
        } break;
        case OPERAND_SIBOffset8: {
            if(op.SIB_OFFSET8.index & 8 || op.SIB_OFFSET8.base & 8) EmitRexByte(ctx, 0, 0, op.SIB_OFFSET8.index, op.SIB_OFFSET8.base);
            Emit8(ctx, 0xFF);
            EmitIndirectDisplaced8SIB(ctx, 6, op.SIB_OFFSET8.base, op.SIB_OFFSET8.index, op.SIB_OFFSET8.scale, op.SIB_OFFSET8.offset);
        } break;
        case OPERAND_SIBOffset32: {
            if(op.SIB_OFFSET32.index & 8 || op.SIB_OFFSET32.base & 8) EmitRexByte(ctx, 0, 0, op.SIB_OFFSET32.index, op.SIB_OFFSET32.base);
            Emit8(ctx, 0xFF);
            EmitIndirectDisplaced32SIB(ctx, 6, op.SIB_OFFSET32.base, op.SIB_OFFSET32.index, op.SIB_OFFSET32.scale, op.SIB_OFFSET32.offset);
        } break;
        case OPERAND_RIP: {
            Emit8(ctx, 0xFF);
            EmitIndirectDisplacedRip(ctx, 6, op.RIP.offset);
        } break;
        case OPERAND_AbsoluteAddr: {
            Emit8(ctx, 0xFF);
            EmitIndirectAbsolute(ctx, 6, op.ABSOLUTE_ADDR.addr);
        } break;
        case OPERAND_Immediate8: {
            Emit8(ctx, 0x6A);
            Emit8(ctx, op.IMMEDIATE8.immediate);
        } break;
        case OPERAND_Immediate32: {
            Emit8(ctx, 0x68);
            Emit32(ctx, op.IMMEDIATE32.immediate);
        } break;
    }
}

void gen_pop(EmiterContext* ctx, Operand op) {
    assert(op.type != OPERAND_Immediate8  && "Operand type not allowed");
    assert(op.type != OPERAND_Immediate32 && "Operand type not allowed");
    switch(op.type) {
        case OPERAND_Register: {
            if(op.REGISTER.reg & 8) EmitRexByte(ctx, 0, 0, 0, op.REGISTER.reg);
            Emit8(ctx, 0x58 | (op.REGISTER.reg & 7));
        } break;
        case OPERAND_AddrInReg: {
            if(op.REGISTER.reg & 8) EmitRexByte(ctx, 0, 0, 0, op.REGISTER.reg);
            Emit8(ctx, 0x8F);
            EmitIndirect(ctx, 0, op.REGISTER.reg);
        } break;
        case OPERAND_AddrInRegOffset8: {
            if(op.REGISTER_OFFSET8.reg & 8) EmitRexByte(ctx, 0, 0, 0, op.REGISTER_OFFSET8.reg);
            Emit8(ctx, 0x8F);
            EmitIndirectDisplaced8(ctx, 0, op.REGISTER_OFFSET8.reg, op.REGISTER_OFFSET8.offset);
        } break;
        case OPERAND_AddrInRegOffset32: {
            if(op.REGISTER_OFFSET32.reg & 8) EmitRexByte(ctx, 0, 0, 0, op.REGISTER_OFFSET32.reg);
            Emit8(ctx, 0x8F);
            EmitIndirectDisplaced32(ctx, 0, op.REGISTER_OFFSET32.reg, op.REGISTER_OFFSET32.offset);
        } break;
        case OPERAND_SIB: {
            if(op.SIB.index & 8 || op.SIB.base & 8) EmitRexByte(ctx, 0, 0, op.SIB.index, op.SIB.base);
            Emit8(ctx, 0x8F);
            EmitIndirectSIB(ctx, 0, op.SIB.base, op.SIB.index, op.SIB.scale);
        } break;
        case OPERAND_SIBOffset8: {
            if(op.SIB_OFFSET8.index & 8 || op.SIB_OFFSET8.base & 8) EmitRexByte(ctx, 0, 0, op.SIB_OFFSET8.index, op.SIB_OFFSET8.base);
            Emit8(ctx, 0x8F);
            EmitIndirectDisplaced8SIB(ctx, 0, op.SIB_OFFSET8.base, op.SIB_OFFSET8.index, op.SIB_OFFSET8.scale, op.SIB_OFFSET8.offset);
        } break;
        case OPERAND_SIBOffset32: {
            if(op.SIB_OFFSET32.index & 8 || op.SIB_OFFSET32.base & 8) EmitRexByte(ctx, 0, 0, op.SIB_OFFSET32.index, op.SIB_OFFSET32.base);
            Emit8(ctx, 0x8F);
            EmitIndirectDisplaced32SIB(ctx, 0, op.SIB_OFFSET32.base, op.SIB_OFFSET32.index, op.SIB_OFFSET32.scale, op.SIB_OFFSET32.offset);
        } break;
        case OPERAND_RIP: {
            Emit8(ctx, 0x8F);
            EmitIndirectDisplacedRip(ctx, 0, op.RIP.offset);
        } break;
        case OPERAND_AbsoluteAddr: {
            Emit8(ctx, 0x8F);
            EmitIndirectAbsolute(ctx, 0, op.ABSOLUTE_ADDR.addr);
        } break;
    }
}

void gen_call(EmiterContext* ctx, Operand op) {
    assert(op.type != OPERAND_Immediate8 && "Operand type not allowed");
    switch(op.type) {
        case OPERAND_Register: {
            if(op.REGISTER.reg & 8) EmitRexByte(ctx, 0, 0, 0, op.REGISTER.reg);
            Emit8(ctx, 0xFF);
            EmitDirect(ctx, 2, op.REGISTER.reg);
        } break;
        case OPERAND_AddrInReg: {
            if(op.REGISTER.reg & 8) EmitRexByte(ctx, 0, 0, 0, op.REGISTER.reg);
            Emit8(ctx, 0xFF);
            EmitIndirect(ctx, 2, op.REGISTER.reg);
        } break;
        case OPERAND_AddrInRegOffset8: {
            if(op.REGISTER_OFFSET8.reg & 8) EmitRexByte(ctx, 0, 0, 0, op.REGISTER_OFFSET8.reg);
            Emit8(ctx, 0xFF);
            EmitIndirectDisplaced8(ctx, 2, op.REGISTER_OFFSET8.reg, op.REGISTER_OFFSET8.offset);
        } break;
        case OPERAND_AddrInRegOffset32: {
            if(op.REGISTER_OFFSET32.reg & 8) EmitRexByte(ctx, 0, 0, 0, op.REGISTER_OFFSET32.reg);
            Emit8(ctx, 0xFF);
            EmitIndirectDisplaced32(ctx, 2, op.REGISTER_OFFSET32.reg, op.REGISTER_OFFSET32.offset);
        } break;
        case OPERAND_SIB: {
            if(op.SIB.index & 8 || op.SIB.base & 8) EmitRexByte(ctx, 0, 0, op.SIB.index, op.SIB.base);
            Emit8(ctx, 0xFF);
            EmitIndirectSIB(ctx, 2, op.SIB.base, op.SIB.index, op.SIB.scale);
        } break;
        case OPERAND_SIBOffset8: {
            if(op.SIB_OFFSET8.index & 8 || op.SIB_OFFSET8.base & 8) EmitRexByte(ctx, 0, 0, op.SIB_OFFSET8.index, op.SIB_OFFSET8.base);
            Emit8(ctx, 0xFF);
            EmitIndirectDisplaced8SIB(ctx, 2, op.SIB_OFFSET8.base, op.SIB_OFFSET8.index, op.SIB_OFFSET8.scale, op.SIB_OFFSET8.offset);
        } break;
        case OPERAND_SIBOffset32: {
            if(op.SIB_OFFSET32.index & 8 || op.SIB_OFFSET32.base & 8) EmitRexByte(ctx, 0, 0, op.SIB_OFFSET32.index, op.SIB_OFFSET32.base);
            Emit8(ctx, 0xFF);
            EmitIndirectDisplaced32SIB(ctx, 2, op.SIB_OFFSET32.base, op.SIB_OFFSET32.index, op.SIB_OFFSET32.scale, op.SIB_OFFSET32.offset);
        } break;
        case OPERAND_RIP: {
            Emit8(ctx, 0xFF);
            EmitIndirectDisplacedRip(ctx, 2, op.RIP.offset);
        } break;
        case OPERAND_AbsoluteAddr: {
            Emit8(ctx, 0xFF);
            EmitIndirectAbsolute(ctx, 2, op.ABSOLUTE_ADDR.addr);
        } break;
        case OPERAND_Immediate32: {
            Emit8(ctx, 0xE8);
            Emit32(ctx, op.IMMEDIATE32.immediate);
        } break;
    }
}

void gen_ret(EmiterContext* ctx) {
    Emit8(ctx, 0xC3);
}

typedef struct Operand2 {
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
} Operand2;

typedef struct Instruction {
    Mnemonic name;
    Operand2 ops[INSTRUCTION_MAX_OPERANDS];
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
            Operand2 op = inst.ops[h];
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

#define OP_REG(_r_) (Operand){.type = OPERAND_Register, .REGISTER = {.reg = (_r_)}}
#define OP_INDIRECT(_r_) (Operand){.type = OPERAND_AddrInReg, .REGISTER = {.reg = (_r_)}}
#define OP_INDIRECT_OFFSET8(_r_, _disp_) (Operand){.type = OPERAND_AddrInRegOffset8, .REGISTER_OFFSET8 = {.reg = (_r_), .offset = (_disp_)}}
#define OP_INDIRECT_OFFSET32(_r_, _disp_) (Operand){.type = OPERAND_AddrInRegOffset32, .REGISTER_OFFSET32 = {.reg = (_r_), .offset = (_disp_)}}
#define OP_INDIRECT_SIB(_b_, _s_, _i_) (Operand){.type = OPERAND_SIB, .SIB = {.base = (_b_), .scale = (_s_), .index = (_i_)}}
#define OP_INDIRECT_SIB_OFFSET8(_b_, _s_, _i_, _disp_) (Operand){.type = OPERAND_SIBOffset8, .SIB_OFFSET8 = {.base = (_b_), .scale = (_s_), .index = (_i_), .offset = (_disp_)}}
#define OP_INDIRECT_SIB_OFFSET32(_b_, _s_, _i_, _disp_) (Operand){.type = OPERAND_SIBOffset32, .SIB_OFFSET32 = {.base = (_b_), .scale = (_s_), .index = (_i_), .offset = (_disp_)}}
#define OP_RIP(_disp_) (Operand){.type = OPERAND_RIP, .RIP = {.offset = (_disp_)}}
#define OP_ABSOLUTE(_disp_) (Operand){.type = OPERAND_AbsoluteAddr, .ABSOLUTE_ADDR = {.addr = (_disp_)}}
#define OP_IMM8(_imm_) (Operand){.type = OPERAND_Immediate8, .IMMEDIATE8 = {.immediate = (_imm_)}}
#define OP_IMM32(_imm_) (Operand){.type = OPERAND_Immediate32, .IMMEDIATE32 = {.immediate = (_imm_)}}

#define OP2_REG(_r_) \
    (Operand2){.type = OPERAND_Register, .reg = (_r_), .isReg = true}
#define OP2_INDIRECT(_r_) \
    (Operand2){.type = OPERAND_AddrInReg, .reg = (_r_), .isIndirect = true}
#define OP2_INDIRECT_OFFSET8(_r_, _disp_) \
    (Operand2){.type = OPERAND_AddrInRegOffset8, .reg = (_r_), .displacement = (_disp_), .isIndirect = true}
#define OP2_INDIRECT_OFFSET32(_r_, _disp_) \
    (Operand2){.type = OPERAND_AddrInRegOffset32, .reg = (_r_), .displacement = (_disp_), .isIndirect = true}
#define OP2_INDIRECT_SIB(_b_, _s_, _i_) \
    (Operand2){.type = OPERAND_SIB, .base = (_b_), .scale = (_s_), .index = (_i_), .isSIB = true}
#define OP2_INDIRECT_SIB_OFFSET8(_b_, _s_, _i_, _disp_) \
    (Operand2){.type = OPERAND_SIBOffset8, .base = (_b_), .scale = (_s_), .index = (_i_), .displacement = (_disp_), .isSIB = true}
#define OP2_INDIRECT_SIB_OFFSET32(_b_, _s_, _i_, _disp_) \
    (Operand2){.type = OPERAND_SIBOffset32, .base = (_b_), .scale = (_s_), .index = (_i_), .displacement = (_disp_), .isSIB = true}
#define OP2_RIP(_disp_) \
    (Operand2){.type = OPERAND_RIP, .displacement = (_disp_), .isRIPorAbs = true}
#define OP2_ABSOLUTE(_disp_) \
    (Operand2){.type = OPERAND_AbsoluteAddr, .displacement = (_disp_), .isRIPorAbs = true}
#define OP2_IMM8(_imm_) \
    (Operand2){.type = OPERAND_Immediate8, .immediate = (_imm_), .isImm = true}
#define OP2_IMM32(_imm_) \
    (Operand2){.type = OPERAND_Immediate32, .immediate = (_imm_), .isImm = true}

#define INST(_mnemonic_, ...) (Instruction){.name = _mnemonic_##_, .ops = {__VA_ARGS__}}

#define gen_callExtern(_ctx_, _name_) gen_callExtern_(_ctx_, _name_, strlen(_name_))
void gen_callExtern_(EmiterContext* ctx, u8* name, u64 size) {
    // 8 + 8 + 32 bits pushed to the ctx, last 32 are the address
    genInstruction(ctx, INST(call, OP2_RIP(0xDEADBEEF)));
    int offset = ctx->code.size - 4;
    NamesToPatch* foo = buffer_allocate(&ctx->names, NamesToPatch);
    foo->name = name;
    foo->len = size;
    foo->offset = offset;
}

#define TEST_IN_MEM_EXECUTION 1
#define TEST_EXE_GENERATION 0
#define TEST_INSTRUCION_ENCODING 0

#include "tests.c"

int main(int argc, char** argv) {
    EmiterContext ctx = {0};
    ctx.code = make_buffer(0x200, PAGE_READWRITE);
    ctx.names = make_buffer(0x200, PAGE_READWRITE);

    #if TEST_IN_MEM_EXECUTION
    ctx.code = make_buffer(MiB(1), PAGE_EXECUTE_READWRITE);
    
    genInstruction(&ctx, INST(push, OP2_REG(RBP)));
    genInstruction(&ctx, INST(mov, OP2_REG(RBP), OP2_REG(RSP)));
    
    genInstruction(&ctx, INST(mov, OP2_REG(RAX), OP2_REG(RCX)));
    // genInstruction(&ctx, INST(add, OP2_REG(RAX), OP2_REG(RDX)));

    genInstruction(&ctx, INST(mov, OP2_REG(RSP), OP2_REG(RBP)));
    genInstruction(&ctx, INST(pop, OP2_REG(RBP)));
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
    genInstruction(&ctx, INST(push, OP2_REG(RBP)));
    genInstruction(&ctx, INST(mov, OP2_REG(RBP), OP2_REG(RSP)));
    
    // STR stack[0] = GetCommandLineA()
    gen_callExtern(&ctx, "GetCommandLineA");
    genInstruction(&ctx, INST(push, OP2_REG(RAX))); // store GetCommandLineA return address
    
    // HANDLE stack[1] = GetStdHandle(STD_OUTPUT_HANDLE)
    genInstruction(&ctx, INST(mov, OP2_REG(RCX), OP2_IMM32(STD_OUTPUT_HANDLE)));
    gen_callExtern(&ctx, "GetStdHandle");
    genInstruction(&ctx, INST(push, OP2_REG(RAX))); // store GetStdHandle return handle
    
    // WriteFile(stack[1], stack[0], 40, 0, 0)
    genInstruction(&ctx, INST(pop, OP2_REG(RCX))); // pop to rcx, first arg
    genInstruction(&ctx, INST(pop, OP2_REG(RDX))); // pop to rdx, second arg
    genInstruction(&ctx, INST(mov, OP2_REG(R8), OP2_IMM8(40))); // str len 40, third arg
    genInstruction(&ctx, INST(mov, OP2_REG(R9), OP2_IMM8(0))); // NULL for bytesWrittenPtr, fourth arg
    genInstruction(&ctx, INST(push, OP2_IMM8(0))); // NULL, fifth arg
    gen_callExtern(&ctx, "WriteFile");
    
    // Function epilog
    genInstruction(&ctx, INST(mov, OP2_REG(RSP), OP2_REG(RBP)));
    genInstruction(&ctx, INST(pop, OP2_REG(RBP)));
    
    // ExitProcess(0);
    genInstruction(&ctx, INST(mov, OP2_REG(RCX), OP2_IMM8(0)));
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

    GEN_TEST(OldAdd);
    GEN_TEST(NewAdd);
    GEN_TEST(OldMov);
    GEN_TEST(NewMov);
    GEN_TEST(OldPush);
    GEN_TEST(NewPush);
    GEN_TEST(OldPop);
    GEN_TEST(NewPop);
    GEN_TEST(OldCall);
    GEN_TEST(NewCall);
    #endif // TEST_INSTRUCION_ENCODING

    return 0;
}

// NOTE: delayed register allocations
