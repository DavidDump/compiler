// Code generator test for x86_64 intel bytecode

#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <assert.h>

// #define ARENA_IMPLEMENTATION
// #include "../src/arena.h"

typedef int8_t  s8;
typedef int16_t s16;
typedef int32_t s32;
typedef int64_t s64;

typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

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

#define KiB(x)   ((x) * 1024)
#define MiB(x) (KB(x) * 1024)
#define GiB(x) (MB(x) * 1024)

#define KB(x)   ((x) * 1000)
#define MB(x) (KB(x) * 1000)
#define GB(x) (MB(x) * 1000)

typedef struct EmiterContext {
    u8* code;
    int count;
    int capacity;
} EmiterContext;

void AppendCode(EmiterContext* ctx, u8 code) {
    if(ctx->count >= ctx->capacity){
        unsigned int newCapacity = ctx->capacity * 2;
        if(newCapacity == 0) newCapacity = 32;
        ctx->code = realloc(ctx->code, newCapacity);
        ctx->capacity = newCapacity;
    }
    ctx->code[ctx->count++] = code;
}

void Emit8(EmiterContext* ctx, u8 data) {
    AppendCode(ctx, data);
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
// the top half of is always set to 0b0100
// in 64 bit mode the W bit is always set, so its safe to consider the base to be 0x48
// if W = 1, the RXB bits are used to extend the ModR/M and SIB bytes
// if W = 0, code segment is used to determine if the RXB bits are used
// R bit is used to extend the reg field of the ModR/M byte
// X bit is used to extend the index field of the SIB byte
// B bit is used to extend the rm field of the ModR/M, base field of the SIB byte or reg field of the ModR/M byte if it is used as an opcode
// ------------------------|-----|-----|-----|------
// |  0  |  1  |  0  |  0  |  W  |  R  |  X  |  B  |
// ------------------------|-----|-----|-----|------

void EmitRexByte(EmiterContext* ctx, Register r, Register x, Register b) {
    assert(r < 16); // 1 bit number, but 16 registers, only the top bit is extracted
    assert(x < 16); // 1 bit number, but 16 registers, only the top bit is extracted
    assert(b < 16); // 1 bit number, but 16 registers, only the top bit is extracted
    Emit8(ctx, 0x48 | ((r >> 3) << 2) | ((x >> 3) << 1) | ((b >> 3) << 0));
}

// ModR/M and SIB byte emiters

// op rax, rcx
// rx = RAX, reg = RCX
void EmitDirect(EmiterContext* ctx, u8 rx, Register reg) {
    EmitModRMByte(ctx, DIRECT, rx, reg);
}

// op rax, [rcx]
// rx = RAX, base = RCX
void EmitIndirect(EmiterContext* ctx, u8 rx, Register reg) {
    assert((reg & 7) != RSP);
    assert((reg & 7) != RBP);
    EmitModRMByte(ctx, INDIRECT_NO_DISPLACE, rx, reg);
}

// op rax, [rcx + 0x12]
// rx = RAX, base = RCX, displacement = 0x12
void EmitIndirectDisplaced8(EmiterContext* ctx, u8 rx, Register base, u8 displacement) {
    assert((base & 7) != RSP);
    EmitModRMByte(ctx, INDIRECT_08_DISPLACE, rx, base);
    Emit8(ctx, displacement);
}

// op rax, [rcx + 0x12345678]
// rx = RAX, base = RCX, displacement = 0x12345678
void EmitIndirectDisplaced32(EmiterContext* ctx, u8 rx, Register base, u32 displacement) {
    assert((base & 7) != RSP);
    EmitModRMByte(ctx, INDIRECT_32_DISPLACE, rx, base);
    Emit32(ctx, displacement);
}

// op rax, [rcx + 4*rdx]
// rx = RAX, base = RCX, index = RDX, scale = X4
void EmitIndirectSIB(EmiterContext* ctx, u8 rx, Register base, Register index, Scale scale) {
    assert((base & 7) != RBP);
    EmitModRMByte(ctx, INDIRECT_NO_DISPLACE, rx, RSP);
    EmitSIBByte(ctx, scale, index, base);
}

// op rax, [rcx + 4*rdx + 0x12]
// rx = RAX, base = RCX, index = RDX, scale = X4, displacement = 0x12
void EmitIndirectDisplaced8SIB(EmiterContext* ctx, u8 rx, Register base, Register index, Scale scale, u8 displacement) {
    EmitModRMByte(ctx, INDIRECT_08_DISPLACE, rx, RSP);
    EmitSIBByte(ctx, scale, index, base);
    Emit8(ctx, displacement);
}

// op rax, [rcx + 4*rdx + 0x12345678]
// rx = RAX, base = RCX, index = RDX, scale = X4, displacement = 0x12345678
void EmitIndirectDisplaced32SIB(EmiterContext* ctx, u8 rx, Register base, Register index, Scale scale, u32 displacement) {
    EmitModRMByte(ctx, INDIRECT_32_DISPLACE, rx, RSP);
    EmitSIBByte(ctx, scale, index, base);
    Emit32(ctx, displacement);
}

// op rax, [rip + 0x12345678]
// rx = RAX, displacement = 0x12345678
void EmitIndirectDisplacedRip(EmiterContext* ctx, u8 rx, s32 displacement) {
    EmitModRMByte(ctx, INDIRECT_NO_DISPLACE, rx, RBP);
    Emit32(ctx, displacement);
}

// op rax, [0x12345678]
// rx = RAX, displacement = 0x12345678
void EmitIndirectAbsolute(EmiterContext* ctx, u8 rx, int32_t displacement) {
    EmitModRMByte(ctx, INDIRECT_NO_DISPLACE, rx, RSP);
    EmitSIBByte(ctx, X0, RSP, RBP);
    Emit32(ctx, displacement);
}

// --------------------------------------------------

typedef enum OperandType {
    OPERAND_NULL,
    OPERAND_REGISTER,
    OPERAND_IMMEDIATE,
    OPERAND_ADDRESS,
    // OPERAND_FRAME_OFFSET,
} OperandType;

typedef struct Operand {
    OperandType type;
    union {
        Register REGISTER;
        u32 IMMEDIATE;
        u32 ADDRESS;
    };
} Operand;

// asm: add lhs, rhs
void gen_add(Operand lhs, Operand rhs);

int main() {
    FILE* f = fopen("test.bin", "wb");

    // mov rax, rbx
    // Emit8(0x48);
    // Emit8(0x8b);
    // Emit8(0xc3);

    EmiterContext ctx = {0};

    for(Register dest = RAX; dest <= R15; dest++){
        for(Register source = RAX; source <= R15; source++){
            EmitRexByte(&ctx, 0, 0, 0);
            Emit8(&ctx, 0x8b);
            EmitDirect(&ctx, dest, source);
            
            if((source & 7) != RSP && (source & 7) != RBP){
                EmitRexByte(&ctx, 0, 0, 0);
                Emit8(&ctx, 0x8b);
                EmitIndirect(&ctx, dest, source);
            }
            
            if((source & 7) != RSP){
                EmitRexByte(&ctx, 0, 0, 0);
                Emit8(&ctx, 0x8b);
                EmitIndirectDisplaced8(&ctx, dest, source, 0x12);
                
                EmitRexByte(&ctx, 0, 0, 0);
                Emit8(&ctx, 0x8b);
                EmitIndirectDisplaced32(&ctx, dest, source, 0x1234);
            }
            
            for(Scale scale = X0; scale <= X8; scale++){
                if((source & 7) != RBP){
                    EmitRexByte(&ctx, 0, 0, 0);
                    Emit8(&ctx, 0x8b);
                    EmitIndirectSIB(&ctx, dest, source, dest, scale);
                }

                EmitRexByte(&ctx, 0, 0, 0);
                Emit8(&ctx, 0x8b);
                EmitIndirectDisplaced8SIB(&ctx, dest, source, dest, scale, 0x12);
                
                EmitRexByte(&ctx, 0, 0, 0);
                Emit8(&ctx, 0x8b);
                EmitIndirectDisplaced32SIB(&ctx, dest, source, dest, scale, 0x1234);
            }
            
            EmitRexByte(&ctx, 0, 0, 0);
            Emit8(&ctx, 0x8b);
            EmitIndirectDisplacedRip(&ctx, dest, 0x1234);
            
            EmitRexByte(&ctx, 0, 0, 0);
            Emit8(&ctx, 0x8b);
            EmitIndirectAbsolute(&ctx, dest, 0x1234);
        }
    }

    fwrite(ctx.code, sizeof(*ctx.code), ctx.count, f);
    fclose(f);
    return 0;
}

// rax
// [rax]
// [rax + 0x12]
// [rax + 0x1234]
// [base + scale * index]
// [base + scale * index + 0x12]
// [base + scale * index + 0x1234]
// [RIP + 0x1234]
// [0x1234]
