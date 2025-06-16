#include "bytecode_x86.h"
#include "parser.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

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
void genModRMByte(GenContext* ctx, AddressingMode mod, Register reg, Register rm) {
    assert(mod < 4, "");  // 2 bit number
    assert(reg < 16, ""); // 3 bit number, but 16 registers, the top bit is encoded in REX byte
    assert(rm < 16, "");  // 3 bit number, but 16 registers, the top bit is encoded in REX byte
    u8 modRmByte = ((mod & 3) << 6) | ((reg & 7) << 3) | ((rm & 7) << 0);
    ArrayAppend(ctx->code, modRmByte);
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
void genSIBByte(GenContext* ctx, Scale scale, Register index, Register base) {
    assert(scale < 4, "");  // 2 bit number
    assert(index < 16, ""); // 3 bit number, but 16 registers, the top bit is encoded in REX byte
    assert(base < 16, "");  // 3 bit number, but 16 registers, the top bit is encoded in REX byte
    u8 sibByte = ((scale & 3) << 6) | ((index & 7) << 3) | ((base & 7) << 0);
    ArrayAppend(ctx->code, sibByte);
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
void genRexByte(GenContext* ctx, u8 w, Register r, Register x, Register b) {
    assert(w == 0 || w == 1, "");
    assert(r < 16, ""); // 1 bit number, but 16 registers, only the top bit is extracted
    assert(x < 16, ""); // 1 bit number, but 16 registers, only the top bit is extracted
    assert(b < 16, ""); // 1 bit number, but 16 registers, only the top bit is extracted
    u8 rexByte = 0x40 | (w << 3) | ((r >> 3) << 2) | ((x >> 3) << 1) | ((b >> 3) << 0);
    ArrayAppend(ctx->code, rexByte);
}

void Emit8(GenContext* ctx, u8 data) {
    ArrayAppend(ctx->code, data);
}

void Emit16(GenContext* ctx, u16 data) {
    Emit8(ctx, (data >> 0) & 0xFF);
    Emit8(ctx, (data >> 8) & 0xFF);
}

void Emit32(GenContext* ctx, u32 data) {
    Emit8(ctx, (data >> 0)  & 0xFF);
    Emit8(ctx, (data >> 8)  & 0xFF);
    Emit8(ctx, (data >> 16) & 0xFF);
    Emit8(ctx, (data >> 24) & 0xFF);
}

void Emit64(GenContext* ctx, u64 data) {
    Emit8(ctx, (data >> 0)  & 0xFF);
    Emit8(ctx, (data >> 8)  & 0xFF);
    Emit8(ctx, (data >> 16) & 0xFF);
    Emit8(ctx, (data >> 24) & 0xFF);
    Emit8(ctx, (data >> 32) & 0xFF);
    Emit8(ctx, (data >> 40) & 0xFF);
    Emit8(ctx, (data >> 48) & 0xFF);
    Emit8(ctx, (data >> 56) & 0xFF);
}

//
// ModR/M and SIB byte emiters
//

// op rax, rcx
// reg = RAX, rm = RCX
void EmitDirect(GenContext* ctx, Register reg, Register rm) {
    genModRMByte(ctx, DIRECT, reg, rm);
}

// op rax, [rcx]
// reg = RAX, rm = RCX
void EmitIndirect(GenContext* ctx, Register reg, Register rm) {
    assert((reg & 7) != RSP, "");
    assert((reg & 7) != RBP, "");
    genModRMByte(ctx, INDIRECT_NO_DISPLACE, reg, rm);
}

// op rax, [rcx + 0x12]
// reg = RAX, rm = RCX, displacement = 0x12
void EmitIndirectDisplaced8(GenContext* ctx, Register reg, Register rm, u8 displacement) {
    assert((rm & 7) != RSP, "");
    genModRMByte(ctx, INDIRECT_08_DISPLACE, reg, rm);
    Emit8(ctx, displacement);
}

// op rax, [rcx + 0x12345678]
// reg = RAX, rm = RCX, displacement = 0x12345678
void EmitIndirectDisplaced32(GenContext* ctx, u8 reg, Register rm, u32 displacement) {
    assert((rm & 7) != RSP, "");
    genModRMByte(ctx, INDIRECT_32_DISPLACE, reg, rm);
    Emit32(ctx, displacement);
}

// op rax, [rcx + 4*rdx]
// reg = RAX, rm = RCX, index = RDX, scale = X4
void EmitIndirectSIB(GenContext* ctx, u8 reg, Register rm, Register index, Scale scale) {
    assert((rm & 7) != RBP, "");
    genModRMByte(ctx, INDIRECT_NO_DISPLACE, reg, RSP);
    genSIBByte(ctx, scale, index, rm);
}

// op rax, [rcx + 4*rdx + 0x12]
// reg = RAX, rm = RCX, index = RDX, scale = X4, displacement = 0x12
void EmitIndirectDisplaced8SIB(GenContext* ctx, u8 reg, Register rm, Register index, Scale scale, u8 displacement) {
    genModRMByte(ctx, INDIRECT_08_DISPLACE, reg, RSP);
    genSIBByte(ctx, scale, index, rm);
    Emit8(ctx, displacement);
}

// op rax, [rcx + 4*rdx + 0x12345678]
// reg = RAX, rm = RCX, index = RDX, scale = X4, displacement = 0x12345678
void EmitIndirectDisplaced32SIB(GenContext* ctx, u8 reg, Register rm, Register index, Scale scale, u32 displacement) {
    genModRMByte(ctx, INDIRECT_32_DISPLACE, reg, RSP);
    genSIBByte(ctx, scale, index, rm);
    Emit32(ctx, displacement);
}

// op rax, [rip + 0x12345678]
// reg = RAX, displacement = 0x12345678
void EmitIndirectDisplacedRip(GenContext* ctx, u8 reg, u32 displacement) {
    genModRMByte(ctx, INDIRECT_NO_DISPLACE, reg, RBP);
    Emit32(ctx, displacement);
}

// op rax, [0x12345678]
// reg = RAX, displacement = 0x12345678
void EmitIndirectAbsolute(GenContext* ctx, u8 reg, u32 displacement) {
    genModRMByte(ctx, INDIRECT_NO_DISPLACE, reg, RSP);
    genSIBByte(ctx, X0, RSP, RBP);
    Emit32(ctx, displacement);
}

bool isZeroStruct(InstructionEncoding* enc) {
    InstructionEncoding* testValBuffer = &(InstructionEncoding){0};
    u64 size = sizeof(InstructionEncoding);
    return memcmp(enc, testValBuffer, size) == 0;
}

bool checkTypesMatch(OperandType operandType, OpType encodingOperandType) {
    switch(encodingOperandType) {
        case OpType_NONE: {
            if(operandType != OPERAND_NONE) return FALSE;
        } break;
        case OpType_REG: {
            if(operandType != OPERAND_Register) return FALSE;
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
            )) return FALSE;
        } break;
        case OpType_IMM8: {
            if(operandType != OPERAND_Immediate8) return FALSE;
        } break;
        case OpType_IMM16: {
            if(!(
                operandType == OPERAND_Immediate8 ||
                operandType == OPERAND_Immediate16
            )) return FALSE;
        } break;
        case OpType_IMM32: {
            if(!(
                operandType == OPERAND_Immediate8 ||
                operandType == OPERAND_Immediate16 ||
                operandType == OPERAND_Immediate32
            )) return FALSE;
        } break;
        case OpType_IMM64: {
            if(!(
                operandType == OPERAND_Immediate8 ||
                operandType == OPERAND_Immediate16 ||
                operandType == OPERAND_Immediate32 ||
                operandType == OPERAND_Immediate64
            )) return FALSE;
        } break;
    }
    return TRUE;
}

void genInstruction(GenContext* ctx, Instruction inst) {
    InstructionEncoding* instructionEncodings = encodings[inst.name];
    for(u64 i = 0; i < MAX_ENCODING_FOR_INSTRUCTION; ++i) {
        InstructionEncoding encoding = instructionEncodings[i];
        if(isZeroStruct(&encoding)) break;
        if(!checkTypesMatch(inst.ops[0].type, encoding.opTypes[0]) || !checkTypesMatch(inst.ops[1].type, encoding.opTypes[1])) continue;
        if(encoding.type != inst.type) continue;

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
        for(u8 h = 0; h < INSTRUCTION_MAX_OPERANDS; h++) {
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
                        
                        // NOTE: just skip these, they are here for completeness sake, to get compiler warnings
                        case OPERAND_NONE:
                        case OPERAND_COUNT:
                        case OPERAND_Immediate8:
                        case OPERAND_Immediate16:
                        case OPERAND_Immediate32:
                        case OPERAND_Immediate64:
                            break;
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
        if(encoding.rexType != RexByte_NONE && (rexW || (rexR & 8) || (rexX & 8) || (rexB & 8))) genRexByte(ctx, rexW, rexR, rexX, rexB);

        // Opcode
        // NOTE: multibyte opcodes are emited like this because of how they are encoded, for an example see conditional jmp instructions ex.: jne
        if(encoding.opcode & 0xFF000000000000) Emit8(ctx, (encoding.opcode >> (7 * 8)) & 0xFF);
        if(encoding.opcode & 0x00FF0000000000) Emit8(ctx, (encoding.opcode >> (6 * 8)) & 0xFF);
        if(encoding.opcode & 0x0000FF00000000) Emit8(ctx, (encoding.opcode >> (5 * 8)) & 0xFF);
        if(encoding.opcode & 0x000000FF000000) Emit8(ctx, (encoding.opcode >> (4 * 8)) & 0xFF);
        if(encoding.opcode & 0x000000FF000000) Emit8(ctx, (encoding.opcode >> (3 * 8)) & 0xFF);
        if(encoding.opcode & 0x00000000FF0000) Emit8(ctx, (encoding.opcode >> (2 * 8)) & 0xFF);
        if(encoding.opcode & 0x0000000000FF00) Emit8(ctx, (encoding.opcode >> (1 * 8)) & 0xFF);
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

                // NOTE: just skip these, they are here for completeness sake, to get compiler warnings
                case OPERAND_NONE:
                case OPERAND_COUNT:
                case OPERAND_Immediate8:
                case OPERAND_Immediate16:
                case OPERAND_Immediate32:
                case OPERAND_Immediate64:
                    break;
            }
        }

        // Immediates
        if(immOpIndex < INSTRUCTION_MAX_OPERANDS) {
            switch(encoding.opTypes[immOpIndex]) {
                case OpType_IMM8:  Emit8(ctx, inst.ops[immOpIndex].immediate); break;
                case OpType_IMM16: Emit16(ctx, inst.ops[immOpIndex].immediate); break;
                case OpType_IMM32: Emit32(ctx, inst.ops[immOpIndex].immediate); break;
                case OpType_IMM64: Emit64(ctx, inst.ops[immOpIndex].immediate); break;
                default: break;
            }
        }
        
        return;
    }
    assertf(FALSE, "Could not find encoding for instruction: %s %s %s", MnemonicStr[inst.name], OperandTypeStr[inst.ops[0].type], OperandTypeStr[inst.ops[1].type]);
}

void gen_callExtern(GenContext* ctx, String name) {
    // 8 + 8 + 32 bits pushed to the ctx, last 32 are the address
    genInstruction(ctx, INST(call, OP_RIP(0xDEADBEEF)));
    u64 offset = ctx->code.size - 4;
    AddrToPatch patch = {
        .name = name,
        .offset = offset,
    };
    ArrayAppend(ctx->externalsToPatch, patch);
}

void gen_call(GenContext* ctx, String name) {
    // 8 + 8 + 32 bits pushed to the ctx, last 32 are the address
    genInstruction(ctx, INST(call, OP_IMM32(0xDEADBEEF)));
    u64 offset = ctx->code.size - 4;
    AddrToPatch patch = {
        .name = name,
        .offset = offset,
    };
    ArrayAppend(ctx->internalsToPatch, patch);
}

// lea rax, [rip + 0xdeadbeef + sizeof(u64)] ; load data
// mov rax, [rip + 0xdeadbeef]               ; load size
// read the data field of the entry into reg register
void gen_DataInReg(GenContext* ctx, Register reg, String name) {
    // NOTE: the rip address get added to the patched address,
    // so adding the size of u64 offsets the address by 8,
    // meaning instead of reading from the data begining, where the size is stored,
    // instead it reads the data stored in the entry
    genInstruction(ctx, INST(lea, OP_REG(reg), OP_RIP(sizeof(u64))));
    u64 offset = ctx->code.size - 4;
    AddrToPatch patch = {
        .name = name,
        .offset = offset,
    };
    ArrayAppend(ctx->dataToPatch, patch);
}

// read the size field of the entry into reg register
void gen_SizeInReg(GenContext* ctx, Register reg, String name) {
    // NOTE: the rip address get added to the patched address,
    // the address gets added to 0 so in this case we read the size field of the entry
    genInstruction(ctx, INST(mov, OP_REG(reg), OP_RIP(0)));
    u64 offset = ctx->code.size - 4;
    AddrToPatch patch = {
        .name = name,
        .offset = offset,
    };
    ArrayAppend(ctx->dataToPatch, patch);
}

// 
// TODO: the above section of the file should be in bytecode_x64.c
// TODO: the below section of the file should be in codegen.c
// 

char* OperandTypeStr[OPERAND_COUNT] = {
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

// NOTE: not tested
u64 align(u64 num, u64 alignment) {
    if(num % alignment == 0) return num;
    return (num + alignment -1) & ~(alignment -1);
}

void genPush(GenContext* ctx, GenScope* scope, Register reg) {
    scope->stackPointer++;
    genInstruction(ctx, INST(push, OP_REG(reg)));
}

void genPop(GenContext* ctx, GenScope* scope, Register reg) {
    scope->stackPointer--;
    genInstruction(ctx, INST(pop, OP_REG(reg)));
}

typedef struct SymbolLocationResult {
    bool err;
    bool inDataSection;
    s64 value;
} SymbolLocationResult;

SymbolLocationResult findSymbolLocation(GenScope* scope, String name) {
    SymbolLocationResult result = {0};
    GenScope* at = scope;
    while(at) {
        if(HashmapGet(String, s64)(&at->localVars, name, &result.value)) {
            if(at->parent == NULL) result.inDataSection = TRUE;
            return result;
        }
        at = at->parent;
    }

    result.err = TRUE;
    return result;
}

TypeInfo* findFunctionType(GenScope* localScope, String id) {
    ConstValue result = {0};
    GenScope* it = localScope;
    while(it) {
        if(HashmapGet(String, ConstValue)(it->functionsDefinedInThisScope, id, &result)) return result.typeInfo;
        it = it->parent;
    }

    UNREACHABLE_VA("function has no type: "STR_FMT, STR_PRINT(id));
    return 0; // silence warninig
}

void gen_x86_64_cast(GenContext* ctx, TypecheckedExpression* lhs, TypecheckedExpression* rhs, GenScope* localScope) {
    UNUSED(ctx);
    UNUSED(lhs);
    UNUSED(rhs);
    UNUSED(localScope);
    UNIMPLEMENTED("gen_x86_64_cast");
    #if 0
    assert(lhs->type == ExpressionType_INT_LIT || lhs->type == ExpressionType_SYMBOL, "Can only cast integers");
    assert(rhs->type == ExpressionType_TYPE, "Can only cast to a type");

    if(lhs->type == ExpressionType_INT_LIT) {
        String value = lhs->expr.INT_LIT.value;

        u64 intValue = 0;
        if(TypeIsSigned(lhs->typeInfo)) intValue = (u64)StringToS64(value);
        else if(TypeIsUnsigned(lhs->typeInfo)) intValue = StringToU64(value);

        Instruction inst = {0};
        Type symbolType = rhs->expr.TYPE.typeInfo->symbolType;
        if(0);
        else if(symbolType == TYPE_S8 || symbolType == TYPE_U8) inst = INST(mov, OP_REG(RAX), OP_IMM8(intValue));
        else if(symbolType == TYPE_S16 || symbolType == TYPE_U16) inst = INST(mov, OP_REG(RAX), OP_IMM16(intValue));
        else if(symbolType == TYPE_S32 || symbolType == TYPE_U32) inst = INST(mov, OP_REG(RAX), OP_IMM32(intValue));
        else if(symbolType == TYPE_S64 || symbolType == TYPE_U64) inst = INST(mov, OP_REG(RAX), OP_IMM64(intValue));
        genInstruction(ctx, inst);
    } else if(lhs->type == ExpressionType_SYMBOL) {
        String id = lhs->expr.SYMBOL.identifier;

        SymbolLocationResult res = findSymbolLocation(localScope, id);
        if(res.err) {
            UNREACHABLE_VA("Symbol not defined: "STR_FMT, STR_PRINT(id));
        }

        if(res.inDataSection) {
            gen_DataInReg(ctx, RAX, id);
        } else {
            Instruction inst = {0};
            Type lhsSymbolType = lhs->typeInfo->symbolType;
            if(lhsSymbolType == TYPE_S8 || lhsSymbolType == TYPE_U8 || lhsSymbolType == TYPE_S16 || lhsSymbolType == TYPE_U16) {
                inst = INST(movzx, OP_REG(RAX), OP_INDIRECT_OFFSET32(RBP, res.value));
            } else {
                inst = INST(mov, OP_REG(RAX), OP_INDIRECT_OFFSET32(RBP, res.value));
            }

            Type rhsSymbolType = rhs->expr.TYPE.typeInfo->symbolType;
            if(0);
            else if(rhsSymbolType == TYPE_S8 || rhsSymbolType == TYPE_U8) inst.type = InstructionType_8BIT;
            else if(rhsSymbolType == TYPE_S16 || rhsSymbolType == TYPE_U16) inst.type = InstructionType_16BIT;
            else if(rhsSymbolType == TYPE_S32 || rhsSymbolType == TYPE_U32) inst.type = InstructionType_32BIT;
            else if(rhsSymbolType == TYPE_S64 || rhsSymbolType == TYPE_U64) inst.type = InstructionType_64BIT;
            genInstruction(ctx, inst);
        }
    }
    #endif
}

TypeInfo* genParsedTypeToTypeInfo(ParsedType* type) {
    assert(type->type == ParsedTypeType_SIMPLE, "Can only convert ParsedTypeType_SIMPLE to TypeInfo");
    return type->as_simple.typeInfo;
}

typedef struct StructFieldResult {
    u64 result;
    bool error;
} StructFieldResult;

StructFieldResult getStructFieldOffsetByName(TypeInfo* typeInfo, String fieldName) {
    assert(typeInfo->symbolType == TYPE_STRUCT_DEF, "TYPE_STRUCT_DEF should be the type of a struct lit");

    StructFieldResult result = {0};
    Array(TypecheckedField) fields = typeInfo->structInfo.fields;
    for(u64 i = 0; i < fields.size; ++i) {
        TypecheckedField field = fields.data[i];
        if(StringEquals(field.id, fieldName)) return result;
        TypeInfo* fieldType = field.type;
        result.result += TypeToByteSize(fieldType);
    }

    result.error = TRUE;
    return result;
}

// index is 0 indexed
StructFieldResult getStructFieldOffsetByPos(TypeInfo* typeInfo, u64 index) {
    assert(typeInfo->symbolType == TYPE_STRUCT_DEF, "TYPE_STRUCT_DEF should be the type of a struct lit");

    StructFieldResult result = {0};
    Array(TypecheckedField) fields = typeInfo->structInfo.fields;
    for(u64 i = 0; i < fields.size; ++i) {
        TypecheckedField field = fields.data[i];
        if(i == index) return result;
        TypeInfo* fieldType = field.type;
        result.result += TypeToByteSize(fieldType);
    }

    result.error = TRUE;
    return result;
}

typedef struct VariableTypeResult {
    TypeInfo* value;
    bool error;
} VariableTypeResult;

VariableTypeResult getVariableType(GenScope* scope, String id) {
    VariableTypeResult result = {0};
    GenScope* it = scope;
    while(it) {
        if(HashmapGet(String, TypeInfoPtr)(&it->variableTypes, id, &result.value)) return result;
        it = it->parent;
    }

    result.error = TRUE;
    return result;
}

void gen_x86_64_expression(GenContext* ctx, TypecheckedExpression* expr, GenScope* localScope) {
    // TODO: change this to a switch
    if(expr->type == ExpressionType_INT_LIT) {
        String value = expr->expr.INT_LIT.value;

        u64 intValue = 0;
        if(TypeIsSigned(expr->typeInfo)) intValue = (u64)StringToS64(value);
        else if(TypeIsUnsigned(expr->typeInfo)) intValue = StringToU64(value);

        genInstruction(ctx, INST(mov, OP_REG(RAX), OP_IMM64(intValue)));
    } else if(expr->type == ExpressionType_FLOAT_LIT) {
        UNIMPLEMENTED("expression generation with floats");
    } else if(expr->type == ExpressionType_STRING_LIT) {
        // TODO: this branch is implemented in a very scuffed way,
        //       only here because i needed a quick working version
        //       potential optimization is to collect all the string literals
        //       and if two match only store one in the data section
        //       is it a problem to use the string lit as a key????
        String lit = expr->expr.STRING_LIT.value;

        UserDataEntry value = {
            .data = &lit.str[1],
            .dataLen = lit.length - 2,
            .dataRVA = INVALID_ADDRESS,
        };
        if(!HashmapSet(String, UserDataEntry)(&ctx->dataSection, lit, value)) {
            UNREACHABLE_VA("failed to insert into hashmap, cap: %llu, count: %llu", ctx->dataSection.capacity, ctx->dataSection.size);
        }
        gen_DataInReg(ctx, RAX, lit);
    } else if(expr->type == ExpressionType_BOOL_LIT) {
        UNIMPLEMENTED("expression generation with bools");
    } else if(expr->type == ExpressionType_SYMBOL) {
        String id = expr->expr.SYMBOL.identifier;

        SymbolLocationResult res = findSymbolLocation(localScope, id);
        if(res.err) {
            UNREACHABLE_VA("Symbol not defined: "STR_FMT, STR_PRINT(id));
        }

        if(res.inDataSection) {
            gen_DataInReg(ctx, RAX, id);
        } else {
            genInstruction(ctx, INST(mov, OP_REG(RAX), OP_INDIRECT_OFFSET32(RBP, res.value)));
        }
    } else if(expr->type == ExpressionType_FUNCTION_CALL) {
        String id = expr->expr.FUNCTION_CALL.identifier;
        Array(TypecheckedExpressionPtr) args = expr->expr.FUNCTION_CALL.args;

        for(u64 i = 0; i < args.size; ++i) {
            TypecheckedExpression* arg = args.data[i];

            gen_x86_64_expression(ctx, arg, localScope);
            if(i == 0) {
                genInstruction(ctx, INST(mov, OP_REG(RCX), OP_REG(RAX)));
            } else if(i == 1) {
                genInstruction(ctx, INST(mov, OP_REG(RDX), OP_REG(RAX)));
            } else if(i == 2) {
                genInstruction(ctx, INST(mov, OP_REG(R8), OP_REG(RAX)));
            } else if(i == 3) {
                genInstruction(ctx, INST(mov, OP_REG(R9), OP_REG(RAX)));
            } else {
                genPush(ctx, localScope, RAX);
            }
        }

        TypeInfo* fnType = findFunctionType(localScope, id);
        assert(fnType->symbolType == TYPE_FUNCTION, "");
        if(fnType->functionInfo->isExternal) {
            // NOTE: "In the Microsoft x64 calling convention, it is the caller's responsibility to allocate 32 bytes of "shadow space"
            //       on the stack right before calling the function, and to pop the stack after the call."
            //       -wikipedia, https://en.wikipedia.org/wiki/X86_calling_conventions#x86-64_calling_conventions
            genInstruction(ctx, INST(sub, OP_REG(RSP), OP_IMM8(32)));
            localScope->stackPointer -= 4;
            gen_callExtern(ctx, id);
            genInstruction(ctx, INST(add, OP_REG(RSP), OP_IMM8(32)));
            localScope->stackPointer += 4;
        } else {
            gen_call(ctx, id);
        }
    } else if(expr->type == ExpressionType_BINARY_EXPRESSION) {
        TypecheckedExpression* lhs = expr->expr.BINARY_EXPRESSION.lhs;
        String operator = expr->expr.BINARY_EXPRESSION.operator.value;
        TypecheckedExpression* rhs = expr->expr.BINARY_EXPRESSION.rhs;

        // NOTE: maybe not the best to hardcode the operators, but then again what do i know
        STATIC_ASSERT(BIN_OPERATORS_COUNT == 13); // binary operator count has changed
        if(StringEqualsCstr(operator, "+")) {
            gen_x86_64_expression(ctx, lhs, localScope);
            genPush(ctx, localScope, RAX);
            gen_x86_64_expression(ctx, rhs, localScope);
            genPop(ctx, localScope, RCX);
            genInstruction(ctx, INST(add, OP_REG(RAX), OP_REG(RCX)));
        } else if(StringEqualsCstr(operator, "-")) {
            gen_x86_64_expression(ctx, rhs, localScope);
            genPush(ctx, localScope, RAX);
            gen_x86_64_expression(ctx, lhs, localScope);
            genPop(ctx, localScope, RCX);
            genInstruction(ctx, INST(sub, OP_REG(RAX), OP_REG(RCX)));
        } else if(StringEqualsCstr(operator, "*")) {
            // NOTE: mul rcx means rax = rax * rcx
            gen_x86_64_expression(ctx, lhs, localScope);
            genPush(ctx, localScope, RAX);
            gen_x86_64_expression(ctx, rhs, localScope);
            genPop(ctx, localScope, RCX);
            genInstruction(ctx, INST(mul, OP_REG(RCX)));
        } else if(StringEqualsCstr(operator, "/")) {
            // NOTE: http://stackoverflow.com/questions/45506439/ddg#45508617
            // div rcx means rax = rax / rcx remainder is rdx
            gen_x86_64_expression(ctx, rhs, localScope);
            genPush(ctx, localScope, RAX);
            gen_x86_64_expression(ctx, lhs, localScope);
            genInstruction(ctx, INST(mov, OP_REG(RDX), OP_IMM8(0))); // TODO: this can be changed to: xor rdx, rdx
            genPop(ctx, localScope, RCX);
            genInstruction(ctx, INST(div, OP_REG(RCX)));
        } else if(StringEqualsCstr(operator, "as")) {
            // UNIMPLEMENTED("codegen for cast");
            printf("[WARNING] as op not accually finished\n");
            gen_x86_64_expression(ctx, lhs, localScope);
        } else if(StringEqualsCstr(operator, "&")) {
            gen_x86_64_expression(ctx, rhs, localScope);
            genPush(ctx, localScope, RAX);
            gen_x86_64_expression(ctx, lhs, localScope);
            genPop(ctx, localScope, RCX);
            genInstruction(ctx, INST(and, OP_REG(RAX), OP_REG(RCX)));
        } else if(StringEqualsCstr(operator, "|")) {
            gen_x86_64_expression(ctx, rhs, localScope);
            genPush(ctx, localScope, RAX);
            gen_x86_64_expression(ctx, lhs, localScope);
            genPop(ctx, localScope, RCX);
            genInstruction(ctx, INST(or, OP_REG(RAX), OP_REG(RCX)));
        } else {
            UNREACHABLE_VA("Unkown operator: "STR_FMT, STR_PRINT(operator));
        }
    } else if(expr->type == ExpressionType_UNARY_EXPRESSION) {
        Token operator = expr->expr.UNARY_EXPRESSION.operator;
        TypecheckedExpression* expr2 = expr->expr.UNARY_EXPRESSION.expr;

        if(operator.type == TokenType_SUB) {
            gen_x86_64_expression(ctx, expr2, localScope);
            genInstruction(ctx, INST(neg, OP_REG(RAX)));
        }
    } else if(expr->type == ExpressionType_STRUCT_LIT) {
        UNIMPLEMENTED("ExpressionType_STRUCT_LIT should not get here");
    } else if(expr->type == ExpressionType_FIELD_ACCESS) {
        String variableName = expr->expr.FIELD_ACCESS.variableName.value;
        String fieldName = expr->expr.FIELD_ACCESS.fieldName.value;

        SymbolLocationResult varLocationRes = findSymbolLocation(localScope, variableName);
        if(varLocationRes.err) UNREACHABLE_VA("Symbol not defined: "STR_FMT, STR_PRINT(variableName));

        VariableTypeResult varTypeRes = getVariableType(localScope, variableName);
        if(varTypeRes.error) UNREACHABLE_VA("Symbol not defined: "STR_FMT, STR_PRINT(variableName));

        TypeInfo* structTypeDef = varTypeRes.value;
        StructFieldResult structFieldOffsetRes = getStructFieldOffsetByName(structTypeDef, fieldName);
        if(structFieldOffsetRes.error) UNREACHABLE_VA("Structure has no field: "STR_FMT, STR_PRINT(fieldName));

        if(varLocationRes.inDataSection) {
            gen_DataInReg(ctx, RAX, variableName);
        } else {
            genInstruction(ctx, INST(mov, OP_REG(RAX), OP_INDIRECT_OFFSET32(RBP, varLocationRes.value + structFieldOffsetRes.result)));
        }
    } else if(expr->type == ExpressionType_ARRAY_ACCESS) {
        String id = expr->expr.ARRAY_ACCESS.id.value;
        u64 index = expr->expr.ARRAY_ACCESS.index;

        SymbolLocationResult res = findSymbolLocation(localScope, id);
        if(res.err) UNREACHABLE_VA("Symbol not defined: "STR_FMT, STR_PRINT(id));

        // NOTE: expr->typeInfo should not be used to figure out the element type of the array,
        //       as it doesnt store the true type, only the type that was used for coercing during typechecking
        //       there should be a function to get the TypeInfo of the array by the id
        //       so that we have access to all the true type info
        u64 elementSize = TypeToByteSize(expr->typeInfo);
        if(res.inDataSection) {
            UNIMPLEMENTED("Array in the data section not implemented");
        } else {
            // mov rax, [rbp + res.value + elementSize * index]
            genInstruction(ctx, INST(mov, OP_REG(RAX), OP_INDIRECT_OFFSET32(RBP, res.value + elementSize * index)));
        }
    } else {
        UNREACHABLE_VA("Unknown StatementType in expression generator: %s", ExpressionTypeStr[expr->type]);
    }
}

// return the offset, from the begining of the code buffer, to the jump instruction target address that needs to be patched
// offset = offset + 4 + scopeLen
u64 gen_x86_64_condition(GenContext* ctx, TypecheckedExpression* expr, GenScope* localScope) {
    // NOTE: if a else if condition is generated and one of the operands is the same as in the previous condition,
    // it doesnt need to be moved into a register as its already there
    assert(expr->type == ExpressionType_BINARY_EXPRESSION, "");
    TypecheckedExpression* rhs = expr->expr.BINARY_EXPRESSION.rhs;
    TypecheckedExpression* lhs = expr->expr.BINARY_EXPRESSION.lhs;
    String op = expr->expr.BINARY_EXPRESSION.operator.value;

    gen_x86_64_expression(ctx, rhs, localScope);
    genPush(ctx, localScope, RAX);
    gen_x86_64_expression(ctx, lhs, localScope);
    genPop(ctx, localScope, RCX);
    genInstruction(ctx, INST(cmp, OP_REG(RAX), OP_REG(RCX)));

    // TODO: the encoding used to generate the jump needs to be selected based on the distance to the target
    // TODO: signed and unsigned jump after comparison is different
    if(StringEqualsCstr(op, "==")) {
        genInstruction(ctx, INST(jne, OP_IMM32(0)));
    } else if(StringEqualsCstr(op, "!=")) {
        genInstruction(ctx, INST(je, OP_IMM32(0)));
    } else if(StringEqualsCstr(op, "<")) {
        genInstruction(ctx, INST(jge, OP_IMM32(0)));
    } else if(StringEqualsCstr(op, ">")) {
        genInstruction(ctx, INST(jle, OP_IMM32(0)));
    } else if(StringEqualsCstr(op, "<=")) {
        genInstruction(ctx, INST(jg, OP_IMM32(0)));
    } else if(StringEqualsCstr(op, ">=")) {
        genInstruction(ctx, INST(jl, OP_IMM32(0)));
    }

    return ctx->code.size - 4;
}

// patch a jump
// patchTarget is the offset from the begining of the buffer in ctx to patch
// targetAddress is the offset from the begining of the buffer in ctx, where the jump should point to
void gen_patchAddress(GenContext* ctx, u64 patchTarget, u64 targetAddress) {
    u64 offset = targetAddress - (patchTarget + 4);
    ctx->code.data[patchTarget + 0] = (offset >> (8 * 0)) & 0xFF;
    ctx->code.data[patchTarget + 1] = (offset >> (8 * 1)) & 0xFF;
    ctx->code.data[patchTarget + 2] = (offset >> (8 * 2)) & 0xFF;
    ctx->code.data[patchTarget + 3] = (offset >> (8 * 3)) & 0xFF;
}

// from is the scope this GenScope is generated from,
// this is used to set the functions defined in this scope
GenScope* genScopeInit(Arena* mem, TypecheckedScope* from, GenScope* parent) {
    GenScope* scope = arena_alloc(mem, sizeof(GenScope));
    scope->parent = parent;
    HashmapInit(scope->localVars, 0x10);     // TODO: some reasonable limit
    HashmapInit(scope->variableTypes, 0x10); // TODO: some reasonable limit
    scope->functionsDefinedInThisScope = &from->functions;
    return scope;
}

// generate a scope used by loops and ifs
void genGenericScope(GenContext* ctx, Arena* mem, TypecheckedScope* scope, GenScope* parentScope) {
    GenScope* localScope = genScopeInit(mem, scope, parentScope);
    localScope->stackPointer = parentScope->stackPointer;

    Array(TypecheckedStatement) statements = scope->statements;
    for(u64 i = 0; i < statements.size; ++i) {
        TypecheckedStatement statement = statements.data[i];
        genStatement(ctx, mem, statement, localScope);
    }

    // clean up the scope
    // genInstruction(ctx, INST(add, OP_REG(RSP), OP_IMM32(localScope->stackSpaceForLocalVars)));
}

void genStatement(GenContext* ctx, Arena* mem, TypecheckedStatement statement, GenScope* genScope) {
    switch(statement.type) {
        case StatementType_VAR_DECL:
        case StatementType_VAR_CONST:
        case StatementType_NONE:
        case StatementType_COUNT: {
            UNREACHABLE("gen statement 2");
        } break;

        case StatementType_VAR_REASSIGN:
        case StatementType_ARRAY_REASSIGN:
        case StatementType_VAR_DECL_ASSIGN: {
            String id = statement.node.VAR_ACCESS.identifier;
            TypecheckedExpression* expr = statement.node.VAR_ACCESS.expr;
            bool isArray = statement.node.VAR_ACCESS.isArray;
            u64 index = statement.node.VAR_ACCESS.index;

            SymbolLocationResult res = findSymbolLocation(genScope, id);
            if(res.err) UNREACHABLE_VA("local variable not defined: "STR_FMT, STR_PRINT(id));

            if(res.inDataSection) {
                UNIMPLEMENTED("writing to values in data section not implemented");
            } else if(expr->type == ExpressionType_STRUCT_LIT) {
                // TODO: this is a temporary fix
                //       after typechecking an expression like:
                //       `foo := vec2{1, 2};`
                //       should be rewritten to look like this
                //       ```asm
                //       ; foo: vec2;
                //       sub rsp, sizeof(vec2) ; the size of all the variables gets added up ans subtracted all at once at the begining of the function
                //       ; foo.x = 1;
                //       mov rax, 1
                //       mov [rpb - sizeof(vec2) + offset(vec2, x)], rax
                //       ; foo.y = 2;
                //       mov rax, 2
                //       mov [rpb - sizeof(vec2) + offset(vec2, y)], rax
                //       ```
                //       codegen currently stores the result of the expression in `rax`,
                //       so literals that are larger than a 64-bit register cannot work with this scheme
                TypeInfo* structType = expr->typeInfo;
                assert(structType->symbolType == TYPE_STRUCT_DEF, "TYPE_STRUCT_DEF should be the type of a struct lit");
                switch(expr->expr.STRUCT_LIT.type) {
                    case StructInitializerListType_NONE: UNREACHABLE("StructInitializerListType_NONE is invalid here"); break;

                    case StructInitializerListType_POSITIONAL: {
                        Array(TypecheckedExpressionPtr) list = expr->expr.STRUCT_LIT.positionalInitializerList;
                        for(u64 i = 0; i < list.size; ++i) {
                            TypecheckedExpression* initializer = list.data[i];
                            StructFieldResult offset = getStructFieldOffsetByPos(structType, i);
                            if(offset.error) UNREACHABLE("Struct doesnt have the specified field");

                            gen_x86_64_expression(ctx, initializer, genScope);
                            genInstruction(ctx, INST(mov, OP_INDIRECT_OFFSET32(RBP, res.value + offset.result), OP_REG(RAX)));
                        }
                    } break;
                    case StructInitializerListType_DESIGNATED: {
                        Array(TypecheckedNamedInitializer) list = expr->expr.STRUCT_LIT.namedInitializerList;
                        for(u64 i = 0; i < list.size; ++i) {
                            TypecheckedNamedInitializer initializer = list.data[i];
                            StructFieldResult offset = getStructFieldOffsetByName(structType, initializer.id);
                            if(offset.error) UNREACHABLE_VA("Struct doesnt have the specified field, named: "STR_FMT, STR_PRINT(initializer.id));

                            gen_x86_64_expression(ctx, initializer.expr, genScope);
                            genInstruction(ctx, INST(mov, OP_INDIRECT_OFFSET32(RBP, res.value + offset.result), OP_REG(RAX)));
                        }
                    } break;
                }
            } else {
                gen_x86_64_expression(ctx, expr, genScope);
                if(isArray) {
                    u64 elementSize = TypeToByteSize(expr->typeInfo);
                    // mov [rbp + res.value + elementSize * index], rax
                    genInstruction(ctx, INST(mov, OP_INDIRECT_OFFSET32(RBP, res.value + elementSize * index), OP_REG(RAX)));
                } else {
                    // mov [rbp + res.value], rax
                    genInstruction(ctx, INST(mov, OP_INDIRECT_OFFSET32(RBP, res.value), OP_REG(RAX)));
                }
            }
        } break;
        case StatementType_RET: {
            TypecheckedExpression* expr = statement.node.RET.expr;

            gen_x86_64_expression(ctx, expr, genScope);

            // NOTE: we may want to generate the function postamble outside of this the `genStatement` function
            genInstruction(ctx, INST(mov, OP_REG(RSP), OP_REG(RBP)));
            genPop(ctx, genScope, RBP); // technically not neseccary, as the scope isnt used after
            // TODO: what is the behaviour when returning from a generic scope? eg.:if, loop

            if(genScope->isMainScope) {
                genInstruction(ctx, INST(mov, OP_REG(RCX), OP_REG(RAX)));
                gen_callExtern(ctx, STR("ExitProcess"));
            } else {
                // NOTE: the {0} argument is to get rid of an annoying c warning, just using 0 is enough
                //       this is used to indicate that the instruction has no operands
                genInstruction(ctx, INST(ret, {0}));
            }
        } break;
        case StatementType_IF: {
            Array(TypechekedConditionalBlock) blocks = statement.node.IF.blocks;
            TypecheckedScope* elze = statement.node.IF.elze;
            bool hasElse = statement.node.IF.hasElse;
            Array(u64) patchTargets = {0};

            // if condition
            {
                TypechekedConditionalBlock firstBlock = blocks.data[0];
                u64 patchTarget = gen_x86_64_condition(ctx, firstBlock.expr, genScope);
                genGenericScope(ctx, mem, firstBlock.scope, genScope);

                if(hasElse) {
                    genInstruction(ctx, INST(jmp, OP_IMM32(0)));
                    ArrayAppend(patchTargets, ctx->code.size - 4);
                }

                gen_patchAddress(ctx, patchTarget, ctx->code.size);
            }

            // optional else if conditions
            for(u64 h = 1; h < blocks.size; ++h) {
                TypechekedConditionalBlock block = blocks.data[h];
                u64 patchTarget = gen_x86_64_condition(ctx, block.expr, genScope);
                genGenericScope(ctx, mem, block.scope, genScope);

                bool isLastBlock = !(h + 1 < blocks.size);
                if(!(isLastBlock && !hasElse)) {
                    genInstruction(ctx, INST(jmp, OP_IMM32(0)));
                    ArrayAppend(patchTargets, ctx->code.size - 4);
                }

                gen_patchAddress(ctx, patchTarget, ctx->code.size);
            }

            // optional else block
            if(hasElse) {
                genGenericScope(ctx, mem, elze, genScope);
            }

            // do all the patches
            for(u64 h = 0; h < patchTargets.size; ++h) {
                u64 patchTarget = patchTargets.data[h];
                gen_patchAddress(ctx, patchTarget, ctx->code.size);
            }

            free(patchTargets.data);
        } break;
        case StatementType_LOOP: {
            TypecheckedExpression* expr = statement.node.LOOP.expr;
            TypecheckedScope* scope = statement.node.LOOP.scope;
            TypeInfo* typeInfo = expr->typeInfo;

            if(typeInfo->symbolType == TYPE_BOOL) {
                // check condition every iteration
                u64 conditionAddress = ctx->code.size;
                u64 patchTarget = gen_x86_64_condition(ctx, expr, genScope);
                genGenericScope(ctx, mem, scope, genScope);

                u64 addressAfterJmp = ctx->code.size + 5; // 5 bytes for jmp instruction
                genInstruction(ctx, INST(jmp, OP_IMM32(conditionAddress - addressAfterJmp)));
                gen_patchAddress(ctx, patchTarget, ctx->code.size);
            } else {
                // iterate n amount of times

                // declare the loop count variable
                gen_x86_64_expression(ctx, expr, genScope);
                s64 loopCountStackLocation = genScope->stackPointer;
                genPush(ctx, genScope, RAX);

                // condition and scope
                u64 addrAtCmp = ctx->code.size;
                genInstruction(ctx, INST(cmp, OP_INDIRECT_OFFSET32(RBP, -(loopCountStackLocation * 8)), OP_IMM32(0))); // TODO: not correct
                genInstruction(ctx, INST(je, OP_IMM32(0))); // jmp after scope
                u64 firstJmpToPatch = ctx->code.size - 4;
                genGenericScope(ctx, mem, scope, genScope);
                genInstruction(ctx, INST(dec, OP_INDIRECT_OFFSET32(RBP, -(loopCountStackLocation * 8)))); // TODO: not correct
                u64 addrAfterJmp = ctx->code.size + 5; // 5 bytes for jmp instruction
                genInstruction(ctx, INST(jmp, OP_IMM32(addrAtCmp - addrAfterJmp))); // jmp to condition
                u64 addrAfterLoop = ctx->code.size;
                gen_patchAddress(ctx, firstJmpToPatch, addrAfterLoop);

                // unallocate the loopCountIterator variable
                genInstruction(ctx, INST(add, OP_REG(RSP), OP_IMM8(8)));
                genScope->stackPointer--;

                // NOTE: this is pretty stupid, jumping twice in unnecessary
                //       if we know the number of iterations its more optimal to just
                //       do the cmp and jmp after the scope and only have one jump
            }
        } break;
        case StatementType_EXPRESSION: {
            TypecheckedExpression* expr = statement.node.EXPRESSION.expr;
            gen_x86_64_expression(ctx, expr, genScope);
        } break;

        case StatementType_DIRECTIVE: break;
    }
}

void calculateSpaceForVariables(GenScope* genScope, TypecheckedScope* scope) {
    HashmapFor(String, TypeInfoPtr, it, &scope->variables) {
        String key = it->key;
        TypeInfo* value = it->value;

        genScope->stackSpaceForLocalVars += TypeToByteSize(value);

        // fill the variables hashmap
        s64 tmp = 0;
        if(HashmapGet(String, s64)(&genScope->localVars, key, &tmp)) {
            UNREACHABLE_VA("two variables with the same name: "STR_FMT, STR_PRINT(key));
        }

        if(!HashmapSet(String, s64)(&genScope->localVars, key, -genScope->stackSpaceForLocalVars)) {
            UNREACHABLE_VA("failed to insert into hashmap, cap: %llu, count: %llu", genScope->localVars.capacity, genScope->localVars.size);
        }

        if(!HashmapSet(String, TypeInfoPtr)(&genScope->variableTypes, key, value)) {
            UNREACHABLE_VA("failed to insert into hashmap, cap: %llu, count: %llu", genScope->localVars.capacity, genScope->localVars.size);
        }
    }

    for(u64 i = 0; i < scope->children.size; ++i) {
        TypecheckedScope* child = scope->children.data[i];
        calculateSpaceForVariables(genScope, child);
    }
}

GenScope* genFunction(GenContext* ctx, Arena* mem, String id, ConstValue fnScope, GenScope* parent) {
    TypecheckedScope* scope = fnScope.as_function;
    TypeInfo* typeInfo = fnScope.typeInfo;

    assertf(typeInfo->symbolType == TYPE_FUNCTION, "fnScope has to be of type function, got: "STR_FMT, STR_PRINT(TypeToString(mem, typeInfo)));
    GenScope* genScope = genScopeInit(mem, scope, parent);
    if(typeInfo->functionInfo->isExternal) return genScope;

    u64 functionLocation = ctx->code.size;
    // TODO: why is this an s64 and not a u64
    if(!HashmapSet(String, s64)(&ctx->functionLocations, id, functionLocation)) UNREACHABLE_VA("failed to insert into hashmap, cap: %llu, count: %llu", ctx->functionLocations.capacity, ctx->functionLocations.size);

    // NOTE: would be usefull here to generating code into a separate buffer
    // TODO: make the main entrypoint name customisable
    genScope->isMainScope = StringEqualsCstr(id, "main");
    if(genScope->isMainScope) ctx->entryPointOffset = ctx->code.size; // NOTE: this isnt technically necessary as all the function offsets are recorded anyway

    genPush(ctx, genScope, RBP); // TODO: this is sus, check after done refactor
    genInstruction(ctx, INST(mov, OP_REG(RBP), OP_REG(RSP)));

    // variables
    calculateSpaceForVariables(genScope, scope);

    // print the local variables define in this function
    #if 0
    printf("Function "STR_FMT"\n", STR_PRINT(id));
    HashmapFor(String, s64 , it, &genScope->localVars) {
        String key = it->key;
        s64 value = it->value;
        printf("  local var: "STR_FMT" at %lli\n", STR_PRINT(key), value);
    }
    #endif

    // function arguments
    for(u64 i = 0; i < typeInfo->functionInfo->args.size; ++i) {
        FunctionArg fnArg = typeInfo->functionInfo->args.data[i];
        String argId = fnArg.id;
        TypeInfo* argType = genParsedTypeToTypeInfo(fnArg.type);
        // Expression* argValue = fnArg.initialValue;
        // UNUSED(argValue);

        genScope->stackSpaceForLocalVars += TypeToByteSize(argType);

        // fill the variables hashmap
        s64 tmp = 0;
        if(HashmapGet(String, s64)(&genScope->localVars, argId, &tmp)) {
            UNREACHABLE_VA("two variables with the same name: "STR_FMT, STR_PRINT(argId));
        }

        if(!HashmapSet(String, s64)(&genScope->localVars, argId, -genScope->stackSpaceForLocalVars)) {
            UNREACHABLE_VA("failed to insert into hashmap, cap: %llu, count: %llu", genScope->localVars.capacity, genScope->localVars.size);
        }

        // move the value to the correct location in the stack
        if(i == 0) {
            genInstruction(ctx, INST(mov, OP_INDIRECT_OFFSET32(RBP, -genScope->stackSpaceForLocalVars), OP_REG(RCX)));
        } else if(i == 1) {
            genInstruction(ctx, INST(mov, OP_INDIRECT_OFFSET32(RBP, -genScope->stackSpaceForLocalVars), OP_REG(RDX)));
        } else if(i == 2) {
            genInstruction(ctx, INST(mov, OP_INDIRECT_OFFSET32(RBP, -genScope->stackSpaceForLocalVars), OP_REG(R8)));
        } else if(i == 3) {
            genInstruction(ctx, INST(mov, OP_INDIRECT_OFFSET32(RBP, -genScope->stackSpaceForLocalVars), OP_REG(R9)));
        } else {
            genInstruction(ctx, INST(mov, OP_REG(RAX), OP_INDIRECT_OFFSET32(RBP, (i - 3) * 8)));
            genInstruction(ctx, INST(mov, OP_INDIRECT_OFFSET32(RBP, -genScope->stackSpaceForLocalVars), OP_REG(RAX)));
        }
    }

    genScope->stackSpaceForLocalVars = align(genScope->stackSpaceForLocalVars, 16);
    genInstruction(ctx, INST(sub, OP_REG(RSP), OP_IMM32(genScope->stackSpaceForLocalVars))); // TODO: this needs to be generated after `mov rbp, rsp`
    genScope->stackPointer += genScope->stackSpaceForLocalVars / 8;
    // TODO: store the stack pointer also in bytes

    // scope
    for(u64 h = 0; h < scope->statements.size; ++h) {
        TypecheckedStatement statement = scope->statements.data[h];
        genStatement(ctx, mem, statement, genScope);
    }

    return genScope;
}

void genAllFunctionsRecursively(GenContext* ctx, Arena* mem, TypecheckedScope* scope, GenScope* parent) {
    HashmapFor(String, ConstValue, it, &scope->functions) {
        String fnName = it->key;
        ConstValue fnScope = it->value;
        TypecheckedScope* parentScope = fnScope.as_function;

        // codegen function
        GenScope* fnGenScope = genFunction(ctx, mem, fnName, fnScope, parent);

        // codegen all the sub functions
        for(u64 i = 0; i < parentScope->children.size; ++i) {
            TypecheckedScope* childScope = parentScope->children.data[i];
            genAllFunctionsRecursively(ctx, mem, childScope, fnGenScope);
        }
    }
}

GenContext gen_x86_64_bytecode(Arena* mem, TypecheckedScope* scope) {
    GenContext ctx = {0};

    // TODO: use arena allocator
    // TODO: make the default size something sane
    HashmapInit(ctx.functionLocations, 0x10);
    HashmapInit(ctx.dataSection, 0x10);

    GenScope* globalScope = genScopeInit(mem, scope, NULL);

    // gen functions
    genAllFunctionsRecursively(&ctx, mem, scope, globalScope);

    // patch all the interal function calls
    for(u64 i = 0; i < ctx.internalsToPatch.size; ++i) {
        String name = ctx.internalsToPatch.data[i].name;
        u64 offset = ctx.internalsToPatch.data[i].offset;

        s64 addr = 0;
        if(!HashmapGet(String, s64)(&ctx.functionLocations, name, &addr)) {
            UNREACHABLE_VA("undefined function: "STR_FMT, STR_PRINT(name));
        }

        u32 nextInstructonAddr = offset + 4;
        u8* targetAddr = &ctx.code.data[offset];
        *(u32*)targetAddr = addr - nextInstructonAddr;
    }

    return ctx;
}

// TODO: make sure to preserve non volitile registers when entering a function
