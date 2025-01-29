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
    int offset = ctx->code.size - 4;
    AddrToPatch patch = {
        .name = name,
        .offset = offset,
    };
    ArrayAppend(ctx->symbolsToPatch, patch);
}

void gen_call(GenContext* ctx, String name) {
    // 8 + 8 + 32 bits pushed to the ctx, last 32 are the address
    genInstruction(ctx, INST(call, OP_IMM32(0xDEADBEEF)));
    int offset = ctx->code.size - 4;
    AddrToPatch patch = {
        .name = name,
        .offset = offset,
    };
    ArrayAppend(ctx->functionsToPatch, patch);
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
    int offset = ctx->code.size - 4;
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
    int offset = ctx->code.size - 4;
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

void genPush(GenContext* ctx, GenScope* scope, Register reg) {
    scope->stackPointer++;
    genInstruction(ctx, INST(push, OP_REG(reg)));
}

void genPop(GenContext* ctx, GenScope* scope, Register reg) {
    scope->stackPointer--;
    genInstruction(ctx, INST(pop, OP_REG(reg)));
}

void gen_x86_64_func_call(GenContext* ctx, Expression* funcCall, GenScope* localScope) {
    assert(funcCall->type == ExpressionType_FUNCTION_CALL, "");
    String id = funcCall->expr.FUNCTION_CALL.identifier;
    Array(ExpressionPtr) args = funcCall->expr.FUNCTION_CALL.args;

    for(u64 i = 0; i < args.size; ++i) {
        Expression* arg = args.data[i];

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

    FuncInfo value = {0};
    if(!HashmapGet(String, FuncInfo)(&ctx->funcInfo, id, &value)) {
        UNREACHABLE("function not found in hashmap");
    }
    if(value.isExtern) {
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
}

void gen_x86_64_expression(GenContext* ctx, Expression* expr, GenScope* localScope) {
    if(expr->type == ExpressionType_INT_LIT) {
        String value = expr->expr.INT_LIT.value;
        // TODO: check if the number is signed or unsigned and parse it differently based on that
        u32 intValue = StringToU32(value);
        Instruction inst = INST(mov, OP_REG(RAX), OP_IMM32(intValue));
        genInstruction(ctx, inst);
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
        if(!HashmapSet(String, UserDataEntry)(&ctx->data, lit, value)) {
            UNREACHABLE("failed to insert into hashmap");
        }
        gen_DataInReg(ctx, RAX, lit);
    } else if(expr->type == ExpressionType_BOOL_LIT) {
        UNIMPLEMENTED("expression generation with bools");
    } else if(expr->type == ExpressionType_SYMBOL) {
        String id = expr->expr.SYMBOL.identifier;

        s64 value;
        GenScope* at = localScope;
        bool success = FALSE;
        while(at) {
            if(HashmapGet(String, s64)(&at->localVars, id, &value)) {
                success = TRUE;
                break;
            }
            at = at->parent;
        }
        if(!success) {
            if(!HashmapGet(String, s64)(&ctx->constants, id, &value)) {
                UNREACHABLE("cannot generate expression with a undefined variable or constant");
            } else {
                genInstruction(ctx, INST(mov, OP_REG(RAX), OP_IMM32(value)));
                // TODO: dont remember writing this anymore, is it correct?
            }
        } else {
            genInstruction(ctx, INST(mov, OP_REG(RAX), OP_INDIRECT_OFFSET32(RBP, -(value * 8))));
        }
    } else if(expr->type == ExpressionType_FUNCTION_CALL) {
        gen_x86_64_func_call(ctx, expr, localScope);
    } else if(expr->type == ExpressionType_BINARY_EXPRESSION) {
        Expression* lhs = expr->expr.BINARY_EXPRESSION.lhs;
        String operator = expr->expr.BINARY_EXPRESSION.operator.value;
        Expression* rhs = expr->expr.BINARY_EXPRESSION.rhs;

        // NOTE: maybe not the best to hardcode the operators, but then again what do i know
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
        }
    } else {
        printf("[ERROR] Unknown ASTNodeType in expression generator: %s\n", ASTNodeTypeStr[expr->type]);
        exit(EXIT_FAILURE);
    }
}

// return the offset, from the begining of the code buffer, to the jump instruction target address that needs to be patched
// offset = offset + 4 + scopeLen
u64 gen_x86_64_condition(GenContext* ctx, Expression* expr, GenScope* localScope) {
    // NOTE: if a else if condition is generated and one of the operands is the same as in the previous condition,
    // it doesnt need to be moved into a register as its already there
    assert(expr->type == ExpressionType_BINARY_EXPRESSION, "");
    Expression* rhs = expr->expr.BINARY_EXPRESSION.rhs;
    Expression* lhs = expr->expr.BINARY_EXPRESSION.lhs;
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

GenScope* genScopeInit(GenContext* ctx, GenScope* parent) {
    GenScope* scope = arena_alloc(&ctx->mem, sizeof(GenScope));
    scope->parent = parent;
    HashmapInit(scope->localVars, 0x10); // TODO: some reasonable limit for local vars
    return scope;
}

// since this is a function scope i dont have to worry about any stack memory as it will be taken care of when the function returns
void genFunctionScope(GenContext* ctx, Scope* scope, GenScope* functionScope) {
    Array(ASTNodePtr) statements = scope->statements;
    for(u64 i = 0; i < statements.size; ++i) {
        ASTNode* statement = statements.data[i];
        genStatement(ctx, statement, functionScope);
    }
}

void genGenericScope(GenContext* ctx, Scope* scope, GenScope* parentScope) {
    GenScope* localScope = genScopeInit(ctx, parentScope);
    localScope->stackPointer = parentScope->stackPointer;

    Array(ASTNodePtr) statements = scope->statements;
    for(u64 i = 0; i < statements.size; ++i) {
        ASTNode* statement = statements.data[i];
        genStatement(ctx, statement, localScope);
    }

    // clean up the scope
    u64 localVarsSize = localScope->localVars.size;
    genInstruction(ctx, INST(add, OP_REG(RSP), OP_IMM32(localVarsSize * 8)));
}

void genStatement(GenContext* ctx, ASTNode* statement, GenScope* localScope) {
    switch(statement->type) {
        case ASTNodeType_NONE:
        case ASTNodeType_COUNT: {
            printf("[ERROR] ast node type ASTNodeType_NONE and ASTNodeType_COUNT are errors.\n");
            exit(EXIT_FAILURE);
        } break;

        case ASTNodeType_FUNCTION_DEF: {
            String id = statement->node.FUNCTION_DEF.identifier;
            Scope* scope = statement->node.FUNCTION_DEF.scope;
            Array(FunctionArg) args = statement->node.FUNCTION_DEF.args;
            TypeInfo retType = statement->node.FUNCTION_DEF.type;
            UNUSED(retType);

            FuncInfo value = {0};
            if(!HashmapGet(String, FuncInfo)(&ctx->funcInfo, id, &value)) {
                assertf(FALSE, "[UNREACHABLE] Function not found in hashmap: "STR_FMT"\n", STR_PRINT(id));
            }
            if(value.isExtern) break;

            if(!HashmapSet(String, s64)(&ctx->functions, id, ctx->code.size)) {
                assertf(FALSE, "[UNREACHABLE] Failed to save the location where function: "STR_FMT" is generated\n", STR_PRINT(id));
            }

            // NOTE: would be usefull here to generating code into a separate buffer
            // TODO: make the main entrypoint name customisable
            bool mainFunction = StringEqualsCstr(id, "main");
            if(mainFunction) ctx->entryPointOffset = ctx->code.size;

            GenScope* functionScope = genScopeInit(ctx, localScope); // localScope should be NULL here as global scope is null??
            functionScope->isMainScope = mainFunction;

            genPush(ctx, functionScope, RBP);
            genInstruction(ctx, INST(mov, OP_REG(RBP), OP_REG(RSP)));

            // function arguments
            for(u64 i = 0; i < args.size; ++i) {
                String argId = args.data[i].id;
                TypeInfo* argType = args.data[i].type;
                UNUSED(argType);

                if(i == 0) {
                    if(!HashmapSet(String, s64)(&functionScope->localVars, argId, functionScope->stackPointer)) UNREACHABLE("failed to insert into hashmap");
                    genPush(ctx, functionScope, RCX);
                } else if(i == 1) {
                    if(!HashmapSet(String, s64)(&functionScope->localVars, argId, functionScope->stackPointer)) UNREACHABLE("failed to insert into hashmap");
                    genPush(ctx, functionScope, RDX);
                } else if(i == 2) {
                    if(!HashmapSet(String, s64)(&functionScope->localVars, argId, functionScope->stackPointer)) UNREACHABLE("failed to insert into hashmap");
                    genPush(ctx, functionScope, R8);
                } else if(i == 3) {
                    if(!HashmapSet(String, s64)(&functionScope->localVars, argId, functionScope->stackPointer)) UNREACHABLE("failed to insert into hashmap");
                    genPush(ctx, functionScope, R9);
                } else {
                    if(!HashmapSet(String, s64)(&functionScope->localVars, argId, functionScope->stackPointer)) UNREACHABLE("failed to insert into hashmap");
                    genInstruction(ctx, INST(mov, OP_REG(RAX), OP_INDIRECT_OFFSET32(RBP, (i - 3) * 8)));
                    genPush(ctx, functionScope, RAX);
                }
            }

            // function body
            genFunctionScope(ctx, scope, functionScope);
        } break;
        case ASTNodeType_FUNCTION_CALL: {
            gen_x86_64_func_call(ctx, statement, localScope);
        } break;
        case ASTNodeType_VAR_DECL: {
            String id = statement->node.VAR_DECL.identifier;
            TypeInfo* type = statement->node.VAR_DECL.type;
            UNUSED(type);

            if(!HashmapSet(String, s64)(&localScope->localVars, id, localScope->stackPointer)) UNREACHABLE("failed to insert into hashmap");
            // NOTE: 8 is the byte size of a 64 stack slot, maybe sould not be hardcoded
            genInstruction(ctx, INST(sub, OP_REG(RSP), OP_IMM8(8)));
            localScope->stackPointer++;
        } break;
        case ASTNodeType_VAR_DECL_ASSIGN: {
            Expression* exprNode = statement->node.VAR_DECL_ASSIGN.expr;
            String id = statement->node.VAR_DECL_ASSIGN.identifier;
            TypeInfo* type = statement->node.VAR_DECL_ASSIGN.type;
            UNUSED(type);

            gen_x86_64_expression(ctx, exprNode, localScope); // gen code for the expression, store in rax??
            // store the variable location on the stack in the context
            if(!HashmapSet(String, s64)(&localScope->localVars, id, localScope->stackPointer)) {
                UNREACHABLE("failed to insert into hashmap");
            }
            genPush(ctx, localScope, RAX); // push the variable value to the stack
        } break;
        case ASTNodeType_VAR_REASSIGN: {
            String id = statement->node.VAR_REASSIGN.identifier;
            Expression* exprNode = statement->node.VAR_REASSIGN.expr;

            gen_x86_64_expression(ctx, exprNode, localScope);

            s64 value;
            bool success = FALSE;
            GenScope* at = localScope;
            while(at) {
                if(HashmapGet(String, s64)(&at->localVars, id, &value)) {
                    success = TRUE;
                    break;
                }
                at = at->parent;
            }
            if(!success) UNREACHABLE("failed to find local variable on the stack");

            genInstruction(ctx, INST(mov, OP_INDIRECT_OFFSET32(RBP, -(value * 8)), OP_REG(RAX)));
        } break;
        case ASTNodeType_VAR_CONST: {
            // TODO: this whole thing is giga fucked, will redo with typechecking
            String id = statement->node.VAR_CONST.identifier;
            Expression* expr = statement->node.VAR_CONST.expr;
            // TODO: will need a type
            // TODO: based on type figure out if its a string or something else
            //       string goes in one hashmap and signed numbers go in another
            
            ExpressionEvaluationResult exprResult = evaluate_expression(expr);
            s64 value = exprResult.result <= S64_MAX ? (s64)exprResult.result : S64_MAX;
            value = exprResult.isNegative ? -value : value;
            // TODO: assuming numerical constant
            if(!HashmapSet(String, s64)(&ctx->constants, id, value)) {
                UNREACHABLE("hashmap full");
            }
        } break;
        case ASTNodeType_RET: {
            // NOTE: dead code elimination, currently internal stack underflows if there is more than one return in a add
            Expression* expr = statement->node.RET.expr;

            gen_x86_64_expression(ctx, expr, localScope);
            genInstruction(ctx, INST(mov, OP_REG(RSP), OP_REG(RBP)));
            genPop(ctx, localScope, RBP); // technically not neseccary, as the scope isnt used after
            // TODO: what is the behaivour when returning from a generic scope? eg.:if, loop

            if(localScope->isMainScope) {
                genInstruction(ctx, INST(mov, OP_REG(RCX), OP_REG(RAX)));
                gen_callExtern(ctx, STR("ExitProcess"));
            } else {
                // NOTE: the {0} argument is to get rid of an annoying c warning, just using 0 is enough
                //       this is used to indicate that the instruction has no operands
                genInstruction(ctx, INST(ret, {0}));
            }
        } break;
        case ASTNodeType_IF: {
            Array(ConditionalBlock) blocks = statement->node.IF.blocks;
            Scope* elze = statement->node.IF.elze;
            bool hasElse = statement->node.IF.hasElse;
            Array(u64) patchTargets = {0};

            // if condition
            {
                ConditionalBlock firstBlock = blocks.data[0];
                u64 patchTarget = gen_x86_64_condition(ctx, firstBlock.expr, localScope);
                genGenericScope(ctx, firstBlock.scope, localScope);

                if(hasElse) {
                    genInstruction(ctx, INST(jmp, OP_IMM32(0)));
                    ArrayAppend(patchTargets, ctx->code.size - 4);
                }

                gen_patchAddress(ctx, patchTarget, ctx->code.size);
            }

            // optional else if conditions
            for(u64 h = 1; h < blocks.size; ++h) {
                ConditionalBlock block = blocks.data[h];
                u64 patchTarget = gen_x86_64_condition(ctx, block.expr, localScope);
                genGenericScope(ctx, block.scope, localScope);

                bool isLastBlock = !(h + 1 < blocks.size);
                if(!(isLastBlock && !hasElse)) {
                    genInstruction(ctx, INST(jmp, OP_IMM32(0)));
                    ArrayAppend(patchTargets, ctx->code.size - 4);
                }

                gen_patchAddress(ctx, patchTarget, ctx->code.size);
            }

            // optional else block
            if(hasElse) {
                genGenericScope(ctx, elze, localScope);
            }

            // do all the patches
            for(u64 h = 0; h < patchTargets.size; ++h) {
                u64 patchTarget = patchTargets.data[h];
                gen_patchAddress(ctx, patchTarget, ctx->code.size);
            }

            free(patchTargets.data);
        } break;
        case ASTNodeType_LOOP: {
            Expression* expr = statement->node.LOOP.expr;
            Scope* scope = statement->node.LOOP.scope;
            
            if(expr->type == ExpressionType_BINARY_EXPRESSION) {
                // check condition every iteration
                u64 conditionAddress = ctx->code.size;
                u64 patchTarget = gen_x86_64_condition(ctx, expr, localScope);
                genGenericScope(ctx, scope, localScope);

                u64 addressAfterJmp = ctx->code.size + 5; // 5 bytes for jmp instruction
                genInstruction(ctx, INST(jmp, OP_IMM32(conditionAddress - addressAfterJmp)));
                gen_patchAddress(ctx, patchTarget, ctx->code.size);
            } else {
                // iterate n amount of times

                // declare the loop count variable
                gen_x86_64_expression(ctx, expr, localScope);
                s64 loopCountStackLocation = localScope->stackPointer;
                genPush(ctx, localScope, RAX);

                // condition and scope
                u64 addrAtCmp = ctx->code.size;
                genInstruction(ctx, INST(cmp, OP_INDIRECT_OFFSET32(RBP, -(loopCountStackLocation * 8)), OP_IMM32(0))); // TODO: not correct
                genInstruction(ctx, INST(je, OP_IMM32(0))); // jmp after scope
                u64 firstJmpToPatch = ctx->code.size - 4;
                genGenericScope(ctx, scope, localScope);
                genInstruction(ctx, INST(dec, OP_INDIRECT_OFFSET32(RBP, -(loopCountStackLocation * 8)))); // TODO: not correct
                u64 addrAfterJmp = ctx->code.size + 5; // 5 bytes for jmp instruction
                genInstruction(ctx, INST(jmp, OP_IMM32(addrAtCmp - addrAfterJmp))); // jmp to condition
                u64 addrAfterLoop = ctx->code.size;
                gen_patchAddress(ctx, firstJmpToPatch, addrAfterLoop);

                // unallocate the loopCountIterator variable
                genInstruction(ctx, INST(add, OP_REG(RSP), OP_IMM8(8)));
                localScope->stackPointer--;

                // NOTE: this is pretty stupid, jumping twice in unnecessary
                //       if we know the number of iterations its more optimal to just
                //       do the cmp and jmp after the scope and only have one jump
            }
        } break;
    }
}

void genGlobalScope(GenContext* ctx, Scope* globalScope) {
    GenScope* scope = genScopeInit(ctx, NULL);
    Array(ASTNodePtr) statements = globalScope->statements;
    for(u64 i = 0; i < statements.size; ++i) {
        ASTNode* statement = statements.data[i];
        genStatement(ctx, statement, scope);
    }
}

GenContext gen_x86_64_bytecode(Scope* globalScope, Hashmap(String, FuncInfo) funcInfo) {
    GenContext ctx = {0};

    ctx.funcInfo = funcInfo;

    // TODO: use arena allocator
    // TODO: make the default size something sane
    HashmapInit(ctx.functions, 0x10);
    HashmapInit(ctx.data, 0x10);
    HashmapInit(ctx.constants, 0x10);
    
    genGlobalScope(&ctx, globalScope);
    return ctx;
}

GenContext gen_x86_64_bytecode(TypecheckedScope* scope) {
    GenContext ctx = {0};

    // ctx.funcInfo = funcInfo;

    // TODO: use arena allocator
    // TODO: make the default size something sane
    HashmapInit(ctx.functions, 0x10);
    HashmapInit(ctx.data, 0x10);
    HashmapInit(ctx.constants, 0x10);

    for(u64 i = 0; i < scope->functionIndicies.size; ++i) {
        u64 index = scope->functionIndicies.data[i];
        String fnName = scope->constants.pairs[index].key;
        ConstValue fnScope = scope->constants.pairs[index].value;

        // decrement rsp based on the variables
        GenScope* genScope = genScopeInit(&ctx, NULL);
        for(u64 h = 0; h < fnScope.as_function->statements.size; ++h) {
            TypecheckedStatement statement = fnScope.as_function->statements.data[i];
            genStatement2(&ctx, statement, genScope);
        }
    }
}

void genStatement2(GenContext* ctx, TypecheckedStatement statement, GenScope* genScope) {
    switch(statement.type) {
        case ASTNodeType_VAR_DECL:
        case ASTNodeType_VAR_CONST:
        case ASTNodeType_NONE: 
        case ASTNodeType_COUNT: {
            UNREACHABLE("gen statement");
        } break;

        case ASTNodeType_VAR_DECL_ASSIGN: {

        } break;
        case ASTNodeType_VAR_REASSIGN: {

        } break;
        case ASTNodeType_RET: {

        } break;
        case ASTNodeType_IF: {

        } break;
        case ASTNodeType_LOOP: {

        } break;
        case ASTNodeType_EXPRESSION: {

        } break;
    }
}

// TODO: make sure to preserve non volitile register when entering a function
