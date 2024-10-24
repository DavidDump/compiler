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
    assert(mod < 4);  // 2 bit number
    assert(reg < 16); // 3 bit number, but 16 registers, the top bit is encoded in REX byte
    assert(rm < 16);  // 3 bit number, but 16 registers, the top bit is encoded in REX byte
    u8 modRmByte = ((mod & 3) << 6) | ((reg & 7) << 3) | ((rm & 7) << 0);
    buffer_append_u8(&ctx->code, modRmByte);
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
    assert(scale < 4);  // 2 bit number
    assert(index < 16); // 3 bit number, but 16 registers, the top bit is encoded in REX byte
    assert(base < 16);  // 3 bit number, but 16 registers, the top bit is encoded in REX byte
    u8 sibByte = ((scale & 3) << 6) | ((index & 7) << 3) | ((base & 7) << 0);
    buffer_append_u8(&ctx->code, sibByte);
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
    assert(w == 0 || w == 1);
    assert(r < 16); // 1 bit number, but 16 registers, only the top bit is extracted
    assert(x < 16); // 1 bit number, but 16 registers, only the top bit is extracted
    assert(b < 16); // 1 bit number, but 16 registers, only the top bit is extracted
    u8 rexByte = 0x40 | (w << 3) | ((r >> 3) << 2) | ((x >> 3) << 1) | ((b >> 3) << 0);
    buffer_append_u8(&ctx->code, rexByte);
}

void Emit8(GenContext* ctx, u8 data) {
    buffer_append_u8(&ctx->code, data);
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
    assert((reg & 7) != RSP);
    assert((reg & 7) != RBP);
    genModRMByte(ctx, INDIRECT_NO_DISPLACE, reg, rm);
}

// op rax, [rcx + 0x12]
// reg = RAX, rm = RCX, displacement = 0x12
void EmitIndirectDisplaced8(GenContext* ctx, Register reg, Register rm, u8 displacement) {
    assert((rm & 7) != RSP);
    genModRMByte(ctx, INDIRECT_08_DISPLACE, reg, rm);
    Emit8(ctx, displacement);
}

// op rax, [rcx + 0x12345678]
// reg = RAX, rm = RCX, displacement = 0x12345678
void EmitIndirectDisplaced32(GenContext* ctx, u8 reg, Register rm, u32 displacement) {
    assert((rm & 7) != RSP);
    genModRMByte(ctx, INDIRECT_32_DISPLACE, reg, rm);
    Emit32(ctx, displacement);
}

// op rax, [rcx + 4*rdx]
// reg = RAX, rm = RCX, index = RDX, scale = X4
void EmitIndirectSIB(GenContext* ctx, u8 reg, Register rm, Register index, Scale scale) {
    assert((rm & 7) != RBP);
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
    u8* testValBuffer = (u8*)&(InstructionEncoding){0};
    u64 size = sizeof(*enc);
    u8* encBuff = (u8*)enc;
    return memcmp(encBuff, testValBuffer, size) == 0;
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
    printf("[ERROR] Could not find encoding for instruction: %s %s %s\n", MnemonicStr[inst.name], OperandTypeStr[inst.ops[0].type], OperandTypeStr[inst.ops[1].type]);
    assert(FALSE);
}

void gen_callExtern(GenContext* ctx, String name) {
    // 8 + 8 + 32 bits pushed to the ctx, last 32 are the address
    genInstruction(ctx, INST(call, OP_RIP(0xDEADBEEF)));
    int offset = ctx->code.size - 4;
    *buffer_allocate(&ctx->symbolsToPatch, AddrToPatch) = (AddrToPatch){
        .name = name,
        .offset = offset,
    };
}

void gen_call(GenContext* ctx, String name) {
    // 8 + 8 + 32 bits pushed to the ctx, last 32 are the address
    genInstruction(ctx, INST(call, OP_RIP(0xDEADBEEF)));
    int offset = ctx->code.size - 4;
    *buffer_allocate(&ctx->functionsToPatch, AddrToPatch) = (AddrToPatch){
        .name = name,
        .offset = offset,
    };
}

// lea rax, [rip + 0xdeadbeef + sizeof(u64)] ; load data
// mov rax, [rip + 0xdeadbeef]               ; load size
// read the data field of the entry into reg register
void _gen_DataInReg(GenContext* ctx, Register reg, String name) {
    // NOTE: the rip address get added to the patched address,
    // so adding the size of u64 offsets the address by 8,
    // meaning instead of reading from the data begining, where the size is stored,
    // instead it reads the data stored in the entry
    genInstruction(ctx, INST(lea, OP_REG(reg), OP_RIP(sizeof(u64))));
    int offset = ctx->code.size - 4;
    *buffer_allocate(&ctx->dataToPatch, AddrToPatch) = (AddrToPatch){
        .name = name,
        .offset = offset,
    };
}

// read the size field of the entry into reg register
void _gen_SizeInReg(GenContext* ctx, Register reg, String name) {
    // NOTE: the rip address get added to the patched address,
    // the address gets added to 0 so in this case we read the size field of the entry
    genInstruction(ctx, INST(mov, OP_REG(reg), OP_RIP(0)));
    int offset = ctx->code.size - 4;
    *buffer_allocate(&ctx->dataToPatch, AddrToPatch) = (AddrToPatch){
        .name = name,
        .offset = offset,
    };
}

// 
// 
// 

void genPush(GenContext* ctx, Register reg) {
    ctx->stackPointer++;
    genInstruction(ctx, INST(push, OP_REG(reg)));
}

void genPop(GenContext* ctx, Register reg) {
    ctx->stackPointer--;
    genInstruction(ctx, INST(pop, OP_REG(reg)));
}

void gen_x86_64_func_call(GenContext* ctx, ASTNode* funcCall) {
    assert(funcCall->type == ASTNodeType_FUNCTION_CALL);
    String id = funcCall->node.FUNCTION_CALL.identifier;
    Args args = funcCall->node.FUNCTION_CALL.args;

    // TODO: cast
    for(u64 i = 0; i < (u64)args.size; ++i) {
        ASTNode* arg = args.args[i];
        assert(arg->type == ASTNodeType_INT_LIT || arg->type == ASTNodeType_EXPRESION);

        gen_x86_64_expression(ctx, arg);
        if(i == 0) {
            Instruction inst = INST(mov, OP_REG(RCX), OP_REG(RAX));
            genInstruction(ctx, inst);
        } else if(i == 1) {
            Instruction inst = INST(mov, OP_REG(RDX), OP_REG(RAX));
            genInstruction(ctx, inst);
        } else if(i == 2) {
            Instruction inst = INST(mov, OP_REG(R8), OP_REG(RAX));
            genInstruction(ctx, inst);
        } else if(i == 3) {
            Instruction inst = INST(mov, OP_REG(R9), OP_REG(RAX));
            genInstruction(ctx, inst);
        } else {
            genPush(ctx, RAX);
        }
    }

    gen_call(ctx, id);
}

void gen_x86_64_expression(GenContext* ctx, ASTNode* expr) {
    if(expr->type == ASTNodeType_INT_LIT) {
        String value = expr->node.INT_LIT.value;
        // TODO: check if the number is signed or unsigned and parse it differently based on that
        u32 intValue = StringToU32(value);
        Instruction inst = INST(mov, OP_REG(RAX), OP_IMM32(intValue));
        genInstruction(ctx, inst);
    } else if(expr->type == ASTNodeType_FLOAT_LIT) {
        UNIMPLEMENTED("expression generation with floats");
    } else if(expr->type == ASTNodeType_SYMBOL) {
        String id = expr->node.SYMBOL.identifier;
        s64 value;
        if(!hashmapGet(&ctx->variables, id, &value)) {
            UNREACHABLE("cannot generate expression with a undefined variable");
        }
        // NOTE: i think the value has to be negative, becouse we are looking backwards on the stack
        // TODO: X8 is not always correct, it needs to be X4 if were generating 32bit code
        Instruction inst = INST(mov, OP_REG(RAX), OP_INDIRECT_SIB(RBP, X8, -value));
        genInstruction(ctx, inst);
    } else if(expr->type == ASTNodeType_FUNCTION_CALL) {
        gen_x86_64_func_call(ctx, expr);
    } else if(expr->type == ASTNodeType_EXPRESION) {
        ASTNode* lhs = expr->node.EXPRESION.lhs;
        String operator = expr->node.EXPRESION.operator;
        ASTNode* rhs = expr->node.EXPRESION.rhs;

        // NOTE: maybe not the best to hardcode the operators, but than again what do i know
        if(StringEqualsCstr(operator, "+")) {
            gen_x86_64_expression(ctx, lhs);
            genPush(ctx, RAX);
            gen_x86_64_expression(ctx, rhs);
            genPop(ctx, RCX);
            genInstruction(ctx, INST(add, OP_REG(RAX), OP_REG(RCX)));
        } else if(StringEqualsCstr(operator, "-")) {
            gen_x86_64_expression(ctx, rhs);
            genPush(ctx, RAX);
            gen_x86_64_expression(ctx, lhs);
            genPop(ctx, RCX);
            genInstruction(ctx, INST(sub, OP_REG(RAX), OP_REG(RCX)));
        } else if(StringEqualsCstr(operator, "*")) {
            // NOTE: mul rcx means rax = rax * rcx
            gen_x86_64_expression(ctx, lhs);
            genPush(ctx, RAX);
            gen_x86_64_expression(ctx, rhs);
            genPop(ctx, RCX);
            genInstruction(ctx, INST(mul, OP_REG(RCX)));
        } else if(StringEqualsCstr(operator, "/")) {
            // NOTE: http://stackoverflow.com/questions/45506439/ddg#45508617
            // div rcx means rax = rax / rcx remainder is rdx
            gen_x86_64_expression(ctx, rhs);
            genPush(ctx, RAX);
            gen_x86_64_expression(ctx, lhs);
            genInstruction(ctx, INST(mov, OP_REG(RDX), OP_IMM8(0))); // TODO: this can be changed to: xor rdx, rdx
            genPop(ctx, RCX);
            genInstruction(ctx, INST(div, OP_REG(RCX)));
        }
    } else {
        printf("[ERROR] Unknown ASTNodeType in epxression generator: %s\n", ASTNodeTypeStr[expr->type]);
        exit(EXIT_FAILURE);
    }
}

void gen_x86_64_condition(GenContext* ctx, ASTNode* expr) {
    // NOTE: if a else if condition is generated and one of the operands is the same as in the previous condition,
    // it doesnt need to be moved into a register as its already there
    assert(expr->type == ASTNodeType_EXPRESION);
    ASTNode* rhs = expr->node.EXPRESION.rhs;
    ASTNode* lhs = expr->node.EXPRESION.lhs;
    String op = expr->node.EXPRESION.operator;

    gen_x86_64_expression(ctx, rhs);
    genPush(ctx, RAX);
    gen_x86_64_expression(ctx, lhs);
    genPop(ctx, RCX);
    genInstruction(ctx, INST(cmp, OP_REG(RAX), OP_REG(RCX)));

    // TODO: the encoding used to generate the jump needs to be selected based on the distance to the target
    // TODO: signed and unsigned jump after comparison is different
    u32 offset = 0;
    UNIMPLEMENTED("offset calculation not finished");
    if(StringEqualsCstr(op, "==")) {
        genInstruction(ctx, INST(je, OP_IMM32(offset)));
    } else if(StringEqualsCstr(op, "!=")) {
        genInstruction(ctx, INST(jne, OP_IMM32(offset)));
    } else if(StringEqualsCstr(op, "<")) {
        genInstruction(ctx, INST(jl, OP_IMM32(offset)));
    } else if(StringEqualsCstr(op, ">")) {
        genInstruction(ctx, INST(jg, OP_IMM32(offset)));
    } else if(StringEqualsCstr(op, "<=")) {
        genInstruction(ctx, INST(jle, OP_IMM32(offset)));
    } else if(StringEqualsCstr(op, ">=")) {
        genInstruction(ctx, INST(jge, OP_IMM32(offset)));
    }
}

// stackToRestore is the value set to the stack in the GenContext when generating a ret instruction
void gen_x86_64_scope(GenContext* ctx, Scope* scope, s64 stackToRestore, bool mainScope) {
    // TODO: cast
    for(u64 i = 0; i < (u64)scope->stmts.size; ++i) {
        ASTNode* node = scope->stmts.statements[i];

        switch(node->type) {
            case ASTNodeType_NONE:
            case ASTNodeType_COUNT: {
                printf("[ERROR] ast node none and count are errors\n");
                exit(EXIT_FAILURE);
            } break;

            case ASTNodeType_VAR_DECL_ASSIGN: {
                ASTNode* exprNode = node->node.VAR_DECL_ASSIGN.expresion;
                String id = node->node.VAR_DECL_ASSIGN.identifier;
                ASTNode* type = node->node.VAR_DECL_ASSIGN.type;
                UNUSED(type);

                gen_x86_64_expression(ctx, exprNode); // gen code for the expression, store in rax??
                hashmapSet(&ctx->variables, id, ctx->stackPointer); // store the variable location on the stack in the context
                genPush(ctx, RAX); // push the variable value to the stack
            } break;
            case ASTNodeType_FUNCTION_DEF: {
                String id = node->node.FUNCTION_DEF.identifier;
                Scope* scope = node->node.FUNCTION_DEF.scope;
                Args args = node->node.FUNCTION_DEF.args;
                ASTNode* retType = node->node.FUNCTION_DEF.type;
                UNUSED(retType);

                // NOTE: would be usefull here to generating code into a separate buffer
                // TODO: jump after function
                // TODO: make the main entrypoint name customisable
                bool mainFunction = StringEqualsCstr(id, "main");
                if(mainFunction) ctx->entryPointOffset = ctx->code.size;

                genPush(ctx, RBP);
                genInstruction(ctx, INST(mov, OP_REG(RBP), OP_REG(RSP)));
                s64 savedStack = ctx->stackPointer;

                // function arguments
                // TODO: cast
                for(u64 i = 0; i < (u64)args.size; ++i) {
                    String argId = args.args[i]->node.VAR_DECL.identifier;
                    ASTNode* argType = args.args[i]->node.VAR_DECL.type;
                    UNUSED(argType);

                    if(i == 0) {
                        hashmapSet(&ctx->variables, argId, ctx->stackPointer);
                        genPush(ctx, RCX);
                    } else if(i == 1) {
                        hashmapSet(&ctx->variables, argId, ctx->stackPointer);
                        genPush(ctx, RDX);
                    } else if(i == 2) {
                        hashmapSet(&ctx->variables, argId, ctx->stackPointer);
                        genPush(ctx, R8);
                    } else if(i == 3) {
                        hashmapSet(&ctx->variables, argId, ctx->stackPointer);
                        genPush(ctx, R9);
                    } else {
                        hashmapSet(&ctx->variables, argId, ctx->stackPointer);
                        genInstruction(ctx, INST(mov, OP_REG(RAX), OP_INDIRECT_SIB(RBP, X8, i - 3)));
                        genPush(ctx, RAX);
                    }
                }

                // function body
                gen_x86_64_scope(ctx, scope, savedStack, mainFunction);
                // TODO: the jump after function should end up here
            } break;
            case ASTNodeType_FUNCTION_CALL: {
                gen_x86_64_func_call(ctx, node);
            } break;
            case ASTNodeType_VAR_DECL: {
                String id = node->node.VAR_DECL.identifier;
                ASTNode* type = node->node.VAR_DECL.type;
                UNUSED(type);

                hashmapSet(&ctx->variables, id, ctx->stackPointer);
                // NOTE: 8 is the byte size of a 64 stack slot, maybe sould not be hardcoded
                genInstruction(ctx, INST(sub, OP_REG(RSP), OP_IMM8(8)));
                ctx->stackPointer++;
            } break;
            case ASTNodeType_VAR_REASSIGN: {
                String id = node->node.VAR_REASSIGN.identifier;
                ASTNode* exprNode = node->node.VAR_REASSIGN.expresion;

                gen_x86_64_expression(ctx, exprNode);

                s64 value;
                if(!hashmapGet(&ctx->variables, id, &value)) {
                    UNREACHABLE("undefined variable");
                }
                s64 savedStack = ctx->stackPointer;
                genInstruction(ctx, INST(lea, OP_REG(RSP), OP_INDIRECT_SIB(RBP, X8, -(value - 1))));
                genPush(ctx, RAX);
                genInstruction(ctx, INST(lea, OP_REG(RSP), OP_INDIRECT_SIB(RBP, X8, -savedStack)));
                // TODO: reseting the stack to its original position is only neccesary if it didnt alrady get reset by the push
                // ie.: if((savedStack - 1) - (loc - 1) != 1) reset stack pos
                ctx->stackPointer = savedStack;
            } break;
            case ASTNodeType_VAR_CONST: {
                String id = node->node.VAR_CONST.identifier;
                String value = node->node.VAR_CONST.value;
                // TODO: will need a type
                
                // TODO: this is broken, memory allocation needed for data field of UserDataEntry
                u64 valueData = StringToU64(value);
                UserDataEntry data = {
                    .data = (u8*)&valueData,
                    .dataLen = sizeof(data),
                    .dataRVA = INVALID_ADDRESS
                };
                if(!hashmapDataSet(&ctx->data, id, data)) {
                    UNREACHABLE("hashmap full");
                }
            } break;
            case ASTNodeType_RET: {
                // NOTE: dead code elimination, currently internal stack underflows if there is more than one return in a add
                ASTNode* expr = node->node.RET.expresion;

                gen_x86_64_expression(ctx, expr);
                genInstruction(ctx, INST(mov, OP_REG(RSP), OP_REG(RBP)));
                ctx->stackPointer = stackToRestore;
                genPop(ctx, RBP);

                if(mainScope) {
                    genInstruction(ctx, INST(mov, OP_REG(RCX), OP_REG(RAX)));
                    gen_callExtern(ctx, STR("ExitProcess"));
                } else {
                    // NOTE: the {0} argument is to get rid of an annoying c warning, just using 0 is enough
                    //       this is used to indicate that the instruction has no operands
                    genInstruction(ctx, INST(ret, {0}));
                }
            } break;
            case ASTNodeType_IF: {
                ASTNode* next = node;
                do {
                    if(next->type != ASTNodeType_IF) i++;
                    if(next->type == ASTNodeType_IF) {
                        ASTNode* expr = next->node.IF.expresion;
                        Scope* scope = next->node.IF.scope;

                        // conditions
                        gen_x86_64_condition(ctx, expr);

                        // body
                        gen_x86_64_scope(ctx, scope, stackToRestore, mainScope);
                        UNIMPLEMENTED("unfished, need out of order appending for instructions");
                    }
                } while(next->type == ASTNodeType_IF || next->type == ASTNodeType_ELSE || next->type == ASTNodeType_ELSE_IF);
            } break;
            case ASTNodeType_LOOP: {
                UNIMPLEMENTED("ASTNodeType_LOOP in codegen");
            } break;
            
            case ASTNodeType_ELSE:
            case ASTNodeType_ELSE_IF:
            case ASTNodeType_EXPRESION:
            case ASTNodeType_INT_LIT:
            case ASTNodeType_FLOAT_LIT:
            case ASTNodeType_STRING_LIT:
            case ASTNodeType_BOOL_LIT:
            case ASTNodeType_SYMBOL:
            case ASTNodeType_TYPE:
                printf("[ERROR] Unhandled AST Node type: %s\n", ASTNodeTypeStr[node->type]);
                break;
        }
    }
}

GenContext gen_x86_64_bytecode(Scope* globalScope) {
    GenContext ctx = {0};
    ctx.code = make_buffer(0x100, PAGE_READWRITE);

    ctx.symbolsToPatch = make_buffer(0x100, PAGE_READWRITE);
    ctx.functionsToPatch = make_buffer(0x100, PAGE_READWRITE);
    ctx.dataToPatch = make_buffer(0x100, PAGE_READWRITE);

    ctx.variables = hashmapInit(&ctx.mem, 0x1000);
    ctx.functions = hashmapInit(&ctx.mem, 0x1000);
    ctx.data  = hashmapDataInit(&ctx.mem, 0x1000);
    
    gen_x86_64_scope(&ctx, globalScope, 0, FALSE);
    return ctx;
}
