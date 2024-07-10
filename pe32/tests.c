#define OP_REG(r) (Operand){.type = OPERAND_Register, .REGISTER = {.reg = (r)}}
#define OP_INDIRECT(r) (Operand){.type = OPERAND_AddrInReg, .REGISTER = {.reg = (r)}}
#define OP_INDIRECT_OFFSET8(r, disp) (Operand){.type = OPERAND_AddrInRegOffset8, .REGISTER_OFFSET8 = {.reg = (r), .offset = (disp)}}
#define OP_INDIRECT_OFFSET32(r, disp) (Operand){.type = OPERAND_AddrInRegOffset32, .REGISTER_OFFSET32 = {.reg = (r), .offset = (disp)}}
#define OP_INDIRECT_SIB(b, s, i) (Operand){.type = OPERAND_SIB, .SIB = {.base = (b), .scale = (s), .index = (i)}}
#define OP_INDIRECT_SIB_OFFSET8(b, s, i, disp) (Operand){.type = OPERAND_SIBOffset8, .SIB_OFFSET8 = {.base = (b), .scale = (s), .index = (i), .offset = (disp)}}
#define OP_INDIRECT_SIB_OFFSET32(b, s, i, disp) (Operand){.type = OPERAND_SIBOffset32, .SIB_OFFSET32 = {.base = (b), .scale = (s), .index = (i), .offset = (disp)}}
#define OP_RIP(disp) (Operand){.type = OPERAND_RIP, .RIP = {.offset = (disp)}}
#define OP_ABSOLUTE(disp) (Operand){.type = OPERAND_AbsoluteAddr, .ABSOLUTE_ADDR = {.addr = (disp)}}
#define OP_IMM8(imm) (Operand){.type = OPERAND_Immediate8, .IMMEDIATE8 = {.immediate = (imm)}}
#define OP_IMM32(imm) (Operand){.type = OPERAND_Immediate32, .IMMEDIATE32 = {.immediate = (imm)}}

// all the different addressing modes for the add instruction
void testAdd() {
    // add RAX, RBX
    gen_add(&ctx, OP_REG(RAX), OP_REG(RBX));
    // add RAX, [RBX]
    gen_add(&ctx, OP_REG(RAX), OP_INDIRECT(RBX));
    // add RAX, [RBX + 0x12]
    gen_add(&ctx, OP_REG(RAX), OP_INDIRECT_OFFSET8(RBX, 0x12));
    // add RAX, [RBX + 0x12345678]
    gen_add(&ctx, OP_REG(RAX), OP_INDIRECT_OFFSET32(RBX, 0x12345678));
    // add RAX, [RBX + X2 * RCX]
    gen_add(&ctx, OP_REG(RAX), OP_INDIRECT_SIB(RBX, X2, RCX));
    // add RAX, [RBX + X2 * RCX + 0x12]
    gen_add(&ctx, OP_REG(RAX), OP_INDIRECT_SIB_OFFSET8(RBX, X2, RCX, 0x12));
    // add RAX, [RBX + X2 * RCX + 0x12345678]
    gen_add(&ctx, OP_REG(RAX), OP_INDIRECT_SIB_OFFSET32(RBX, X2, RCX, 0x12345678));
    // add RAX, [rip + 0x12345678]
    gen_add(&ctx, OP_REG(RAX), OP_RIP(0x12345678));
    // add RAX, [0x12345678]
    gen_add(&ctx, OP_REG(RAX), OP_ABSOLUTE(0x12345678));
    // add RAX, 0x12
    gen_add(&ctx, OP_REG(RAX), OP_IMM8(0x12));
    // add RAX, 0x12345678
    gen_add(&ctx, OP_REG(RAX), OP_IMM32(0x12345678));

    // add R8, R9
    gen_add(&ctx, OP_REG(R8), OP_REG(R9));
    // add R8, [R9]
    gen_add(&ctx, OP_REG(R8), OP_INDIRECT(R9));
    // add R8, [R9 + 0x12]
    gen_add(&ctx, OP_REG(R8), OP_INDIRECT_OFFSET8(R9, 0x12));
    // add R8, [R9 + 0x12345678]
    gen_add(&ctx, OP_REG(R8), OP_INDIRECT_OFFSET32(R9, 0x12345678));
    // add R8, [R9 + X2 * R10]
    gen_add(&ctx, OP_REG(R8), OP_INDIRECT_SIB(R9, X2, R10));
    // add R8, [R9 + X2 * R10 + 0x12]
    gen_add(&ctx, OP_REG(R8), OP_INDIRECT_SIB_OFFSET8(R9, X2, R10, 0x12));
    // add R8, [R9 + X2 * R10 + 0x12345678]
    gen_add(&ctx, OP_REG(R8), OP_INDIRECT_SIB_OFFSET32(R9, X2, R10, 0x12345678));
    // add R8, [rip + 0x12345678]
    gen_add(&ctx, OP_REG(R8), OP_RIP(0x12345678));
    // add R8, [0x12345678]
    gen_add(&ctx, OP_REG(R8), OP_ABSOLUTE(0x12345678));
    // add R8, 0x12
    gen_add(&ctx, OP_REG(R8), OP_IMM8(0x12));
    // add R8, 0x12345678
    gen_add(&ctx, OP_REG(R8), OP_IMM32(0x12345678));

    // swaped
    // add RBX, RAX
    gen_add(&ctx, OP_REG(RBX), OP_REG(RAX));
    // add [RBX], RAX
    gen_add(&ctx, OP_INDIRECT(RBX), OP_REG(RAX));
    // add [RBX + 0x12], RAX
    gen_add(&ctx, OP_INDIRECT_OFFSET8(RBX, 0x12), OP_REG(RAX));
    // add [RBX + 0x12345678], RAX
    gen_add(&ctx, OP_INDIRECT_OFFSET32(RBX, 0x12345678), OP_REG(RAX));
    // add [RBX + X2 * RCX], RAX
    gen_add(&ctx, OP_INDIRECT_SIB(RBX, X2, RCX), OP_REG(RAX));
    // add [RBX + X2 * RCX + 0x12], RAX
    gen_add(&ctx, OP_INDIRECT_SIB_OFFSET8(RBX, X2, RCX, 0x12), OP_REG(RAX));
    // add [RBX + X2 * RCX + 0x12345678], RAX
    gen_add(&ctx, OP_INDIRECT_SIB_OFFSET32(RBX, X2, RCX, 0x12345678), OP_REG(RAX));
    // add [rip + 0x12345678], RAX
    gen_add(&ctx, OP_RIP(0x12345678), OP_REG(RAX));
    // add [0x12345678], RAX
    gen_add(&ctx, OP_ABSOLUTE(0x12345678), OP_REG(RAX));

    // add R9, R8
    gen_add(&ctx, OP_REG(R9), OP_REG(R8));
    // add [R9], R8
    gen_add(&ctx, OP_INDIRECT(R9), OP_REG(R8));
    // add [R9 + 0x12], R8
    gen_add(&ctx, OP_INDIRECT_OFFSET8(R9, 0x12), OP_REG(R8));
    // add [R9 + 0x12345678], R8
    gen_add(&ctx, OP_INDIRECT_OFFSET32(R9, 0x12345678), OP_REG(R8));
    // add [R9 + X2 * R10], R8
    gen_add(&ctx, OP_INDIRECT_SIB(R9, X2, R10), OP_REG(R8));
    // add [R9 + X2 * R10 + 0x12], R8
    gen_add(&ctx, OP_INDIRECT_SIB_OFFSET8(R9, X2, R10, 0x12), OP_REG(R8));
    // add [R9 + X2 * R10 + 0x12345678], R8
    gen_add(&ctx, OP_INDIRECT_SIB_OFFSET32(R9, X2, R10, 0x12345678), OP_REG(R8));
    // add [rip + 0x12345678], R8
    gen_add(&ctx, OP_RIP(0x12345678), OP_REG(R8));
    // add [0x12345678], R8
    gen_add(&ctx, OP_ABSOLUTE(0x12345678), OP_REG(R8));

    // imm as source insted of reg
    // add RBX, 0x12
    gen_add(&ctx, OP_REG(RBX), OP_IMM8(0x12));
    // add [RBX], 0x12
    gen_add(&ctx, OP_INDIRECT(RBX), OP_IMM8(0x12));
    // add [RBX + 0x12], 0x12
    gen_add(&ctx, OP_INDIRECT_OFFSET8(RBX, 0x12), OP_IMM8(0x12));
    // add [RBX + 0x12345678], 0x12
    gen_add(&ctx, OP_INDIRECT_OFFSET32(RBX, 0x12345678), OP_IMM8(0x12));
    // add [RBX + X2 * RCX], 0x12
    gen_add(&ctx, OP_INDIRECT_SIB(RBX, X2, RCX), OP_IMM8(0x12));
    // add [RBX + X2 * RCX + 0x12], 0x12
    gen_add(&ctx, OP_INDIRECT_SIB_OFFSET8(RBX, X2, RCX, 0x12), OP_IMM8(0x12));
    // add [RBX + X2 * RCX + 0x12345678], 0x12
    gen_add(&ctx, OP_INDIRECT_SIB_OFFSET32(RBX, X2, RCX, 0x12345678), OP_IMM8(0x12));
    // add [rip + 0x12345678], 0x12
    gen_add(&ctx, OP_RIP(0x12345678), OP_IMM8(0x12));
    // add [0x12345678], 0x12
    gen_add(&ctx, OP_ABSOLUTE(0x12345678), OP_IMM8(0x12));

    // add R9, 0x12
    gen_add(&ctx, OP_REG(R9), OP_IMM8(0x12));
    // add [R9], 0x12
    gen_add(&ctx, OP_INDIRECT(R9), OP_IMM8(0x12));
    // add [R9 + 0x12], 0x12
    gen_add(&ctx, OP_INDIRECT_OFFSET8(R9, 0x12), OP_IMM8(0x12));
    // add [R9 + 0x12345678], 0x12
    gen_add(&ctx, OP_INDIRECT_OFFSET32(R9, 0x12345678), OP_IMM8(0x12));
    // add [R9 + X2 * R10], 0x12
    gen_add(&ctx, OP_INDIRECT_SIB(R9, X2, R10), OP_IMM8(0x12));
    // add [R9 + X2 * R10 + 0x12], 0x12
    gen_add(&ctx, OP_INDIRECT_SIB_OFFSET8(R9, X2, R10, 0x12), OP_IMM8(0x12));
    // add [R9 + X2 * R10 + 0x12345678], 0x12
    gen_add(&ctx, OP_INDIRECT_SIB_OFFSET32(R9, X2, R10, 0x12345678), OP_IMM8(0x12));
    // add [rip + 0x12345678], 0x12
    gen_add(&ctx, OP_RIP(0x12345678), OP_IMM8(0x12));
    // add [0x12345678], 0x12
    gen_add(&ctx, OP_ABSOLUTE(0x12345678), OP_IMM8(0x12));

    // add RBX, 0x12345678
    gen_add(&ctx, OP_REG(RBX), OP_IMM32(0x12345678));
    // add [RBX], 0x12345678
    gen_add(&ctx, OP_INDIRECT(RBX), OP_IMM32(0x12345678));
    // add [RBX + 0x12], 0x12345678
    gen_add(&ctx, OP_INDIRECT_OFFSET8(RBX, 0x12), OP_IMM32(0x12345678));
    // add [RBX + 0x12345678], 0x12345678
    gen_add(&ctx, OP_INDIRECT_OFFSET32(RBX, 0x12345678), OP_IMM32(0x12345678));
    // add [RBX + X2 * RCX], 0x12345678
    gen_add(&ctx, OP_INDIRECT_SIB(RBX, X2, RCX), OP_IMM32(0x12345678));
    // add [RBX + X2 * RCX + 0x12], 0x12345678
    gen_add(&ctx, OP_INDIRECT_SIB_OFFSET8(RBX, X2, RCX, 0x12), OP_IMM32(0x12345678));
    // add [RBX + X2 * RCX + 0x12345678], 0x12345678
    gen_add(&ctx, OP_INDIRECT_SIB_OFFSET32(RBX, X2, RCX, 0x12345678), OP_IMM32(0x12345678));
    // add [rip + 0x12345678], 0x12345678
    gen_add(&ctx, OP_RIP(0x12345678), OP_IMM32(0x12345678));
    // add [0x12345678], 0x12345678
    gen_add(&ctx, OP_ABSOLUTE(0x12345678), OP_IMM32(0x12345678));

    // add R9, 0x12345678
    gen_add(&ctx, OP_REG(R9), OP_IMM32(0x12345678));
    // add [R9], 0x12345678
    gen_add(&ctx, OP_INDIRECT(R9), OP_IMM32(0x12345678));
    // add [R9 + 0x12], 0x12345678
    gen_add(&ctx, OP_INDIRECT_OFFSET8(R9, 0x12), OP_IMM32(0x12345678));
    // add [R9 + 0x12345678], 0x12345678
    gen_add(&ctx, OP_INDIRECT_OFFSET32(R9, 0x12345678), OP_IMM32(0x12345678));
    // add [R9 + X2 * R10], 0x12345678
    gen_add(&ctx, OP_INDIRECT_SIB(R9, X2, R10), OP_IMM32(0x12345678));
    // add [R9 + X2 * R10 + 0x12], 0x12345678
    gen_add(&ctx, OP_INDIRECT_SIB_OFFSET8(R9, X2, R10, 0x12), OP_IMM32(0x12345678));
    // add [R9 + X2 * R10 + 0x12345678], 0x12345678
    gen_add(&ctx, OP_INDIRECT_SIB_OFFSET32(R9, X2, R10, 0x12345678), OP_IMM32(0x12345678));
    // add [rip + 0x12345678], 0x12345678
    gen_add(&ctx, OP_RIP(0x12345678), OP_IMM32(0x12345678));
    // add [0x12345678], 0x12345678
    gen_add(&ctx, OP_ABSOLUTE(0x12345678), OP_IMM32(0x12345678));
}

// Testing the low level emmiters
EmiterContext testEmmiters() {
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

    return ctx;
}
