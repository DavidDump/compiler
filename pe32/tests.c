void testAdd(EmiterContext* context) {
    // add RAX, RBX
    genInstruction(context, INST(add, OP_REG(RAX), OP_REG(RBX)));
    // add RAX, [RBX]
    genInstruction(context, INST(add, OP_REG(RAX), OP_INDIRECT(RBX)));
    // add RAX, [RBX + 0x12]
    genInstruction(context, INST(add, OP_REG(RAX), OP_INDIRECT_OFFSET8(RBX, 0x12)));
    // add RAX, [RBX + 0x12345678]
    genInstruction(context, INST(add, OP_REG(RAX), OP_INDIRECT_OFFSET32(RBX, 0x12345678)));
    // add RAX, [RBX + X2 * RCX]
    genInstruction(context, INST(add, OP_REG(RAX), OP_INDIRECT_SIB(RBX, X2, RCX)));
    // add RAX, [RBX + X2 * RCX + 0x12]
    genInstruction(context, INST(add, OP_REG(RAX), OP_INDIRECT_SIB_OFFSET8(RBX, X2, RCX, 0x12)));
    // add RAX, [RBX + X2 * RCX + 0x12345678]
    genInstruction(context, INST(add, OP_REG(RAX), OP_INDIRECT_SIB_OFFSET32(RBX, X2, RCX, 0x12345678)));
    // add RAX, [rip + 0x12345678]
    genInstruction(context, INST(add, OP_REG(RAX), OP_RIP(0x12345678)));
    // add RAX, [0x12345678]
    genInstruction(context, INST(add, OP_REG(RAX), OP_ABSOLUTE(0x12345678)));
    // add RAX, 0x12
    genInstruction(context, INST(add, OP_REG(RAX), OP_IMM8(0x12)));
    // add RAX, 0x12345678
    genInstruction(context, INST(add, OP_REG(RAX), OP_IMM32(0x12345678)));

    // add R8, R9
    genInstruction(context, INST(add, OP_REG(R8), OP_REG(R9)));
    // add R8, [R9]
    genInstruction(context, INST(add, OP_REG(R8), OP_INDIRECT(R9)));
    // add R8, [R9 + 0x12]
    genInstruction(context, INST(add, OP_REG(R8), OP_INDIRECT_OFFSET8(R9, 0x12)));
    // add R8, [R9 + 0x12345678]
    genInstruction(context, INST(add, OP_REG(R8), OP_INDIRECT_OFFSET32(R9, 0x12345678)));
    // add R8, [R9 + X2 * R10]
    genInstruction(context, INST(add, OP_REG(R8), OP_INDIRECT_SIB(R9, X2, R10)));
    // add R8, [R9 + X2 * R10 + 0x12]
    genInstruction(context, INST(add, OP_REG(R8), OP_INDIRECT_SIB_OFFSET8(R9, X2, R10, 0x12)));
    // add R8, [R9 + X2 * R10 + 0x12345678]
    genInstruction(context, INST(add, OP_REG(R8), OP_INDIRECT_SIB_OFFSET32(R9, X2, R10, 0x12345678)));
    // add R8, [rip + 0x12345678]
    genInstruction(context, INST(add, OP_REG(R8), OP_RIP(0x12345678)));
    // add R8, [0x12345678]
    genInstruction(context, INST(add, OP_REG(R8), OP_ABSOLUTE(0x12345678)));
    // add R8, 0x12
    genInstruction(context, INST(add, OP_REG(R8), OP_IMM8(0x12)));
    // add R8, 0x12345678
    genInstruction(context, INST(add, OP_REG(R8), OP_IMM32(0x12345678)));

    // swaped
    // add RBX, RAX
    genInstruction(context, INST(add, OP_REG(RBX), OP_REG(RAX)));
    // add [RBX], RAX
    genInstruction(context, INST(add, OP_INDIRECT(RBX), OP_REG(RAX)));
    // add [RBX + 0x12], RAX
    genInstruction(context, INST(add, OP_INDIRECT_OFFSET8(RBX, 0x12), OP_REG(RAX)));
    // add [RBX + 0x12345678], RAX
    genInstruction(context, INST(add, OP_INDIRECT_OFFSET32(RBX, 0x12345678), OP_REG(RAX)));
    // add [RBX + X2 * RCX], RAX
    genInstruction(context, INST(add, OP_INDIRECT_SIB(RBX, X2, RCX), OP_REG(RAX)));
    // add [RBX + X2 * RCX + 0x12], RAX
    genInstruction(context, INST(add, OP_INDIRECT_SIB_OFFSET8(RBX, X2, RCX, 0x12), OP_REG(RAX)));
    // add [RBX + X2 * RCX + 0x12345678], RAX
    genInstruction(context, INST(add, OP_INDIRECT_SIB_OFFSET32(RBX, X2, RCX, 0x12345678), OP_REG(RAX)));
    // add [rip + 0x12345678], RAX
    genInstruction(context, INST(add, OP_RIP(0x12345678), OP_REG(RAX)));
    // add [0x12345678], RAX
    genInstruction(context, INST(add, OP_ABSOLUTE(0x12345678), OP_REG(RAX)));

    // add R9, R8
    genInstruction(context, INST(add, OP_REG(R9), OP_REG(R8)));
    // add [R9], R8
    genInstruction(context, INST(add, OP_INDIRECT(R9), OP_REG(R8)));
    // add [R9 + 0x12], R8
    genInstruction(context, INST(add, OP_INDIRECT_OFFSET8(R9, 0x12), OP_REG(R8)));
    // add [R9 + 0x12345678], R8
    genInstruction(context, INST(add, OP_INDIRECT_OFFSET32(R9, 0x12345678), OP_REG(R8)));
    // add [R9 + X2 * R10], R8
    genInstruction(context, INST(add, OP_INDIRECT_SIB(R9, X2, R10), OP_REG(R8)));
    // add [R9 + X2 * R10 + 0x12], R8
    genInstruction(context, INST(add, OP_INDIRECT_SIB_OFFSET8(R9, X2, R10, 0x12), OP_REG(R8)));
    // add [R9 + X2 * R10 + 0x12345678], R8
    genInstruction(context, INST(add, OP_INDIRECT_SIB_OFFSET32(R9, X2, R10, 0x12345678), OP_REG(R8)));
    // add [rip + 0x12345678], R8
    genInstruction(context, INST(add, OP_RIP(0x12345678), OP_REG(R8)));
    // add [0x12345678], R8
    genInstruction(context, INST(add, OP_ABSOLUTE(0x12345678), OP_REG(R8)));

    // imm as source insted of reg
    // add RBX, 0x12
    genInstruction(context, INST(add, OP_REG(RBX), OP_IMM8(0x12)));
    // add [RBX], 0x12
    genInstruction(context, INST(add, OP_INDIRECT(RBX), OP_IMM8(0x12)));
    // add [RBX + 0x12], 0x12
    genInstruction(context, INST(add, OP_INDIRECT_OFFSET8(RBX, 0x12), OP_IMM8(0x12)));
    // add [RBX + 0x12345678], 0x12
    genInstruction(context, INST(add, OP_INDIRECT_OFFSET32(RBX, 0x12345678), OP_IMM8(0x12)));
    // add [RBX + X2 * RCX], 0x12
    genInstruction(context, INST(add, OP_INDIRECT_SIB(RBX, X2, RCX), OP_IMM8(0x12)));
    // add [RBX + X2 * RCX + 0x12], 0x12
    genInstruction(context, INST(add, OP_INDIRECT_SIB_OFFSET8(RBX, X2, RCX, 0x12), OP_IMM8(0x12)));
    // add [RBX + X2 * RCX + 0x12345678], 0x12
    genInstruction(context, INST(add, OP_INDIRECT_SIB_OFFSET32(RBX, X2, RCX, 0x12345678), OP_IMM8(0x12)));
    // add [rip + 0x12345678], 0x12
    genInstruction(context, INST(add, OP_RIP(0x12345678), OP_IMM8(0x12)));
    // add [0x12345678], 0x12
    genInstruction(context, INST(add, OP_ABSOLUTE(0x12345678), OP_IMM8(0x12)));

    // add R9, 0x12
    genInstruction(context, INST(add, OP_REG(R9), OP_IMM8(0x12)));
    // add [R9], 0x12
    genInstruction(context, INST(add, OP_INDIRECT(R9), OP_IMM8(0x12)));
    // add [R9 + 0x12], 0x12
    genInstruction(context, INST(add, OP_INDIRECT_OFFSET8(R9, 0x12), OP_IMM8(0x12)));
    // add [R9 + 0x12345678], 0x12
    genInstruction(context, INST(add, OP_INDIRECT_OFFSET32(R9, 0x12345678), OP_IMM8(0x12)));
    // add [R9 + X2 * R10], 0x12
    genInstruction(context, INST(add, OP_INDIRECT_SIB(R9, X2, R10), OP_IMM8(0x12)));
    // add [R9 + X2 * R10 + 0x12], 0x12
    genInstruction(context, INST(add, OP_INDIRECT_SIB_OFFSET8(R9, X2, R10, 0x12), OP_IMM8(0x12)));
    // add [R9 + X2 * R10 + 0x12345678], 0x12
    genInstruction(context, INST(add, OP_INDIRECT_SIB_OFFSET32(R9, X2, R10, 0x12345678), OP_IMM8(0x12)));
    // add [rip + 0x12345678], 0x12
    genInstruction(context, INST(add, OP_RIP(0x12345678), OP_IMM8(0x12)));
    // add [0x12345678], 0x12
    genInstruction(context, INST(add, OP_ABSOLUTE(0x12345678), OP_IMM8(0x12)));

    // add RBX, 0x12345678
    genInstruction(context, INST(add, OP_REG(RBX), OP_IMM32(0x12345678)));
    // add [RBX], 0x12345678
    genInstruction(context, INST(add, OP_INDIRECT(RBX), OP_IMM32(0x12345678)));
    // add [RBX + 0x12], 0x12345678
    genInstruction(context, INST(add, OP_INDIRECT_OFFSET8(RBX, 0x12), OP_IMM32(0x12345678)));
    // add [RBX + 0x12345678], 0x12345678
    genInstruction(context, INST(add, OP_INDIRECT_OFFSET32(RBX, 0x12345678), OP_IMM32(0x12345678)));
    // add [RBX + X2 * RCX], 0x12345678
    genInstruction(context, INST(add, OP_INDIRECT_SIB(RBX, X2, RCX), OP_IMM32(0x12345678)));
    // add [RBX + X2 * RCX + 0x12], 0x12345678
    genInstruction(context, INST(add, OP_INDIRECT_SIB_OFFSET8(RBX, X2, RCX, 0x12), OP_IMM32(0x12345678)));
    // add [RBX + X2 * RCX + 0x12345678], 0x12345678
    genInstruction(context, INST(add, OP_INDIRECT_SIB_OFFSET32(RBX, X2, RCX, 0x12345678), OP_IMM32(0x12345678)));
    // add [rip + 0x12345678], 0x12345678
    genInstruction(context, INST(add, OP_RIP(0x12345678), OP_IMM32(0x12345678)));
    // add [0x12345678], 0x12345678
    genInstruction(context, INST(add, OP_ABSOLUTE(0x12345678), OP_IMM32(0x12345678)));

    // add R9, 0x12345678
    genInstruction(context, INST(add, OP_REG(R9), OP_IMM32(0x12345678)));
    // add [R9], 0x12345678
    genInstruction(context, INST(add, OP_INDIRECT(R9), OP_IMM32(0x12345678)));
    // add [R9 + 0x12], 0x12345678
    genInstruction(context, INST(add, OP_INDIRECT_OFFSET8(R9, 0x12), OP_IMM32(0x12345678)));
    // add [R9 + 0x12345678], 0x12345678
    genInstruction(context, INST(add, OP_INDIRECT_OFFSET32(R9, 0x12345678), OP_IMM32(0x12345678)));
    // add [R9 + X2 * R10], 0x12345678
    genInstruction(context, INST(add, OP_INDIRECT_SIB(R9, X2, R10), OP_IMM32(0x12345678)));
    // add [R9 + X2 * R10 + 0x12], 0x12345678
    genInstruction(context, INST(add, OP_INDIRECT_SIB_OFFSET8(R9, X2, R10, 0x12), OP_IMM32(0x12345678)));
    // add [R9 + X2 * R10 + 0x12345678], 0x12345678
    genInstruction(context, INST(add, OP_INDIRECT_SIB_OFFSET32(R9, X2, R10, 0x12345678), OP_IMM32(0x12345678)));
    // add [rip + 0x12345678], 0x12345678
    genInstruction(context, INST(add, OP_RIP(0x12345678), OP_IMM32(0x12345678)));
    // add [0x12345678], 0x12345678
    genInstruction(context, INST(add, OP_ABSOLUTE(0x12345678), OP_IMM32(0x12345678)));
}

void testMov(EmiterContext* context) {
    // mov RAX, RBX
    genInstruction(context, INST(mov, OP_REG(RAX), OP_REG(RBX)));
    // mov RAX, [RBX]
    genInstruction(context, INST(mov, OP_REG(RAX), OP_INDIRECT(RBX)));
    // mov RAX, [RBX + 0x12]
    genInstruction(context, INST(mov, OP_REG(RAX), OP_INDIRECT_OFFSET8(RBX, 0x12)));
    // mov RAX, [RBX + 0x12345678]
    genInstruction(context, INST(mov, OP_REG(RAX), OP_INDIRECT_OFFSET32(RBX, 0x12345678)));
    // mov RAX, [RBX + X2 * RCX]
    genInstruction(context, INST(mov, OP_REG(RAX), OP_INDIRECT_SIB(RBX, X2, RCX)));
    // mov RAX, [RBX + X2 * RCX + 0x12]
    genInstruction(context, INST(mov, OP_REG(RAX), OP_INDIRECT_SIB_OFFSET8(RBX, X2, RCX, 0x12)));
    // mov RAX, [RBX + X2 * RCX + 0x12345678]
    genInstruction(context, INST(mov, OP_REG(RAX), OP_INDIRECT_SIB_OFFSET32(RBX, X2, RCX, 0x12345678)));
    // mov RAX, [rip + 0x12345678]
    genInstruction(context, INST(mov, OP_REG(RAX), OP_RIP(0x12345678)));
    // mov RAX, [0x12345678]
    genInstruction(context, INST(mov, OP_REG(RAX), OP_ABSOLUTE(0x12345678)));
    // mov RAX, 0x12
    genInstruction(context, INST(mov, OP_REG(RAX), OP_IMM8(0x12)));
    // mov RAX, 0x12345678
    genInstruction(context, INST(mov, OP_REG(RAX), OP_IMM32(0x12345678)));

    // mov R8, R9
    genInstruction(context, INST(mov, OP_REG(R8), OP_REG(R9)));
    // mov R8, [R9]
    genInstruction(context, INST(mov, OP_REG(R8), OP_INDIRECT(R9)));
    // mov R8, [R9 + 0x12]
    genInstruction(context, INST(mov, OP_REG(R8), OP_INDIRECT_OFFSET8(R9, 0x12)));
    // mov R8, [R9 + 0x12345678]
    genInstruction(context, INST(mov, OP_REG(R8), OP_INDIRECT_OFFSET32(R9, 0x12345678)));
    // mov R8, [R9 + X2 * R10]
    genInstruction(context, INST(mov, OP_REG(R8), OP_INDIRECT_SIB(R9, X2, R10)));
    // mov R8, [R9 + X2 * R10 + 0x12]
    genInstruction(context, INST(mov, OP_REG(R8), OP_INDIRECT_SIB_OFFSET8(R9, X2, R10, 0x12)));
    // mov R8, [R9 + X2 * R10 + 0x12345678]
    genInstruction(context, INST(mov, OP_REG(R8), OP_INDIRECT_SIB_OFFSET32(R9, X2, R10, 0x12345678)));
    // mov R8, [rip + 0x12345678]
    genInstruction(context, INST(mov, OP_REG(R8), OP_RIP(0x12345678)));
    // mov R8, [0x12345678]
    genInstruction(context, INST(mov, OP_REG(R8), OP_ABSOLUTE(0x12345678)));
    // mov R8, 0x12
    genInstruction(context, INST(mov, OP_REG(R8), OP_IMM8(0x12)));
    // mov R8, 0x12345678
    genInstruction(context, INST(mov, OP_REG(R8), OP_IMM32(0x12345678)));

    // swaped
    // mov RBX, RAX
    genInstruction(context, INST(mov, OP_REG(RBX), OP_REG(RAX)));
    // mov [RBX], RAX
    genInstruction(context, INST(mov, OP_INDIRECT(RBX), OP_REG(RAX)));
    // mov [RBX + 0x12], RAX
    genInstruction(context, INST(mov, OP_INDIRECT_OFFSET8(RBX, 0x12), OP_REG(RAX)));
    // mov [RBX + 0x12345678], RAX
    genInstruction(context, INST(mov, OP_INDIRECT_OFFSET32(RBX, 0x12345678), OP_REG(RAX)));
    // mov [RBX + X2 * RCX], RAX
    genInstruction(context, INST(mov, OP_INDIRECT_SIB(RBX, X2, RCX), OP_REG(RAX)));
    // mov [RBX + X2 * RCX + 0x12], RAX
    genInstruction(context, INST(mov, OP_INDIRECT_SIB_OFFSET8(RBX, X2, RCX, 0x12), OP_REG(RAX)));
    // mov [RBX + X2 * RCX + 0x12345678], RAX
    genInstruction(context, INST(mov, OP_INDIRECT_SIB_OFFSET32(RBX, X2, RCX, 0x12345678), OP_REG(RAX)));
    // mov [rip + 0x12345678], RAX
    genInstruction(context, INST(mov, OP_RIP(0x12345678), OP_REG(RAX)));
    // mov [0x12345678], RAX
    genInstruction(context, INST(mov, OP_ABSOLUTE(0x12345678), OP_REG(RAX)));

    // mov R9, R8
    genInstruction(context, INST(mov, OP_REG(R9), OP_REG(R8)));
    // mov [R9], R8
    genInstruction(context, INST(mov, OP_INDIRECT(R9), OP_REG(R8)));
    // mov [R9 + 0x12], R8
    genInstruction(context, INST(mov, OP_INDIRECT_OFFSET8(R9, 0x12), OP_REG(R8)));
    // mov [R9 + 0x12345678], R8
    genInstruction(context, INST(mov, OP_INDIRECT_OFFSET32(R9, 0x12345678), OP_REG(R8)));
    // mov [R9 + X2 * R10], R8
    genInstruction(context, INST(mov, OP_INDIRECT_SIB(R9, X2, R10), OP_REG(R8)));
    // mov [R9 + X2 * R10 + 0x12], R8
    genInstruction(context, INST(mov, OP_INDIRECT_SIB_OFFSET8(R9, X2, R10, 0x12), OP_REG(R8)));
    // mov [R9 + X2 * R10 + 0x12345678], R8
    genInstruction(context, INST(mov, OP_INDIRECT_SIB_OFFSET32(R9, X2, R10, 0x12345678), OP_REG(R8)));
    // mov [rip + 0x12345678], R8
    genInstruction(context, INST(mov, OP_RIP(0x12345678), OP_REG(R8)));
    // mov [0x12345678], R8
    genInstruction(context, INST(mov, OP_ABSOLUTE(0x12345678), OP_REG(R8)));

    // imm as source insted of reg
    // mov RBX, 0x12
    genInstruction(context, INST(mov, OP_REG(RBX), OP_IMM8(0x12)));
    // mov [RBX], 0x12
    genInstruction(context, INST(mov, OP_INDIRECT(RBX), OP_IMM8(0x12)));
    // mov [RBX + 0x12], 0x12
    genInstruction(context, INST(mov, OP_INDIRECT_OFFSET8(RBX, 0x12), OP_IMM8(0x12)));
    // mov [RBX + 0x12345678], 0x12
    genInstruction(context, INST(mov, OP_INDIRECT_OFFSET32(RBX, 0x12345678), OP_IMM8(0x12)));
    // mov [RBX + X2 * RCX], 0x12
    genInstruction(context, INST(mov, OP_INDIRECT_SIB(RBX, X2, RCX), OP_IMM8(0x12)));
    // mov [RBX + X2 * RCX + 0x12], 0x12
    genInstruction(context, INST(mov, OP_INDIRECT_SIB_OFFSET8(RBX, X2, RCX, 0x12), OP_IMM8(0x12)));
    // mov [RBX + X2 * RCX + 0x12345678], 0x12
    genInstruction(context, INST(mov, OP_INDIRECT_SIB_OFFSET32(RBX, X2, RCX, 0x12345678), OP_IMM8(0x12)));
    // mov [rip + 0x12345678], 0x12
    genInstruction(context, INST(mov, OP_RIP(0x12345678), OP_IMM8(0x12)));
    // mov [0x12345678], 0x12
    genInstruction(context, INST(mov, OP_ABSOLUTE(0x12345678), OP_IMM8(0x12)));

    // mov R9, 0x12
    genInstruction(context, INST(mov, OP_REG(R9), OP_IMM8(0x12)));
    // mov [R9], 0x12
    genInstruction(context, INST(mov, OP_INDIRECT(R9), OP_IMM8(0x12)));
    // mov [R9 + 0x12], 0x12
    genInstruction(context, INST(mov, OP_INDIRECT_OFFSET8(R9, 0x12), OP_IMM8(0x12)));
    // mov [R9 + 0x12345678], 0x12
    genInstruction(context, INST(mov, OP_INDIRECT_OFFSET32(R9, 0x12345678), OP_IMM8(0x12)));
    // mov [R9 + X2 * R10], 0x12
    genInstruction(context, INST(mov, OP_INDIRECT_SIB(R9, X2, R10), OP_IMM8(0x12)));
    // mov [R9 + X2 * R10 + 0x12], 0x12
    genInstruction(context, INST(mov, OP_INDIRECT_SIB_OFFSET8(R9, X2, R10, 0x12), OP_IMM8(0x12)));
    // mov [R9 + X2 * R10 + 0x12345678], 0x12
    genInstruction(context, INST(mov, OP_INDIRECT_SIB_OFFSET32(R9, X2, R10, 0x12345678), OP_IMM8(0x12)));
    // mov [rip + 0x12345678], 0x12
    genInstruction(context, INST(mov, OP_RIP(0x12345678), OP_IMM8(0x12)));
    // mov [0x12345678], 0x12
    genInstruction(context, INST(mov, OP_ABSOLUTE(0x12345678), OP_IMM8(0x12)));

    // mov RBX, 0x12345678
    genInstruction(context, INST(mov, OP_REG(RBX), OP_IMM32(0x12345678)));
    // mov [RBX], 0x12345678
    genInstruction(context, INST(mov, OP_INDIRECT(RBX), OP_IMM32(0x12345678)));
    // mov [RBX + 0x12], 0x12345678
    genInstruction(context, INST(mov, OP_INDIRECT_OFFSET8(RBX, 0x12), OP_IMM32(0x12345678)));
    // mov [RBX + 0x12345678], 0x12345678
    genInstruction(context, INST(mov, OP_INDIRECT_OFFSET32(RBX, 0x12345678), OP_IMM32(0x12345678)));
    // mov [RBX + X2 * RCX], 0x12345678
    genInstruction(context, INST(mov, OP_INDIRECT_SIB(RBX, X2, RCX), OP_IMM32(0x12345678)));
    // mov [RBX + X2 * RCX + 0x12], 0x12345678
    genInstruction(context, INST(mov, OP_INDIRECT_SIB_OFFSET8(RBX, X2, RCX, 0x12), OP_IMM32(0x12345678)));
    // mov [RBX + X2 * RCX + 0x12345678], 0x12345678
    genInstruction(context, INST(mov, OP_INDIRECT_SIB_OFFSET32(RBX, X2, RCX, 0x12345678), OP_IMM32(0x12345678)));
    // mov [rip + 0x12345678], 0x12345678
    genInstruction(context, INST(mov, OP_RIP(0x12345678), OP_IMM32(0x12345678)));
    // mov [0x12345678], 0x12345678
    genInstruction(context, INST(mov, OP_ABSOLUTE(0x12345678), OP_IMM32(0x12345678)));

    // mov R9, 0x12345678
    genInstruction(context, INST(mov, OP_REG(R9), OP_IMM32(0x12345678)));
    // mov [R9], 0x12345678
    genInstruction(context, INST(mov, OP_INDIRECT(R9), OP_IMM32(0x12345678)));
    // mov [R9 + 0x12], 0x12345678
    genInstruction(context, INST(mov, OP_INDIRECT_OFFSET8(R9, 0x12), OP_IMM32(0x12345678)));
    // mov [R9 + 0x12345678], 0x12345678
    genInstruction(context, INST(mov, OP_INDIRECT_OFFSET32(R9, 0x12345678), OP_IMM32(0x12345678)));
    // mov [R9 + X2 * R10], 0x12345678
    genInstruction(context, INST(mov, OP_INDIRECT_SIB(R9, X2, R10), OP_IMM32(0x12345678)));
    // mov [R9 + X2 * R10 + 0x12], 0x12345678
    genInstruction(context, INST(mov, OP_INDIRECT_SIB_OFFSET8(R9, X2, R10, 0x12), OP_IMM32(0x12345678)));
    // mov [R9 + X2 * R10 + 0x12345678], 0x12345678
    genInstruction(context, INST(mov, OP_INDIRECT_SIB_OFFSET32(R9, X2, R10, 0x12345678), OP_IMM32(0x12345678)));
    // mov [rip + 0x12345678], 0x12345678
    genInstruction(context, INST(mov, OP_RIP(0x12345678), OP_IMM32(0x12345678)));
    // mov [0x12345678], 0x12345678
    genInstruction(context, INST(mov, OP_ABSOLUTE(0x12345678), OP_IMM32(0x12345678)));
}

void testPush(EmiterContext* context) {
    // push rax
    genInstruction(context, INST(push, OP_REG(RAX)));
    // push [rax]
    genInstruction(context, INST(push, OP_INDIRECT(RAX)));
    // push [rax + 0x12]
    genInstruction(context, INST(push, OP_INDIRECT_OFFSET8(RAX, 0x12)));
    // push [rax + 0x12345678]
    genInstruction(context, INST(push, OP_INDIRECT_OFFSET32(RAX, 0x12345678)));
    // push [rax + X2 * rbx]
    genInstruction(context, INST(push, OP_INDIRECT_SIB(RAX, X2, RBX)));
    // push [rax + X2 * rbx + 0x12]
    genInstruction(context, INST(push, OP_INDIRECT_SIB_OFFSET8(RAX, X2, RBX, 0x12)));
    // push [rax + X2 * rbx + 0x12345678]
    genInstruction(context, INST(push, OP_INDIRECT_SIB_OFFSET32(RAX, X2, RBX, 0x12345678)));
    // push [rip + 0x12345678]
    genInstruction(context, INST(push, OP_RIP(0x12345678)));
    // push [0x12345678]
    genInstruction(context, INST(push, OP_ABSOLUTE(0x12345678)));
    // push 0x12
    genInstruction(context, INST(push, OP_IMM8(0x12)));
    // push 0x12345678
    genInstruction(context, INST(push, OP_IMM32(0x12345678)));

    // extended registers
    // push rax
    genInstruction(context, INST(push, OP_REG(R8)));
    // push [rax]
    genInstruction(context, INST(push, OP_INDIRECT(R8)));
    // push [rax + 0x12]
    genInstruction(context, INST(push, OP_INDIRECT_OFFSET8(R8, 0x12)));
    // push [rax + 0x12345678]
    genInstruction(context, INST(push, OP_INDIRECT_OFFSET32(R8, 0x12345678)));
    // push [rax + X2 * rbx]
    genInstruction(context, INST(push, OP_INDIRECT_SIB(R8, X2, R9)));
    // push [rax + X2 * rbx + 0x12]
    genInstruction(context, INST(push, OP_INDIRECT_SIB_OFFSET8(R8, X2, R9, 0x12)));
    // push [rax + X2 * rbx + 0x12345678]
    genInstruction(context, INST(push, OP_INDIRECT_SIB_OFFSET32(R8, X2, R9, 0x12345678)));
}

void testPop(EmiterContext* context) {
    // pop rax
    genInstruction(context, INST(pop, OP_REG(RAX)));
    // pop [rax]
    genInstruction(context, INST(pop, OP_INDIRECT(RAX)));
    // pop [rax + 0x12]
    genInstruction(context, INST(pop, OP_INDIRECT_OFFSET8(RAX, 0x12)));
    // pop [rax + 0x12345678]
    genInstruction(context, INST(pop, OP_INDIRECT_OFFSET32(RAX, 0x12345678)));
    // pop [rax + X2 * rbx]
    genInstruction(context, INST(pop, OP_INDIRECT_SIB(RAX, X2, RBX)));
    // pop [rax + X2 * rbx + 0x12]
    genInstruction(context, INST(pop, OP_INDIRECT_SIB_OFFSET8(RAX, X2, RBX, 0x12)));
    // pop [rax + X2 * rbx + 0x12345678]
    genInstruction(context, INST(pop, OP_INDIRECT_SIB_OFFSET32(RAX, X2, RBX, 0x12345678)));
    // pop [rip + 0x12345678]
    genInstruction(context, INST(pop, OP_RIP(0x12345678)));
    // pop [0x12345678]
    genInstruction(context, INST(pop, OP_ABSOLUTE(0x12345678)));

    // extended registers
    // pop rax
    genInstruction(context, INST(pop, OP_REG(R8)));
    // pop [rax]
    genInstruction(context, INST(pop, OP_INDIRECT(R8)));
    // pop [rax + 0x12]
    genInstruction(context, INST(pop, OP_INDIRECT_OFFSET8(R8, 0x12)));
    // pop [rax + 0x12345678]
    genInstruction(context, INST(pop, OP_INDIRECT_OFFSET32(R8, 0x12345678)));
    // pop [rax + X2 * rbx]
    genInstruction(context, INST(pop, OP_INDIRECT_SIB(R8, X2, R9)));
    // pop [rax + X2 * rbx + 0x12]
    genInstruction(context, INST(pop, OP_INDIRECT_SIB_OFFSET8(R8, X2, R9, 0x12)));
    // pop [rax + X2 * rbx + 0x12345678]
    genInstruction(context, INST(pop, OP_INDIRECT_SIB_OFFSET32(R8, X2, R9, 0x12345678)));
}

void testCall(EmiterContext* context) {
    // call rax
    genInstruction(context, INST(call, OP_REG(RAX)));
    // call [rax]
    genInstruction(context, INST(call, OP_INDIRECT(RAX)));
    // call [rax + 0x12]
    genInstruction(context, INST(call, OP_INDIRECT_OFFSET8(RAX, 0x12)));
    // call [rax + 0x12345678]
    genInstruction(context, INST(call, OP_INDIRECT_OFFSET32(RAX, 0x12345678)));
    // call [rax + X2 * rbx]
    genInstruction(context, INST(call, OP_INDIRECT_SIB(RAX, X2, RBX)));
    // call [rax + X2 * rbx + 0x12]
    genInstruction(context, INST(call, OP_INDIRECT_SIB_OFFSET8(RAX, X2, RBX, 0x12)));
    // call [rax + X2 * rbx + 0x12345678]
    genInstruction(context, INST(call, OP_INDIRECT_SIB_OFFSET32(RAX, X2, RBX, 0x12345678)));
    // call [rip + 0x12345678]
    genInstruction(context, INST(call, OP_RIP(0x12345678)));
    // call [0x12345678]
    genInstruction(context, INST(call, OP_ABSOLUTE(0x12345678)));

    // extended registers
    // call rax
    genInstruction(context, INST(call, OP_REG(R8)));
    // call [rax]
    genInstruction(context, INST(call, OP_INDIRECT(R8)));
    // call [rax + 0x12]
    genInstruction(context, INST(call, OP_INDIRECT_OFFSET8(R8, 0x12)));
    // call [rax + 0x12345678]
    genInstruction(context, INST(call, OP_INDIRECT_OFFSET32(R8, 0x12345678)));
    // call [rax + X2 * rbx]
    genInstruction(context, INST(call, OP_INDIRECT_SIB(R8, X2, R9)));
    // call [rax + X2 * rbx + 0x12]
    genInstruction(context, INST(call, OP_INDIRECT_SIB_OFFSET8(R8, X2, R9, 0x12)));
    // call [rax + X2 * rbx + 0x12345678]
    genInstruction(context, INST(call, OP_INDIRECT_SIB_OFFSET32(R8, X2, R9, 0x12345678)));
}

// Testing the low level emmiters
EmiterContext testEmmiters() {
    EmiterContext ctx = {0};

    for(Register dest = RAX; dest <= R15; dest++){
        for(Register source = RAX; source <= R15; source++){
            EmitRexByte(&ctx, 1, 0, 0, 0);
            Emit8(&ctx, 0x8b);
            EmitDirect(&ctx, dest, source);
            
            if((source & 7) != RSP && (source & 7) != RBP){
                EmitRexByte(&ctx, 1, 0, 0, 0);
                Emit8(&ctx, 0x8b);
                EmitIndirect(&ctx, dest, source);
            }
            
            if((source & 7) != RSP){
                EmitRexByte(&ctx, 1, 0, 0, 0);
                Emit8(&ctx, 0x8b);
                EmitIndirectDisplaced8(&ctx, dest, source, 0x12);
                
                EmitRexByte(&ctx, 1, 0, 0, 0);
                Emit8(&ctx, 0x8b);
                EmitIndirectDisplaced32(&ctx, dest, source, 0x1234);
            }
            
            for(Scale scale = X0; scale <= X8; scale++){
                if((source & 7) != RBP){
                    EmitRexByte(&ctx, 1, 0, 0, 0);
                    Emit8(&ctx, 0x8b);
                    EmitIndirectSIB(&ctx, dest, source, dest, scale);
                }

                EmitRexByte(&ctx, 1, 0, 0, 0);
                Emit8(&ctx, 0x8b);
                EmitIndirectDisplaced8SIB(&ctx, dest, source, dest, scale, 0x12);
                
                EmitRexByte(&ctx, 1, 0, 0, 0);
                Emit8(&ctx, 0x8b);
                EmitIndirectDisplaced32SIB(&ctx, dest, source, dest, scale, 0x1234);
            }
            
            EmitRexByte(&ctx, 1, 0, 0, 0);
            Emit8(&ctx, 0x8b);
            EmitIndirectDisplacedRip(&ctx, dest, 0x1234);
            
            EmitRexByte(&ctx, 1, 0, 0, 0);
            Emit8(&ctx, 0x8b);
            EmitIndirectAbsolute(&ctx, dest, 0x1234);
        }
    }

    return ctx;
}
