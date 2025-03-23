#include "instructions_x86.h"

#pragma GCC diagnostic ignored "-Wunused-variable"
const char* MnemonicStr[Mnemonic_COUNT] = {
    [push_] = "push",
    [pop_]  = "pop",
    [call_] = "call",
    [ret_]  = "ret",
    [mov_]  = "mov",
    [add_]  = "add",
    [sub_]  = "sub",
    [mul_]  = "mul",
    [div_]  = "div",
    [cmp_]  = "cmp",
    [lea_]  = "lea",
    [inc_]  = "inc",
    [dec_]  = "dec",
    [jmp_]  = "jmp",
    [je_]   = "je",
    [jne_]  = "jne",
    [jl_]   = "jl",
    [jg_]   = "jg",
    [jle_]  = "jle",
    [jge_]  = "jge",
    [neg_]  = "neg",
};

// NOTE: when needing to iterate all the encodings for one instruction,
// it should be safe to assume that there wont be MAX_ENCODING_FOR_INSTRUCTION number of encodings,
// so the loop can safely break on an empty encoding
InstructionEncoding encodings[][MAX_ENCODING_FOR_INSTRUCTION] = {
    [push_] = {
        // Opcode | Instruction | Op/En | 64-Bit Mode | Compat/Leg Mode | Description
        // FF /6  | PUSH r/m16  | M     | Valid       | Valid           | Push r/m16.
        // FF /6  | PUSH r/m32  | M     | N.E.        | Valid           | Push r/m32.
        // FF /6  | PUSH r/m64  | M     | Valid       | N.E.            | Push r/m64.
        {.type = InstructionType_64BIT, .rexType = RexByte_NoW, .opcode = 0xFF, .modRMType = ModRMType_EXT, .opcodeExtension = 6, .opTypes[0] = OpType_RM},
        
        // 50+rw  | PUSH r16    | O     | Valid       | Valid           | Push r16.
        // 50+rd  | PUSH r32    | O     | N.E.        | Valid           | Push r32.
        // 50+rd  | PUSH r64    | O     | Valid       | N.E.            | Push r64.
        {.type = InstructionType_64BIT, .rexType = RexByte_NoW, .opcode = 0x50, .regInOpcode = TRUE, .opTypes[0] = OpType_REG},
        
        // 6A ib  | PUSH imm8   | I     | Valid       | Valid           | Push imm8.
        // 68 iw  | PUSH imm16  | I     | Valid       | Valid           | Push imm16.
        // 68 id  | PUSH imm32  | I     | Valid       | Valid           | Push imm32.
        {.type = InstructionType_64BIT, .opcode = 0x68, .opTypes[0] = OpType_IMM32},
        
        // 0E     | PUSH CS     | ZO    | Invalid     | Valid           | Push CS.
        // 16     | PUSH SS     | ZO    | Invalid     | Valid           | Push SS.
        // 1E     | PUSH DS     | ZO    | Invalid     | Valid           | Push DS.
        // 06     | PUSH ES     | ZO    | Invalid     | Valid           | Push ES.
        // 0F A0  | PUSH FS     | ZO    | Valid       | Valid           | Push FS.
        // 0F A8  | PUSH GS     | ZO    | Valid       | Valid           | Push GS.
    },
    [pop_] = {
        // Opcode  | Instruction | Op/En | 64-Bit Mode | Compat/Leg Mode | Description
        // 8F /0   | POP r/m16   | M     | Valid       | Valid           | Pop top of stack into m16; increment stack pointer.
        // 8F /0   | POP r/m32   | M     | N.E.        | Valid           | Pop top of stack into m32; increment stack pointer.
        // 8F /0   | POP r/m64   | M     | Valid       | N.E.            | Pop top of stack into m64; increment stack pointer. Cannot encode 32-bit operand size.
        {.type = InstructionType_64BIT, .rexType = RexByte_NoW, .opcode = 0x8F, .modRMType = ModRMType_EXT, .opcodeExtension = 0, .opTypes[0] = OpType_RM},

        // 58+ rw  | POP r16     | O     | Valid       | Valid           | Pop top of stack into r16; increment stack pointer.
        // 58+ rd  | POP r32     | O     | N.E.        | Valid           | Pop top of stack into r32; increment stack pointer.
        // 58+ rd  | POP r64     | O     | Valid       | N.E.            | Pop top of stack into r64; increment stack pointer. Cannot encode 32-bit operand size.
        {.type = InstructionType_64BIT, .rexType = RexByte_NoW, .opcode = 0x58, .regInOpcode = TRUE, .opTypes[0] = OpType_REG},

        // 1F      | POP DS      | ZO    | Invalid     | Valid           | Pop top of stack into DS; increment stack pointer.
        // 07      | POP ES      | ZO    | Invalid     | Valid           | Pop top of stack into ES; increment stack pointer.
        // 17      | POP SS      | ZO    | Invalid     | Valid           | Pop top of stack into SS; increment stack pointer.
        // 0F A1   | POP FS      | ZO    | Valid       | Valid           | Pop top of stack into FS; increment stack pointer by 16 bits.
        // 0F A1   | POP FS      | ZO    | N.E.        | Valid           | Pop top of stack into FS; increment stack pointer by 32 bits.
        // 0F A1   | POP FS      | ZO    | Valid       | N.E.            | Pop top of stack into FS; increment stack pointer by 64 bits.
        // 0F A9   | POP GS      | ZO    | Valid       | Valid           | Pop top of stack into GS; increment stack pointer by 16 bits.
        // 0F A9   | POP GS      | ZO    | N.E.        | Valid           | Pop top of stack into GS; increment stack pointer by 32 bits.
        // 0F A9   | POP GS      | ZO    | Valid       | N.E.            | Pop top of stack into GS; increment stack pointer by 64 bits.
    },
    [call_] = {
        // Opcode      | Instruction   | Op/En | 64-bit Mode | Compat/Leg Mode | Description
        // E8 cw       | CALL rel16    | D     | N.S.        | Valid           | Call near, relative, displacement relative to next instruction.
        // E8 cd       | CALL rel32    | D     | Valid       | Valid           | Call near, relative, displacement relative to next instruction. 32-bit displacement sign extended to 64-bits in 64-bit mode.
        {.type = InstructionType_64BIT, .opcode = 0xE8, .opTypes[0] = OpType_IMM32},
        // FF /2       | CALL r/m16    | M     | N.E.        | Valid           | Call near, absolute indirect, address given in r/m16.
        // FF /2       | CALL r/m32    | M     | N.E.        | Valid           | Call near, absolute indirect, address given in r/m32.
        // FF /2       | CALL r/m64    | M     | Valid       | N.E.            | Call near, absolute indirect, address given in r/m64.
        {.type = InstructionType_64BIT, .rexType = RexByte_NoW, .opcode = 0xFF, .modRMType = ModRMType_EXT, .opcodeExtension = 2, .opTypes[0] = OpType_RM},
        // 9A cd       | CALL ptr16:16 | D     | Invalid     | Valid           | Call far, absolute, address given in operand.
        // 9A cp       | CALL ptr16:32 | D     | Invalid     | Valid           | Call far, absolute, address given in operand.
        // FF /3       | CALL m16:16   | M     | Valid       | Valid           | Call far, absolute indirect address given in m16:16. In 32-bit mode: if selector points to a gate, then RIP = 32-bit zero extended displacement taken from gate; else RIP = zero extended 16-bit offset from far pointer referenced in the instruction.
        // FF /3       | CALL m16:32   | M     | Valid       | Valid           | In 64-bit mode: If selector points to a gate, then RIP = 64-bit displacement taken from gate; else RIP = zero extended 32-bit offset from far pointer referenced in the instruction.
        // REX.W FF /3 | CALL m16:64   | M     | Valid       | N.E.            | In 64-bit mode: If selector points to a gate, then RIP = 64-bit displacement taken from gate; else RIP = 64-bit offset from far pointer referenced in the instruction.
    },
    [ret_] = {
        // Opcode | Instruction | Op/En | 64-Bit Mode | Compat/Leg Mode | Description
        // C3     | RET         | ZO    | Valid       | Valid           | Near return to calling procedure.
        {.type = InstructionType_64BIT, .opcode = 0xC3},
        // CB     | RET         | ZO    | Valid       | Valid           | Far return to calling procedure.
        // C2 iw  | RET imm16   | I     | Valid       | Valid           | Near return to calling procedure and pop imm16 bytes from stack.
        // CA iw  | RET imm16   | I     | Valid       | Valid           | Far return to calling procedure and pop imm16 bytes from stack.
    },
    [mov_] = {
        // Opcode            | Instruction            | Op/En | 64-Bit Mode | Compat/Leg Mode | Description
        // 88 /r             | MOV r/m8, r8           | MR    | Valid       | Valid           | Move r8 to r/m8.
        // REX + 88 /r       | MOV r/m81, r81         | MR    | Valid       | N.E.            | Move r8 to r/m8.
        // 89 /r             | MOV r/m16, r16         | MR    | Valid       | Valid           | Move r16 to r/m16.
        // 89 /r             | MOV r/m32, r32         | MR    | Valid       | Valid           | Move r32 to r/m32.
        // REX.W + 89 /r     | MOV r/m64, r64         | MR    | Valid       | N.E.            | Move r64 to r/m64.
        {.type = InstructionType_64BIT, .rexType = RexByte_W, .opcode = 0x89, .modRMType = ModRMType_REG, .opTypes[0] = OpType_RM, .opTypes[1] = OpType_REG},
        // 8A /r             | MOV r8, r/m8           | RM    | Valid       | Valid           | Move r/m8 to r8.
        {.type = InstructionType_8BIT, .opcode = 0x8A, .modRMType = ModRMType_REG, .opTypes[0] = OpType_REG, .opTypes[1] = OpType_RM},
        // REX + 8A /r       | MOV r8 , r/m8          | RM    | Valid       | N.E.            | Move r/m8 to r8.
        // 8B /r             | MOV r16, r/m16         | RM    | Valid       | Valid           | Move r/m16 to r16.
        {.type = InstructionType_16BIT, .opcode = 0x8B, .modRMType = ModRMType_REG, .opTypes[0] = OpType_REG, .opTypes[1] = OpType_RM},
        // 8B /r             | MOV r32, r/m32         | RM    | Valid       | Valid           | Move r/m32 to r32.
        {.type = InstructionType_32BIT, .opcode = 0x8B, .modRMType = ModRMType_REG, .opTypes[0] = OpType_REG, .opTypes[1] = OpType_RM},
        // REX.W + 8B /r     | MOV r64, r/m64         | RM    | Valid       | N.E.            | Move r/m64 to r64.
        {.type = InstructionType_64BIT, .rexType = RexByte_W, .opcode = 0x8B, .modRMType = ModRMType_REG, .opTypes[0] = OpType_REG, .opTypes[1] = OpType_RM},
        
        // 8C /r             | MOV r/m16, Sreg        | MR    | Valid       | Valid           | Move segment register to r/m16.
        // 8C /r             | MOV r16/r32/m16, Sreg  | MR    | Valid       | Valid           | Move zero extended 16-bit segment register to r16/r32/m16.
        // REX.W + 8C /r     | MOV r64/m16, Sreg      | MR    | Valid       | Valid           | Move zero extended 16-bit segment register to r64/m16.
        // 8E /r             | MOV Sreg, r/m16        | RM    | Valid       | Valid           | Move r/m16 to segment register.
        // REX.W + 8E /r     | MOV Sreg, r/m64        | RM    | Valid       | Valid           | Move lower 16 bits of r/m64 to segment register.
        
        // A0                | MOV AL, moffs8         | FD    | Valid       | Valid           | Move byte at (seg:offset) to AL.
        // REX.W + A0        | MOV AL, moffs8         | FD    | Valid       | N.E.            | Move byte at (offset) to AL.
        // A1                | MOV AX, moffs16        | FD    | Valid       | Valid           | Move word at (seg:offset) to AX.
        // A1                | MOV EAX, moffs32       | FD    | Valid       | Valid           | Move doubleword at (seg:offset) to EAX.
        // REX.W + A1        | MOV RAX, moffs64       | FD    | Valid       | N.E.            | Move quadword at (offset) to RAX.
        // A2                | MOV moffs8, AL         | TD    | Valid       | Valid           | Move AL to (seg:offset).
        // REX.W + A2        | MOV moffs8, AL         | TD    | Valid       | N.E.            | Move AL to (offset).
        // A3                | MOV moffs16, AX        | TD    | Valid       | Valid           | Move AX to (seg:offset).
        // A3                | MOV moffs32, EAX       | TD    | Valid       | Valid           | Move EAX to (seg:offset).
        // REX.W + A3        | MOV moffs64, RAX       | TD    | Valid       | N.E.            | Move RAX to (offset).
        
        // B0+ rb ib         | MOV r8, imm8           | OI    | Valid       | Valid           | Move imm8 to r8.
        // REX + B0+ rb ib   | MOV r8, imm8           | OI    | Valid       | N.E.            | Move imm8 to r8.
        // B8+ rw iw         | MOV r16, imm16         | OI    | Valid       | Valid           | Move imm16 to r16.
        // B8+ rd id         | MOV r32, imm32         | OI    | Valid       | Valid           | Move imm32 to r32.
        // REX.W + B8+ rd io | MOV r64, imm64         | OI    | Valid       | N.E.            | Move imm64 to r64.
        {.type = InstructionType_64BIT, .rexType = RexByte_W, .opcode = 0xB8, .regInOpcode = TRUE, .opTypes[0] = OpType_REG, .opTypes[1] = OpType_IMM64},
        // C6 /0 ib          | MOV r/m8, imm8         | MI    | Valid       | Valid           | Move imm8 to r/m8.
        {.type = InstructionType_64BIT, .opcode = 0xC6, .modRMType = ModRMType_EXT, .opcodeExtension = 0, .opTypes[0] = OpType_RM, .opTypes[1] = OpType_IMM8},
        // REX + C6 /0 ib    | MOV r/m8, imm8         | MI    | Valid       | N.E.            | Move imm8 to r/m8.
        // C7 /0 iw          | MOV r/m16, imm16       | MI    | Valid       | Valid           | Move imm16 to r/m16.
        // C7 /0 id          | MOV r/m32, imm32       | MI    | Valid       | Valid           | Move imm32 to r/m32.
        // REX.W + C7 /0 id  | MOV r/m64, imm32       | MI    | Valid       | N.E.            | Move imm32 sign extended to 64-bits to r/m64.
        {.type = InstructionType_64BIT, .rexType = RexByte_W, .opcode = 0xC7, .modRMType = ModRMType_EXT, .opcodeExtension = 0, .opTypes[0] = OpType_RM, .opTypes[1] = OpType_IMM32},
    },
    // TODO: these cannot be generated right now, only once the encodings can differentiate between register sizes
    [movzx_] = {
        // Opcode           | Instruction      | Op/En | 64-Bit Mode | Compat/Leg Mode | Description
        // 0F B6 /r         | MOVZX r16, r/m8  | RM    | Valid       | Valid           | Move byte to word with zero-extension.
        {.type = InstructionType_16BIT, .opcode = 0x0FB6, .modRMType = ModRMType_REG, .opTypes[0] = OpType_REG, .opTypes[1] = OpType_RM},
        // 0F B6 /r         | MOVZX r32, r/m8  | RM    | Valid       | Valid           | Move byte to doubleword, zero-extension.
        {.type = InstructionType_32BIT, .opcode = 0x0FB6, .modRMType = ModRMType_REG, .opTypes[0] = OpType_REG, .opTypes[1] = OpType_RM},
        // REX.W + 0F B6 /r | MOVZX r64, r/m8  | RM    | Valid       | N.E.            | Move byte to quadword, zero-extension.
        {.type = InstructionType_64BIT, .rexType = RexByte_W, .opcode = 0x0FB6, .modRMType = ModRMType_REG, .opTypes[0] = OpType_REG, .opTypes[1] = OpType_RM},
        // 0F B7 /r         | MOVZX r32, r/m16 | RM    | Valid       | Valid           | Move word to doubleword, zero-extension.
        {.type = InstructionType_32BIT, .opcode = 0x0FB7, .modRMType = ModRMType_REG, .opTypes[0] = OpType_REG, .opTypes[1] = OpType_RM},
        // REX.W + 0F B7 /r | MOVZX r64, r/m16 | RM    | Valid       | N.E.            | Move word to quadword, zero-extension.
        {.type = InstructionType_64BIT, .rexType = RexByte_W, .opcode = 0x0FB7, .modRMType = ModRMType_REG, .opTypes[0] = OpType_REG, .opTypes[1] = OpType_RM},
    },
    [add_] = {
        // Opcode           | Instruction      | Op/En | 64-bit Mode | Compat/Leg Mode | Description
        // 04 ib            | ADD AL, imm8     | I     | Valid       | Valid           | Add imm8 to AL.
        // 05 iw            | ADD AX, imm16    | I     | Valid       | Valid           | Add imm16 to AX.
        // 05 id            | ADD EAX, imm32   | I     | Valid       | Valid           | Add imm32 to EAX.
        // REX.W + 05 id    | ADD RAX, imm32   | I     | Valid       | N.E.            | Add imm32 sign-extended to 64-bits to RAX.

        // 80 /0 ib         | ADD r/m8, imm8   | MI    | Valid       | Valid           | Add imm8 to r/m8.
        // REX + 80 /0 ib   | ADD r/m8, imm8   | MI    | Valid       | N.E.            | Add sign-extended imm8 to r/m8.
        // 81 /0 iw         | ADD r/m16, imm16 | MI    | Valid       | Valid           | Add imm16 to r/m16.
        // 81 /0 id         | ADD r/m32, imm32 | MI    | Valid       | Valid           | Add imm32 to r/m32.
        // REX.W + 81 /0 id | ADD r/m64, imm32 | MI    | Valid       | N.E.            | Add imm32 sign-extended to 64-bits to r/m64.
        {.type = InstructionType_64BIT, .rexType = RexByte_W, .opcode = 0x81, .modRMType = ModRMType_EXT, .opcodeExtension = 0, .opTypes[0] = OpType_RM, .opTypes[1] = OpType_IMM32},
        // 83 /0 ib         | ADD r/m16, imm8  | MI    | Valid       | Valid           | Add sign-extended imm8 to r/m16.
        // 83 /0 ib         | ADD r/m32, imm8  | MI    | Valid       | Valid           | Add sign-extended imm8 to r/m32.
        // REX.W + 83 /0 ib | ADD r/m64, imm8  | MI    | Valid       | N.E.            | Add sign-extended imm8 to r/m64.

        // 00 /r            | ADD r/m8, r8     | MR    | Valid       | Valid           | Add r8 to r/m8.
        // REX + 00 /r      | ADD r/m8, r8     | MR    | Valid       | N.E.            | Add r8 to r/m8.
        // 01 /r            | ADD r/m16, r16   | MR    | Valid       | Valid           | Add r16 to r/m16.
        // 01 /r            | ADD r/m32, r32   | MR    | Valid       | Valid           | Add r32 to r/m32.
        // REX.W + 01 /r    | ADD r/m64, r64   | MR    | Valid       | N.E.            | Add r64 to r/m64.
        {.type = InstructionType_64BIT, .rexType = RexByte_W, .opcode = 0x01, .modRMType = ModRMType_REG, .opTypes[0] = OpType_RM, .opTypes[1] = OpType_REG},

        // 02 /r            | ADD r8, r/m8     | RM    | Valid       | Valid           | Add r/m8 to r8.
        // REX + 02 /r      | ADD r8, r/m8     | RM    | Valid       | N.E.            | Add r/m8 to r8.
        // 03 /r            | ADD r16, r/m16   | RM    | Valid       | Valid           | Add r/m16 to r16.
        // 03 /r            | ADD r32, r/m32   | RM    | Valid       | Valid           | Add r/m32 to r32.
        // REX.W + 03 /r    | ADD r64, r/m64   | RM    | Valid       | N.E.            | Add r/m64 to r64.
        {.type = InstructionType_64BIT, .rexType = RexByte_W, .opcode = 0x03, .modRMType = ModRMType_REG, .opTypes[0] = OpType_REG, .opTypes[1] = OpType_RM},
    },
    [sub_] = {
        // Opcode           | Instruction      | Op/En | 64-Bit Mode | Compat/Leg Mode | Description
        // 2C ib            | SUB AL, imm8     | I     | Valid       | Valid           | Subtract imm8 from AL.
        // 2D iw            | SUB AX, imm16    | I     | Valid       | Valid           | Subtract imm16 from AX.
        // 2D id            | SUB EAX, imm32   | I     | Valid       | Valid           | Subtract imm32 from EAX.
        // REX.W + 2D id    | SUB RAX, imm32   | I     | Valid       | N.E.            | Subtract imm32 sign-extended to 64-bits from RAX.

        // 80 /5 ib         | SUB r/m8, imm8   | MI    | Valid       | Valid           | Subtract imm8 from r/m8.
        // REX + 80 /5 ib   | SUB r/m81, imm8  | MI    | Valid       | N.E.            | Subtract imm8 from r/m8.
        // 81 /5 iw         | SUB r/m16, imm16 | MI    | Valid       | Valid           | Subtract imm16 from r/m16.
        // 81 /5 id         | SUB r/m32, imm32 | MI    | Valid       | Valid           | Subtract imm32 from r/m32.
        // REX.W + 81 /5 id | SUB r/m64, imm32 | MI    | Valid       | N.E.            | Subtract imm32 sign-extended to 64-bits from r/m64.
        {.type = InstructionType_64BIT, .rexType = RexByte_W, .opcode = 0x81, .modRMType = ModRMType_EXT, .opcodeExtension = 5, .opTypes[0] = OpType_RM, .opTypes[1] = OpType_IMM32},
        // 83 /5 ib         | SUB r/m16, imm8  | MI    | Valid       | Valid           | Subtract sign-extended imm8 from r/m16.
        // 83 /5 ib         | SUB r/m32, imm8  | MI    | Valid       | Valid           | Subtract sign-extended imm8 from r/m32.
        // REX.W + 83 /5 ib | SUB r/m64, imm8  | MI    | Valid       | N.E.            | Subtract sign-extended imm8 from r/m64.

        // 28 /r            | SUB r/m8, r8     | MR    | Valid       | Valid           | Subtract r8 from r/m8.
        // REX + 28 /r      | SUB r/m81, r81   | MR    | Valid       | N.E.            | Subtract r8 from r/m8.
        // 29 /r            | SUB r/m16, r16   | MR    | Valid       | Valid           | Subtract r16 from r/m16.
        // 29 /r            | SUB r/m32, r32   | MR    | Valid       | Valid           | Subtract r32 from r/m32.
        // REX.W + 29 /r    | SUB r/m64, r64   | MR    | Valid       | N.E.            | Subtract r64 from r/m64.
        {.type = InstructionType_64BIT, .rexType = RexByte_W, .opcode = 0x29, .modRMType = ModRMType_REG, .opTypes[0] = OpType_RM, .opTypes[1] = OpType_REG},

        // 2A /r            | SUB r8, r/m8     | RM    | Valid       | Valid           | Subtract r/m8 from r8.
        // REX + 2A /r      | SUB r81, r/m81   | RM    | Valid       | N.E.            | Subtract r/m8 from r8.
        // 2B /r            | SUB r16, r/m16   | RM    | Valid       | Valid           | Subtract r/m16 from r16.
        // 2B /r            | SUB r32, r/m32   | RM    | Valid       | Valid           | Subtract r/m32 from r32.
        // REX.W + 2B /r    | SUB r64, r/m64   | RM    | Valid       | N.E.            | Subtract r/m64 from r64.
        {.type = InstructionType_64BIT, .rexType = RexByte_W, .opcode = 0x2B, .modRMType = ModRMType_REG, .opTypes[0] = OpType_REG, .opTypes[1] = OpType_RM},
    },
    [mul_] = {
        // Opcode        | Instruction | Op/En | 64-Bit Mode | Compat/Leg Mode | Description
        // F6 /4         | MUL r/m8    | M     | Valid       | Valid           | Unsigned multiply (AX := AL ∗ r/m8).
        // REX + F6 /4   | MUL r/m8    | M     | Valid       | N.E.            | Unsigned multiply (AX := AL ∗ r/m8).
        // F7 /4         | MUL r/m16   | M     | Valid       | Valid           | Unsigned multiply (DX:AX := AX ∗ r/m16).
        // F7 /4         | MUL r/m32   | M     | Valid       | Valid           | Unsigned multiply (EDX:EAX := EAX ∗ r/m32).
        // REX.W + F7 /4 | MUL r/m64   | M     | Valid       | N.E.            | Unsigned multiply (RDX:RAX := RAX ∗ r/m64).
        {.type = InstructionType_64BIT, .rexType = RexByte_W, .opcode = 0xF7, .modRMType = ModRMType_EXT, .opcodeExtension = 4, .opTypes[0] = OpType_RM},
    },
    [div_] = {
        // Opcode        | Instruction | Op/En | 64-Bit Mode | Compat/Leg Mode | Description
        // F6 /6         | DIV r/m8    | M     | Valid       | Valid           | Unsigned divide AX by r/m8, with result stored in AL := Quotient, AH := Remainder.
        // REX + F6 /6   | DIV r/m81   | M     | Valid       | N.E.            | Unsigned divide AX by r/m8, with result stored in AL := Quotient, AH := Remainder.
        // F7 /6         | DIV r/m16   | M     | Valid       | Valid           | Unsigned divide DX:AX by r/m16, with result stored in AX := Quotient, DX := Remainder.
        // F7 /6         | DIV r/m32   | M     | Valid       | Valid           | Unsigned divide EDX:EAX by r/m32, with result stored in EAX := Quotient, EDX := Remainder.
        // REX.W + F7 /6 | DIV r/m64   | M     | Valid       | N.E.            | Unsigned divide RDX:RAX by r/m64, with result stored in RAX := Quotient, RDX := Remainder.
        {.type = InstructionType_64BIT, .rexType = RexByte_W, .opcode = 0xF7, .modRMType = ModRMType_EXT, .opcodeExtension = 6, .opTypes[0] = OpType_RM},
    },
    [cmp_] = {
        // Opcode           | Instruction      | Op/En | 64-Bit Mode | Compat/Leg Mode | Description
        // 3C ib            | CMP AL, imm8     | I     | Valid       | Valid           | Compare imm8 with AL.
        // 3D iw            | CMP AX, imm16    | I     | Valid       | Valid           | Compare imm16 with AX.
        // 3D id            | CMP EAX, imm32   | I     | Valid       | Valid           | Compare imm32 with EAX.
        // REX.W + 3D id    | CMP RAX, imm32   | I     | Valid       | N.E.            | Compare imm32 sign-extended to 64-bits with RAX.

        // 80 /7 ib         | CMP r/m8, imm8   | MI    | Valid       | Valid           | Compare imm8 with r/m8.
        // REX + 80 /7 ib   | CMP r/m8, imm8   | MI    | Valid       | N.E.            | Compare imm8 with r/m8.
        // 81 /7 iw         | CMP r/m16, imm16 | MI    | Valid       | Valid           | Compare imm16 with r/m16.
        // 81 /7 id         | CMP r/m32, imm32 | MI    | Valid       | Valid           | Compare imm32 with r/m32.
        // REX.W + 81 /7 id | CMP r/m64, imm32 | MI    | Valid       | N.E.            | Compare imm32 sign-extended to 64-bits with r/m64.
        {.type = InstructionType_64BIT, .rexType = RexByte_W, .opcode = 0x81, .modRMType = ModRMType_EXT, .opcodeExtension = 7, .opTypes[0] = OpType_RM, .opTypes[1] = OpType_IMM32},
        // 83 /7 ib         | CMP r/m16, imm8  | MI    | Valid       | Valid           | Compare imm8 with r/m16.
        // 83 /7 ib         | CMP r/m32, imm8  | MI    | Valid       | Valid           | Compare imm8 with r/m32.
        // REX.W + 83 /7 ib | CMP r/m64, imm8  | MI    | Valid       | N.E.            | Compare imm8 with r/m64.

        // 38 /r            | CMP r/m8, r8     | MR    | Valid       | Valid           | Compare r8 with r/m8.
        // REX + 38 /r      | CMP r/m8, r8     | MR    | Valid       | N.E.            | Compare r8 with r/m8.
        // 39 /r            | CMP r/m16, r16   | MR    | Valid       | Valid           | Compare r16 with r/m16.
        // 39 /r            | CMP r/m32, r32   | MR    | Valid       | Valid           | Compare r32 with r/m32.
        // REX.W + 39 /r    | CMP r/m64,r64    | MR    | Valid       | N.E.            | Compare r64 with r/m64.
        {.type = InstructionType_64BIT, .rexType = RexByte_W, .opcode = 0x39, .modRMType = ModRMType_REG, .opTypes[0] = OpType_RM, .opTypes[1] = OpType_REG},

        // 3A /r            | CMP r8, r/m8     | RM    | Valid       | Valid           | Compare r/m8 with r8.
        // REX + 3A /r      | CMP r8, r/m8     | RM    | Valid       | N.E.            | Compare r/m8 with r8.
        // 3B /r            | CMP r16, r/m16   | RM    | Valid       | Valid           | Compare r/m16 with r16.
        // 3B /r            | CMP r32, r/m32   | RM    | Valid       | Valid           | Compare r/m32 with r32.
        // REX.W + 3B /r    | CMP r64, r/m64   | RM    | Valid       | N.E.            | Compare r/m64 with r64.
        {.type = InstructionType_64BIT, .rexType = RexByte_W, .opcode = 0x3B, .modRMType = ModRMType_REG, .opTypes[0] = OpType_REG, .opTypes[1] = OpType_RM},
    },
    [lea_] = {
        // Opcode        | Instruction | Op/En | 64-Bit Mode | Compat/Leg Mode | Description
        // 8D /r         | LEA r16,m   | RM    | Valid       | Valid           | Store effective address for m in register r16.
        // 8D /r         | LEA r32,m   | RM    | Valid       | Valid           | Store effective address for m in register r32.
        // REX.W + 8D /r | LEA r64,m   | RM    | Valid       | N.E.            | Store effective address for m in register r64.
        {.type = InstructionType_64BIT, .rexType = RexByte_W, .opcode = 0x8D, .modRMType = ModRMType_REG, .opTypes[0] = OpType_REG, .opTypes[1] = OpType_RM},
    },
    [inc_] = {
        // Opcode        | Instruction | Op/En | 64-Bit Mode | Compat/Leg Mode | Description
        // FE /0         | INC r/m8    | M     | Valid       | Valid           | Increment r/m byte by 1.
        // REX + FE /0   | INC r/m8    | M     | Valid       | N.E.            | Increment r/m byte by 1.
        // FF /0         | INC r/m16   | M     | Valid       | Valid           | Increment r/m word by 1.
        // FF /0         | INC r/m32   | M     | Valid       | Valid           | Increment r/m doubleword by 1.
        // REX.W + FF /0 | INC r/m64   | M     | Valid       | N.E.            | Increment r/m quadword by 1.
        {.type = InstructionType_64BIT, .rexType = RexByte_W, .opcode = 0xFF, .modRMType = ModRMType_EXT, .opcodeExtension = 0, .opTypes[0] = OpType_RM},
        // 40+ rw        | INC r16     | O     | N.E.        | Valid           | Increment word register by 1.
        // 40+ rd        | INC r32     | O     | N.E.        | Valid           | Increment doubleword register by 1.
    },
    [dec_] = {
        // Opcode        | Instruction | Op/En | 64-Bit Mode | Compat/Leg Mode | Description
        // FE /1         | DEC r/m8    | M     | Valid       | Valid           | Decrement r/m8 by 1.
        // REX + FE /1   | DEC r/m8*   | M     | Valid       | N.E.            | Decrement r/m8 by 1.
        // FF /1         | DEC r/m16   | M     | Valid       | Valid           | Decrement r/m16 by 1.
        // FF /1         | DEC r/m32   | M     | Valid       | Valid           | Decrement r/m32 by 1.
        // REX.W + FF /1 | DEC r/m64   | M     | Valid       | N.E.            | Decrement r/m64 by 1.
        {.type = InstructionType_64BIT, .rexType = RexByte_W, .opcode = 0xFF, .modRMType = ModRMType_EXT, .opcodeExtension = 1, .opTypes[0] = OpType_RM},
        // 48+rw         | DEC r16     | O     | N.E.        | Valid           | Decrement r16 by 1.
        // 48+rd         | DEC r32     | O     | N.E.        | Valid           | Decrement r32 by 1.
    },
    [jmp_]  = {
        // Opcode      | Instruction  | Op/En | 64-Bit Mode | Compat/Leg Mode | Description
        // EB cb       | JMP rel8     | D     | Valid       | Valid           | Jump short, RIP = RIP + 8-bit displacement sign extended to 64-bits.
        // E9 cw       | JMP rel16    | D     | N.S.        | Valid           | Jump near, relative, displacement relative to next instruction. Not supported in 64-bit mode.
        // E9 cd       | JMP rel32    | D     | Valid       | Valid           | Jump near, relative, RIP = RIP + 32-bit displacement sign extended to 64-bits.
        {.type = InstructionType_64BIT, .opcode = 0xE9, .opTypes[0] = OpType_IMM32},
        // FF /4       | JMP r/m16    | M     | N.S.        | Valid           | Jump near, absolute indirect, address = zero-extended r/m16. Not supported in 64-bit mode.
        // FF /4       | JMP r/m32    | M     | N.S.        | Valid           | Jump near, absolute indirect, address given in r/m32. Not supported in 64-bit mode.
        // FF /4       | JMP r/m64    | M     | Valid       | N.E.            | Jump near, absolute indirect, RIP = 64-Bit offset from register or memory.
        // EA cd       | JMP ptr16:16 | S     | Inv.        | Valid           | Jump far, absolute, address given in operand.
        // EA cp       | JMP ptr16:32 | S     | Inv.        | Valid           | Jump far, absolute, address given in operand.
        // FF /5       | JMP m16:16   | M     | Valid       | Valid           | Jump far, absolute indirect, address given in m16:16.
        // FF /5       | JMP m16:32   | M     | Valid       | Valid           | Jump far, absolute indirect, address given in m16:32.
        // REX.W FF /5 | JMP m16:64   | M     | Valid       | N.E.            | Jump far, absolute indirect, address given in m16:64.
    },
    [je_] = {
        // Opcode        | Instruction | Op/En | 64-Bit Mode | Compat/Leg Mode | Description
        // 74 cb         | JE rel8     | D     | Valid       | Valid           | Jump short if equal (ZF=1).
        // 0F 84 cw      | JE rel16    | D     | N.S.        | Valid           | Jump near if equal (ZF=1). Not supported in 64-bit mode.
        // 0F 84 cd      | JE rel32    | D     | Valid       | Valid           | Jump near if equal (ZF=1).
        {.type = InstructionType_64BIT, .opcode = 0x0F84, .opTypes[0] = OpType_IMM32},
        // TODO: 64 bit instruction size is incorrect here, only temporary unti the codegen searches for non 64bit encodings
    },
    [jne_] = {
        // Opcode        | Instruction | Op/En | 64-Bit Mode | Compat/Leg Mode | Description
        // 75 cb         | JNE rel8    | D     | Valid       | Valid           | Jump short if not equal (ZF=0).
        // 0F 85 cw      | JNE rel16   | D     | N.S.        | Valid           | Jump near if not equal (ZF=0). Not supported in 64-bit mode.
        // 0F 85 cd      | JNE rel32   | D     | Valid       | Valid           | Jump near if not equal (ZF=0).
        {.type = InstructionType_64BIT, .opcode = 0x0F85, .opTypes[0] = OpType_IMM32},
        // TODO: 64 bit instruction size is incorrect here, only temporary unti the codegen searches for non 64bit encodings
    },
    [jl_] = {
        // Opcode        | Instruction | Op/En | 64-Bit Mode | Compat/Leg Mode | Description
        // 7C cb         | JL rel8     | D     | Valid       | Valid           | Jump short if less (SF≠ OF).
        // 0F 8C cw      | JL rel16    | D     | N.S.        | Valid           | Jump near if less (SF≠ OF). Not supported in 64-bit mode.
        // 0F 8C cd      | JL rel32    | D     | Valid       | Valid           | Jump near if less (SF≠ OF).
        {.type = InstructionType_64BIT, .opcode = 0x0F8C, .opTypes[0] = OpType_IMM32},
        // TODO: 64 bit instruction size is incorrect here, only temporary unti the codegen searches for non 64bit encodings
    },
    [jg_] = {
        // Opcode        | Instruction | Op/En | 64-Bit Mode | Compat/Leg Mode | Description
        // 7F cb         | JG rel8     | D     | Valid       | Valid           | Jump short if greater (ZF=0 and SF=OF).
        // 0F 8F cw      | JG rel16    | D     | N.S.        | Valid           | Jump near if greater (ZF=0 and SF=OF). Not supported in 64-bit mode.
        // 0F 8F cd      | JG rel32    | D     | Valid       | Valid           | Jump near if greater (ZF=0 and SF=OF).
        {.type = InstructionType_64BIT, .opcode = 0x0F8F, .opTypes[0] = OpType_IMM32},
        // TODO: 64 bit instruction size is incorrect here, only temporary unti the codegen searches for non 64bit encodings
    },
    [jle_] = {
        // Opcode        | Instruction | Op/En | 64-Bit Mode | Compat/Leg Mode | Description
        // 7E cb         | JLE rel8    | D     | Valid       | Valid           | Jump short if less or equal (ZF=1 or SF≠ OF).
        // 0F 8E cw      | JLE rel16   | D     | N.S.        | Valid           | Jump near if less or equal (ZF=1 or SF≠ OF). Not supported in 64-bit mode.
        // 0F 8E cd      | JLE rel32   | D     | Valid       | Valid           | Jump near if less or equal (ZF=1 or SF≠ OF).
        {.type = InstructionType_64BIT, .opcode = 0x0F8E, .opTypes[0] = OpType_IMM32},
        // TODO: 64 bit instruction size is incorrect here, only temporary unti the codegen searches for non 64bit encodings
    },
    [jge_] = {
        // Opcode        | Instruction | Op/En | 64-Bit Mode | Compat/Leg Mode | Description
        // 7D cb         | JGE rel8    | D     | Valid       | Valid           | Jump short if greater or equal (SF=OF).
        // 0F 8D cw      | JGE rel16   | D     | N.S.        | Valid           | Jump near if greater or equal (SF=OF). Not supported in 64-bit mode.
        // 0F 8D cd      | JGE rel32   | D     | Valid       | Valid           | Jump near if greater or equal (SF=OF).
        {.type = InstructionType_64BIT, .opcode = 0x0F8D, .opTypes[0] = OpType_IMM32},
        // TODO: 64 bit instruction size is incorrect here, only temporary unti the codegen searches for non 64bit encodings
    },
    [neg_] = {
        // Opcode        | Instruction | Op/En | 64-Bit Mode | Compat/Leg Mode | Description
        // F6 /3         | NEG r/m8    | M     | Valid       | Valid           | Two's complement negate r/m8.
        // REX + F6 /3   | NEG r/m8    | M     | Valid       | N.E.            | Two's complement negate r/m8.
        // F7 /3         | NEG r/m16   | M     | Valid       | Valid           | Two's complement negate r/m16.
        // F7 /3         | NEG r/m32   | M     | Valid       | Valid           | Two's complement negate r/m32.
        // REX.W + F7 /3 | NEG r/m64   | M     | Valid       | N.E.            | Two's complement negate r/m64.
        {.type = InstructionType_64BIT, .opcode = 0xF7, .rexType = RexByte_W, .modRMType = ModRMType_EXT, .opcodeExtension = 3, .opTypes[0] = OpType_RM},
    },
};
