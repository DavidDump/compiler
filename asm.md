## Registers
### 64-bit
- RAX
- RBX
- RCX
- RDX
- RSP - stack pointer
- RBP - frame pointer, store stack pointer inside function context for easy local variable access
- RSI - Source Index, legacy, used for repetetive copy instrucions
- RDI - Destination index, legacy, used for repetetive copy instrucions
- R8-R15
### 32-bit
- EAX
- EBX
- ECX
- EDX
- ESP - stack pointer
- EBP - frame pointer, store stack pointer inside function context for easy local variable access
- ESI - Source Index, legacy, used for repetetive copy instrucions
- EDI - Destination index, legacy, used for repetetive copy instrucions
- R8D-R15D
### 16-bit
- AX
- BX
- CX
- DX
- SP - stack pointer
- BP - frame pointer, store stack pointer inside function context for easy local variable access
- SI - Source Index, legacy, used for repetetive copy instrucions
- DI - Destination index, legacy, used for repetetive copy instrucions
- R8W-R15W
### 8-bit
- AH/AL
- BH/BL
- CH/CL
- DH/DL
- SPL - stack pointer
- BPL - frame pointer, store stack pointer inside function context for easy local variable access
- SIL - Source Index, legacy, used for repetetive copy instrucions
- DIL - Destination index, legacy, used for repetetive copy instrucions
- R8B-R15B

## Windows x86_64 calling convention
The first four arguments are placed onto the registers. That means RCX, RDX, R8, R9 (in that order) for integer, struct or pointer arguments, and XMM0, XMM1, XMM2, XMM3 for floating point arguments.

- integers arguments: RAX func(RCX, RDX, R8, R9, ...(on the stack));
- floats arguments: XMM0 func(XMM0, XMM1, XMM2, XMM3, ...(on the stack));

The registers RAX, RCX, RDX, R8, R9, R10, R11 are considered volatile (caller-saved).
The registers RBX, RBP, RDI, RSI, RSP, R12, R13, R14, and R15 are considered nonvolatile (callee-saved).

https://learn.microsoft.com/en-us/cpp/build/x64-calling-convention?view=msvc-170
https://en.wikipedia.org/wiki/X86_calling_conventions