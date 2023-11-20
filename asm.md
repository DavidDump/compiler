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

## Linux X86_64 calling convention
Register | Usage | Preserve racross function calls
--- | --- | ---
%rax | temporary register; with variable arguments, passes information about the number of vector registers used; 1st return register | no
%rbx | callee-saved register; optionaly used as base pointer | yes
%rcx | used to pass the 4th integer arguments to functions | no
%rdx | used to pass the 3rd argument to functions; 2nd return register | no
%rsp | stack pointer | yes
%rbp | callee-saved register; optionally used as frame pointer | yes
%rsi | used to pass the 2nd argument to functions | no
%rdi | used to pass the 1nd argument to functions | no
%r8  | used to pass the 5nd argument to functions | no
%r9  | used to pass the 6nd argument to functions | no
%r10 | temporary register; used for passing function's static chain pointer | no
%r11 | temporary register | no
%r12 - %r15 | callee-saved registers | yes
%xmm0 - %xmm1 | used to pass and return floating point argumetns | no
%xmm2 - %xmm7 | used to pass floating point arguments | no
%xmm8 - %xmm15 | temporary registers | no
%mmx0 - %mmx7  | temporary registers | no
%st0,  %st1 | temporary registers; used to return `long double` arguments | no
%st2 - %st7 | temporary registers | no
%fs | reserved for system (as thread specific data register) | no
mxcsr | SSE2 control and status word | partial
x87 SW | x87 status word | no
x87 CW | x87 control word | yes