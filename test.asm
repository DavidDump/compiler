; 64-bit program
bits 64
default rel

section .data
    num dq 12

section .text
global _start
_start:
    ; save stack position
    mov rbp, rsp
    
    ; var a = 9;
    mov rax, 9
    push rax
    
    ; var b = 5;
    mov rax, 5
    push rax
    
    ; var d;
    sub rsp, 8

    ; var c = num;
    mov rax, [num]
    push rax

    ; return d;
    push QWORD [rsp + 2 * 8] ; 8 is the size of one int (8 bytes)
    pop rax
    mov rsp, rbp ; restore stack position before returning
    ret
