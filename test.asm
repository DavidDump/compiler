; 64-bit program
global _main

segment .text
_main:
    ; save stack position
    mov rbp, rsp
    
    ; var a = 9;
    mov rax, 9
    push rax
    
    ; var b = 5;
    mov rax, 5
    push rax
    
    ; var d;
    ; sub rsp, 8

    ; var c = 7;
    mov rax, 7
    push rax

    ; return d;
    push QWORD [rsp + 2 * 4] ; 8 is the size of one int (8 bytes)
    pop rax
    mov rsp, rbp ; restore stack position before returning
    ret
