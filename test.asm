global _main

segment .text
_main:
    ; save stack position
    mov ebp, esp
    
    ; var a = 9;
    mov eax, 9
    push eax
    
    ; var b = 5;
    mov eax, 5
    push eax
    
    ; var d;
    sub esp, 4

    ; var c = 7;
    mov eax, 7
    push eax

    ; return d;
    push DWORD [esp + 1 * 4]
    pop eax
    mov esp, ebp ; restore stack position before returning
    ret
