nasm -f win64 output.asm
ld -e _start -o output.exe output.obj
output.exe