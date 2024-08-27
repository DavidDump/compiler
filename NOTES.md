### Optimizations
Nops for allignment are slow because only one byte for instruction, so inneficient for padding, many instructions are needed to pad a lot, all the instuctions need to be loaded by the CPU which take the same amount of time for all instructions, so instead use this:
```
48 8d bc 27 00 00 00 00    lea    rdi,[rdi+riz*1+0x0]
```
eight bytes of padding in one instruction that does nothing, reads the value stored in rdi back to rdi, the 0x48 rax prefix is optional so it can be dropped, propapbly more usefull that way, also the ModR/M and SIB bytes can be adjusted to use a 8 bit displacement instead of a 32 bit one


[docs](https://learn.microsoft.com/en-us/windows/win32/debug/pe-format)
[PE32 format](https://github.com/corkami/pics/blob/master/binary/pe101/README.md)

## References
[compiler project](https://github.com/grassator/mass)
[compiler on youtube](https://www.youtube.com/@DmitriyKubyshkin)
