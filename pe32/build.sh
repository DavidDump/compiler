set -xe

gcc -o emiter.exe emiter.c
./emiter.exe
objdump -D -b binary -M intel -m i386:x86-64 test.bin > test.dump