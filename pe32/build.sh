set -xe

gcc -o emiter.exe emiter.c
./emiter.exe
for file in ./*.bin; do
    objdump -D -b binary -M intel -m i386:x86-64 $file > ${file/\.bin/\.dump}
done