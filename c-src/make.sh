
SHELL=/data/data/com.termux/files/usr/bin/bash
ARCH=$(uname -m)

if [ "$ARCH" == "x86_64" ]; then
    gcc -O3 -mavx2 -mfma -shared -o libdense-numericals.so -fpic dense-numericals.c
    # gcc -O3 -mavx512f -shared -o libdense-numericals.so -fpic dense-numericals.c
    # gcc -O3 -msse2 -shared -o libdense-numericals.so -fpic dense-numericals.c
    # TODO: Prepare according to different architectures
else
    gcc -O3 -shared -o libdense-numericals.so -fpic dense-numericals.c
fi
