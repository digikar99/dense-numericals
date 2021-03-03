
SHELL=/data/data/com.termux/files/usr/bin/bash
ARCH=$(uname -m)

if [ "$ARCH" == "x86_64" ]; then
    gcc -O3 -mavx2 -shared -o libdense-numericals.so -fpic -Wall -Werror dense-numericals.c -lsleef
else
    gcc -O3 -shared -o libdense-numericals.so -fpic -Wall -Werror dense-numericals.c -lsleef 
fi
