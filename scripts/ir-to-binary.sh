#!/bin/bash

clang-17 -c ./std/string.c -o string.o
clang-17 -c ./std/runtime.c -o runtime.o
ld -r string.o runtime.o -o support.o
clang-17 ./testprograms/choco.py.ll support.o -o binary -g
./binary
