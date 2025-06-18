#!/bin/bash

clang-17 -c ./std/string.c -o string.o
clang-17 ./testprograms/choco.py.ll string.o -o binary -g
./binary
