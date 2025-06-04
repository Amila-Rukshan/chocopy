#!/bin/bash

clang-17 -x ir ./testprograms/choco.py.ll -o binary -g
./binary
