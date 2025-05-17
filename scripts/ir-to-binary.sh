#!/bin/bash

clang-17 -x ir ./tests/choco.py.ll -o binary -g
./binary
