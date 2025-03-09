#include <iostream>

#include "llvm/ADT/StringRef.h"

int main() { 
    llvm::StringRef strRef = "ChocoPy Compiler";
    std::cout << strRef.str() << std::endl; 
}
