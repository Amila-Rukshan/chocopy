add_executable(SemanticCheckTest
    SemanticCheckTest.cpp
    ${CMAKE_SOURCE_DIR}/src/SemanticCheck.cpp
)

target_include_directories(SemanticCheckTest PRIVATE
  ${googletest_SOURCE_DIR}/googletest/include
)

target_link_libraries(SemanticCheckTest
  PRIVATE
  gtest_main
)

llvm_config(SemanticCheckTest ${_llvm_use_shared} Support)

add_test(NAME SemanticCheckTests COMMAND SemanticCheckTest)
