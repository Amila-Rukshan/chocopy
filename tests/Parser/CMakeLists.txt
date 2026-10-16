add_executable(ParserTest
  ParserTest.cpp
)

target_include_directories(ParserTest PRIVATE
  ${googletest_SOURCE_DIR}/googletest/include
)

target_link_libraries(ParserTest
  PRIVATE
  gtest_main
)

llvm_config(ParserTest ${_llvm_use_shared} Support)

add_test(NAME ParserTests COMMAND ParserTest)
