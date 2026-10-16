add_executable(CodeGenTest
    CodeGenTest.cpp
)

target_include_directories(CodeGenTest PRIVATE
  ${googletest_SOURCE_DIR}/googletest/include
)

target_link_libraries(CodeGenTest
  PRIVATE
  gtest_main
  ChocoPyCompiler
)

add_test(NAME CodeGenTests COMMAND CodeGenTest)
