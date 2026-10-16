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

# Test programs are referenced as ./testprograms/...
add_test(NAME CodeGenTests COMMAND CodeGenTest
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})
