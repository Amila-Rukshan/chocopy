add_executable(CodeGenTest
    CodeGenTest.cpp
    ${CMAKE_SOURCE_DIR}/src/CodeGen.cpp
    ${CMAKE_SOURCE_DIR}/src/SemanticCheck.cpp
)

target_include_directories(CodeGenTest PRIVATE
  ${googletest_SOURCE_DIR}/googletest/include
)

target_link_libraries(CodeGenTest
  PRIVATE
  gtest_main
  MLIRSupport
)

add_test(NAME CodeGenTests COMMAND CodeGenTest)
