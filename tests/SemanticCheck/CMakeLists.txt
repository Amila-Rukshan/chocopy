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
  MLIRSupport
)

add_test(NAME SemanticCheckTests COMMAND SemanticCheckTest)
