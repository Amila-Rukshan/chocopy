add_executable(CodeGenTest
    CodeGenTest.cpp
    ${CMAKE_SOURCE_DIR}/src/CodeGen.cpp
    ${CMAKE_SOURCE_DIR}/src/SemanticCheck.cpp
)

add_dependencies(CodeGenTest ChocopySupport)

target_compile_definitions(CodeGenTest
  PRIVATE
  CHOCOPY_LIB_PATH="$<TARGET_FILE:ChocopySupport>"
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
