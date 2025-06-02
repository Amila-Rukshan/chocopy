add_executable(ParserTest
  ParserTest.cpp
)

target_include_directories(ParserTest PRIVATE
  ${googletest_SOURCE_DIR}/googletest/include
)

target_link_libraries(ParserTest
  PRIVATE
  gtest_main MLIRSupport
)

add_test(NAME ParserTests COMMAND ParserTest)
