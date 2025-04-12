add_executable(LexerTest
    LexerTest.cpp
)

target_include_directories(LexerTest PRIVATE
  ${googletest_SOURCE_DIR}/googletest/include
)

target_link_libraries(LexerTest
  PRIVATE
  gtest_main
)

add_test(NAME LexerTests COMMAND LexerTest)
