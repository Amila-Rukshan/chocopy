add_library(ChocopySupport STATIC string.c runtime.c)

# Generated programs are linked as position-independent executables
set_target_properties(ChocopySupport PROPERTIES POSITION_INDEPENDENT_CODE ON)

if(APPLE)
  target_compile_options(ChocopySupport PRIVATE -mmacosx-version-min=${CHOCOPY_MACOS_MIN_VERSION})
endif()
