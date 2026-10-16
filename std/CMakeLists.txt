add_library(ChocopySupport STATIC string.c runtime.c)

set_target_properties(ChocopySupport PROPERTIES
  # Generated programs are linked as position-independent executables
  POSITION_INDEPENDENT_CODE ON
  ARCHIVE_OUTPUT_DIRECTORY ${CHOCOPY_SUPPORT_DIR})

install(TARGETS ChocopySupport ARCHIVE DESTINATION lib/chocopy)

if(APPLE)
  target_compile_options(ChocopySupport PRIVATE -mmacosx-version-min=${CHOCOPY_MACOS_MIN_VERSION})

  configure_file(libSystem.tbd ${CHOCOPY_SUPPORT_DIR}/libSystem.tbd COPYONLY)
  install(FILES libSystem.tbd DESTINATION lib/chocopy)
endif()
