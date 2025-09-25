set(cpp "-cpp") # default for all other versions
if(CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER_EQUAL 2025.2 AND CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 2025.3)

  message(STATUS "Working around ifx ${CMAKE_Fortran_COMPILER_VERSION} FPP bug (using external cpp -P)")

  # Find a preprocessor; prefer 'cpp', fall back to 'clang-cpp'
  find_program(cpp_exe NAMES cpp clang-cpp)
  if(NOT cpp_exe)
    message(FATAL_ERROR "ifx 2025.2 workaround requested but no 'cpp' or 'clang-cpp' found")
  endif()
  message(STATUS "Found preprocessor: ${cpp_exe}")

  # Make a small wrapper that injects -P (no linemarkers)
  set(cpp_wrapper "${CMAKE_BINARY_DIR}/tools/cpp_no_lines")
  file(MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/tools")

  if(WIN32)
    # If you actually build on Windows with ifx, create a .bat wrapper
    file(WRITE "${cpp_wrapper}.bat"
"@echo off\r
\"${cpp_exe}\" -P -traditional-cpp -undef %*\r
")
    set(cpp "-fpp-name=${cpp_wrapper}.bat")
  else()
    file(WRITE "${cpp_wrapper}"
"#!/usr/bin/env bash
exec \"${cpp_exe}\" -P -traditional-cpp -undef \"$@\"
")
    file(CHMOD "${cpp_wrapper}" FILE_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)
    set(cpp "-fpp-name=${cpp_wrapper}")
  endif()
endif()

set (SUPPRESS_LINE_LENGTH_WARNING "-diag-disable 5268")
set (CMAKE_Fortran_FLAGS_RELEASE "${cpp} -O3 -free -stand f08 ${SUPPRESS_LINE_LENGTH_WARNING}")
set (CMAKE_Fortran_FLAGS_DEBUG   "${cpp} -O0 -g -traceback \
      -check nouninit -free -stand f08 -save-temps ${SUPPRESS_LINE_LENGTH_WARNING}")

