# ------------------------------------------------------------------------ #
# ASTG Logger top level CmakeLists.txt
# ------------------------------------------------------------------------ #
cmake_minimum_required (VERSION 2.8)
project (logger Fortran)

set (CMAKE_VERBOSE_MAKEFILE 1)

set(CMAKE_MODULE_PATH
   ${CMAKE_SOURCE_DIR}/cmake/modules
)

include(PreventInSourceBuild)

# add global defaults
include(DefineCMakeDefaults)
include(DefinePlatformDefaults)

# Default is to build in SERIAL(MPI=NO)
set (MPI "NO"
   CACHE STRING "Use MPI=YES for parallel builds"
)

# Compiler flags
include(setCompilerFlags)

add_subdirectory(src)
if(WITH_PFUNIT MATCHES YES)
   add_subdirectory(tests)
endif()
add_subdirectory(app)
