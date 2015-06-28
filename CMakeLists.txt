# ------------------------------------------------------------------------ #
# ASTG Logger top level CmakeLists.txt
#
# Usage:
#   cmake -DMPI=YES ..
#
# Default is MPI=NO.
#
# Note that tests use a Mock MPI mechanism and thus do not run under a real
# MPI. However, applications must link with a real MPI library if MPI=YES is 
# used.
#
# ------------------------------------------------------------------------ #
cmake_minimum_required (VERSION 2.8)
project (logger Fortran)

set(CMAKE_MODULE_PATH
   ${CMAKE_SOURCE_DIR}/cmake/modules
)

include(PreventInSourceBuild)

# add global defaults
include(DefineCMakeDefaults)
include(DefinePlatformDefaults)

if (PFUNIT)
    add_definitions(-DUSE_PFUNIT)
endif()

if (MPI MATCHES YES)
  add_definitions(-DLOGGER_USE_MPI)
endif()

if (DEFINED ENV{FTL})
    message(STATUS "FTL is set to $ENV{FTL}")
else ()
    message(FATAL_ERROR "Please set FTL installation path. For example: export FTL=/mylibraries/FTL")
endif ()
set(FTL $ENV{FTL})

# Compiler flags
include(setCompilerFlags)

add_subdirectory(src)
if (WITH_PFUNIT MATCHES YES)
   add_subdirectory(tests)
endif()
add_subdirectory(examples)
