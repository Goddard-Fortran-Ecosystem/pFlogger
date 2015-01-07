# ------------------------------------------------------------------------ #
# ASTG Logger top level CmakeLists.txt
#
# Usage:
#   cmake -DMPI=YES -DCOMPILE_WITH_DEBUG=NO ..
#
# Default is USE_MPI=NO, COMPILE_WITH_DEBUG=YES
#
# Note that tests use a Mock MPI mechanism and thus do not run under a real
# MPI. The apps, however, do.
#
# ------------------------------------------------------------------------ #
INCLUDE(CMakeForceCompiler)
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

if ( DEFINED ENV{PFUNIT} )
#    message( FATAL_ERROR "You can not do this at all, CMake will exit." $ENV{PFUNIT} )
    OPTION(PFUNIT "Use PFUNIT for development" YES)
else ()
    OPTION(PFUNIT "Use PFUNIT for development" NO)
endif ()

if ( PFUNIT )
    add_definitions(-DUSE_PFUNIT)
endif()

option(COMPILE_WITH_DEBUG "Compile with debug flags" YES)

# Compiler flags
include(setCompilerFlags)

set(FTL $ENV{FTL})

add_subdirectory(src)
if(WITH_PFUNIT MATCHES YES)
   add_subdirectory(tests)
endif()
add_subdirectory(app)
