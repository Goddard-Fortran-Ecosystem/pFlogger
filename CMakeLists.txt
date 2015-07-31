# ------------------------------------------------------------------------ #
# ASTG Logger top level CmakeLists.txt
#
# Usage:
#   cmake -DMPI=YES ..
#
# Default is MPI=NO.
#
# Note that tests use a Mock MPI mechanism and thus do not run under a real
# MPI. Thus, tests must link with a SERIAL installation of pFUnit regardless
# of MPI option used. However, if MPI=YES is used then examples must link
# with a real MPI library and tests executable will fail to link. So,
# 
#  export PFUNIT=/mypath/pFUnit_serial
#
#  cmake -DMPI=NO ..  : build/run serial tests ONLY
#  cmake -DMPI=YES .. : build/run mpi tests ONLY
#
#  unset PFUNIT 
#  
#  cmake -DMPI=NO ..  : build/run serial examples ONLY
#  cmake -DMPI=YES .. : build/run mpi examples ONLY
#
# ------------------------------------------------------------------------ #
cmake_minimum_required (VERSION 3.0)
project (logger Fortran)

set(CMAKE_MODULE_PATH
   ${CMAKE_SOURCE_DIR}/cmake/modules
)

include(PreventInSourceBuild)
include(DefineCMakeDefaults)
include(DefinePlatformDefaults)

# FTL library is required to build logger
if (DEFINED ENV{FTL})
   message(STATUS " +++ FTL is set to $ENV{FTL}")
else ()
   message(FATAL_ERROR " +++ Please set FTL installation path. \
      For example: export FTL=/mylibraries/FTL")
endif ()
set(FTL $ENV{FTL})

# By default logger builds in SERIAL
if (MPI MATCHES YES)
   add_definitions(-DLOGGER_USE_MPI)
   find_package(MPI REQUIRED)
   include_directories(${MPI_Fortran_INCLUDE_PATH})
   message( STATUS "MPI enabled")
endif()

# Compiler flags
include(setCompilerFlags)

add_subdirectory(doc)
add_subdirectory(src)
if (PFUNIT)
   add_subdirectory(tests)
else()
   add_subdirectory(examples)
endif()

