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
#  cmake -DMPI=YES .. : build/run serial and mpi tests
#
#  unset PFUNIT 
#  
#  cmake -DMPI=NO ..  : build/run serial examples ONLY
#  cmake -DMPI=YES .. : build/run mpi examples ONLY
#
# ------------------------------------------------------------------------ #
cmake_minimum_required (VERSION 3.8.0)
project (PFLOGGER
  VERSION 1.1
  LANGUAGES Fortran)

set (CMAKE_MODULE_PATH
   ${CMAKE_SOURCE_DIR}/cmake
)

include (PreventInSourceBuild)
include (DefineCMakeDefaults)
include (DefinePlatformDefaults)

find_package (GFTL REQUIRED)
find_package (GFTL_SHARED REQUIRED)
find_package (YAFYAML REQUIRED)

# By default logger builds in SERIAL
if (MPI MATCHES YES)
  add_definitions (-D_LOGGER_USE_MPI)
  find_package (MPI REQUIRED)
  include_directories (${MPI_Fortran_INCLUDE_PATH})
  message( STATUS "MPI enabled")
elseif ()
  message (FATAL_ERROR
    "MPI support not available - MPI implementation"
    )
endif()

#-----------------------------------
# Set default Fortran compiler flags
#-----------------------------------
include(${CMAKE_Fortran_COMPILER_ID} RESULT_VARIABLE found)
if(NOT found)
  message( FATAL_ERROR "Unrecognized Fortran compiler. Please use Intel (ifort), GCC (gfortran), or NAG (nagfor).")
endif()

include(CheckCompilerCapabilities)

if (MPI MATCHES YES)
  if (NOT SUPPORT_FOR_MPI_ALLOC_MEM_CPTR)
    if (NOT DISABLE_MPI_LOCK MATCHES YES)
      message (FATAL_ERROR
	"Version of MPI is too old to support MPI_Alloc_mem() with C_PTR argument. 
 To use MPI use -DDISABLE_MPI_LOCK=YES."
	)
    endif ()
  endif ()
endif ()


add_subdirectory(src)
add_subdirectory(doc EXCLUDE_FROM_ALL)

find_package (PFUNIT QUIET)
if (PFUNIT_FOUND)
  if (NOT TARGET (tests))
    add_custom_target(tests COMMAND ${CMAKE_CTEST_COMMAND})
  endif ()
   add_subdirectory(tests EXCLUDE_FROM_ALL)
endif()

add_subdirectory(examples EXCLUDE_FROM_ALL)
add_subdirectory(benchmarks EXCLUDE_FROM_ALL)

