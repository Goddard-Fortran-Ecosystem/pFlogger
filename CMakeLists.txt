# ------------------------------------------------------------------------ #
# ASTG Logger top level CmakeLists.txt
#
# Usage:
#   cmake <top-dir> -DCMAKE_PREFIX_PATH=<path>
#
# The following (cmake) packages must be already installed at <path>
#
# - gFTL
# - gFTL-shared
# - yaFyaml
#
# and if running the unit tests
#
# - pFUnit
#
# MPI support will be built if FindPackage() finds MPI in the build
# environment.
#
# ------------------------------------------------------------------------ #
cmake_minimum_required (VERSION 3.8.0)
project (PFLOGGER
  VERSION 1.4.5
  LANGUAGES Fortran)

set (CMAKE_MODULE_PATH
   ${CMAKE_SOURCE_DIR}/cmake
)

include (PreventInSourceBuild)
include (DefineCMakeDefaults)
include (DefinePlatformDefaults)

set (CMAKE_POSITION_INDEPENDENT_CODE ON)

find_package (GFTL REQUIRED)
find_package (GFTL_SHARED REQUIRED)
find_package (YAFYAML REQUIRED)

find_package (MPI QUIET)
find_package (PFUNIT QUIET)

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

if (PFUNIT_FOUND)
  enable_testing()
  if (NOT TARGET tests)
    add_custom_target(tests COMMAND ${CMAKE_CTEST_COMMAND})
  endif ()
   add_subdirectory(tests EXCLUDE_FROM_ALL)
endif()

add_subdirectory(examples EXCLUDE_FROM_ALL)
add_subdirectory(benchmarks EXCLUDE_FROM_ALL)

# find_package() support
configure_file (PFLOGGERConfig-version.cmake.in ${PROJECT_BINARY_DIR}/PFLOGGERConfig-version.cmake @ONLY)
configure_file (PFLOGGERConfig.cmake.in ${PROJECT_BINARY_DIR}/PFLOGGERConfig.cmake @ONLY)

set (top_dir PFLOGGER-${PFLOGGER_VERSION_MAJOR}.${PFLOGGER_VERSION_MINOR})
install (
  FILES ${PROJECT_BINARY_DIR}/PFLOGGERConfig.cmake ${PROJECT_BINARY_DIR}/PFLOGGERConfig-version.cmake
  DESTINATION "${top_dir}/cmake")

