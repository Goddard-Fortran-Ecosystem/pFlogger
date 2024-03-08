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
cmake_minimum_required (VERSION 3.12)
project (PFLOGGER
  VERSION 1.13.1
  LANGUAGES Fortran)

set (CMAKE_MODULE_PATH
   ${PROJECT_SOURCE_DIR}/cmake
)

include (PreventInSourceBuild)
include (DefineCMakeDefaults)
include (DefinePlatformDefaults)

set (CMAKE_POSITION_INDEPENDENT_CODE ON)

if (NOT TARGET GFTL::gftl)
  find_package (GFTL REQUIRED VERSION 1.10.0)
endif ()
if (NOT TARGET GFTL_SHARED::gftl-shared)
  find_package (GFTL_SHARED REQUIRED VERSION 1.6.0)
endif ()
if (NOT TARGET YAFYAML::yafyaml)
  find_package (YAFYAML REQUIRED VERSION 1.3.0)
endif ()

option (ENABLE_MPI "Enable MPI support" ON)
if (ENABLE_MPI)
  find_package (MPI QUIET)
endif ()
find_package (PFUNIT QUIET)

#-----------------------------------
# Set default Fortran compiler flags
#-----------------------------------
include(${CMAKE_Fortran_COMPILER_ID} RESULT_VARIABLE found)
if(NOT found)
  message( FATAL_ERROR "Unrecognized Fortran compiler. Please use Intel (ifort), GCC (gfortran), or NAG (nagfor).")
endif()

include(CheckCompilerCapabilities)
include(check_intrinsic_kinds RESULT_VARIABLE found)

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

# The following is needed for external projects using *nix make when
# parent project builds gFTL-shared as a subproject.
set (top_dir PFLOGGER-${PFLOGGER_VERSION_MAJOR}.${PFLOGGER_VERSION_MINOR})
set (PFLOGGER_TOP_DIR "${CMAKE_INSTALL_PREFIX}/${top_dir}" CACHE PATH "")

# find_package() support
configure_file (PFLOGGERConfig-version.cmake.in ${PROJECT_BINARY_DIR}/PFLOGGERConfig-version.cmake @ONLY)
configure_file (PFLOGGERConfig.cmake.in ${PROJECT_BINARY_DIR}/PFLOGGERConfig.cmake @ONLY)

install (
  FILES ${PROJECT_BINARY_DIR}/PFLOGGERConfig.cmake ${PROJECT_BINARY_DIR}/PFLOGGERConfig-version.cmake
  DESTINATION "${top_dir}/cmake")

configure_file (PFLOGGER.mk.in ${PROJECT_BINARY_DIR}/PFLOGGER.mk @ONLY)
install (
  FILES ${CMAKE_CURRENT_BINARY_DIR}/PFLOGGER.mk ${CMAKE_CURRENT_BINARY_DIR}/PFLOGGER.mk
  DESTINATION "${top_dir}/include")


install (EXPORT PFLOGGER
  FILE PFLOGGERTargets.cmake
  NAMESPACE PFLOGGER::
  DESTINATION "${top_dir}/cmake")

export(EXPORT PFLOGGER
  FILE "${CMAKE_CURRENT_BINARY_DIR}/PFLOGGERTargets.cmake"
  NAMESPACE PFLOGGER::
  )
