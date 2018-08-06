#-------------------------

# Input Variables
# FTL_ROOT
# Produces:
# FTL_LIBRARY
# FTL_INCLUDE_DIR

# By default pFUnit uses FTL as a git submodule.   But this can be overridden with:
set(FTL "" CACHE PATH "Optional path to externally installed FTL package.")

if (FTL) # use default
  set(FTL_INSTALL_DIR ${FTL})
else()
  set(FTL_SOURCE_DIR ${CMAKE_SOURCE_DIR}/gFTL)
  set(FTL_INSTALL_DIR ${CMAKE_BINARY_DIR}/gFTL/install)

  include(${CMAKE_ROOT}/Modules/ExternalProject.cmake)
  file(GLOB all_files ${FTL_SOURCE_DIR}/*)
  list(LENGTH all_files n_files)

  if(n_files LESS_EQUAL 3)
    # git clone command did not use --recurse-submodules
    set(repository git@github.com:Goddard-Fortran-Ecosystem/gFTL.git)
    set(download_command git submodule init)
    set(update_command git submodule update)
  else()
    set(repository "")
    set(download_command "")
    set(update_command "")
  endif()

  ExternalProject_Add(gFTL
    GIT_REPOSITORY ${repository}
    DOWNLOAD_COMMAND ${download_command}
    UPDATE_COMMAND ${update_command}
    PREFIX ${CMAKE_CURRENT_BINARY_DIR}/FTL
    SOURCE_DIR ${FTL_SOURCE_DIR}
    INSTALL_DIR ${FTL_INSTALL_DIR}
    BUILD_COMMAND make
    CMAKE_ARGS -DCMAKE_INSTALL_PREFIX=${FTL_INSTALL_DIR}  -DCMAKE_INSTALL_MESSAGE=LAZY
    INSTALL_COMMAND make install)
endif()

#-------------------------

