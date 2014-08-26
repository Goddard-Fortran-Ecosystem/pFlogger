# Print error message on an attempt to build inside the source directory tree:
if ("${CMAKE_CURRENT_SOURCE_DIR}" STREQUAL "${CMAKE_CURRENT_BINARY_DIR}")
   message(FATAL_ERROR "ERROR! "
   "CMAKE_CURRENT_SOURCE_DIR=${CMAKE_CURRENT_SOURCE_DIR}"
   " == CMAKE_CURRENT_BINARY_DIR=${CMAKE_CURRENT_BINARY_DIR}"
   "\nmodelE does not support in-source builds:\n"
   "You must now delete the CMakeCache.txt file and the CMakeFiles/ directory under"
   "the top ${PROJECT_NAME} directory  or you will not be able to configure correctly!"
   "\nYou must now run something like:\n"
   "  $ rm -r CMakeCache.txt CMakeFiles/"
   "\n"
   "Please create a different directory and configure ${PROJECT_NAME} under that different directory such as:\n"
   "  $ mkdir MY_BUILD\n"
   "  $ cd MY_BUILD\n"
   "  $ cmake [OPTIONS] .."
   )
endif()
