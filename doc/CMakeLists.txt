# add a target to generate API documentation with Doxygen

# Temporary for DISCOVER.
if (LINUX)
   set(DOXYGEN_EXECUTABLE /usr/local/other/doxygen/1.5.6/bin/doxygen)
   set(DOXYGEN_FOUND TRUE)
else()
   find_package(Doxygen)
endif()

if(DOXYGEN_FOUND)
   configure_file(${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile.in ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile @ONLY)
   add_custom_target(doc
   ${DOXYGEN_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile
   WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
   COMMENT "Generating API documentation with Doxygen" VERBATIM
   )
endif(DOXYGEN_FOUND)
