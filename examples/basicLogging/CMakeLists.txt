include_directories(${CMAKE_BINARY_DIR}/src)
add_executable(loggingLevel.x loggingLevel.F90)
target_link_libraries(loggingLevel.x logger pfunit )
