include_directories(${CMAKE_BINARY_DIR}/src)
add_executable(basicLogging.x basicLogging.F90)
target_link_libraries(basicLogging.x logger pfunit )
