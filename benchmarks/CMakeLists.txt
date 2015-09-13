include_directories(${CMAKE_BINARY_DIR}/src)

add_executable(basic.x basic.F90)
target_link_libraries(basic.x logger)

