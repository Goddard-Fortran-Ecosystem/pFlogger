include_directories(${CMAKE_BINARY_DIR}/src)

add_executable(basic.x basic.F90)
target_link_libraries(basic.x logger)

add_executable(verify.x verify.F90)
target_link_libraries(verify.x logger)

