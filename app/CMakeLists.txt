include_directories(${CMAKE_BINARY_DIR}/src)
add_executable(app.x app.F90)
target_link_libraries(app.x logger)
