include_directories(${CMAKE_BINARY_DIR}/src)
add_executable(loggerTest.x loggerTest.F90)
target_link_libraries(loggerTest.x logger)
