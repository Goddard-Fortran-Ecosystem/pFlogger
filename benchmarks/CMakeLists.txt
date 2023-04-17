include_directories(${CMAKE_BINARY_DIR}/src)
include_directories(${FTL}/mod)

link_directories(${FTL}/lib)

add_executable(basic.x basic.F90)
target_link_libraries(basic.x logger ftl)

add_executable(verify.x verify.F90)
target_link_libraries(verify.x logger ftl)

