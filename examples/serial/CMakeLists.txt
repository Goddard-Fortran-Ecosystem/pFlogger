include_directories(${CMAKE_BINARY_DIR}/src)
include_directories(${FTL}/mod)
link_directories(${FTL}/lib)

add_executable(loggingLevel.x loggingLevel.F90)
target_link_libraries(loggingLevel.x logger ftl)

add_executable(loggingFormat.x loggingFormat.F90)
target_link_libraries(loggingFormat.x logger ftl)
