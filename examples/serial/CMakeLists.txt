include_directories(${CMAKE_BINARY_DIR}/src)
include_directories(${FTL}/mod)
link_directories(${FTL}/lib)

file (GLOB CONFIG_INPUTS "${CMAKE_CURRENT_SOURCE_DIR}/*.cfg")
file (COPY ${CONFIG_INPUTS} DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

add_executable(complex.x complex.F90)
target_link_libraries(complex.x pflogger)
if (MPI_FOUND)
  add_compile_definitions(LOGGER_USE_MPI)
  target_link_libraries(complex.x pflogger MPI::MPI_Fortran)
endif ()
  

