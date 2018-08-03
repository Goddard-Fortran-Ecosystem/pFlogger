include_directories(${CMAKE_BINARY_DIR}/src)
include_directories(${FTL}/mod)

link_directories(${FTL}/lib)

# Need to access cfg files when running examples.
file (GLOB CONFIG_INPUTS "${CMAKE_CURRENT_SOURCE_DIR}/*.cfg")
file (COPY ${CONFIG_INPUTS} DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

if (MPI)

   add_executable(sharedFileHandle.x sharedFileHandle.F90)
   target_link_libraries(sharedFileHandle.x logger ftl ${MPI_Fortran_LIBRARIES})

   add_executable(complexMpi.x complexMpi.F90)
   target_link_libraries(complexMpi.x logger ftl ${MPI_Fortran_LIBRARIES})
   
endif()

