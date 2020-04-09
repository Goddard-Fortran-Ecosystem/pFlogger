# Need to access cfg files when running examples.
file (GLOB CONFIG_INPUTS "${CMAKE_CURRENT_SOURCE_DIR}/*.cfg")
file (COPY ${CONFIG_INPUTS} DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

if (MPI_FOUND)

   add_executable(sharedFileHandle.x sharedFileHandle.F90)
   target_link_libraries(sharedFileHandle.x pflogger MPI::MPI_Fortran)
   set_target_properties(sharedFileHandle.x PROPERTIES IS_EXAMPLE 1)

   add_executable(complexMpi.x complexMpi.F90)
   target_link_libraries(complexMpi.x pflogger MPI::MPI_Fortran)
   set_target_properties(complexMpi.x PROPERTIES IS_EXAMPLE 1)
   
endif()

