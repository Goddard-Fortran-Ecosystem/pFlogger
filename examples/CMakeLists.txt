add_subdirectory(serial)
add_subdirectory(basic_config)
if (MPI_FOUND)
  add_subdirectory(mpi)
endif()
