include (fortran_try)

foreach (kind 32 64 128)
  set(CMAKE_REQUIRED_FLAGS = -fpp)
  set(CMAKE_REQUIRED_DEFINITIONS -D_KIND=REAL${kind})

  try_compile (
    code_compiles
    ${PFLOGGER_BINARY_DIR}
    ${CMAKE_CURRENT_LIST_DIR}/trial_sources/REAL_KIND.F90
    CMAKE_FLAGS "-DCOMPILE_DEFINITIONS=${CMAKE_REQUIRED_DEFINITIONS}")
  
  if (code_compiles)
    CHECK_Fortran_SOURCE_RUN (
      ${CMAKE_CURRENT_LIST_DIR}/trial_sources/REAL_KIND.F90
      _ISO_REAL${kind}
      )
  endif ()
  
  try_compile (
    code_compiles
    ${PFLOGGER_BINARY_DIR}
    ${CMAKE_CURRENT_LIST_DIR}/trial_sources/REAL_KIND.F90
    CMAKE_FLAGS "-DCOMPILE_DEFINITIONS=${CMAKE_REQUIRED_DEFINITIONS}")
  

endforeach()



