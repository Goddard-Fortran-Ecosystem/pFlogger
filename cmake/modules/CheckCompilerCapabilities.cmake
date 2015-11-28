include (${CMAKE_SOURCE_DIR}/cmake/modules/CheckFortranSource.cmake)

CHECK_FORTRAN_SOURCE_COMPILE (
  ${CMAKE_SOURCE_DIR}/cmake/modules/supportForAssumedType.F90
  SUPPORT_FOR_ASSUMED_TYPE
)

CHECK_FORTRAN_SOURCE_COMPILE (
  ${CMAKE_SOURCE_DIR}/cmake/modules/supportFor_c_loc_assumed_size.F90
  SUPPORT_FOR_C_LOC_ASSUMED_SIZE
)


