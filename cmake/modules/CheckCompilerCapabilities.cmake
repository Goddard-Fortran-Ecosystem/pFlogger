include (${CMAKE_SOURCE_DIR}/cmake/modules/CheckFortranSource.cmake)

CHECK_FORTRAN_SOURCE_COMPILE (
  ${CMAKE_SOURCE_DIR}/cmake/modules/supportForAssumedType.F90
  SUPPORT_FOR_ASSUMED_TYPE
)


