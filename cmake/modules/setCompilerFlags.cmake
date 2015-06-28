# Add flags for PFUNIT
if (DEFINED ENV{PFUNIT})
   include_directories($ENV{PFUNIT}/mod)
   include_directories($ENV{PFUNIT}/include)
   link_directories($ENV{PFUNIT}/lib)
   set(CPPFLAGS -DUSE_PFUNIT)
   set(WITH_PFUNIT YES)
   set(PFUNIT $ENV{PFUNIT})
   message(STATUS "PFUNIT is set to $ENV{PFUNIT}") 
else()
   message(WARNING "PFUNIT is NOT set!")
endif()

include_directories(${CMAKE_SOURCE_DIR}/src)

# Intel compiler flags
if (${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel")

   exec_program(ifort ARGS "--version" OUTPUT_VARIABLE ver)
   string(REGEX REPLACE "^.*[ ]([0-9]+)\\.[0-9].*$" "\\1" IFORT_MAJOR "${ver}")
   
   if (${IFORT_MAJOR} MATCHES 14)
      set(CPPFLAGS "${CPPFLAGS} -DINTEL_14 -cpp")
   endif()

   set(F90FLAGS "${CPPFLAGS} -free -assume realloc_lhs -stand f08")

   if (CMAKE_BUILD_TYPE MATCHES Debug)
      set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O0 -traceback -g -check uninit")
   endif()

# GNU compiler flags
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL GNU)

   set(F90FLAGS "${CPPFLAGS} -cpp -ffree-line-length-none")

   if (CMAKE_BUILD_TYPE MATCHES Debug)
      set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O0 -fbacktrace -fcheck=pointer -fcheck=mem -fcheck=bounds -g")
   endif()

# NAG compiler flags
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL NAG)
   set (CPPFLAGS "${CPPFLAGS} -fpp -D__NAG__")
 
  set(F90FLAGS  "${CPPFLAGS}" )

   if (CMAKE_BUILD_TYPE MATCHES Debug)
      set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O0")
   endif()

else()

   message( FATAL_ERROR "Unrecognized compiler ${CMAKE_Fortran_COMPILER_ID}. Valid vendors are INTEL, GNU or NAG" )

endif()

set(CMAKE_SHARED_LIBRARY_LINK_Fortran_FLAGS "")
set(CMAKE_SKIP_RPATH ON)

