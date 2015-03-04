if(MPI MATCHES YES)
  set(TEST_MPI YES)
   # Add CPP flags for MPI=YES
   add_definitions(-DLOGGER_USE_MPI)
   find_package(MPI REQUIRED)
   include_directories(${MPI_INCLUDE_PATH})
   set (CMAKE_Fortran_COMPILER mpif90)
endif()

# Add flags for PFUNIT
if(EXISTS $ENV{PFUNIT})
   include_directories($ENV{PFUNIT}/mod)
   include_directories($ENV{PFUNIT}/include)
   link_directories($ENV{PFUNIT}/lib)
   set(CPPFLAGS -DUSE_PFUNIT)
   set(WITH_PFUNIT YES)
   set(PFUNIT $ENV{PFUNIT})
endif()

include_directories(${CMAKE_SOURCE_DIR}/src)

# Intel compiler flags
if (${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel")

   exec_program(ifort ARGS "--version" OUTPUT_VARIABLE ver)
   string(REGEX REPLACE "^.*[ ]([0-9]+)\\.[0-9].*$" "\\1" IFORT_MAJOR "${ver}")
   
   if (${IFORT_MAJOR} MATCHES 14)
      set(CPPFLAGS "${CPPFLAGS} -DINTEL_14 -cpp")
   endif()

   set(F90FLAGS 
      "${CPPFLAGS} ${FFLAGS_RELEASE} -free -assume realloc_lhs -stand f08 -g -O2"
   )

   if (COMPILE_WITH_DEBUG MATCHES YES OR WITH_PFUNIT MATCHES YES)
      set(F90FLAGS 
         "${F90FLAGS} -O0 -traceback"
      )
   endif()

# GNU compiler flags
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL GNU)

   if (CMAKE_SYSTEM_NAME MATCHES Linux)
      set (CPPFLAGS "${CPPFLAGS} -cpp")
   else()
      set (CPPFLAGS "${CPPFLAGS}")
   endif()

   set(F90FLAGS 
      "${CPPFLAGS} -cpp -O2 -ffree-line-length-none"
   )

   if (COMPILE_WITH_DEBUG MATCHES YES OR WITH_PFUNIT MATCHES YES)
      set(F90FLAGS 
         "${F90FLAGS} -O0 -fbacktrace"
      )
   endif()

elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL NAG)
   set (CPPFLAGS "${CPPFLAGS} -fpp -D__NAG__")
 
  set(F90FLAGS 
      "${CPPFLAGS}"
   )

   if (COMPILE_WITH_DEBUG MATCHES YES OR WITH_PFUNIT MATCHES YES)
      set(F90FLAGS 
         "${F90FLAGS} -O0"
      )
   endif()

else()

   message( FATAL_ERROR "Unrecognized compiler ${CMAKE_Fortran_COMPILER_ID}. Please use ifort or gfortran" )

endif()

set(CMAKE_SHARED_LIBRARY_LINK_Fortran_FLAGS "")
set(CMAKE_SKIP_RPATH ON)

