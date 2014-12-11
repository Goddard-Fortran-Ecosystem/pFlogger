if (MPI MATCHES YES)
   # Add CPP flags for MPI=YES
   add_definitions(-DUSE_MPI)
   find_package(MPI REQUIRED)
   include_directories(${MPI_INCLUDE_PATH})
   set (COMPILER_WRAPPER mpif90)
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

set (CPPFLAGS 
  "-cpp ${CPPFLAGS}"
  )

include_directories(${CMAKE_SOURCE_DIR}/src)

# Intel compiler flags
if (${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel")

   if (CMAKE_SYSTEM_NAME MATCHES Linux)
      set (CPPFLAGS 
         "${CPPFLAGS}"
      )
   else()
      set (CPPFLAGS 
         "${CPPFLAGS}"
      )
   endif()


   set(F90FLAGS 
      "${CPPFLAGS} ${FFLAGS_RELEASE} -g -O2 -free -assume realloc_lhs"
   )

   if (COMPILE_WITH_DEBUG MATCHES YES)
      set(FFLAGS 
         "${FFLAGS} -g -O0 -traceback -assume realloc_lhs"
      )
      set(F90FLAGS 
         "${F90FLAGS} -g -O0 -traceback -assume realloc_lhs"
      )
   endif()

   if (WITH_PFUNIT MATCHES YES)
      set(F90FLAGS 
         "-g -O0 -traceback -assume realloc_lhs"
      )
   endif()


# GNU compiler flags
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL GNU)

   if (CMAKE_SYSTEM_NAME MATCHES Linux)
      set (CPPFLAGS "${CPPFLAGS}")
   else()
      set (CPPFLAGS "${CPPFLAGS}")
   endif()

   # Base flags
   set(F90FLAGS 
      "${CPPFLAGS} -cpp -O2 -ffree-line-length-none"
   )
   if (COMPILE_WITH_DEBUG MATCHES YES)
      set(FFLAGS 
         "${FFLAGS} -g -O0 -fbacktrace"
      )
      set(F90FLAGS 
         "${F90FLAGS} -g -O0 -fbacktrace"
      )
   endif()

   if (WITH_PFUNIT MATCHES YES)
      set(F90FLAGS 
         "${F90FLAGS} -g -O0 -fbacktrace"
      )
   endif()

else()

   message( FATAL_ERROR "Unrecognized compiler. Please use ifort or gfortran" )

endif()

set(CMAKE_SHARED_LIBRARY_LINK_Fortran_FLAGS "")
set(CMAKE_SKIP_RPATH ON)

