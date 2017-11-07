# Add flags for PFUNIT
if (PFUNIT)
   include_directories(${PFUNIT}/mod)
   include_directories(${PFUNIT}/include)
   link_directories(${PFUNIT}/lib)
   set(PFUNIT ${PFUNIT})
   message(STATUS " +++ PFUNIT is set to ${PFUNIT}") 
else()
   message(STATUS " +++ PFUNIT is NOT set. PFUNIT is needed for testing.")
endif()

# Intel compiler flags
if (${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel")

   # CMake 3.3 will have CMAKE_Fortran_COMPILER_VERSION. 
   # For now,we need this hack:
   exec_program(ifort ARGS "--version" OUTPUT_VARIABLE ver)
   string(REGEX REPLACE "^.*[ ]([0-9]+)\\.[0-9].*$" "\\1" IFORT_MAJOR "${ver}")
   if (${IFORT_MAJOR} MATCHES 14)
      set(CPPFLAGS "${CPPFLAGS} -DINTEL_14 -cpp")
   else()
      set(CPPFLAGS "${CPPFLAGS} -cpp")
   endif()

   set (CMAKE_Fortran_FLAGS_RELEASE "${CPPFLAGS} -O3 \
      -free -assume realloc_lhs -stand f08")
   set (CMAKE_Fortran_FLAGS_DEBUG   "${CPPFLAGS} -O0 -g -traceback \
      -check uninit -free -assume realloc_lhs -stand f08 -save-temps")

# GNU compiler flags
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL GNU)

   set(CPPFLAGS "${CPPFLAGS} -cpp")
   set (CMAKE_Fortran_FLAGS_RELEASE "${CPPFLAGS} -O3")
   set (CMAKE_Fortran_FLAGS_DEBUG "${CPPFLAGS} -O0 -g -fbacktrace -fcheck=pointer -fcheck=mem -fcheck=bounds -ffpe-trap=invalid,zero,overflow -finit-real=snan -ffree-line-length-255")

# NAG compiler flags
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL NAG)

   set (CPPFLAGS "${CPPFLAGS} -fpp")
   set (CMAKE_Fortran_FLAGS_RELEASE "${CPPFLAGS} -O3")
   set (CMAKE_Fortran_FLAGS_DEBUG   "${CPPFLAGS} -O0 -gline -C=all")

else()

   message(FATAL_ERROR "Unrecognized compiler ${CMAKE_Fortran_COMPILER_ID}. \
      Valid vendors are INTEL, GNU or NAG" )

endif()

