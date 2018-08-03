# Add flags for PFUNIT
if (PFUNIT)
   include_directories(${PFUNIT}/mod)
   include_directories(${PFUNIT}/include)
   link_directories(${PFUNIT}/lib)
   set(PFUNIT ${PFUNIT})
   message(STATUS " +++ PFUNIT is set to ${PFUNIT}") 
else()
   message(FATAL_ERROR " +++ PFUNIT is NOT set. PFUNIT is needed for testing.")
endif()


