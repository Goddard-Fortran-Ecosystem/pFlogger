# CMakeLists.txt for logger tests

get_filename_component(DIRNAME "${CMAKE_CURRENT_SOURCE_DIR}" NAME)
set(THIS loggerTests)

# Pre-process pf files
file(GLOB pfSources "*.pf")
foreach(file ${pfSources})
   get_filename_component(basename ${file} NAME_WE)
   add_custom_command (
      OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${basename}.F90
      COMMAND python
      ARGS ${PFUNIT}/bin/pFUnitParser.py ${file} 
      ${CMAKE_CURRENT_BINARY_DIR}/${basename}.F90
      DEPENDS ${file}
   )
endforeach()

set (SRCS
   MockHandler.F90
)

set(TEST_SRCS 
   Test_StringUtilities.F90
   Test_DefaultHandler.F90
   Test_FileHandler.F90
   Test_Logger.F90
)

set(ALL_SRCS ${SRCS} ${TEST_SRCS} )

set_source_files_properties(
    ${TEST_SRCS}
    PROPERTIES COMPILE_FLAGS "${CMAKE_Fortran_FLAGS} ${F90FLAGS}"
    GENERATED TRUE
)

include_directories(
   ${CMAKE_SOURCE_DIR}/tests
   ${CMAKE_BINARY_DIR}/src
   ${PFUNIT}/include
)

# Targets:

add_library(${THIS} STATIC ${ALL_SRCS}
)

target_link_libraries(${THIS}
   logger
   pfunit
)

set(EXE ${THIS}.x)
add_executable (${EXE} ${PFUNIT}/include/driver.F90 testSuites.inc)
target_link_libraries(${EXE} ${THIS} logger pfunit)

if (MPI)
   target_link_libraries(${EXE} ${MPI_Fortran_LIBRARIES} )
   add_custom_target(tests 
     COMMAND mpirun -np 4 tests/${EXE} 
     DEPENDS ${EXE} 
     WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
     COMMENT "Run MPI ${EXE}"
   )
else()
    add_custom_target(tests
      COMMAND ${EXE} 
      DEPENDS ${EXE} 
      WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
      COMMENT "Run SERIAL ${EXE}"
    )
endif()
add_dependencies(tests ${THIS})
