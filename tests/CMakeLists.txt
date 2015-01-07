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
   MockMpi.F90
)

set(TEST_SRCS 
   Test_Filter.F90
   Test_Filterer.F90
   Test_StreamHandler.F90
   Test_RotatingFileHandler.F90
   Test_FileHandler.F90
   Test_MpiFileHandler.F90
   Test_Logger.F90
   Test_LogRecord.F90
   Test_LoggerManager.F90
   Test_Formatter.F90
   Test_FormatParser.F90
   Test_MpiFilter.F90
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
   ${FTL}/mod
   ${PFUNIT}/include
)

# Targets:

add_library(${THIS} STATIC ${ALL_SRCS}
)

link_directories(${FTL}/lib)

target_link_libraries(${THIS}
   logger
   pfunit
   ftl
)

set(EXE ${THIS}.x)
add_executable (${EXE} ${PFUNIT}/include/driver.F90 testSuites.inc)
target_link_libraries(${EXE} ${THIS} logger ftl pfunit)

add_custom_target(tests
   COMMAND ${EXE} 
   DEPENDS ${EXE} 
   WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
   COMMENT "Run SERIAL ${EXE}"
)
add_dependencies(tests ${THIS})
