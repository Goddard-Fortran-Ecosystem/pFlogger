# CMakeLists.txt for logger tests

get_filename_component(DIRNAME "${CMAKE_CURRENT_SOURCE_DIR}" NAME)
set(THIS logger_tests)

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
   initTests.F90
   MockHandler.F90
   MockDateFormat.F90
   ConfigurableFilter.F90
)

set(TEST_SRCS 
   Test_Filter.F90
   Test_Filterer.F90
   Test_StreamHandler.F90
   Test_RotatingFileHandler.F90
   Test_FileHandler.F90
   Test_Logger.F90
   Test_LogRecord.F90
   Test_LoggerManager.F90
   Test_Formatter.F90
   Test_ParserContexts.F90
   Test_FormatParser.F90
   Test_FormatString.F90
   Test_ArgListUtilities.F90
   Test_SeverityLevels.F90
   Test_Config.F90
)

if (MPI) 
  list(APPEND SRCS
     MockMpi.F90
  )

  list(APPEND TEST_SRCS
     Test_MpiFileHandler.F90
     Test_MpiFilter.F90
  )
endif()


set(ALL_SRCS ${SRCS} ${TEST_SRCS} )

set_source_files_properties(
   ${TEST_SRCS}
   PROPERTIES GENERATED TRUE
)

include_directories(
   ${CMAKE_SOURCE_DIR}/tests
   ${CMAKE_BINARY_DIR}/src
   ${FTL}/include
   ${PFUNIT}/include
)

# Targets:

add_library(${THIS} STATIC ${ALL_SRCS})

target_link_libraries(${THIS}
   logger
   pfunit
)

add_definitions (-DPFUNIT_EXTRA_INITIALIZE=initTests)

#set_target_properties (${EXE}
#  PROPERTIES DEFINE_SYMBOL -DPFUNIT_EXTRA_INITIALIZE=initTests )

set(EXE tests.x)
add_executable (${EXE} ${PFUNIT}/include/driver.F90 testSuites.inc)
target_link_libraries(${EXE} ${THIS} logger pfunit)

add_custom_target(tests
   COMMAND ${EXE} 
   DEPENDS ${EXE} 
   WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
   COMMENT "Run ${EXE}"
)
add_dependencies(tests ${THIS})
