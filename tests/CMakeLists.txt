# CMakeLists.txt for logger tests

get_filename_component(DIRNAME "${CMAKE_CURRENT_SOURCE_DIR}" NAME)

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
   init_tests.F90
   MockHandler.F90
   MockDateFormat.F90
   MockSimTime.F90
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
   Test_Complex_Config.F90
   Test_SimTime.F90
)

if (MPI)
  list (APPEND SRCS
    MockMpi.F90
    )
  list (APPEND TEST_SRCS
    Test_MpiFilter.F90
    Test_MpiFormatter.F90
    Test_MpiCommConfig.F90
    )
  if (SUPPORT_FOR_C_LOC_ASSUMED_SIZE)
    list (APPEND TEST_SRCS
       Test_MpiLock.F90
       )
   endif ()
endif ()


set(ALL_SRCS ${SRCS} ${TEST_SRCS} )

set_source_files_properties(
   ${TEST_SRCS}
   PROPERTIES GENERATED TRUE
)

include_directories(
   ${CMAKE_SOURCE_DIR}/tests
   ${CMAKE_SOURCE_DIR}/src
   ${CMAKE_BINARY_DIR}/src
   ${CMAKE_BINARY_DIR}/src/include
   ${FTL_INSTALL_DIR}/include
   ${PFUNIT}/include
)

# Targets:

add_definitions (-DPFUNIT_EXTRA_INITIALIZE=init_tests)

# Can this be made to work?
#set_target_properties (${EXE}
#  PROPERTIES DEFINE_SYMBOL -DPFUNIT_EXTRA_INITIALIZE=initTests )

add_library(pflogger_tests STATIC ${ALL_SRCS})
target_link_libraries(pflogger_tests pflogger pfunit)

set(EXE tests.x)
add_executable (${EXE} EXCLUDE_FROM_ALL ${PFUNIT}/include/driver.F90 testSuites.inc)
target_link_libraries(${EXE} pflogger_tests pflogger pfunit)


add_custom_target(tests
   COMMAND ${EXE} 
   DEPENDS ${EXE} 
   WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
   COMMENT "Run ${EXE}"
   )

