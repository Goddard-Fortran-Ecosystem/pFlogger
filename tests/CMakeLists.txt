

set (srcs
   init_tests.F90
   MockHandler.F90
   MockDateFormat.F90
   MockSimTime.F90
   ConfigurableFilter.F90
)

set(pf_tests
   Test_Filter.pf
   Test_Filterer.pf
   Test_StreamHandler.pf
   Test_RotatingFileHandler.pf
   Test_FileHandler.pf
   Test_Logger.pf
   Test_LogRecord.pf
   Test_LoggerManager.pf
   Test_Formatter.pf
   Test_FormatParser.pf
   Test_FormatString.pf
   Test_ArgListUtilities.pf
   Test_SeverityLevels.pf
   Test_Config.pf
   Test_Complex_Config.pf
   Test_SimTime.pf
)

if (MPI_FOUND)
  list (APPEND pf_tests
    Test_MpiFilter.pf
    Test_MpiFormatter.pf
    Test_MpiCommConfig.pf
    )
  if (SUPPORT_FOR_C_LOC_ASSUMED_SIZE)
    list (APPEND pf_tests
      Test_MpiLock.pf
      )
  endif ()
endif ()

set (link_libraries pflogger GFTL_SHARED::gftl-shared)
if (MPI_FOUND)
  list(APPEND link_libraries mock-mpi)
endif()

add_pfunit_ctest (pflogger_tests.x
    TEST_SOURCES ${pf_tests}
    OTHER_SOURCES ${srcs}
    LINK_LIBRARIES ${link_libraries})

set_target_properties(pflogger_tests.x PROPERTIES IS_TEST 1)

target_compile_definitions (pflogger_tests.x PRIVATE -DPFUNIT_EXTRA_INITIALIZE=init_tests)

add_dependencies (tests pflogger_tests.x)


