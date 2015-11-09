!!$! Parser
ADD_TEST_SUITE(Test_ParserContexts_mod_suite)
ADD_TEST_SUITE(Test_FormatParser_mod_suite)
ADD_TEST_SUITE(Test_FormatString_mod_suite)

! Filter and Filterer
ADD_TEST_SUITE(Test_Filter_mod_suite)
ADD_TEST_SUITE(Test_Filterer_mod_suite)

! Handlers
ADD_TEST_SUITE(Test_StreamHandler_mod_suite)
ADD_TEST_SUITE(Test_FileHandler_mod_suite)
ADD_TEST_SUITE(Test_RotatingFileHandler_mod_suite)

! Logger
ADD_TEST_SUITE(Test_Logger_mod_suite)

! Logging
ADD_TEST_SUITE(Test_LoggerManager_mod_suite)

! Log record
ADD_TEST_SUITE(Test_LogRecord_mod_suite)

! Formatter
ADD_TEST_SUITE(Test_Formatter_mod_suite)

ADD_TEST_SUITE(Test_ArgListUtilities_mod_suite)

! Others
ADD_TEST_SUITE(Test_SeverityLevels_mod_suite)

! MPI related
#if defined(LOGGER_USE_MPI) && defined(SUPPORT_FOR_ASSUMED_TYPE)
ADD_TEST_SUITE(Test_MpiFilter_mod_suite)
ADD_TEST_SUITE(Test_MpiFileHandler_mod_suite)
ADD_TEST_SUITE(Test_MpiLock_mod_suite)
#endif

! config
ADD_TEST_SUITE(Test_Config_mod_suite)
ADD_TEST_SUITE(Test_SimTime_mod_suite)
