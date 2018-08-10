# CMakeLists.txt for logger src

set (CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include)
get_filename_component(DIRNAME "${CMAKE_CURRENT_SOURCE_DIR}" NAME)
set(THIS pflogger)

set(SRCS
   ArgListUtilities.F90
   AbstractFilter.F90
   AbstractLogger.F90
   AbstractLock.F90
   Config.F90
   DynamicBuffer.F90
   Exception.F90
   Filter.F90
   Filterer.F90
   FastFormatter.F90
   FormatToken.F90
   FormatParser.F90
   FormatString.F90
   Formatter.F90
   KeywordEnforcer.F90
   LevelFilter.F90
   LogRecord.F90
   Logger.F90
   LoggerManager.F90
   Object.F90
   pflogger.F90
   RootLogger.F90
   SeverityLevels.F90
   AbstractHandler.F90
   StreamHandler.F90
   FileHandler.F90
   RotatingFileHandler.F90
   String.F90
   StringUtilities.F90
   StringUnlimitedPolyMap.F90
   FTL_Config.F90
   WrapArray.F90
# FTL containers
   StringIntegerMap.F90
   IntegerStringMap.F90
   StringAbstractLoggerPolyMap.F90
   StringFormatterMap.F90
   StringFilterMap.F90
   StringHandlerMap.F90
   StringLockMap.F90
   LoggerPolyVector.F90
   AbstractHandlerPolyVector.F90
   AbstractFilterPolyVector.F90
   UnlimitedVector.F90
   StringUnlimitedMap.F90
   FormatTokenVector.F90
   StringVector.F90
   YAML_Parser.F90
)

if (MPI) 
  list (APPEND SRCS
     MpiFilter.F90
     MpiFormatter.F90
     MpiCommConfig.F90
     )
   if (SUPPORT_FOR_MPI_ALLOC_MEM_CPTR)
     list (APPEND SRCS MpiLock.F90)
   endif ()

endif ()

set (bin ${CMAKE_CURRENT_BINARY_DIR} )
set (src ${CMAKE_CURRENT_SOURCE_DIR} )


include_directories (${CMAKE_CURRENT_BINARY_DIR}/include)
include_directories (${FTL_INSTALL_DIR}/include)

add_library (${THIS} STATIC ${SRCS})

if (NOT FTL)
  add_dependencies(pflogger gFTL)
endif()

install(TARGETS ${THIS} DESTINATION lib)
install(DIRECTORY  ${CMAKE_CURRENT_BINARY_DIR}/include DESTINATION .)



