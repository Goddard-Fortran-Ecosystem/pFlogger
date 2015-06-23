# CMakeLists.txt for logger src

get_filename_component(DIRNAME "${CMAKE_CURRENT_SOURCE_DIR}" NAME)
set(THIS logger)

set(SRCS
   StringUtilities.F90
   CaseInsensitiveString.F90
   AbstractFilter.F90
   AbstractHandler.F90
   AbstractLogger.F90
   Exception.F90
   FileHandler.F90
   Filter.F90
   Filterer.F90
   FormatToken.F90
   newFormatParser.F90
   FormatParser.F90
   Formatter.F90
   LogRecord.F90
   Logger.F90
   LoggerManager.F90
   Object.F90
   Placeholder.F90
   RootLogger.F90
   RotatingFileHandler.F90
   SeverityLevels.F90
   StreamHandler.F90
   ArgListUtilities.F90
# FTL containers
   CIStringAbstractLoggerPolyMap.F90
   AbstractHandlerPolyVector.F90
   AbstractFilterPolyVector.F90
   UnlimitedVector.F90
   CIStringUnlimitedMap.F90
   FormatTokenVector.F90
   StringVector.F90
FormatTokenVector.F90
)

if (MPI) 
list(APPEND SRCS
   MpiFilter.F90
   MpiFileHandler.F90
)
endif()

set (bin ${CMAKE_CURRENT_BINARY_DIR} )
set (src ${CMAKE_CURRENT_SOURCE_DIR} )

include_directories (${NEWFTL}/include)

set_source_files_properties (
   ${SRCS}
   PROPERTIES COMPILE_FLAGS 
   "${CMAKE_Fortran_FLAGS} ${F90FLAGS}"
)

add_library (${THIS} STATIC ${SRCS})


