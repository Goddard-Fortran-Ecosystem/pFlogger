# CMakeLists.txt for logger src

get_filename_component(DIRNAME "${CMAKE_CURRENT_SOURCE_DIR}" NAME)
set(THIS logger)

set(SRCS
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
)

if (TEST_MPI) 
list(APPEND SRCS
   MpiFilter.F90
   MpiFileHandler.F90
)
endif()

set(FTL_SRCS
   AbstractFilterPolyWrap.F90
   AbstractFilterPolyWrapVec.F90
   AbstractHandlerPolyWrap.F90
   AbstractHandlerPolyWrapVec.F90
   XWrap.F90
   XWrapVec.F90
   CIStringAbstractLoggerPolyPair.F90
   CIStringAbstractLoggerPolyPairVec.F90
   CIStringAbstractLoggerPolyPairVecVec.F90
   CIStringAbstractLoggerPolyUMap.F90
   CIStringXPair.F90
   CIStringXPairVec.F90
   CIStringXPairVecVec.F90
   CIStringXUMap.F90
   StringVec.F90
)

list(APPEND SRCS ${FTL_SRCS})

set( bin ${CMAKE_CURRENT_BINARY_DIR} )
set( src ${CMAKE_CURRENT_SOURCE_DIR} )

add_custom_command(OUTPUT ${FTL_SRCS}
  WORKING_DIRECTORY ${bin}
  COMMAND ${FTL}/bin/driver.py ${src}/ftl.config ${src}/ftl.contractions
)

include_directories(${FTL}/mod)

set_source_files_properties(
   ${SRCS}
   PROPERTIES COMPILE_FLAGS 
   "${CMAKE_Fortran_FLAGS} ${F90FLAGS}"
)

add_library(${THIS} STATIC ${SRCS})


