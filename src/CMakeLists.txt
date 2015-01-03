# CMakeLists.txt for logger src

get_filename_component(DIRNAME "${CMAKE_CURRENT_SOURCE_DIR}" NAME)
set(THIS logger)

list(APPEND SRCS Object.F90)
list(APPEND SRCS Exception.F90)
list(APPEND SRCS Filter.F90)
list(APPEND SRCS LogRecord.F90)
list(APPEND SRCS Formatter.F90)
list(APPEND SRCS Filterer.F90)

# list(APPEND SRCS AbstractHandlerPolyWrap.F90)
# list(APPEND SRCS AbstractHandlerPolyWrapVec.F90)

# list(APPEND SRCS FilterPolyWrap.F90)
# list(APPEND SRCS FilterPolyWrapVec.F90)

# list(APPEND SRCS XWrap.F90)
# list(APPEND SRCS XWrapVec.F90)
# list(APPEND SRCS CIStringAbstractLoggerPolyPair.F90)
# list(APPEND SRCS CIStringAbstractLoggerPolyPairVec.F90)
# list(APPEND SRCS CIStringAbstractLoggerPolyPairVecVec.F90)
# list(APPEND SRCS CIStringAbstractLoggerPolyUMap.F90)
# list(APPEND SRCS StringVec.F90)

list(APPEND SRCS SeverityLevels.F90)
list(APPEND SRCS AbstractHandler.F90)
list(APPEND SRCS StreamHandler.F90)
list(APPEND SRCS RotatingFileHandler.F90)
list(APPEND SRCS FileHandler.F90)
list(APPEND SRCS AbstractLogger.F90)
list(APPEND SRCS Logger.F90)
list(APPEND SRCS RootLogger.F90)
list(APPEND SRCS Placeholder.F90)
list(APPEND SRCS LoggerManager.F90)
list(APPEND SRCS FormatParser.F90)

set(FTL_SRCS
  AbstractHandlerPolyWrap.F90
  AbstractHandlerPolyWrapVec.F90
  FilterPolyWrap.F90
  FilterPolyWrapVec.F90
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


message("F90FLAGS ${F90FLAGS}")
include_directories(${FTL}/mod)

set_source_files_properties(
   ${SRCS}
   PROPERTIES COMPILE_FLAGS 
   "${CMAKE_Fortran_FLAGS} ${F90FLAGS}"
)

add_library(${THIS} STATIC ${SRCS})
target_link_libraries(${THIS} ftl)
