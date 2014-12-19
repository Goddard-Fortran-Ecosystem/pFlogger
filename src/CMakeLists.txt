# CMakeLists.txt for logger src

add_subdirectory(FTL)

get_filename_component(DIRNAME "${CMAKE_CURRENT_SOURCE_DIR}" NAME)
set(THIS logger)

list(APPEND SRCS Filter.F90)
list(APPEND SRCS LogRecord.F90)
list(APPEND SRCS Filterer.F90)

list(APPEND SRCS AbstractHandlerPolyWrap.F90)
list(APPEND SRCS AbstractHandlerPolyWrapVector.F90)

list(APPEND SRCS FilterPolyWrap.F90)
list(APPEND SRCS FilterPolyWrapVector.F90)

list(APPEND SRCS CaseInsensitiveStringLoggerPair.F90)
list(APPEND SRCS CaseInsensitiveStringLoggerPairVector.F90)
list(APPEND SRCS CaseInsensitiveStringLoggerPairVectorVector.F90)
list(APPEND SRCS CaseInsensitiveStringLoggerUnorderedMap.F90)

list(APPEND SRCS SeverityLevels.F90)
list(APPEND SRCS AbstractHandler.F90)
list(APPEND SRCS StreamHandler.F90)
list(APPEND SRCS RotatingFileHandler.F90)
list(APPEND SRCS FileHandler.F90)
list(APPEND SRCS Logger.F90)
list(APPEND SRCS LoggerManager.F90)

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/FTL)
include_directories(${CMAKE_CURRENT_BINARY_DIR}/FTL)

add_library(${THIS} STATIC ${SRCS})
target_link_libraries(${THIS} ftl)
