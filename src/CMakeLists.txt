# CMakeLists.txt for logger src

get_filename_component(DIRNAME "${CMAKE_CURRENT_SOURCE_DIR}" NAME)
set(THIS logger)

list(APPEND SRCS StringUtilities.F90)
list(APPEND SRCS AbstractLogger.F90)
list(APPEND SRCS BasicLogger.F90)
list(APPEND SRCS Logger.F90)


add_library(${THIS} STATIC ${SRCS})
