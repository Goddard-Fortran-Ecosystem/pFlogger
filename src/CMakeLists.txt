# CMakeLists.txt for logger src

get_filename_component(DIRNAME "${CMAKE_CURRENT_SOURCE_DIR}" NAME)
set(THIS logger)

list(APPEND SRCS StringUtilities.F90)
list(APPEND SRCS AbstractHandler.F90)
list(APPEND SRCS DefaultHandler.F90)
list(APPEND SRCS FileHandler.F90)
list(APPEND SRCS AbstractHandlerPolyWrap.F90)
list(APPEND SRCS AbstractHandlerPolyWrapVector.F90)
list(APPEND SRCS Logger.F90)


add_library(${THIS} STATIC ${SRCS})
