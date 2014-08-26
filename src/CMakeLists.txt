# CMakeLists.txt for logger src

get_filename_component(DIRNAME "${CMAKE_CURRENT_SOURCE_DIR}" NAME)
set(THIS logger)

add_library(${THIS} Logger.F90)
target_link_libraries(${THIS})
