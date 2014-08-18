cmake_minimum_required (VERSION 2.8)
project (logger Fortran)

set (CMAKE_VERBOSE_MAKEFILE 1)

set(CMAKE_INCLUDE_CURRENT_DIR ON)
set(CMAKE_INCLUDE_DIRECTORIES_PROJECT_BEFORE ON)

add_subdirectory(src)
add_subdirectory(app)
