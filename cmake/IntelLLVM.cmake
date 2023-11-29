set (FPP_FLAG "-cpp")
set (SUPPRESS_LINE_LENGTH_WARNING "-diag-disable 5268")
set (CMAKE_Fortran_FLAGS_RELEASE "${FPP_FLAG} -O3 -free -stand f08 ${SUPPRESS_LINE_LENGTH_WARNING}")
set (CMAKE_Fortran_FLAGS_DEBUG   "${FPP_FLAG} -O0 -g -traceback \
      -check nouninit -free -stand f08 -save-temps ${SUPPRESS_LINE_LENGTH_WARNING}")

