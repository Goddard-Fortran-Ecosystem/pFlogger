set (FPP_FLAG "-cpp")
set (CMAKE_Fortran_FLAGS_RELEASE "${FPP_FLAG} -O3 -free -stand f08")
set (CMAKE_Fortran_FLAGS_DEBUG   "${FPP_FLAG} -O0 -g -traceback \
      -check uninit -free -stand f08 -save-temps")

