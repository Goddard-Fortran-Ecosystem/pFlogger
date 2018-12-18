set (FPP_FLAG -cpp)
set (common_flags "${FPP_FLAG} -ffree-line-length-255")

set (CMAKE_Fortran_FLAGS_RELEASE "${common_flags} -O3")
set (CMAKE_Fortran_FLAGS_DEBUG "${common_flags} -O0 -g -fbacktrace -fcheck=pointer -fcheck=mem \
                               -fcheck=bounds -ffpe-trap=invalid,zero,overflow -finit-real=snan")
