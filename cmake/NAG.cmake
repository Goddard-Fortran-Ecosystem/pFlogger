set (FPP_FLAG "${FPP_FLAG} -fpp")

# set (CMAKE_Fortran_FLAGS_DEBUG   "${FPP_FLAG} -O0 -gline -C=all")
# workaround for nag 6.2
set (CMAKE_Fortran_FLAGS_DEBUG "-quiet -C=array -C=alias -C=bits -C=calls -C=do -C=intovf -C=present -C=pointer -O0")
set (CMAKE_Fortran_FLAGS_RELEASE "-quiet ${FPP_FLAG} -O3")
