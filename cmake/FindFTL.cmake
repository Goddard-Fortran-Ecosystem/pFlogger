#-------------------------

# Input Variables
# FTL_ROOT
# Produces:
# FTL_LIBRARY
# FTL_INCLUDE_DIR

if (DEFINED FTL)
  message ("-- Detecting FTL on command line: ${FTL}")
else ()
  if (DEFINED ENV{FTL})
    set (FTL "$ENV{FTL}")
    message ("-- Detecting FTL in environment: ${FTL}")
  endif ()
endif ()

if (NOT DEFINED FTL)
  message(FATAL_ERROR "Unable to find FTL package. Provide path as FTL.")
endif ()

#-------------------------

