list(APPEND SRCS Object.F90)
list(APPEND SRCS StringUtilities.F90)
list(APPEND SRCS String.F90)
list(APPEND SRCS CaseInsensitiveString.F90)
list(APPEND SRCS CIString.F90)
list(APPEND SRCS HashFunction.F90)
list(APPEND SRCS Exception.F90)

add_library(ftl STATIC ${SRCS})

