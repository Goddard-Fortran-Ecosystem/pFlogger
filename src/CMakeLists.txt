# CMakeLists.txt for logger src

get_filename_component(DIRNAME "${CMAKE_CURRENT_SOURCE_DIR}" NAME)
set(this pflogger)

set(SRCS
   ArgListUtilities.F90
   AbstractFilter.F90
   AbstractLogger.F90
   AbstractLock.F90
   Config.F90
   DynamicBuffer.F90
   Exception.F90
   Filter.F90
   Filterer.F90
   FastFormatter.F90
   FormatToken.F90
   FormatParser.F90
   FormatString.F90
   Formatter.F90
   KeywordEnforcer.F90
   LevelFilter.F90
   LogRecord.F90
   Logger.F90
   LoggerManager.F90
   Object.F90
   pflogger.F90
   RootLogger.F90
   SeverityLevels.F90
   AbstractHandler.F90
   StreamHandler.F90
   FileHandler.F90
   RotatingFileHandler.F90
   StringUtilities.F90
   WrapArray.F90
# FTL containers
   StringAbstractLoggerPolyMap.F90
   StringFormatterMap.F90
   StringFilterMap.F90
   StringHandlerMap.F90
   StringLockMap.F90
   LoggerPolyVector.F90
   AbstractHandlerPolyVector.F90
   AbstractHandlerPtrVector.F90
   AbstractFilterPolyVector.F90
   FormatTokenVector.F90
)

if (MPI_FOUND) 
  list (APPEND SRCS
     MpiFilter.F90
     MpiFormatter.F90
     MpiCommConfig.F90
     )
   if (SUPPORT_FOR_MPI_ALLOC_MEM_CPTR)
     list (APPEND SRCS MpiLock.F90)
   endif ()

endif ()

set (bin ${CMAKE_CURRENT_BINARY_DIR} )
set (src ${CMAKE_CURRENT_SOURCE_DIR} )


add_library (${this} STATIC ${SRCS})
add_library(PFLOGGER::pflogger ALIAS pflogger)
target_link_libraries (${this} PUBLIC YAFYAML::yafyaml)
set_target_properties(${this} PROPERTIES Fortran_MODULE_DIRECTORY ${PFLOGGER_BINARY_DIR}/include)



if (MPI_FOUND)
  target_link_libraries (${this} PRIVATE
    $<INSTALL_INTERFACE:MPI::MPI_Fortran>)
  target_compile_definitions(${this} PRIVATE _LOGGER_USE_MPI)
endif ()


if (MPI_FOUND)
  if (PFUNIT_FOUND)
    add_library (mock-mpi MockMpi.F90)
    set_target_properties(mock-mpi PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mock-mpi)
    target_include_directories (mock-mpi PUBLIC ${MPI_Fortran_INCLUDE_DIRS} ${CMAKE_CURRENT_BINARY_DIR}/mock-mpi)
    target_include_directories (mock-mpi PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
    target_link_libraries (mock-mpi PUBLIC PFUNIT::funit)
    if (SUPPORT_FOR_C_LOC_ASSUMED_SIZE)
      target_compile_definitions(mock-mpi PRIVATE -DSUPPORT_FOR_C_LOC_ASSUMED_SIZE)
    endif ()

    # Complex linkage depending on target:
    set(isTest $<STREQUAL:$<TARGET_PROPERTY:IS_TEST>,1>)
    set(notTest $<NOT:${isTest}>)

    target_link_libraries (pflogger PUBLIC
#      $<BUILD_INTERFACE:mock-mpi>
      $<${notTest}:MPI::MPI_Fortran>
      $<${isTest}:mock-mpi>
      $<${isTest}:PFUNIT::funit>)

  else ()
    target_link_libraries (pflogger PRIVATE MPI::MPI_Fortran)
  endif ()
  
endif()


  
set (dest "PFLOGGER-${PFLOGGER_VERSION_MAJOR}.${PFLOGGER_VERSION_MINOR}")

target_include_directories(${this} PUBLIC
  $<BUILD_INTERFACE:${PFLOGGER_BINARY_DIR}/include>
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
  $<INSTALL_INTERFACE:${dest}/include>
  )

target_link_libraries(${this} PUBLIC GFTL_SHARED::gftl-shared GFTL::gftl)

install (TARGETS pflogger EXPORT PFLOGGER DESTINATION ${dest}/lib)
install (DIRECTORY  ${PROJECT_BINARY_DIR}/include/ DESTINATION ${dest}/include)
install (FILES  ${PROJECT_SOURCE_DIR}/src/error_handling_macros.fh DESTINATION ${dest}/include)




