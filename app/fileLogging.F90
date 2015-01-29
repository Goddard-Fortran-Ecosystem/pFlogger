program fileLogging
   use ASTG_Logger_mod
   use ASTG_FileHandler_mod
   use ASTG_Formatter_mod
   use ASTG_SeverityLevels_mod
   implicit none
   
   type (Logger) :: log
   type (FileHandler) :: fHandler
   type(Formatter) :: fmt
   
   ! Create a Logger and give it a name.
   ! NOTE: Loggers have message levels to filter out messages
   ! and default is INFO.
   log = Logger('fileLog')

   ! Create a file handler, argument is fileName
   fHandler = FileHandler('app.LOG')
   ! By default handler level is INFO, change to DEBUG:
   call fHandler%setLevel(DEBUG)

   ! Create a logging format, this excludes LEVEL info
   fmt = Formatter('%(name::a): %(message::a)')

   call fHandler%setFormatter(fmt)
   
   ! Add this handler to logger so that logger can use it
   call log%addHandler(fHandler)
   
   ! Start logging!
   print *,'---Logging to file app.LOG---'
   call log%info('This is a message in a LOG file.')
   
   ! Done
   print *,'---DONE---'
   
end program fileLogging
