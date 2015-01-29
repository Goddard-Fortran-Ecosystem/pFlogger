program basicLogging
   use ASTG_Logger_mod
   use ASTG_StreamHandler_mod
   use ASTG_FileHandler_mod
   use ASTG_SeverityLevels_mod, only: DEBUG, WARNING
   implicit none
   
   type (Logger) :: log
   type (StreamHandler) :: stdout
   type (FileHandler) :: logfile

   ! Create a Logger and give it a name. A log message will be displayed
   ! with the following default format: <LEVEL: NAME: MESSAGE>.
   ! Note that if the log name is the empty string then it will be given
   ! the default name 'ROOT'.
   log = Logger('')

   ! We will log in two places : STDOUT and a file
   ! Specify a handler for STDOUT
   stdout = StreamHandler()
   call stdout%setLevel(WARNING) 
   call log%addHandler(stdout)

   ! Specify a handler for log file
   logfile = FileHandler('LOG')
   call logfile%setLevel(DEBUG) 
   call log%addHandler(logfile)

   ! Log away...
   print *,'---WITH DEFAULT LEVEL---'
   call log%info('Starting MAIN program.')
   call log%warning('Time step is too large.')
   call log%error('Max number of iterations exceeded.')
   call log%critical('CFL criterion violated. Program will abort.')
   call log%debug('T at (140,35,10) is  273K.')

   ! Default log level is INFO. Therefore to diagnose
   ! problems, change log level to DEBUG
   call log%setLevel(DEBUG)
   print *,'---WITH DEBUG LEVEL---'
   call log%info('Starting MAIN program.')
   call log%warning('Time step is too large.')
   call log%error('Max number of iterations exceeded.')
   call log%critical('CFL criterion violated. Program will abort.')
   call log%debug('T at (140,35,10) is  273K.')   

   print *,'---DONE---'

end program basicLogging

