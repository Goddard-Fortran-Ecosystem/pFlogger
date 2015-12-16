program basicLogging
   use ASTG_Logger_mod
   use ASTG_StreamHandler_mod
   use ASTG_FileHandler_mod
   use ASTG_SeverityLevels_mod, only: DEBUG, WARNING,ERROR
   use pflogger
   implicit none
   
   type (Logger) :: log
   type (StreamHandler) :: stdout
   type (FileHandler) :: logfile

   call initialize()  ! pfloggger

   ! Create a Logger and give it a name. A log message will be displayed
   ! with the following default format: <LEVEL: NAME: MESSAGE>.
   ! Note that if the log name is the empty string then it will be given
   ! the default name 'ROOT'.
   log = Logger('')

   ! We will log in two places : STDOUT and a file
   ! Specify a handler for STDOUT
   stdout = StreamHandler()
   call log%addHandler(stdout)

   ! Specify a handler for log file
   logfile = FileHandler('basicLevel.log')
   call log%addHandler(logfile)

   ! Default log level is INFO. 
   print *,'---WITH DEFAULT(INFO) LEVEL---'
   call log%debug('you should not see this message')
   call log%info('Starting MAIN program.')
   call log%warning('Time step is too large.')
   call log%error('Max number of iterations exceeded.')
   call log%critical('CFL criterion violated. Program will abort.')

 
   print *,'---WITH ERROR LEVEL---'
   call log%setLevel(ERROR)
   call log%debug('You should not see this message. debug')   
   call log%info('You should not see this message. info')
   call log%warning('You should not see this message. warning')
   call log%error('Max number of iterations exceeded.')
   call log%critical('CFL criterion violated. Program will abort.')

   ! To diagnose problems, change log level to lower level DEBUG
   ! 1) remove the handlers and close the open logfiles
   ! 2) create new ones to go lower level
   ! 3) the first step can be ignored if there is no handlers in the que.

   ! 1)
   call logfile%close()
   call log%removeHandler(stdout)
   call log%removeHandler(logfile)
   ! 2)
   stdout = StreamHandler()
   call stdout%setLevel(DEBUG)
   call log%addHandler(stdout)

   logfile = FileHandler('basicLevel.log')
   call logfile%setLevel(DEBUG)
   call log%addHandler(logfile)

   ! Specify a handler for log file
   print *,'---WITH DEBUG LEVEL---'
   call log%setLevel(DEBUG)
   call log%debug('T at (140,35,10) is  273K.')   
   call log%info('Starting MAIN program.')
   call log%warning('Time step is too large.')
   call log%error('Max number of iterations exceeded.')
   call log%critical('CFL criterion violated. Program will abort.')

   print*,"---DONE---"

end program basicLogging

