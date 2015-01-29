program loggingWithFormat
   use ASTG_Logger_mod
   use ASTG_StreamHandler_mod
   use ASTG_SeverityLevels_mod, only: DEBUG, WARNING
   use ASTG_Formatter_mod
   implicit none
   
   type (Logger) :: log
   type (StreamHandler) :: stdout1, stdout2, stdout3

   ! Create a Logger and give it a name. A log message will be displayed
   ! with the following default format: <LEVEL: NAME: MESSAGE>.
   ! Note that if the log name is the empty string then it will be given
   ! the default name 'ROOT'.
   log = Logger('myApp')

   ! Create handlers
   stdout1 = StreamHandler()
   call stdout1%setLevel(DEBUG)
   stdout2 = StreamHandler()
   call stdout2%setLevel(DEBUG)
   stdout3 = StreamHandler()
   call stdout3%setLevel(DEBUG)
   
   ! Change the output format. Now the output will simply display
   ! <MESSAGE> rather than <LEVEL: NAME: MESSAGE>.
   call stdout1%setFormatter(Formatter('%(message::a)'))
   call log%addHandler(stdout1)
   
   ! Change log level to DEBUG
   call log%setLevel(DEBUG)
   print *,'---DISPLAY MESSAGE ONLY---'
   call log%info('Starting MAIN program.')

   ! Change the output format. Will use %(asctime::) attribute to set
   ! date format:
   call stdout2%setFormatter(Formatter(fmt='%(asctime::a) %(message::a)', &
      datefmt='%(Y::i4.4)-%(M::i2.2)-%(D::i2.2) %(HH::i2.2)-%(MM::i2.2)-%(SS::i2.2)'))
   call log%removeHandler(stdout1)
   call log%addHandler(stdout2)

   print *,'---DISPLAY DATE and TIME and MESSAGE---'
   call log%info('Starting MAIN program.')
   
   ! Change the output format. Will use %(asctime::) attribute to set
   ! a new date format:
   call stdout3%setFormatter(Formatter(fmt='%(asctime::a) %(message::a)', &
                         datefmt='%(HH::i2.2)-%(MM::i2.2)-%(SS::i2.2)'))
   call log%removeHandler(stdout2)
   call log%addHandler(stdout3)

   print *,'---DISPLAY only TIME and MESSAGE---'
   call log%info('Starting MAIN program.')
   print *,'---DONE---'

end program loggingWithFormat

