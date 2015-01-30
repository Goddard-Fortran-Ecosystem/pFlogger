program loggingWithFormat
   use ASTG_Logger_mod
   use ASTG_StreamHandler_mod
   use ASTG_SeverityLevels_mod, only: DEBUG, WARNING
   use ASTG_Formatter_mod
   implicit none
   
   type (Logger) :: log
   type (StreamHandler) :: stdout
   integer :: i, j
   real(kind=8) :: T(15,10)
   
   ! Create a Logger and give it a name. A log message will be displayed
   ! with the following default format: <LEVEL: NAME: MESSAGE>.
   ! Note that if the log name is the empty string then it will be given
   ! the default name 'ROOT'.
   log = Logger('myApp')

   ! Create handlers
   stdout = StreamHandler()
   call stdout%setLevel(DEBUG)
   
   ! Change the output format. Now the output will simply display
   ! <MESSAGE> rather than <LEVEL: NAME: MESSAGE>.
   call stdout%setFormatter(Formatter('%(message::a)'))
   ! addHandler associates (creates a copy) stdout with logger
   call log%addHandler(stdout)
   
   ! Change log level to DEBUG
   call log%setLevel(DEBUG)
   print *,'---MESSAGE ONLY---'
   call log%info('Starting MAIN program.')

   ! Change the output format. Will use %(asctime::) attribute to set
   ! date format:
   call log%removeHandler(stdout) ! removeHandler disassociates stdout with logger
   call stdout%setFormatter(Formatter(fmt='%(asctime::a) %(message::a)', &
      datefmt='%(Y::i4.4)-%(M::i2.2)-%(D::i2.2) %(HH::i2.2)-%(MM::i2.2)-%(SS::i2.2)'))
   call log%addHandler(stdout)

   print *,'---DATE, TIME and MESSAGE---'
   call log%info('Starting MAIN program')
   
   ! Change the output format. Will use %(asctime::) attribute to set
   ! a new date format:
   call log%removeHandler(stdout) ! removeHandler disassociates stdout with logger
   call stdout%setFormatter(Formatter(fmt='%(asctime::a) %(message::a)', &
                         datefmt='%(HH::i2.2)-%(MM::i2.2)-%(SS::i2.2)'))
   call log%addHandler(stdout)

   print *,'---TIME and MESSAGE---'
   call log%info('Starting MAIN program.')

   ! Change the output format. Print message and numbers.
   call log%removeHandler(stdout) ! removeHandler disassociates stdout with logger
   call stdout%setFormatter(Formatter())
   call log%addHandler(stdout)

   print *,'---MESSAGE and real numbers ---'
   call log%info('e is %(g20.11) and Pi is %(g20.14)', &
        arg1=2.718281828459, arg2=4.d0*datan(1.d0))

   i = 13; j = 7   
   call random_number(T)
   print *,'---MESSAGE combining integers and reals---'
   call log%info('Temperature at (%(i2),%(i1)) is %(f8.4)', &
        arg1=i,arg2=j,arg3=T(i,j)*100)
   
   print *,'---DONE---'

end program loggingWithFormat

