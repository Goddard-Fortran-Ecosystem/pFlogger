program basicLogging
   use ASTG_Logger_mod
   use ASTG_StreamHandler_mod
   use ASTG_SeverityLevels_mod
   implicit none
   
   type (Logger) :: log
   type (StreamHandler) :: stdHandler

   ! Create a Logger and give it a name.
   ! NOTE: Loggers have message levels to filter out messages
   ! and default is INFO.
   log = Logger('appLog')

   ! We need to specify where to output messages, Choose STDOUT.
   ! So, we need to create a STDOUT stream handler:
   stdHandler = StreamHandler()
   ! By default handler level is INFO, change to DEBUG:
   call stdHandler%setLevel(DEBUG)

   ! Add this handler to logger so that logger can use it
   call log%addHandler(stdHandler)

   ! Start logging!
   call log%info('Starting demo')
  
   ! inform about errors - note ERROR > INFO
   call log%error('Temperature cannot be zero')
   
   ! critical messages - note CRITICAL > INFO
   call log%critical('Missing IC file. Program will stop.')
   
   ! warnings - note WARNING > INFO
   call log%warning('Max number of iterations reached')
   
   ! debugging: filtering messages via severity level
   ! note DEBUG < INFO - therefore next DEBUG message is not logged
   call log%debug('Cannot check temperature values...')
   ! so we need to set logger level:
   call log%setLevel(DEBUG)
   call log%debug('Now checking temperature values...')
   ! When done, set back to INFO
   call log%setLevel(INFO)

   ! Done
   call log%info('Demo is DONE')
   
end program basicLogging
