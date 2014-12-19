program app
   use ASTG_Logger_mod
   use ASTG_StreamHandler_mod
   use ASTG_SeverityLevels_mod
   implicit none
   
   type (Logger) :: log
   type (StreamHandler) :: sh
   
   log = Logger('myLogger')
   
   ! Will show all messages of severity >= DEBUG
   call sh%setLevel(DEBUG)
   call log%addHandler(sh)
   
   call log%warning('Starting app...')
   !
   !  do something
   call log%error('There is a problem.')
   !
   !  do something
   call log%info('Ending app.')
   
end program app
