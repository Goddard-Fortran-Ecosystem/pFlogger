program app
   use ASTG_Logger_mod
   use ASTG_StreamHandler_mod
   use ASTG_SeverityLevels_mod
   implicit none
   
   type (Logger) :: log
   type (StreamHandler) :: sh
   
   print *,'Create logger'
   log = Logger('myLogger')
   call sh%setLevel(WARNING)
   call log%addHandler(sh)

   call log%warning('Hello world')
      
end program app
