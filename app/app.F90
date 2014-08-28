program app
   use Logger_mod
   type (Logger) :: log
   print *,'Create logger'
   log = newLogger()
end program app
