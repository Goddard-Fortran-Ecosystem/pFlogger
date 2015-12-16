program main
   use mpi
   use pflogger
   use iso_c_binding

   type (FileHandler) :: fh
   type (RotatingFileHandler) :: rfh
   type (MpiLock) :: lock

   class (Logger), pointer :: loggerA

   integer :: ier, rank

   type (StringUnlimitedMap) :: extra
   type (Formatter) :: fmtr

   
   call mpi_init(ier)
   call initialize()
   
   call mpi_comm_rank(MPI_COMM_WORLD, rank, ier)
   call extra%insert('rank', rank)

   loggerA => logging%getLogger('A')

   fmtr = Formatter('%(message) %(rank)')
   fh = FileHandler('foo.txt', delay=.true.)
   call fh%setFormatter(fmtr)
   call fh%setLock(MpiLock(MPI_COMM_WORLD))

   rfh = newRotatingFileHandler('foorot.txt', delay=.true., maxBytes='100', backupCount=2)
   call rfh%setFormatter(fmtr)
   call rfh%setLock(MpiLock(MPI_COMM_WORLD))


   call loggerA%addHandler(fh)
   call loggerA%addHandler(rfh)
   select case (rank)
   case (0)
      call loggerA%setlevel(DEBUG)
   case (1:)
      call loggerA%setLevel(INFO)
   end select

   block
     integer :: i
     do i = 1, 10
        call loggerA%info('hello', extra=extra)
     end do
   end block
      
   
   call mpi_finalize(ier)

end program main
