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

   loggerA => logging%get_logger('A')

   fmtr = newFormatter('%(message) %(rank)')

   fh = FileHandler('foo.txt', delay=.true.)
   call fh%set_formatter(fmtr)
   call fh%set_lock(MpiLock(MPI_COMM_WORLD))

   rfh = newRotatingFileHandler('foorot.txt', delay=.true., max_bytes='100', backup_count=2)
   call rfh%set_formatter(fmtr)
   call rfh%set_lock(MpiLock(MPI_COMM_WORLD))


   call loggerA%add_handler(fh)
   call loggerA%add_handler(rfh)
   select case (rank)
   case (0)
      call loggerA%set_level(DEBUG)
   case (1:)
      call loggerA%set_level(INFO)
   end select

   block
     integer :: i
     do i = 1, 10
        call loggerA%info('hello', extra=extra)
     end do
   end block
      
   
   call mpi_finalize(ier)

end program main
