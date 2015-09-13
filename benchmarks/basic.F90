program main
   use pflogger_mod
   implicit none

   integer :: log_n
   integer :: n

   integer :: c0, c1, crate
   real :: time_pflogger, time_raw
   
   integer :: unit

   call initialize()
   call config()

   print*, '  n          T(pflogger)       T(raw)      ratio'

   do log_n = 3, 5
      n = 10** log_n
   
      call system_clock(c0, crate)
      call bench_pflogger(n)
      call system_clock(c1)
      time_pflogger = real(c1 - c0) / crate

      call system_clock(c0)
      call bench_raw(n)
      call system_clock(c1)
      time_raw = real(c1 - c0) / crate

      print*,n, time_pflogger, time_raw, time_pflogger / time_raw

      open(file='foo.txt',newunit=unit)
      close(unit, status='delete')

      open(file='foo_raw.txt',newunit=unit)
      close(unit, status='delete')

   end do

   call finalize()

contains

   subroutine config()
      type (Logger), pointer :: my_logger
      type (FileHandler) :: h
      type (Formatter) :: f

      my_logger => logging%getLogger('A')

      call my_logger%setLevel(INFO)
      h = FileHandler('foo.txt')
      f = Formatter('%(message)')
      call h%setFormatter(f)

      call my_logger%addHandler(h)

   end subroutine config

   subroutine bench_pflogger(n)
      integer, intent(in) :: n
      type (Logger), pointer :: my_logger
      integer :: i
      
      my_logger => logging%getLogger('A')
      do i = 1, n
         call my_logger%INFO('hello %i8', i)
      end do

   end subroutine bench_pflogger


   subroutine bench_raw(n)
      integer, intent(in) :: n

      integer :: i
      integer :: unit

      open(file='foo_raw.txt', newunit=unit, status='new', form='formatted')

      do i = 1, n
         write(unit,'(a, i8)')'hello ', i
      end do

   end subroutine bench_raw

end program main

