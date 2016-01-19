program main
   use pflogger
   use timer_mod
   implicit none

   integer :: log_n, n_iter
   integer :: log_n_words, n_words

   integer :: c0, c1, crate
   real :: time_pflogger, time_raw
   
   integer :: unit

   call initialize()
   call config()

   print*, '  n_iter   n_words    T(pflogger)       T(raw)      ratio'

   do log_n = 5, 5
      n_iter = 10** log_n

      do log_n_words = 1, 4
         n_words = 2**(log_n_words-1)
         call system_clock(c0, crate)
         call bench_pflogger(n_iter, n_words)
         call system_clock(c1)
         time_pflogger = real(c1 - c0) / crate

         call system_clock(c0)
         call bench_raw(n_iter, n_words)
         call system_clock(c1)
         time_raw = real(c1 - c0) / crate

         print*,n_iter, n_words, time_pflogger, time_raw, time_pflogger / time_raw

         ! clean up files
         open(file='foo.txt',newunit=unit)
         close(unit, status='delete')

         open(file='foo_raw.txt',newunit=unit)
         close(unit, status='delete')

         block
           integer :: i
           do i = 1, size(c_all)
              print*,i,  real(c_all(i))/crate
           end do
           c_all = 0
         end block
      end do
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
      f = Formatter('%(message)a')
      call h%setFormatter(f)

      call my_logger%addHandler(h)

   end subroutine config

   subroutine bench_pflogger(n_iter, n_words)
      integer, intent(in) :: n_iter
      integer, intent(in) :: n_words
      type (Logger), pointer :: my_logger
      integer :: i

      real :: x(n_words)
      
      my_logger => logging%getLogger('A')
      do i = 1, n_iter
         call do_work(i, x)
         call my_logger%INFO('hello %i8 %10(f8.4,1x)', i, wrapArray(x))
      end do

   end subroutine bench_pflogger


   subroutine bench_raw(n_iter, n_words)
      integer, intent(in) :: n_iter
      integer, intent(in) :: n_words

      integer :: i
      integer :: unit
      real :: x(n_words)

      open(file='foo_raw.txt', newunit=unit, status='new', form='formatted')

      do i = 1, n_iter
         call do_work(i, x)
         write(unit,'(a, i8, 1x, 10(f8.4,1x))')'hello ', i, x
      end do

   end subroutine bench_raw

   subroutine do_work(i, x)
      integer, intent(in) :: i
      real, intent(out) :: x(:)

      call random_number(x)

   end subroutine do_work

end program main

