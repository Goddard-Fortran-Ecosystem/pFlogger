program main
   use pflogger_mod
   implicit none

   integer :: i
   integer :: c_0, c_1, c_rate
   real :: x(3)
   integer :: unit
     character(len=80) :: buffer
     character(len=:), allocatable :: str

   open(file='foo_raw.txt', newunit=unit, status='new', form='formatted')

   call system_clock(c_0, c_rate)
   do i = 1, 100000
      call random_number(x)
      write(unit,'(a, i8, 1x, 10(f8.4,1x))')'hello ', i, x
   end do
   call system_clock(c_1)

   close(unit, status='delete')
   print*,real(c_1-c_0)/c_rate

   open(file='foo_raw.txt', newunit=unit, status='new', form='formatted')

   call system_clock(c_0, c_rate)

     do i = 1, 100000
        call random_number(x)
        call foo(x)
        str = 'a ' // str
     end do
     call system_clock(c_1)
     print*,real(c_1-c_0)/c_rate

   close(unit, status='delete')

contains

   subroutine foo(x)
      class (*), intent(in) :: x(:)
      select type (x)
      type is (real)
         write(buffer,'(a, i8, 1x, 10(f8.4,1x))')'hello ', i, x
         str = trim(buffer)
      end select
      write(unit,'(a)') str
   end subroutine foo
end program main


   
