   intrinsic = .true.
   select type (arg)
   type is (integer(int8))
      write(*,*)'DEBUG :: Attempting to write int8: ', arg
      include 'write_to_buffer.inc'
   type is (integer(int16))
      write(*,*)'DEBUG :: Attempting to write int16: ', arg
      include 'write_to_buffer.inc'
   type is (integer(int32))
      write(*,*)'DEBUG :: Attempting to write int32: ', arg
      include 'write_to_buffer.inc'
   type is (integer(int64))
      write(*,*)'DEBUG :: Attempting to write int64: ', arg
      include 'write_to_buffer.inc'
   type is (real(real32))
      write(*,*)'DEBUG :: Attempting to write real32: ', arg
      include 'write_to_buffer.inc'
   type is (real(real64))
      write(*,*)'DEBUG :: Attempting to write real64: ', arg
      include 'write_to_buffer.inc'
   type is (real(real128))
      write(*,*)'DEBUG :: Attempting to write real128: ', arg
      include 'write_to_buffer.inc'
   type is (complex(real32))
      write(*,*)'DEBUG :: Attempting to write complex32: ', arg
      include 'write_to_buffer.inc'
   type is (complex(real64))
      write(*,*)'DEBUG :: Attempting to write complex64: ', arg
      include 'write_to_buffer.inc'
   type is (complex(real128))
      write(*,*)'DEBUG :: Attempting to write complex128: ', arg
      include 'write_to_buffer.inc'
   type is (logical)
      write(*,*)'DEBUG :: Attempting to write int8: ', arg
      include 'write_to_buffer.inc'
   type is (character(len=*))
      if (fmt(1:1) == LIST_DIRECTED_FORMAT) then
      write(*,*)'DEBUG :: Attempting to write list directed format string: ', arg
         write(buffer%buffer,'(a)',iostat=iostat) arg ! avoid leading space
      else
      write(*,*)'DEBUG :: Attempting to write formatted string: ', arg
         write(buffer%buffer,fmt,iostat=iostat) arg
      end if
   class default
      intrinsic = .false.
   end select
