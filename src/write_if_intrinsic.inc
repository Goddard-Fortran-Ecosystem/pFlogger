   intrinsic = .true.
   select type (arg)
   type is (integer(int8))
      include 'write_to_buffer.inc'
   type is (integer(int16))
      include 'write_to_buffer.inc'
   type is (integer(int32))
      include 'write_to_buffer.inc'
   type is (integer(int64))
      include 'write_to_buffer.inc'
   type is (real(real32))
      include 'write_to_buffer.inc'
   type is (real(real64))
      include 'write_to_buffer.inc'
   type is (real(real128))
      include 'write_to_buffer.inc'
   type is (complex(real32))
      include 'write_to_buffer.inc'
   type is (complex(real64))
      include 'write_to_buffer.inc'
   type is (complex(real128))
      include 'write_to_buffer.inc'
   type is (logical)
      include 'write_to_buffer.inc'
   type is (character(len=*))
      if (fmt(1:1) == LIST_DIRECTED_FORMAT) then
         if (scalar) then
            write(buffer%buffer,'(a,a1)',iostat=iostat) arg, EOT ! avoid leading space
         else
            write(buffer%buffer,*,iostat=iostat) arg, EOT ! avoid leading space
         end if
      else
         write(buffer%buffer,fmt,iostat=iostat) arg
      end if
   class default
      intrinsic = .false.
   end select
