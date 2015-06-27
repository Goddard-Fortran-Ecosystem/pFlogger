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
      include 'write_to_buffer.inc'
      class default
      intrinsic = .false.
   end select
