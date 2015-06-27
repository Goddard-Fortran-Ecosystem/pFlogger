      type is (integer(int8))
         if (fmt_ == LIST_DIRECTED_FORMAT) then
            write(buffer,*) arg
         else
            write(buffer,fmt_) arg
         end if
      type is (integer(int16))
         if (fmt_ == LIST_DIRECTED_FORMAT) then
            write(buffer,*) arg
         else
            write(buffer,fmt_) arg
         end if
      type is (integer(int32))
         if (fmt_ == LIST_DIRECTED_FORMAT) then
            write(buffer,*) arg
         else
            write(buffer,fmt_) arg
         end if
      type is (integer(int64))
         if (fmt_ == LIST_DIRECTED_FORMAT) then
            write(buffer,*) arg
         else
            write(buffer,fmt_) arg
         end if
      type is (real(real32))
         if (fmt_ == LIST_DIRECTED_FORMAT) then
            write(buffer,*) arg
         else
            write(buffer,fmt_) arg
         end if
      type is (real(real64))
         if (fmt_ == LIST_DIRECTED_FORMAT) then
            write(buffer,*) arg
         else
            write(buffer,fmt_) arg
         end if
      type is (real(real128))
         if (fmt_ == LIST_DIRECTED_FORMAT) then
            write(buffer,*) arg
         else
            write(buffer,fmt_) arg
         end if
     type is (complex(real32))
        if (fmt_ == LIST_DIRECTED_FORMAT) then
           write(buffer,*) arg
        else
           write(buffer,fmt_) arg
        end if
      type is (complex(real64))
         if (fmt_ == LIST_DIRECTED_FORMAT) then
            write(buffer,*) arg
         else
            write(buffer,fmt_) arg
         end if
      type is (complex(real128))
         if (fmt_ == LIST_DIRECTED_FORMAT) then
            write(buffer,*) arg
         else
            write(buffer,fmt_) arg
         end if
      type is (logical)
         if (fmt_ == LIST_DIRECTED_FORMAT) then
            write(buffer,*) arg
         else
            write(buffer,fmt_) arg
         end if
      type is (character(len=*))
         if (fmt_ == LIST_DIRECTED_FORMAT) then
            write(buffer,*) arg
         else
            write(buffer,fmt_) arg
         end if
