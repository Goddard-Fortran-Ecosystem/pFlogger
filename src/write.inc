         if (fmt_ == LIST_DIRECTED_FORMAT) then
            write(buffer%buffer,*,iostat=iostat) arg
         else
            write(buffer%buffer,fmt_,iostat=iostat) arg
         end if
