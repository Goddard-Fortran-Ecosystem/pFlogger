         if (fmt == LIST_DIRECTED_FORMAT) then
            write(buffer%buffer,*,iostat=iostat) arg, achar(003)
         else
            write(buffer%buffer,'('//fmt//',"'//achar(003)//'")',iostat=iostat) arg
         end if
