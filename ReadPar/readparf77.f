*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c BASIC functions

      subroutine get_cl_par (name,value)
      implicit none
      character*(*) name, value

      character*200 parname
      integer n

      call ini_readpar()        ! Initialize the library if necessary

      call rp_nullterm_string (name,parname)
      n=len(value)              ! remember Fortran length of value
      call rp_get_cl_par (parname,value) ! read paramater
      call rp_null2for_string (value,n) ! convert null-term value to Fortran
      
      return
      end

*xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
      subroutine get_numbered_par (key,npar,value)
      implicit none
      character*(*) key, value
      integer npar

      character*200 parname
      integer n
      
      call ini_readpar()        ! Initialize the library if necessary
      call rp_nullterm_string (key,parname)
      n=len(value)              ! remember Fortran length of value
      call rp_get_numbered_par (parname,npar,value) ! read paramater
      call rp_null2for_string (value,n) ! convert null-term value to Fortran

      return
      end
      

*xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

      subroutine get_string_par (string,name,value)
      implicit none
      character*(*) string, name, value

      character*200 parname
      character*8192 buffer
      integer n

      call ini_readpar()        ! Initialize the library if necessary

      call rp_nullterm_string (name,parname)
      call rp_nullterm_string (string,buffer)
      n=len(value)              ! remember Fortran length of value
      call rp_get_string_par (buffer,parname,value) ! read paramater
      call rp_null2for_string (value,n) ! convert null-term value to Fortran
      
      return
      end




*xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

      subroutine get_pf_par (file,name,value)
      implicit none
      character*(*) file, name, value
      
      character*200 filename, parname
      integer n
      
      call rp_nullterm_string (file,filename)
      call rp_nullterm_string (name,parname)

      n = len(value)
      call rp_get_pf_par (filename,parname,value)
      call rp_null2for_string (value,n)
      return
      end

*xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
c Misc utilities
      subroutine get_cl_par_position (name,position)
      implicit none
      character*(*) name
      integer position
      character*1024 value
      logical defined

      call get_cl_par (name,value)
      if (defined(value)) then
        call rp_get_cl_par_position (position)
      else
        position = -1
      endif
      return
      end

*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c Set default par name

      subroutine set_default_par (name,value)
      implicit none
      character*(*) name, value

      character*2048 parname, parvalue
      
      call rp_nullterm_string (name,parname)
      call rp_nullterm_string (value,parvalue)
      call set_default_par_value (parname,parvalue)
      
      return
      end


*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c Variable expansion
      subroutine rp_expand_variables (string)
      implicit none
      character*(*) string
      character*1024 buf
      integer n

      call rp_nullterm_string (string,buf)
      call rp_expand_variables_c (buf)
      n = len(buf)
      call rp_null2for_string (buf,n)
      string = buf
      return
      end
      
*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c Compatibility
      
      subroutine get_command_line_par (name,cond,value)
      implicit none
      character*(*) name, value
      integer cond

      character*80 namelocal

      namelocal = name
c if cond = 1, par name is case insensitive
      if (cond.ne.1) then
        if (name(1:1).ne.'^') then
          call get_cl_par ('^'//namelocal,value)
          return
        endif
      endif

      call get_cl_par (name,value)
      
      return
      end

      
      subroutine get_parameter_file_par (file,name,cond,value)
      implicit none
      character*(*) name, file, value
      integer cond

      character*80 namelocal

      namelocal = name
c if cond = 1, par name is case insensitive
      if (cond.ne.1) then
        if (name(1:1).ne.'^') then
          call get_pf_par (file,'^'//namelocal,value)
          return
        endif
      endif

      call get_pf_par (file,name,value)
      
      return
      end
      

*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c Logics      
      logical function defined (string)
      implicit none
      character*(*) string
      
      defined = string .ne. 'undefined'
      return
      end

*xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

      logical function yespar (name)
      implicit none
      character*(*) name
      character*200 value
      character*1 c
      
      call get_cl_par (name,value)

      c = value(1:1)
      if ( (c.ne.'y') .and. (c.ne.'Y') .and. (c.ne.'д') ) then
        yespar = .false.
      endif

      yespar = (value.eq.'y') .or. (value.eq.'yes') .or.
     ~    (value.eq.'Yes') .or. (value.eq.'YES') .or.
     ~    (value.eq.'да')

      return
      end
      

*xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
      
      logical function nopar (name)
      implicit none
      character*(*) name
      character*200 value
      character*1 c
      
      call get_cl_par (name,value)
      
      c = value(1:1)
      if ( (c.ne.'n') .and. (c.ne.'N') .and. (c.ne.'н') ) then
        nopar = .false.
      endif
      
      nopar = (value.eq.'n') .or. (value.eq.'no') .or.
     ~    (value.eq.'No') .or. (value.eq.'NO') .or.
     ~    (value.eq.'нет')
      
      return
      end

*xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
      
      logical function defpar (name)
      implicit none
      character*(*) name
      character*200 value
      logical defined

      call get_cl_par (name,value)
      defpar = defined (value)
      
      return
      end
      
      
*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c Work functions for Fortran's getarg interface

      subroutine rp_getarg (iarg,arg)
c
c Return null-terminated argument astring for use in C
c
      implicit none
      integer iarg
      character*(*) arg
      character*1000 argument
      integer lnblnk, iargc
      integer n,i
      character*1 NULL

      NULL = char(0)

      if (iarg.gt.iargc()) then
        arg(1:1)=NULL
        return
      endif
      
      
      call getarg(iarg,argument)
      if (argument.eq.' ') then
        arg(2:2)=NULL
        return
      endif
      
      n = lnblnk(argument)
      do i=1,n
        arg(i:i)=argument(i:i)
      enddo
      arg(n+1:n+1)=NULL


      return
      end
      
      
      subroutine rp_iargc (n)
      implicit none
      integer n
      integer iargc
      
      n = iargc()

      return
      end
      
*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c Small utilities

      subroutine rp_nullterm_string (string,nullstring)
      implicit none
      character*(*) string, nullstring
      integer lnblnk
      integer n

      character*1 NULL

      NULL = char(0)

      nullstring = string
      n = lnblnk(string) + 1
      nullstring(n:n) = NULL
      return
      end
      
      
      subroutine rp_null2for_string (string,n)
      implicit none
      integer n
      character*(*) string
      integer k,i
      character*1 NULL

      NULL = char(0)

      k = 1
      do while ( string(k:k) .ne. NULL )
        k = k + 1
      enddo
      do i=k,n
        string(i:i) = ' '
      enddo

      return
      end
