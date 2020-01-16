      subroutine get_pv_default (name,value,defvalue,type)
      implicit none
      character*(*) name, type
      integer value, defvalue

      if (type.eq.'j'.or.type.eq.'J') then
        call get_pv_default_j (name,value,defvalue)
      else if (type.eq.'e'.or.type.eq.'E') then
        call get_pv_default_e (name,value,defvalue)
      else if (type.eq.'d'.or.type.eq.'D') then
        call get_pv_default_d (name,value,defvalue)
      else
        write (0,*) ' Use type = j, e, or d in get_pv_default'
        call exit (1)
      endif
      
      return
      end
       
*xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

      subroutine get_pv_default_j (name,value,defvalue)
      implicit none
      character*(*) name
      integer value, defvalue

      character*200 arg
      integer status
      logical defined
      integer lnblnk

      call get_cl_par (name,arg)
      if (defined(arg)) then
        read (arg,*,iostat=status) value
        if (status.ne.0) then
          write (0,*) 'Bad value of parameter ',name(1:lnblnk(name))
     ~        ,': ',arg(1:lnblnk(arg))
          write (0,*) ' (attempt to read as an integer) '
          call exit(1)
        endif
      else
        value = defvalue
      endif
      return
      end
      

*xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
       
      subroutine get_pv_default_e (name,value,defvalue)
      implicit none
      character*(*) name
      real value, defvalue

      character*200 arg
      integer status
      logical defined
      integer lnblnk

      call get_cl_par (name,arg)
      if (defined(arg)) then
        read (arg,*,iostat=status) value
        if (status.ne.0) then
          write (0,*) 'Bad value of parameter ',name(1:lnblnk(name))
     ~        ,': ',arg(1:lnblnk(arg))
          write (0,*) ' (attempt to read as a real) '
          call exit(1)
        endif
      else
        value = defvalue
      endif
      return
      end

      
*xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
       
      subroutine get_pv_default_d (name,value,defvalue)
      implicit none
      character*(*) name
      double precision value, defvalue

      character*200 arg
      integer status
      logical defined
      integer lnblnk

      call get_cl_par (name,arg)
      if (defined(arg)) then
        read (arg,*,iostat=status) value
        if (status.ne.0) then
          write (0,*) 'Bad value of parameter ',name(1:lnblnk(name))
     ~        ,': ',arg(1:lnblnk(arg))
          write (0,*) ' (attempt to read as double precision) '
          call exit(1)
        endif
      else
        value = defvalue
      endif
      return
      end

      
