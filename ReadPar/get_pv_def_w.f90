!*xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

subroutine get_pv_default_j (name,value,defvalue)
  implicit none
  character(len=*) :: name
  integer :: value, defvalue

  character(len=200) :: arg
  integer :: status
  logical, external :: defined

  call get_cl_par (name,arg)
  if (defined(arg)) then
    read (arg,*,iostat=status) value
    if (status.ne.0) then
      write (0,*) 'Bad value of parameter ',trim(name),': ',trim(arg)
      write (0,*) ' (attempt to read as an integer) '
      call exit(1)
    endif
  else
    value = defvalue
  endif
  return
end subroutine get_pv_default_j
      

!*xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
       
subroutine get_pv_default_e (name,value,defvalue)
  implicit none
  character(len=*) :: name
  real :: value, defvalue

  character(len=200) :: arg
  integer :: status
  logical, external :: defined

  call get_cl_par (name,arg)
  if (defined(arg)) then
    read (arg,*,iostat=status) value
    if (status.ne.0) then
      write (0,*) 'Bad value of parameter ',trim(name),': ',trim(arg)
      write (0,*) ' (attempt to read as a real) '
      call exit(1)
    endif
  else
    value = defvalue
  endif
  return
end subroutine get_pv_default_e

      
!*xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
       
subroutine get_pv_default_d (name,value,defvalue)
  implicit none
  character (len=*) :: name
  double precision :: value, defvalue

  character(len=200) :: arg
  integer :: status
  logical, external :: defined

  call get_cl_par (name,arg)
  if (defined(arg)) then
    read (arg,*,iostat=status) value
    if (status.ne.0) then
      write (0,*) 'Bad value of parameter ',trim(name),': ',trim(arg)
      write (0,*) ' (attempt to read as double precision) '
      call exit(1)
    endif
  else
    value = defvalue
  endif
  return
end subroutine get_pv_default_d

      
