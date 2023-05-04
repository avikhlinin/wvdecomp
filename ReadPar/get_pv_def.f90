subroutine get_pv_default (name,value,defvalue,type)
  implicit none
  character(len=*) :: name, type
  integer :: value, defvalue
  
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
end subroutine get_pv_default
