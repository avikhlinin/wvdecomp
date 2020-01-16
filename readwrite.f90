subroutine write_r8(data,ndata,ifile)
  integer ndata,ifile
  real*8 data(ndata)
      
  write(ifile)data
  return
end subroutine write_r8

subroutine write_r4(data,ndata,ifile)
  integer ndata,ifile
  real data(ndata)
  
  write(ifile)data
  return
end subroutine write_r4
      
subroutine write_i4(data,ndata,ifile)
  integer ndata,ifile
  integer data(ndata)
  
  write(ifile)data
  return
end subroutine write_i4

subroutine write_i2(data,ndata,ifile)
  integer ndata,ifile
  integer*2 data(ndata)
  
  write(ifile)data
  return
end subroutine write_i2

subroutine write_i1(data,ndata,ifile)
  integer ndata,ifile
  integer(kind=1) :: data(ndata)
  
  write(ifile)data
  return
end subroutine write_i1

subroutine write_c1(data,ndata,ifile)
  integer ndata,ifile
  integer(kind=1) :: data(ndata)
  
  write(ifile)data
  return
end subroutine write_c1

subroutine read_c1(data,ndata,ifile)
  integer ndata,ifile
  
  integer(kind=1) :: data(ndata)
  
  read(ifile)data
  return
end subroutine read_c1

subroutine read_i1(data,ndata,ifile)
  integer ndata,ifile
  integer(kind=1) :: data(ndata)
      
  read(ifile)data
  return
end subroutine read_i1
      
subroutine read_i2(data,ndata,ifile)
  integer ndata,ifile
  integer*2 data(ndata)
  
  read(ifile)data
  return
end subroutine read_i2
      
subroutine read_i4(data,ndata,ifile)
  integer ndata,ifile
  integer data(ndata)
  
  read(ifile)data
  return
end subroutine read_i4

subroutine read_r8(data,ndata,ifile)
  integer ndata,ifile
  real*8 data(ndata)
      
  read(ifile)data
  return
end subroutine read_r8

subroutine read_r4(data,ndata,ifile)
  integer ndata,ifile
  real data(ndata)
  
  read(ifile)data
  return
end subroutine read_r4
