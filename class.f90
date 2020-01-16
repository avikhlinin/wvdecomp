subroutine classj (img,classimg,nx,ny,classval,nclass,i,j)
  implicit none
  integer :: nx,ny
  integer, dimension(nx,ny) :: img, classimg
  integer :: classval
  integer :: nclass
      
  integer :: i,j,ii,jj,iclass,i1,i2,j1,j2

  logical :: qstep
  integer :: step
  integer :: nx1,ny1
      
  iclass=nclass
  classimg(i,j)=iclass
      
  step=1
  qstep=.true.
      
  nx1=nx-1
  ny1=ny-1
  
  do
    if(.not.qstep) exit
        
    qstep=.false.
    i1=max(i-step,2)
    i2=min(i+step,nx1)
    j1=max(j-step,2)
    j2=min(j+step,ny1)
        
    do ii=i1,i2
      do jj=j1,j2
        if (classimg(ii,jj).eq.0) then
          if (img(ii,jj).eq.classval) then
            if ( &
                 classimg(ii,jj-1).eq.iclass .or. &
                 classimg(ii-1,jj).eq.iclass .or. &
                 classimg(ii+1,jj).eq.iclass .or. &
                 classimg(ii,jj+1).eq.iclass) then
              qstep=.true.
              classimg(ii,jj)=iclass
            endif
          endif
        endif
      enddo
    enddo

    step=step+1
        
  enddo
      
          
  return
end subroutine classj
      
!*xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

subroutine classb (img,classimg,nx,ny,classval,nclass,i,j)
  implicit none
  integer nx,ny
  integer(kind=1), dimension(nx,ny) :: img
  integer(kind=1) :: classval
  integer, dimension(nx,ny) :: classimg
  integer nclass
      
  integer i,j,ii,jj,iclass,i1,i2,j1,j2

  logical qstep
  integer step
  integer nx1,ny1
      
  iclass=nclass
  classimg(i,j)=iclass
      
  step=1
  qstep=.true.
  
  nx1=nx-1
  ny1=ny-1

  do
    if (.not.qstep) exit
        
    qstep=.false.
    i1=max(i-step,2)
    i2=min(i+step,nx1)
    j1=max(j-step,2)
    j2=min(j+step,ny1)
        
    do ii=i1,i2
      do jj=j1,j2
        if (classimg(ii,jj).eq.0) then
          if (img(ii,jj).eq.classval) then
            if ( &
                 classimg(ii,jj-1).eq.iclass .or. &
                 classimg(ii-1,jj).eq.iclass .or. &
                 classimg(ii+1,jj).eq.iclass .or. &
                 classimg(ii,jj+1).eq.iclass) then
              qstep=.true.
              classimg(ii,jj)=iclass
            endif
          endif
        endif
      enddo
    enddo

    step=step+1
    
  enddo
      
          
  return
end subroutine classb
      
