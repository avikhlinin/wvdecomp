subroutine restore_decomp (img,outimg,nx,ny,oimg,work,wksp,&
     scalemin,scalemax,savehist,shdescriptor,qnoise,nimg,outkey)
  implicit none
  integer :: nx,ny
  real, dimension(nx*ny) :: img, outimg, wksp
  integer :: scalemin,scalemax

  integer (kind=1), dimension(nx*ny,*) :: savehist
  
  logical :: qnoise
  real, dimension(nx*ny,*) :: nimg

  character(len=20), dimension(300) :: shdescriptor
      
  integer :: nrec
  real, dimension(nx*ny) :: oimg,work
      
  integer :: w1,w2
  integer :: irec,iscale,iiter,i, iscaleold
      
  integer :: kernel
  character (len=*) :: outkey
  character (len=20) :: affix
  character (len=200) :: outname
      
!c ///      
  read(shdescriptor(1),*)w1,w2,nrec, kernel
      
  call msarith (oimg,'=',0.0,nx*ny)
!c /// 
  iscaleold = -1
  do irec=1,nrec
    read (shdescriptor(irec+1),*) iscale,iiter
    if (iscale.ne.iscaleold.and. &
         (iscale.ge.scalemin.and.iscale.le.scalemax)) then
      if (iscaleold.gt.0) then
        affix=' '
        write (affix,'(i2)') iscaleold
        call rmblanks(affix)
        outname = outkey
        call strcat (outname,'.')
        call strcat (outname,affix)
        call write_fits_image (outname,outimg,nx,ny,'e','e')
      endif
      iscaleold=iscale
      call msarith (outimg,'=',0.0,nx*ny)
    endif
            
    print*,'Restoring: ',iscale,iiter
    call marith (work,'=',img,'-',oimg,nx*ny)
        
    call conv_wavelet (kernel,work,work,wksp,nx,ny,iscale,.true.)
    call marith (work,"=",work,"-",wksp,nx*ny)
        
    do i=1,nx*ny
      if (savehist(i,irec).eq.0) work(i)=0.0
    enddo
        
    call marith (oimg,'=',oimg,'+',work,nx*ny)
        
    if (iscale.ge.scalemin.and.iscale.le.scalemax) then
      call marith (outimg,'=',outimg,'+',work,nx*ny)
    endif
        
  enddo

  if (iscaleold.gt.0) then
    affix=' '
    write (affix,'(i2)') iscaleold
    call rmblanks(affix)
    outname = outkey
    call strcat (outname,'.')
    call strcat (outname,affix)
    call write_fits_image (outname,outimg,nx,ny,'e','e')
  endif
      


  return
end subroutine restore_decomp
