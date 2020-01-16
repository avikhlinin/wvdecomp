program WVDECOMP
  implicit none
  integer iargc
  character(len=200) :: imgname, arg, bgtosubtract, exptodiv, bgforthresholds
  integer :: nx,ny,unit, unitrestore
  integer :: scalemin,scalemax, niter(100)
  integer :: mainsize, worksize, savesize
  integer :: pcj1,poimg,pnimg,pcond,pw,pclass,pbg, pbgthresh, nrec, pexp
  integer :: nn
  logical :: poisson, subtractbg, qsavehist, qrestore, divbyexp, threshbg
  character(len=20), dimension(300) :: shdescriptor(300)
  logical, external :: yespar, defined
  integer :: w1,w2

  real, allocatable, dimension(:) :: img, outimg, work
  integer(kind=1), allocatable, dimension(:) :: savehist

  integer :: tmode,tminmode
  character(len=80), dimension(100) :: word
  integer :: nwords
  integer :: j
  integer, external :: newunit

  if (iargc().lt.2) call zhhelp ('wvdecomp')

!c Input image
!c     call getarg(1,imgname)
  call set_default_par ('img','${ARGV[1]}')
  call get_command_line_par ('img',1,imgname)
  if (.not.defined(imgname)) then
    call zhhelp ('wvdecomp')
  endif

  call op_fits_img (imgname,unit,nx,ny)

!c Min/Max scale
  scalemin=1
  scalemax=6
  call get_command_line_par ('smin',1,arg)
  if (defined(arg)) read(arg,*) scalemin
  call get_command_line_par ('smax',1,arg)
  if (defined(arg)) read(arg,*) scalemax
  call get_command_line_par ('scalemin',1,arg)
  if (defined(arg)) read(arg,*) scalemin
  call get_command_line_par ('scalemax',1,arg)
  if (defined(arg)) read(arg,*) scalemax

  mainsize = nx*ny*max((scalemax+1),4)

!c number of iterations
  call get_command_line_par ('iter',1,arg)
  if (defined(arg)) then
    call replace_characters (arg,',',' ')
    call splitwords (arg,word,nwords)
    if (nwords.eq.1) then
      read(arg,*) niter(1)
      do j=2,scalemax
        niter(j)=niter(1)
      enddo
    else if (nwords.eq.scalemax-scalemin+1) then
      do j=scalemin,scalemax
        read (word(j-scalemin+1),*) niter(j)
      enddo
      do j=1,scalemin-1
        niter(j)=niter(scalemin)
      enddo
    else
      write(0,*) 'You must set either one or scalemax-scalemin+1 iters'
      call exit(1)
    endif
  else
    do j=1,scalemax
      niter(j)=1
    enddo
  endif

!c Statistics
  poisson = .true.
  call get_command_line_par ('stat',1,arg)
  if (defined(arg)) then
    if (arg(1:1).ne.'p'.and.arg(1:1).ne.'P') then
      poisson=.false.
      print*,'use Gaussian statistics'
    endif
  endif

!c Error mode
  tmode=1                   ! sigma-signifcances by default
  tminmode=1                ! sigma-signifcances by default
  call get_command_line_par ('tmode',1,arg)
  if (defined(arg)) then
    if (arg.eq.'nfalse') then
      tmode = 2
      tminmode = 2
    endif
  endif
  call get_command_line_par ('tminmode',1,arg)
  if (defined(arg)) then
    if (arg.eq.'nfalse') then
      tminmode = 2
    else if (arg(1:3).eq.'sig') then
      tminmode = 1
    endif
  endif
      

!c . check if the user wants to subtract BG
  call get_command_line_par('bg',1,arg)
  if (defined(arg)) then
    bgtosubtract=arg
    if (poisson) then
      subtractbg=.true.
    else
      subtractbg=.false.
    endif
  else
    bgtosubtract='none'
  endif

!c . check if he wants to supply a background for threshold calculations
  call get_command_line_par ('bgthresh',1,arg)
  if (defined(arg)) then
    bgforthresholds=arg
    if (poisson) then
      threshbg = .true.
    else
      threshbg = .false.
    endif
  else
    threshbg = .false.
    bgforthresholds='none'
  endif

!c . check if the user wants to corect by exposure
  call get_command_line_par('exp',1,arg)
  if (defined(arg)) then
    exptodiv=arg
    divbyexp=.true.
  else
    exptodiv='none'
    divbyexp=.false.
  endif

 
!c /// In restoration mode?      
  call get_command_line_par ('restore',1,arg)
  if (defined(arg)) then
    unitrestore = newunit()
    qrestore=.true.
    open(unitrestore,file=arg,form='unformatted',status='old')
    read(unitrestore) shdescriptor
    read(shdescriptor(1),*)w1,w2,nrec
    if (w1.ne.nx.or.w2.ne.ny) then
      write(0,*)'wrong dimensions in the save file'
      call exit(1)
    endif
    
    savesize = nx*ny*nrec
  else
    nrec = 1
    qrestore=.false.
    savesize = 1
  endif

!c check if we want to save history
  if (.not.qrestore ) then
    if (yespar('save')) then
      qsavehist=.true.
      savesize = 0
      do j=scalemin,scalemax
        savesize=savesize+nx*ny*niter(j)
      enddo
    else
      qsavehist=.false.
      savesize=1
    endif
  endif



  nn=0
      
  pcj1=nn+1
  nn=nn+nx*ny
      
  poimg=nn+1
  nn=nn+nx*ny
      
  pnimg=nn+1
  call get_command_line_par ('errimg',1,arg)
  if (defined(arg)) then
    nn=nn+nx*ny*2
  else
    nn=nn+nx*ny
  endif
  if (tmode.ne.tminmode) then
    nn=nn+nx*ny             ! we need additional storage for nimg
  endif
  
  pcond=nn+1
  nn=nn+(nx*ny/4+5)         ! cond image will be 1-byte integer
  
  pclass=nn+1
  nn=nn+nx*ny
      
      
  pw=nn+1
!!!!! no work space is needed any more in decomp mode
  if (qrestore) nn=nn+nx*ny
      
  if (subtractbg) then
    pbg=nn+1
    nn=nn+nx*ny
  else
    pbg=pw
  endif

  if (threshbg) then
    pbgthresh=nn+1
    nn=nn+nx*ny
  else
    pbgthresh=pw
  endif

  if (divbyexp) then
    pexp=nn+1
    nn=nn+nx*ny
  else
    pexp=pw
  endif


  worksize = nn

  allocate(img(nx*ny),outimg(nx*ny),work(worksize))
  allocate(savehist(savesize))

  call do_wvdecomp (imgname,unit,nx,ny,scalemin,scalemax,niter,poisson,&
       bgtosubtract,subtractbg,bgforthresholds,threshbg,exptodiv,divbyexp,&
       qrestore,qsavehist,unitrestore,shdescriptor,mainsize, savesize,&
       worksize,pcj1,poimg,pnimg,pcond,pw,pclass,pbg, pbgthresh,pexp, nrec,&
       tmode,tminmode,img, outimg, work,savehist)
      
  call exit(0)
end program WVDECOMP

!*xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

subroutine do_wvdecomp &
     (imgname,unit,nx,ny,scalemin,scalemax,niter,poisson,&
     bgtosubtract,subtractbg,bgforthresholds,threshbg,exptodiv,divbyexp,&
     qrestore,qsavehist,unitrestore,shdescriptor,mainsize, savesize,&
     worksize,pcj1,poimg,pnimg,pcond,pw,pclass,pbg,pbgthresh,pexp,nrec,&
     tmode,tminmode, img, outimg,work, savehist)
!* . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
  implicit none
  character(len=*) :: imgname, bgtosubtract, exptodiv, bgforthresholds
  integer :: unit,nx,ny,unitrestore
  integer :: scalemin,scalemax,niter(*)
  logical :: poisson,subtractbg,qrestore,qsavehist, divbyexp, threshbg
  integer :: mainsize, savesize, worksize
  integer :: pcj1,poimg,pnimg,pcond,pw,pclass,pbg,pbgthresh, nrec, pexp,&
       tmode, tminmode
  character(len=20), dimension(300) :: shdescriptor

!* . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
      
  real, dimension(nx*ny) :: img, outimg
  real, dimension(worksize) :: work
  integer(kind=1), dimension(savesize) :: savehist
      
!c / savehistory map and descriptor
  logical :: qsavenoise
  integer :: i
  logical :: qnoise
  integer status
  character(len=80) :: arg
      
      
  character(len=200) :: outname
  character(len=20) :: prefix
      
  real, dimension(100) :: threshold,thresholdmin,rwvmax
      
  integer :: kernel
  integer :: j
      
  
  logical :: detectnegative,considershape
      
  logical, external :: defined, yes, no, none, yespar
      
  character(len=80), dimension(100) :: word
  integer nwords

!c--------------------------------------------------------------

!!c     call getarg(2,outname)
  call set_default_par ('out','${ARGV[2]}')
  call get_command_line_par ('out',1,outname)
  if (.not.defined(outname)) then
    call zhhelp ('wvdecomp')
  endif
      
  threshold(1)=-1
  thresholdmin(1)=-1
       
  call get_command_line_par ('threshold',1,arg)
  if (.not.defined(arg)) call get_command_line_par ('t',1,arg)
  if (defined(arg)) then
    call replace_characters (arg,',',' ')
    call splitwords (arg,word,nwords)
    if (nwords.eq.1) then
      read(arg,*) threshold(1)
      do j=2,scalemax
        threshold(j)=threshold(1)
      enddo
    else if (nwords.eq.scalemax-scalemin+1) then
      do j=scalemin,scalemax
        read (word(j-scalemin+1),*) threshold(j)
      enddo
      do j=1,scalemin-1
        threshold(j)=threshold(scalemin)
      enddo
    else
      write(0,*) 'You must set either one or scalemax-scalemin+1 thresholds'
      call exit(1)
    endif
  endif
 
      
  call get_command_line_par ('thresholdmin',1,arg)
  if (.not.defined(arg)) call get_command_line_par ('tmin',1,arg)
  if (defined(arg)) then
    call replace_characters (arg,',',' ')
    call splitwords (arg,word,nwords)
    if (nwords.eq.1) then
      read(arg,*) thresholdmin(1)
      do j=2,scalemax
        thresholdmin(j)=thresholdmin(1)
      enddo
    else if (nwords.eq.scalemax-scalemin+1) then
      do j=scalemin,scalemax
        read (word(j-scalemin+1),*) thresholdmin(j)
      enddo
      do j=1,scalemin-1
        thresholdmin(j)=thresholdmin(scalemin)
      enddo
    else
      write(0,*) 'You must set either one or scalemax-scalemin+1 thresholdmins'
      call exit(1)
    endif
  endif

  if (threshold(1).lt.0.0) then
    do j=1,scalemax
      threshold(j)=3.0
    enddo
  endif
      
  if (thresholdmin(1).lt.0.0) then
    do j=1,scalemax
      thresholdmin(j)=3.0
    enddo
  endif
  
      

 
  rwvmax(1)=-1
  call get_command_line_par ('rwvmax',1,arg)
  if (defined(arg)) then
    call replace_characters (arg,',',' ')
    call splitwords (arg,word,nwords)
    if (nwords.eq.1) then
      read(arg,*) rwvmax(1)
      do j=2,scalemax
        rwvmax(j)=rwvmax(1)
      enddo
    else if (nwords.eq.scalemax-scalemin+1) then
      do j=scalemin,scalemax
        read (word(j-scalemin+1),*) rwvmax(j)
      enddo
      do j=1,scalemin-1
        rwvmax(j)=rwvmax(scalemin)
      enddo
    else
      write(0,*) 'You must set either one or scalemax-scalemin+1 rwvmax'
      call exit(1)
    endif
  endif
  if (rwvmax(1).lt.0.0) then
    do j=1,scalemax
      rwvmax(j)=-1
    enddo
  endif


  call get_command_line_par ('kernel',1,arg)
  if (arg(1:5).eq.'gauss') then
    kernel = 2
  else
    kernel = 1              ! A trous kernel by default
  endif
 
  qsavenoise = yespar('savenoise')
      
  call get_command_line_par ('detectnegative',1,arg)
  if (yes(arg)) then
    detectnegative=.true.
    considershape=.true.
  else
    detectnegative=.false.
    considershape=.false.
  endif
      
  call get_command_line_par ('considershape',1,arg)
  if (yes(arg)) then
    considershape=.true.
  else if (no(arg)) then
    considershape=.false.
  endif
  
!c Read input image
  call read_fits_image_unit (unit,imgname,img,nx,ny,'e')
  status=0
  call ftclos(unit,status)
  if (status.ne.0) then
    call perror_fitsio (imgname,status)
    call exit(1)
  endif
      
!c . deal with BG subtraction
  if (.not.none(bgtosubtract)) then
    if (subtractbg) then
      call read_fits_image (bgtosubtract,work(pbg),nx,ny,'e')
    else
      call read_fits_image (bgtosubtract,outimg,nx,ny,'e')
      img=img-outimg
    endif
  endif

  if (threshbg) then
    call read_fits_image (bgforthresholds,work(pbgthresh),nx,ny,'e')
  endif

!c . Exposure
  if (divbyexp) then
    call read_fits_image (exptodiv,work(pexp),nx,ny,'e')
  endif

!c /// In restoration mode?
  if (qrestore) then
!c .    determine if the user wants to calculate errors
    qnoise = yespar ('restorenoise')
    do i=1,nrec
      call read_i1 (savehist((i-1)*nx*ny+1),nx*ny,unitrestore)
    enddo
    close(unitrestore)
    print*,'Restore information red in'
  endif
      
  if (qrestore) then
    call restore_decomp (img,outimg,nx,ny,work(poimg),work(pw),work(pcj1),&
         scalemin,scalemax,savehist,shdescriptor,qnoise,work(pnimg),&
         outname)
  else
    call dodecomp (kernel,img,outimg,work(pcj1),work(poimg),&
         work(pnimg),work(pcond),work(pclass),work(pbg),work(pbgthresh),&
         work(pexp),nx,ny,scalemin,scalemax,threshold,thresholdmin,poisson,&
         niter,tmode,tminmode,detectnegative,subtractbg,divbyexp,threshbg,&
         considershape,qsavehist,qsavenoise,savehist,shdescriptor,outname,&
         rwvmax)
  endif
      
      
  if (qsavehist) then
    prefix='.save'
    open(1,file=trim(outname)//prefix,form='unformatted')
    write(1)shdescriptor
    read (shdescriptor(1),*) i,j,nrec
    !*        print*,shdescriptor
    do i=1,nrec
      call write_c1 (savehist((i-1)*nx*ny+1),nx*ny,1)
    enddo
    close(1)
  endif
      
      
  call exit(0)
end subroutine do_wvdecomp








