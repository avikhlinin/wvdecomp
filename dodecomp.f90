!c kernel = 1   - a trous
!c kernel = 2   - gauss
!c
!c tmode = 1 - sigma-significance
!c tmode = 2 - nfalse-significance
!c
subroutine dodecomp &
     (kernel,img,outimg,cj1,oimg,nimg,cond,class,bg,bgthresh,exposure,&
     nx,ny,scalemin,scalemax,threshold,thresholdmin,poisson,niter,tmode,&
     tminmode,detectnegative,subtractbg,divbyexp,threshbg,considershape, &
     qsavehist,qsavenoise,savehist,shdescriptor,outkey,rwvmax)
  implicit none
  integer :: kernel
  integer :: nx,ny
  integer :: scalemin,scalemax
  integer, dimension(*) :: niter
  real, dimension(nx,ny) :: img, outimg
  real, dimension(*) :: threshold,thresholdmin
  integer(kind=1), dimension(nx*ny) :: cond
  integer, dimension(nx*ny) :: class
  real, dimension(*) :: bg, exposure, bgthresh, rwvmax
  logical :: poisson, qsavenoise
  character(len=*) :: outkey
      
  logical :: detectnegative,considershape,subtractbg,divbyexp,threshbg
      
  real, dimension(nx*ny) ::  cj1,oimg
  real, dimension(nx*ny,*) ::  nimg

  integer :: tmode,tminmode

  logical :: savescale (1000)
  integer :: isavescale (1000)
  character(len=200) :: arg
      
  real, dimension(100) :: sigmae
  real, dimension(7), parameter ::  sigmae0_atrous = &
       (/ 8.907963e-01, 2.006639e-01, 8.550748e-02, 4.121747e-02, &
       2.042496e-02, 1.018975e-02, 5.092043e-03 /)
      
  real, dimension(7), parameter :: sigmae0_gauss = &
       (/  0.912579, 0.125101, 0.104892, 4.97810e-02,&
       2.46556e-02, 1.14364e-02, 6.02736e-03 /)


  integer :: iter
  real :: sigmagauss
  integer :: j,i,ii,jj, jerror, jerrormin
      
  integer :: nclass
  integer, parameter :: nclassmax=5000000
  integer, dimension(nclassmax) :: idetect, xpeak, ypeak
  logical, external :: if_peak,if_min
  logical :: peak
  integer :: ndetect, npeak
      
  logical :: qerror
  character(len=200) :: errimg
  logical :: poisson0
  integer :: cx,cy
  real :: cmin,cmax
      
  real :: t,s, tminj
      
  logical :: qsavehist
  integer(kind=1), dimension(nx*ny,*) :: savehist

  character(len=20), dimension(300) :: shdescriptor
  integer :: irecsh
  logical :: quiet
  logical, external :: yespar, defined
  character(len=20) :: affix
  character (len=200) :: outname

  quiet = yespar ('quiet')

  poisson0=poisson

!c% Verify kernel
  if (kernel.ne.1.and.kernel.ne.2) then
    write (0,*) 'wrong kernel in dodecomp:', kernel
    call exit(1)
  endif

!c% Read which scales not to save
  call get_cl_par ('nosave',arg)
  if (defined(arg)) then
    do i=1,1000
      savescale(i)=.true.
      isavescale(i)=-1
    enddo
    read (arg,*,iostat=j) (isavescale(i),i=1,1000)
    do i=1,1000
      if (isavescale(i).gt.0) then
        savescale(isavescale(i))=.false.
      endif
    enddo
  else
    do i=1,1000
      savescale(i)=.true.
    enddo
  endif
  
!c% Prepare rms arrays for the required kernel
  do j=1,7
    if (kernel.eq.1) then
      sigmae(j)=sigmae0_atrous(j)
    else
      sigmae(j)=sigmae0_gauss(j)
    endif
  enddo
  
  do j=8,scalemax
    sigmae(j)=sigmae(j-1)/2.0
  enddo

!c% If errorimg is supplied, read it and reset the poisson flag
  call get_command_line_par ('errimg',1,errimg)
  if (defined(errimg)) then
    qerror=.true.
    call read_fits_image (errimg,nimg,nx,ny,'e')
    poisson0=.false.
  else
    qerror=.false.
  endif
      
!c% Initialize arrays before iterations
  call msarith (oimg,'=',0.0,nx*ny)
  if (.not.qerror) then 
!*        if (qsavenoise) then
!*          call msarith (nimg,'=',0.0,nx*ny*(scalemax))
!*        else ! we need only one plane of nimg
    call msarith (nimg,'=',0.0,nx*ny)
!*        endif
  endif
      
  irecsh=1

!c     d) loop over levels
        
  do j=scalemin,scalemax
    if (qerror) then ! calculate error image for this scale
      jerror = 2
!*          if (qsavenoise) then
!*            jerror = j
!*          else
!*            jerror = 2
      !*          endif
      call conv_equiv_gauss (nimg,outimg,nx,ny,j,j,sigmae,jerror)
    else             ! define the plane in errorimg to store noise on
                         ! this scale
      jerror = 1
    endif
    if (tmode.ne.tminmode) then
      jerrormin = jerror+1
    else
      jerrormin = jerror
    endif
    
    ! initialize the results on this scale
    call msarith (outimg,'=',0.0,nx*ny)

    do iter=1,niter(j)
      if (.not.quiet) then
        print '(a,i3,3x,a,i2,a,f3.1,a,f3.1)','Iter=',iter,'     scale=',j, &
             '   t=',threshold(j),'   tmin=',thresholdmin(j)
      endif
          
      call marith (cj1,'=',img,'-',oimg,nx*ny)
          
      if (j.gt.1.and.j.le.3) then
        if ((.not.poisson0).and.(.not.qerror)) then
          call mcopy (nimg(1,jerror),cj1,nx,ny)
          call conv_wavelet (kernel,nimg(1,jerror),nimg,class,nx,ny,1,.true.)
          call marith (nimg(1,jerror),'=',nimg(1,jerror),'-',class,nx*ny)
          call image_sigma (nimg(1,jerror),nx*ny,sigmagauss)
          sigmagauss=sigmagauss/sigmae(1)
          call msarith (nimg(1,jerror),'=',sigmagauss*sigmae(j),nx*ny)
        endif
      endif

      if (subtractbg) then
        call marith(cj1,'=',cj1,'-',bg,nx*ny)
      endif
      if (divbyexp) then
        do i=1,nx*ny
          if (exposure(i).gt.0.0) then
            cj1(i)=cj1(i)/exposure(i)
          else
            cj1(i)=0
          endif
        enddo
      endif
      call conv_wavelet (kernel,cj1,cj1,class,nx,ny,j,.true.)
      call marith (cj1,'=',cj1,'-',class,nx*ny)
      if (divbyexp) then
        call marith(class,'=',class,'*',exposure,nx*ny)
        call marith(cj1,'=',cj1,'*',exposure,nx*ny)
      endif
      ! if (subtractbg) then
      !   call marith(class,'=',class,'+',bg,nx*ny)
      ! endif
      
      if (j.eq.1) then
        if ((.not.poisson0).and.(.not.qerror)) then
          call image_sigma (cj1,nx*ny,sigmagauss)
          sigmagauss=sigmagauss/sigmae(1)
          call msarith (nimg(1,jerror),'=',sigmagauss*sigmae(j),nx*ny)
        endif
      endif


!c Store the error image and prepare the poisson one if iter=1          
      if (.not.qerror) then
        if (iter.eq.1) then
          ! if poisson statistics, prepare the error array
          if (poisson0) then
            
            if (threshbg) then
              call copy_real_arrays (class,bgthresh,nx*ny)
            endif
                
            s=1.0/sigmae(j)**2
            if (tmode.eq.1) then
              call calc_poisson_rms (nimg(1,jerror),class,nx*ny,s,sigmae(j))
            else
              call calc_poisson_nfalse_threshold &
                   (nimg(1,jerror),class,nx*ny,threshold(j),j,kernel)
            endif
            if (tminmode.ne.tmode) then
              if (tminmode.eq.1) then
                call calc_poisson_rms &
                     (nimg(1,jerrormin),class,nx*ny,s,sigmae(j))
              else
                call mcopy (nimg(1,jerrormin),nimg(1,jerror),nx,ny)
                call msarith (nimg(1,jerrormin),'*',threshold(j),nx*ny)
              endif
            endif
          endif
        endif
      endif
          
      if (j.gt.1) then

!c If rwvmax(j)>0, find significant peaks before doing thresholdmin
        if (rwvmax(j).gt.0.0) then
          npeak = 0
          do i=1,nx*ny
            if (tmode.eq.1) then
              t=threshold(j)*nimg(i,jerror)
            else
              t=nimg(i,jerror)
            endif
            peak=.false.
            if (cj1(i).gt.t) then
              jj=(i-1)/nx+1
              ii=i-(jj-1)*nx
              if (ii.gt.1.and.ii.lt.nx.and.jj.gt.1.and.jj.lt.ny) then
                peak=if_peak(cj1,nx,ny,ii,jj)
              endif
            else
              if (detectnegative) then
                if (cj1(i).lt.-t) then
                  jj=(i-1)/nx+1
                  ii=i-(jj-1)*nx
                  if (ii.gt.1.and.ii.lt.nx.and.jj.gt.1.and.jj.lt.ny) then
                    peak=if_min(cj1,nx,ny,ii,jj)
                  endif
                endif
              endif
            endif
                
            if (peak) then
              npeak = npeak + 1
              xpeak(npeak)=ii
              ypeak(npeak)=jj
            endif
          enddo
        endif
                


!c make a 'condition' image  

!******            t=sigmagauss*sigmae(j)*thresholdmin(j)
!c  s is the effective area of the kernel
        s=1.0/sigmae(j)**2
        tminj = thresholdmin(j)
        do i=1,nx*ny
          t=tminj*nimg(i,jerrormin)
          ! for tmode=2, the meaning of thresholdmin
          ! is the ratio of the threshold and the minimum
          ! rejection threshold.
          if (cj1(i).gt.t) then
            cond(i)=1
          else
            if (detectnegative) then
              if (cj1(i).lt.-t) then
                cond(i)=-1
              else
                cond(i)=0
              endif
            else
              cond(i)=0
            endif
          endif
        enddo
            
        if (rwvmax(j).gt.0.0) then
          call make_rwvmax_mask (class,nx,ny,xpeak,ypeak,npeak,rwvmax(j))
          do i=1,nx*ny
            cond(i)=cond(i)*class(i)
          enddo
        endif

        do i=1,nx*ny
          if (cond(i).ne.0) then
            class(i)=1
          else
            class(i)=0
          endif
        enddo


        call ini_flood_fill (class,nx,ny,1,4) ! use 4-connect
        nclass=0
!****            t=sigmagauss*threshold(j)*sigmae(j)
        s=1.0/sigmae(j)**2
        do ii=2,nx-1
          i = ii         ! corresponding to jj=1
          do jj=2,ny-1
            i = i+nx
            if (class(i).ge.0) then ! if not in island yet
              if (cond(i).ne.0) then ! and cond = true
                
                ! is it a local max/min?
                if (cond(i).lt.0) then 
                  peak=if_min(cj1,nx,ny,ii,jj)
                else
                  peak=if_peak(cj1,nx,ny,ii,jj)
                endif
                    
                ! if yes, is it above the threshold?
                if (peak) then
                      
                  if (tmode.eq.1) then
                    t=threshold(j)*nimg(i,jerror)
                  else
                    t=nimg(i,jerror)
                  endif
                  
                  if (detectnegative) then
                    if (abs(cj1(i)).gt.t) then
                      nclass=nclass+1
!*                          call classb (cond,class,nx,ny,cond(i),nclass,ii,jj)
                      call floodfill (ii,jj,-nclass,1)
                    endif
                  else
                    if (cj1(i).gt.t) then
                      nclass=nclass+1
!*                          call classb (cond,class,nx,ny,cond(i),nclass,ii,jj)
                      call floodfill (ii,jj,-nclass,1)
                    endif
                  endif
                endif
              endif
            endif
          enddo
        enddo

        do i=1,nx*ny
          if (class(i).lt.0) then
            class(i)=-class(i)
          else
            class(i)=0
          endif
        enddo

        if (nclass.gt.nclassmax) then
          write (0,*) 'ERROR: increase nclassmax to >',nclass,&
               ' in dodecomp'
          call exit(1)
        endif

        do i=1,nclass
          if (considershape) then
            call classshape (class,cj1,nx,ny,i,cx,cy,cmin,cmax)
            if (class(cx+(cy-1)*nx).eq.i) then
              idetect(i)=1
            else
              idetect(i)=0
            endif
          else
            idetect(i)=1
          endif
        enddo
        

!c leave only detected classes

        ndetect=0
        do i=1,nx*ny
          if (class(i).ne.0) then
            if (idetect(class(i)).eq.0) then
              cj1(i)=0.0
            else
              ndetect=ndetect+1
            endif
          else
            cj1(i)=0.0
          endif
        enddo
            
        if (qsavehist) then
          write(shdescriptor(irecsh+1),'(i3,1x,i3)')j,iter
          do i=1,nx*ny
            if (class(i).gt.0) then
              if (idetect(class(i)).eq.0) then
                savehist(i,irecsh)=0
              else
                savehist(i,irecsh)=1
              endif
            else
              savehist(i,irecsh)=0
            endif
          enddo
          irecsh=irecsh+1
        endif
                
            
      else

        if (qsavehist) then
          write(shdescriptor(irecsh+1),'(i3,1x,i3)')j,iter
        endif


!*            t=sigmagauss*threshold(j)*sigmae(j)
        s=1.0/sigmae(j)**2
        ndetect=0
        do i=1,nx*ny
          if (tmode.eq.1) then
            t=threshold(j)*nimg(i,jerror)
          else
            t=nimg(i,jerror)
          endif


          if (cj1(i).lt.t) then
            cj1(i)=0.0
          else
            ndetect=ndetect+1
          endif
              
          if (qsavehist) then
            if (cj1(i).gt.0) then
              savehist(i,irecsh)=1
            else
              savehist(i,irecsh)=0
            endif
          endif

        enddo
            
        if (qsavehist) then
          irecsh=irecsh+1
        endif

      endif

      call marith (oimg,'=',oimg,'+',cj1,nx*ny)
      call marith (outimg,'=',outimg,'+',cj1,nx*ny)
      if (ndetect.eq.0) goto 100
    enddo
100 continue
        
!c       Iterations on this scale are over, save results
    if (savescale(j)) then
      affix=' '
      write (affix,'(i2)') j
      call rmblanks(affix)
      outname = outkey
      call strcat (outname,'.')
      call strcat (outname,affix)
      call write_fits_image (outname,outimg,nx,ny,'e','e')
      
      if (qsavenoise) then
        outname = outkey
        call strcat (outname,'.noise.')
        call strcat (outname,affix)
        call write_fits_image (outname,nimg(1,jerror),nx,ny,'e','e')
      endif
    endif

  enddo
      
  ! Write largest scale
  j = scalemax
!*      call marith (class,'=',img,'-',oimg,nx*ny)
!*      call marith (cj1,'=',class,'+',cj1,nx*ny) 
!*                                ! add the result of last iteration
  call marith (cj1,'=',img,'-',oimg,nx*ny)
  if (subtractbg) call marith(cj1,'=',cj1,'-',bg,nx*ny)
  if (divbyexp) then
    do i=1,nx*ny
      if (exposure(i).gt.0.0) then
        cj1(i)=cj1(i)/exposure(i)
      else
        cj1(i)=0
      endif
    enddo
  endif
  call conv_wavelet (kernel,cj1,cj1,class,nx,ny,j,.true.)
  if (divbyexp) call marith(class,'=',class,'*',exposure,nx*ny)
  if (subtractbg) call marith(class,'=',class,'+',bg,nx*ny)

  j = scalemax + 1
  if (savescale(j)) then
    affix=' '
    write (affix,'(i2)')j
    call rmblanks(affix)
    outname = outkey
    call strcat (outname,'.')
    call strcat (outname,affix)
    call write_fits_image (outname,class,nx,ny,'e','e')
  endif

  ! Write the sum of everything detected
  call write_fits_image (outkey,oimg,nx,ny,'e','e')

      

  if (qsavehist) then
    write(shdescriptor(1),'(i5,1x,i5,1x,i4,1x,i1)')nx,ny,irecsh-1,kernel
  endif
      
  return
end subroutine dodecomp
      








