      subroutine open_fits_image (
     ~    filename,
     ~    mode,
     ~    unit,
     ~    nx,ny,
     ~    bitpix,
     ~    status
     ~    )
      implicit none
      character filename*(*), mode*(*)
      integer unit,nx,ny,bitpix,status
      integer rwmode

      integer naxis,pcount,gcount
      integer naxes(10)
      integer lnblnk
      logical simple, extend
      integer i
      logical twodimage
      
      call ftgiou (unit,status)
      if (mode.eq.'rw'.or.mode.eq.'w') then
        rwmode=1
      else
        rwmode=0
      endif
      
*      call ftopen (unit,filename,rwmode,blocksize,status)
      call ftnopn (unit,filename,rwmode,status)
      call ftghpr (unit,10,simple,bitpix,naxis,naxes,pcount,gcount,extend
     ~    ,status)
      if (status.eq.0) then
        call check_naxis(naxis,naxes,nx,ny,twodimage)
        if (.not.twodimage) then
          write(0,*) filename(1:lnblnk(filename)),': not a 2-D image'
          write(0,*)(naxes(i),i=1,min(naxis,10))
          call exit(1)
        endif
*        nx = naxes(1)
*        ny = naxes(2)
      endif

      return
      end


      subroutine op_fits_img (filename,unit,nx,ny)
      implicit none
      character filename*(*)
      integer unit
      integer nx,ny
      integer status,bitpix


      status=0
      call open_fits_image (
     ~    filename,
     ~    'r',
     ~    unit,
     ~    nx,ny,
     ~    bitpix,
     ~    status
     ~    )
      if (status.ne.0) then
        call perror_fitsio (filename,status)
        call exit(1)
      endif

      return
      end
