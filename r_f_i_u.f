c Assumes that the fits file iis already opened and positioned to the desired
c image extenstion. The iunit indicates an input stream and filename is used
c only for error messages. Input file is not closed
c
      subroutine read_fits_image_unit (iunit,filename,img,nx,ny,imgtype)
      implicit none
      character filename*(*)
      integer img(*)
      integer nx,ny
      character imgtype*(*)
      character nullb*1
      integer*2 nulli
      integer nullj
      real nulle
      double precision nulld
      
      
      integer iunit
      integer status
      integer lnblnk
      logical anyf
      
      status=0

      if      (imgtype(1:1).eq.'b'.or.imgtype(1:1).eq.'B') then
        nullb=char(0)
        call ftg2db(iunit,0,nullb,nx,nx,ny,img,anyf,status)
      else if (imgtype(1:1).eq.'i'.or.imgtype(1:1).eq.'I') then
        nulli=0
        call ftg2di(iunit,0,nulli,nx,nx,ny,img,anyf,status)
      else if (imgtype(1:1).eq.'j'.or.imgtype(1:1).eq.'J') then
        nullj=0
        call ftg2dj(iunit,0,nullj,nx,nx,ny,img,anyf,status)
      else if (imgtype(1:1).eq.'e'.or.imgtype(1:1).eq.'E') then
        nulle=0
        call ftg2de(iunit,0,nulle,nx,nx,ny,img,anyf,status)
      else if (imgtype(1:1).eq.'d'.or.imgtype(1:1).eq.'D') then
        nulld=0
        call ftg2dd(iunit,0,nulld,nx,nx,ny,img,anyf,status)
      else
        write(0,*)'wrong image type: ',imgtype(1:lnblnk(imgtype))
     ~      ,' in read_fits_image'
        call exit(1)
      endif
      
      if (status.ne.0) then
        call perror_fitsio (filename,status)
        call exit(1)
      endif
      
      
      return
      end
      









