      subroutine create_fits_image (
     ~    filename,
     ~    nx,ny,
     ~    datatype,
     ~    bscale,bzero,
     ~    headerfrom, unitfrom,
     ~    ekeys,
     ~    unit,
     ~    status)
c
c PURPOSE: 
c  1) to create a fits image,
c  2) optionally copy keywords from an existing FITS image
c  3) optionally delete/add/modify keywords
c
c  and return the fitsio file unit and status
c
c
c FEATURES:
c  File with the header is specified by its name or name and fitsio unit.
c
c  If filename and headerfrom are identical, open headerfrom and modify its
c  keywords
c
c
c
      implicit none
      character filename*(*), datatype*(*), headerfrom*(*), ekeys*(*)
      integer nx,ny,unitfrom, unit, status
      double precision bscale, bzero

      character path_o*200, path_h*200
      logical user_wants_clobber
      integer bitpix,blocksize,naxis,naxes(10),pcount,gcount,status1
      logical simple, extend
      integer unitheader

! A) Define the structure of the output file
      naxis=2
      pcount=0
      gcount=1
      naxes(1)=nx
      naxes(2)=ny
      simple=.true.
      extend=.false.

      if (datatype(1:1).eq.'e'.or.datatype(1:1).eq.'E') then
        bitpix=-32
      else if (datatype(1:1).eq.'i'.or.datatype(1:1).eq.'I') then
        bitpix=16
      else if (datatype(1:1).eq.'j'.or.datatype(1:1).eq.'J') then
        bitpix=32
      else if (datatype(1:1).eq.'b'.or.datatype(1:1).eq.'B') then
        bitpix=8
      else if (datatype(1:1).eq.'d'.or.datatype(1:1).eq.'D') then
        bitpix=-64
      endif



! B) Find absolute path to output file
      if (filename.ne.'-') then
        call get_absolute_path (filename,path_o)
      else
        path_o='-'
      endif

! C) If headerfrom is specified, find its absolute path
      if (headerfrom.eq.' ') then
        path_h=' '
      else if (headerfrom.eq.'-') then
        path_h='-'
      else
        call get_absolute_path (headerfrom,path_h)
      endif

! D) If path_o and path_h are different, or path_h is stdin, we unlink filename
!    and open a new file.

      if ( (path_o.ne.path_h) .or. (path_h.eq.'-') ) then
        if (user_wants_clobber()) then
          if (path_o.ne.'-') call unlink(filename)
        endif

        ! a) Allocate FITSIO unit
        call ftgiou (unit,status)

        path_o = filename
        ! b) If the header file is not opened, use headername as a template
        if (unitfrom.le.0) then
          if (headerfrom.ne.' ') then ! but if headerfrom is empty, ignore it
*            call strcat (path_o,'(')
*            call strcat (path_o,headerfrom)
*            call strcat (path_o,')')
            call ftgiou (unitheader,status)
            call ftnopn (unitheader,headerfrom,0,status)
          else
            unitheader = -1
          endif
        else
          unitheader=unitfrom
        endif

        ! c) Ini. output file
        call ftinit(unit,path_o,0,status)
        if (status.ne.0) return
        

        ! d) Put image size etc
        call ftphps (unit, bitpix,naxis,naxes, status)
*        call ftphpr(unit,simple,bitpix,naxis,naxes,pcount,gcount,extend
*     ~      ,status)

        ! e) If header file was opened we did not use it as a template;
        !    instead, we need to copy the keywords manually
        if (unitheader.gt.0) then
          call xcopyscale(unitheader,unit,status)
          if (unitfrom.lt.0) then ! we need to close template file
            call ftclos (unitheader,status)
          endif
        endif
        
! E) If output and headerfile are the same, open headerfile and change its size
!    But the headerfile should first be closed because it may have been opened
!    in the readonly mode
      else
        if (unitfrom.gt.0) then
          status1 = 0
          call ftclos(unitfrom,status1)
          unit=unitfrom
        else                    ! we need to allocate the unit
          call ftgiou (unit,status)
        endif
        
        call ftopen(unit,filename,1,blocksize,status)
        call resize_image (unit,datatype,nx,ny,status)
        
      endif
          

      call ftpdef(unit,bitpix,naxis,naxes,pcount,gcount,status)

! D) Define scaling. If scaling is set, modify/add BSCALE, BZERO and se scaling
      call ftpscl(unit,bscale,bzero,status)
      if ( (bscale.ne.1.0d0) .or. (abs(bzero).gt.1.0d-50) ) then
        call ftukyd(unit,'BSCALE',bscale,10,'&',status)
        call ftukyd(unit,'BZERO',bzero,10,'&',status)
      else                      ! remove scaling keywords if they existed
        if (status.ne.0) return
        call ftdkey(unit,'BSCALE',status)
        status=0
        call ftdkey(unit,'BZERO',status)
        status=0
      endif

      return
      end





