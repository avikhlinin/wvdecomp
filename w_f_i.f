      subroutine write_fits_image (outfile,rimg,nx,ny,imgtype,outtype)
      implicit none
      character outfile*(*)
      integer nx,ny
      real rimg(nx,ny)
      character outtype*(*),imgtype*(*)
      integer ounit
      integer status

      status = 0
      call create_fits_image (
     ~    outfile,
     ~    nx,ny,
     ~    outtype,
     ~    1.0d0,0.0d0,
     ~    '', -1,
     ~    '',
     ~    ounit,
     ~    status)

      if      (imgtype(1:1).eq.'b'.or.imgtype(1:1).eq.'B') then
        call ftp2db(ounit,0,nx,nx,ny,rimg,status)
      else if (imgtype(1:1).eq.'i'.or.imgtype(1:1).eq.'I') then
        call ftp2di(ounit,0,nx,nx,ny,rimg,status)
      else if (imgtype(1:1).eq.'j'.or.imgtype(1:1).eq.'J') then
        call ftp2dj(ounit,0,nx,nx,ny,rimg,status)
      else if (imgtype(1:1).eq.'e'.or.imgtype(1:1).eq.'E') then
        call ftp2de(ounit,0,nx,nx,ny,rimg,status)
      else if (imgtype(1:1).eq.'d'.or.imgtype(1:1).eq.'D') then
        call ftp2dd(ounit,0,nx,nx,ny,rimg,status)
      else
        write(0,*)'wrong image type: ',imgtype
        call exit(1)
      endif

      call ftclos(ounit,status)
      call ftfiou(ounit,status)
      if (status.ne.0) then
        call perror_fitsio(outfile,status)
        call exit(1)
      endif
        

        
      return
      end
