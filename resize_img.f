      subroutine resize_image (ounit,type,nx,ny,status)
c
c Resize opened FITS image; calls ftrsim
c      
      implicit none 
      integer ounit,status
      character type*(*)
      integer nx,ny
      integer naxis,naxes(10), bitpix
      logical simple,extend
      integer bitpixo,pcount,gcount

      if (status .gt. 0)return


      if (type.eq.'d'.or.type.eq.'D') then
        bitpix=-64
      else if (type.eq.'e'.or.type.eq.'E') then
        bitpix=-32
      else if (type.eq.'j'.or.type.eq.'J') then
        bitpix=32
      else if (type.eq.'i'.or.type.eq.'I') then
        bitpix=16
      else if (type.eq.'b'.or.type.eq.'B') then
        bitpix=8
      endif

      call ftghpr (ounit,10,simple,bitpixo,naxis,naxes,pcount,gcount,extend
     ~    ,status)
      if (status.ne.0) return
      if (
     ~    (naxis.eq.2) .and. 
     ~    (naxes(1).eq.nx) .and. 
     ~    (naxes(2).eq.ny) .and.
     ~    (bitpix.eq.bitpixo) ) then
        return
      endif

      naxis=2
      naxes(1)=nx
      naxes(2)=ny

      call ftrsim(ounit,bitpix,naxis,naxes,status)

      return
      end
