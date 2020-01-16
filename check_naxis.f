      subroutine check_naxis (naxis,naxes,nx,ny,twodimage)
c
c This program parses the naxes array two determine whether the
c image is 2d and what are nx and ny
c
      implicit none
      integer naxis
      integer naxes(naxis)
      integer nx,ny
      logical twodimage

      integer i, ngt1, igt1,jgt1


      if (naxis.eq.2) then
        nx=naxes(1)
        ny=naxes(2)
        twodimage = .true.

      else
        ngt1 = 0
        igt1 = 0
        do i=1,naxis
          if (naxes(i).gt.1) then
            ngt1 = ngt1 + 1
            if (igt1.eq.0) then
              igt1 = i
            else
              jgt1 = i
            endif
          endif
        enddo
        
        if (ngt1.ne.2) then
          twodimage = .false.
        else
          nx = naxes(igt1)
          ny = naxes(jgt1)
          twodimage = .true.
        endif
      endif
      
      return
      end
