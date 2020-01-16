      function if_peak (img,nx,ny,i0,j0)
      implicit none
      logical if_peak
      integer nx,ny,i0,j0
      real img(nx,ny),im0

      integer i1,i2,j1,j2
      i1 = max(i0-1,1)
      i2 = min(i0+1,nx)
      j1 = max(j0-1,1)
      j2 = min(j0+1,ny)
      
      im0=img(i0,j0)

      
      if_peak= 
     ~     (im0.ge.img(i2,j0)) .and.
     ~     (im0.ge.img(i1,j0)) .and.
     ~     (im0.ge.img(i0,j2)) .and.
     ~     (im0.ge.img(i0+1,j2)) .and.
     ~     (im0.ge.img(i1,j2)) .and.
     ~     (im0.ge.img(i1,j1)) .and.
     ~     (im0.ge.img(i2,j1)) .and.
     ~     (im0.ge.img(i0,j1))

         
      return
      end
      
