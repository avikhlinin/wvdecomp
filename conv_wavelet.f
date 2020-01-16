      subroutine conv_wavelet (kernel,img,cj1,cj,nx,ny,jscale,cj1mode)
      implicit none
      integer kernel
      real img(*)
      real cj1(*)
      real cj(*)
      integer jscale,nx,ny
      logical cj1mode
      
      if (kernel.eq.1) then
        call  atrouswv (img,cj1,cj,nx,ny,jscale,cj1mode)
      else
        call gausswv (img,cj1,cj,nx,ny,jscale)
      endif

      return
      end
