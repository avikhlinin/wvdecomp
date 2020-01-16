c-------------------------------------------------------
      subroutine image_sigma (x,n,sigma)
      implicit none
      integer n
      real x(n),sigma
      integer i
      
      real mean,disp
      
      mean=0.0
      disp=0.0
      do i=1,n
        mean=mean+x(i)
        disp=disp+x(i)**2
      enddo
      
      mean=mean/n
      sigma=sqrt(disp/n-mean**2)
      
      return
      end

cxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

      function if_min (img,nx,ny,i0,j0)
      implicit none
      logical if_min
      integer nx,ny,i0,j0
      real img(nx,ny),im0
      
      im0=img(i0,j0)
      if_min= 
     ~    (im0.le.img(i0+1,j0)) .and.
     ~    (im0.le.img(i0-1,j0)) .and.
     ~    (im0.le.img(i0,j0+1)) .and.
     ~    (im0.le.img(i0+1,j0+1)) .and.
     ~    (im0.le.img(i0-1,j0+1)) .and.
     ~    (im0.le.img(i0-1,j0-1)) .and.
     ~    (im0.le.img(i0+1,j0-1)) .and.
     ~    (im0.le.img(i0,j0-1))
      
      return
      end
      
cxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
      
      subroutine make_rwvmax_mask (mask,nx,ny,xpeak,ypeak,npeak,r)
      implicit none
      integer nx,ny
      integer mask(nx,ny)
      integer npeak
      integer xpeak(npeak), ypeak(npeak)
      real r

      integer idistance,i,j,imin,imax,jmin,jmax,ipeak
      integer ir,x,y,ir1

      do i=1,nx
        do j=1,ny
          mask(i,j)=0
        enddo
      enddo

      ir = int(r**2)+1
      ir1 = int(r)+1
      do ipeak = 1, npeak
        x = xpeak(ipeak)
        y = ypeak(ipeak)
        imin = min(max(x-ir,1),nx)
        imax = max(min(x+ir,nx),1)
        jmin = min(max(y-ir,1),ny)
        jmax = max(min(y+ir,ny),1)
        do i=imin,imax
          do j=jmin,jmax
            if (idistance(x,y,i,j).le.ir) then
              mask(i,j)=1
            endif
          enddo
        enddo
      enddo

      return
      end
