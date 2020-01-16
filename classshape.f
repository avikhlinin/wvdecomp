      subroutine classshape (class,img,nx,ny,iclass,cx,cy,cmin,cmax)
      implicit none
      integer nx,ny
      integer class(nx,ny)
      real img(nx,ny),im
      integer iclass
      integer cx,cy
      real cmin,cmax
      
      integer isumx,isumy

      integer i,j,n
      
      isumx=0
      isumy=0
      n=0
      cmin=1e10
      cmax=-1e10
      do i=1,nx
        do j=1,ny
          if (class(i,j).eq.iclass) then
            isumx=isumx+i
            isumy=isumy+j
            im=img(i,j)
            cmin=min(cmin,im)
            cmax=max(cmax,im)
            n=n+1
          endif
        enddo
      enddo
      
      cx=nint(float(isumx)/float(n))
      cy=nint(float(isumy)/float(n))
      return
      end
