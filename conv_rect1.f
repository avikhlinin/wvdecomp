      subroutine conv_rect1(d,nx,ny,nrecx,nrecy)
c
c  convolves real d(nx,ny) with rectangle nrecx, nrecy
c
c
c
      implicit none
      integer nx,ny
      integer nrecx,nrecy
      real d(nx,ny)
      integer nmax
      parameter (nmax=262144)
      double precision row(nmax), drow(nmax)
      integer i,j
      integer ii,jj,ii1,ii2,jj1,jj2
      
      integer nsx1,nsx2,nsy1,nsy2
      
      double precision w
      
      if ((nrecx/2)*2.eq.nrecx.or.(nrecy/2)*2.eq.nrecy) then
         call exiterror('must be odd nrecx in conv_rect1')
        return
      endif
      
      nsx1=nrecx/2
      nsx2=nrecx/2
      nsy1=nrecy/2
      nsy2=nrecy/2
      
c==Columns
      
      do i=1,nx

c..copy column
        do j=1,ny
          row(j)=d(i,j)
          drow(j)=row(j)
        enddo
        
c..Compute first element        
        w=0.
        do j=1-nsy1,1+nsy2
          if(j.le.0)then
            jj=j+ny
          else
            jj=j
          endif
          w=w+row(jj)
        enddo

        drow(1)=w
        
c..convolve each column        
        do j=2,ny
          jj=j-nsy1-1
          if(jj.le.0)then
            jj1=jj+ny
          else
            jj1=jj
          endif
          
          jj=j+nsy2
          if(jj.gt.ny)then
            jj2=jj-ny
          else
            jj2=jj
          endif
          
          drow(j)=drow(j-1)+row(jj2)-row(jj1)
        enddo
        
        do j=1,ny
          d(i,j)=drow(j)
        enddo
        
      enddo

c==Rows
      do j=1,ny
        
c..copy row
        do i=1,nx
          row(i)=d(i,j)
          drow(i)=row(i)
        enddo
        
c..compute first element
        w=0.
        do i=1-nsx1,1+nsx2
          if(i.le.0)then
            ii=i+nx
          else
            ii=i
          endif
          w=w+row(ii)
        enddo
        
        drow(1)=w
        
        do i=2,nx
          ii=i-nsx1-1
          if(ii.le.0)then
            ii1=ii+nx
          else
            ii1=ii
          endif
          
          ii=i+nsx2
          if(ii.gt.nx)then
            ii2=ii-nx
          else
            ii2=ii
          endif
          
          drow(i)=drow(i-1)+row(ii2)-row(ii1)
        enddo

        do i=1,nx
          d(i,j)=drow(i)
        enddo
        
      enddo
      
      
c==Normalize      
      w=nrecx*nrecy
      w=1.0/w
      do i=1,nx
        do j=1,ny
          d(i,j)=d(i,j)*w
        enddo
      enddo
      
      
      return
      end




