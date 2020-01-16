      subroutine conv_rect1_1D_d (row,drow,n,nrec)
c% Double precision 1-D box convolution      
      implicit none
      integer n,nrec
      double precision row(n), drow(n)
      double precision drowsum
      double precision w
      integer ns1,ns2
      integer j,jj,jj1,jj2
      
      ns1=nrec/2
      ns2=nrec/2

c..Compute first element        
      w=0.0
      do j=1-ns1,1+ns2
        if(j.le.0)then
          jj=j+n
        else
          jj=j
        endif
        w=w+row(jj)
      enddo
      drowsum = w
      drow(1) = drowsum
      
c..Compute other elements          
      do j=2,n
        jj1=j-ns1-1
        if(jj1.le.0)then
          jj1=jj1+n
        endif
          
        jj2=j+ns2
        if(jj2.gt.n)then
          jj2=jj2-n
        endif
        
        drowsum = drowsum +row(jj2)-row(jj1)
        drow(j)=drowsum
      enddo
      
      return
      end
