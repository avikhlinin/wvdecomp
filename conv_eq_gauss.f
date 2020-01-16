      subroutine conv_equiv_gauss (nimg,work,nx,ny,scalemin,scalemax,sigmae
     ~    ,jstart)
c  
c Equivalent Gaussiian is the one which produced sigmae when convolved with
c the image containing normal gaussian noise
c
c Results are written starting with jstart plane
c
c  it is defined by equation
c%\begin{equation}
c% \sigma = \sqrt{\frac{1}{4\pi\sigma_e^2}}
c%\end{equation}
c% We additionally increase $\sigma$ by $\sqrt{2}$
c    
      implicit none
      integer nx,ny
      integer scalemin,scalemax
      real nimg(nx*ny,*),work(nx*ny)
      real sigmae(*)
      integer jstart
      integer jj,jout
      
      real sigma
      integer i
      
c //// remember the input errors
      do i=1,nx*ny
        work(i)=nimg(i,1)**2
      enddo
c ////
      jout=jstart
      print*,'Calculate errors     [',jout,scalemax,']'
      do jj=scalemin,scalemax
        sigma = sqrt(2.0)/sqrt(4.0*3.1415926536*sigmae(jj)**2)
        print*,'                      ',jj,sigma,sigmae(jj)
        do i=1,nx*ny
          nimg(i,jout)=work(i)
        enddo
        if (jj.ne.1) then
          call imsmo (nimg(1,jout),nx,ny,sigma)
         endif
        do i=1,nx*ny
          nimg(i,jout)=sqrt(max(0.0,nimg(i,jout)))*sigmae(jj)
        enddo
      enddo

      return
      end

