      subroutine atrouswv (img,cj1,cj,nx,ny,jscale,cj1mode)
      implicit none
      integer nx,ny
      real img(nx,ny), cj1(nx,ny), cj(nx,ny)
      integer jscale
      logical cj1mode

      if (cj1mode) then ! img and cj1 arrays are equivalent
        call atrouswv_range (cj1,nx,ny,1,jscale-1)
        call copy_real_arrays (cj,cj1,nx*ny)
        call atrouswv_range (cj,nx,ny,jscale,jscale)
      else
        call copy_real_arrays (cj,img,nx*ny)
        call atrouswv_range (cj,nx,ny,1,jscale-1)
        call copy_real_arrays (cj1,cj,nx*ny)
        call atrouswv_range (cj,nx,ny,jscale,jscale)
      endif
      return
      end
        
      subroutine atrouswv_range (img,nx,ny,jmin,jmax)
      implicit none
      integer nx,ny
      real img(nx,ny)
      integer jmin,jmax
      
      integer i,j,result
      real array1 (-8191:24576), array2(-8191:24576)
      
      if (jmin.gt.jmax) return

c       convolve columns
      do i=1,nx
        call copy_real_arrays_step2 (array1(1),img(i,1),ny,nx)
        call atrouswv_range_iterate (array1,array2,ny,result,jmin,jmax)
        if (result.eq.1) then
          call copy_real_arrays_step1 (img(i,1),array1(1),ny,nx)
        else
          call copy_real_arrays_step1 (img(i,1),array2(1),ny,nx)
        endif
      enddo
        
c     convolve rows
      do j=1,ny
        call copy_real_arrays (array1(1),img(1,j),nx)
        call atrouswv_range_iterate (array1,array2,nx,result,jmin,jmax)
        if (result.eq.1) then
          call copy_real_arrays (img(1,j),array1(1),nx)
        else
          call copy_real_arrays (img(1,j),array2(1),nx)
        endif
      enddo
        
      
      return
      end

*xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
      subroutine atrouswv_range_iterate (array1,array2,n,result,jmin,jmax)
      implicit none
      integer n
      real array1(*), array2(*)
      integer result, jmin, jmax
      integer js
      
      result = 1
      do js=jmin,jmax
        if (result.eq.1) then
          call atrousstep_02 (array1,array2,n,js)
          result = 2
        else
          call atrousstep_02 (array2,array1,n,js)
          result = 1
        endif
      enddo

      return
      end

      
*xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
c
c see atrousstep0.c for alternative implementation
c
c
      subroutine atrousstep_02f (cc,c,n,jscale)
      implicit none
      integer n, jscale
      real c(-8191:24576), cc(-8191:24576)
      
      integer i,step2,step


      step = 2**(jscale-1)
      step2=step*2
      do i=1-step2,0
        cc(i)=cc(1)
      enddo
      
      do i=n+1,n+step2
        cc(i)=cc(n)
      enddo

      do i=1,n
        c(i) = (
     ~      cc(i-step2)+cc(i+step2) +
     ~      (cc(i-step)+cc(i+step))*4 + 
     ~      cc(i)*6
     ~      )/16
      enddo
      
      return
      end
   




