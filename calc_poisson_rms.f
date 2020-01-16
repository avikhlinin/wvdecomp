! This function is needed because bg is actually stored in an integer array
! in dodecomp

      subroutine calc_poisson_rms (rms,bg,n,s,sigmae)
      implicit none
      integer n
      real rms(n), bg(n)
      real s, sigmae

      integer i

      do i=1,n
        rms(i)=sqrt((max(bg(i),0.0)*s+0.75)/s)*sigmae
      enddo
      
      return
      end
