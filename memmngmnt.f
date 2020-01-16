      subroutine copy_real_arraysF (to, from, n)
      implicit none
      integer n
      real to(n), from(n)
      integer m,i

      m = n
      do i=1,n
        to(i)=from(i)
      enddo

      return
      end

      subroutine copy_real_arrays_step2F (to, from, n, step)
      implicit none
      integer n, step
      real to(n), from(n)
      integer m,i,k

      m = n
      k = 1
      do i=1,n
        to(i)=from(k)
        k = k + step
      enddo

      return
      end

      subroutine copy_real_arrays_step1F (to, from, n, step)
      implicit none
      integer n, step
      real to(n), from(n)
      integer m,i,k

      m = n
      k = 1
      do i=1,n
        to(k)=from(i)
        k = k + step
      enddo

      return
      end

      subroutine copy_double_arrays (to, from, n)
      implicit none
      integer n
      double precision to(n), from(n)
      integer m,i

      m = n
      do i=1,m
        to(i)=from(i)
      enddo
      return
      end

      
