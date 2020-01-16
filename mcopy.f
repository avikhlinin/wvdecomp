      subroutine mcopy(a,b,n,m)
      implicit none
      integer n,m
      real a(*),b(*)
      integer i
      integer nn
      
      nn=n*m
      do i=1,nn
        a(i)=b(i)
      enddo
      
      return
      end
      
      
      subroutine mcopy8(a,b,n,m)
      implicit none
      integer n,m
      real*8 a(*),b(*)
      integer i
      integer nn
      
      nn=n*m
      do i=1,nn
        a(i)=b(i)
      enddo
      
      return
      end
      
*xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

      subroutine mcopyj (a,b,n,m)
      implicit none
      integer n,m
      integer a(*),b(*)
      integer i
      integer nn
      
      nn=n*m
      do i=1,nn
        a(i)=b(i)
      enddo
      
      return
      end
