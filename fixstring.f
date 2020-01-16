      subroutine fixstring (s)
      implicit none
      character s*(*)
      integer n,len
      integer i
      n=len(s)
      if (n.le.0) return
      
      i=1
      do while (i.lt.n.and.ichar(s(i:i)).ne.0)
        i=i+1
      enddo
      if (i.le.n) then
        s(i:)=' '
      endif
      
      
      return
      end
      
      
