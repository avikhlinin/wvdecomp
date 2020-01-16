      subroutine replace_characters (s,c1,c2)
      implicit none
      character s*(*)
      character c1*1,c2*1
      
      integer i,n,len
      
      n=len(s)
      
      do i=1,n
        if (s(i:i).eq.c1) then
          s(i:i)=c2
        endif
      enddo
      
      return
      end
      
