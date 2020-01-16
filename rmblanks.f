      subroutine rmblanks (s)
c
c Remove blanks from a string
c
      implicit none
      character s*(*)
      
      integer lnblnk,len,i,n
      
      
      len=lnblnk(s)
      
      n=0
      do i=1,len
        if(s(i:i).ne.' '.and.s(i:i).ne.'\t')then
          n=n+1
          s(n:n)=s(i:i)
        endif
      enddo
      
      do i=n+1,len
        s(i:i)=' '
      enddo
      
      return
      end
      
