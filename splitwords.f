      subroutine splitwords (string,words,nwords)
      implicit none
      character string*(*)
      character words(*)*(*)
      integer nwords
      integer len,i,j,lnblnk
      
      len=lnblnk(string)
      
      i=1
      
      nwords=0
      
      do while (i.le.len)
        do while ((string(i:i).eq.' '.or.string(i:i).eq.'\t').and.i.lt.len)
          i=i+1
        enddo
        if (i.eq.len) then
          if (string(i:i).ne.' '.and.string(i:i).ne.'\t') then
            nwords=nwords+1
            words(nwords)=string(i:i)
          endif
          goto 100
        endif
        
        nwords=nwords+1
        j=1
        words(nwords)=' '
        do while ((string(i:i).ne.' '.and.string(i:i).ne.'\t').and.i.le.len)
          words(nwords)(j:j)=string(i:i)
          i=i+1
          j=j+1
        enddo
      enddo
      
 100  continue
      
      return
      end
      
