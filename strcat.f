      subroutine strcat (s1,s2)
      implicit none
      character s1*(*),s2*(*)
      integer lnblnk
      s1=s1(1:lnblnk(s1))//s2(1:lnblnk(s2))
      return
      end
      













