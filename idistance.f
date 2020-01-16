      integer function idistance (i1,j1,i2,j2)
c
c return (i1-i2)**2+(j1-j2)**2
c
      implicit none
      integer i1,j1,i2,j2
      
      integer i,j
      integer n
      parameter (n=16384)
      integer d(0:n)
      
      logical firstcall
      data firstcall /.true./
      
      save firstcall, d
      
      if(firstcall)then
        firstcall=.false.
        do i=0,n
          d(i)=i**2
        enddo
      endif
      
      i=iabs(i1-i2)
      j=iabs(j1-j2)
      idistance=d(i)+d(j)
      
      return
      end
      
