      subroutine marith (z,dummy,x,operation,y,n)
      implicit none
      integer n
      character dummy*1,operation*1
      real z(n),x(n),y(n)
      integer i
      
      
      if      (operation.eq.'+') then
        do i=1,n
          z(i)=x(i)+y(i)
        enddo
        
      else if (operation.eq.'-') then
        do i=1,n
          z(i)=x(i)-y(i)
        enddo
        
      else if (operation.eq.'*') then
        do i=1,n
          z(i)=x(i)*y(i)
        enddo
        
      else if (operation.eq.'/') then
        do i=1,n
          z(i)=x(i)/y(i)
        enddo
        
      else
         call exiterror('wrong operation in marith')
      endif
      
      return
      end
      
