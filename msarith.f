      subroutine msarith (x,operation,value,n)
      implicit none
      integer n
      real x(n),value
      character operation*1
      
      integer i
      
      if (operation.eq.'=') then
        do i=1,n
          x(i)=value
        enddo
        
      else if (operation.eq.'+') then
        do i=1,n
          x(i)=x(i)+value
        enddo
        
      else if (operation.eq.'-') then
        do i=1,n
          x(i)=x(i)-value
        enddo
        
      else if (operation.eq.'*') then
        do i=1,n
          x(i)=x(i)*value
        enddo
        
      else if (operation.eq.'/') then
        do i=1,n
          x(i)=x(i)/value
        enddo
        
      else
         call exiterror('wrong operation in msarith')
      endif
      
      return
      end
      
        
