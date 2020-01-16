
      function newunit ()
      
      logical opened
      
      opened=.true.
      i=6
      do while (opened)
        i=i+1
        inquire(unit=i,opened=opened)
      enddo
      
      newunit=i
      
      end
