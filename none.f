      logical function none (argument)
      character argument*(*)
      
      none=(argument.eq.'none').or.(argument.eq.'NONE')
      return
      end
      
