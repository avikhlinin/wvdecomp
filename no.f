      logical function no (argument)
      character argument*1
      
      no=(argument.eq.'n').or.(argument.eq.'N')
      return
      end
