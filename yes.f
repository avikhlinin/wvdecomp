      logical function yes (argument)
      character argument*1
      
      yes=(argument.eq.'y').or.(argument.eq.'Y')
      return
      end
