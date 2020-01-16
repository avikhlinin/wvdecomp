c Print a error message to stderr and exit(1)
      subroutine exiterror (message)
      character message*(*)

      write (0,*) message
      call exit(1)
      end
