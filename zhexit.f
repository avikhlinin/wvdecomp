      subroutine zhexit(status)
      implicit none
      integer status

      call flush(0)
      call flush(6)

      call exit(status)

      return
      end
