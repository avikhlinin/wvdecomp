      subroutine fitsio_print_error (status)
      implicit none
      integer status
      character errtext*50
      integer lnblnk
      
      call ftgerr (status,errtext)
      write(0,*) 'FITSIO:',status,'  (',errtext(1:lnblnk(errtext)),')'
      
      return
      end
      
      
      
      subroutine fitsio_write_error (status,file)
      implicit none
      integer status,file
      character errtext*50
      integer lnblnk
      
      call ftgerr (status,errtext)
      write(file,*)'FITSIO:',status,'  (',errtext(1:lnblnk(errtext)),')'
      
      return
      end
      
      
      subroutine exit_fitsio (message,status)
      implicit none
      character message*(*)
      integer status

      call perror_fitsio (message,status)
      call exit(1)
      return
      end
       
      
      subroutine perror_fitsio (message,status)
      implicit none
      character message*(*)
      integer lnblnk
      integer status
      character errtext*80
      
      if (status.ne.0) then
        call ftgerr(status,errtext)
        write(0,'(a,a,a)')message(1:lnblnk(message)),': '
     ~      ,errtext(1:lnblnk(errtext))
      endif
      return
      end
