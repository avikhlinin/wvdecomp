c% Convert relative to absolute path name. The program creates file if it
c% does not exist and then calls ``realpath'' --- C-wrapper to the library
c% function ``realpath''
c%
c% Bugs: this program fails if file does not exist and cannot be created by
c% the user, or if directory is unreadable.
  
      subroutine get_absolute_path (filename,pathname)
      implicit none
      character filename*(*),pathname*(*)
      integer lnblnk
      logical exist
      integer unit,newunit
      character name*200
      integer i

c% create file if it does not exist      
      inquire (file=filename,exist=exist)
      if (.not.exist) then
        unit = newunit()
        open(unit,file=filename,status='new')
      endif

c% put file name to a C (null-terminated) string
      name = filename
      i = lnblnk(filename)
      name (i+1:i+1) = char(0)

c% realpath\_ just calls the library function realpath (see real\_path.c)
      call realpath (name,pathname)

c% convert C string to the fortran format
      call fixstring(pathname)

c% delete temprorary file.
      if (.not.exist) close (unit,status='delete')

      return
      end

