      logical function user_wants_clobber ()
      implicit none
c
c This function may be set up to read the user environment to see if he wants
c to clobber existing FITS files
c
      user_wants_clobber = .true.
      return
      end
