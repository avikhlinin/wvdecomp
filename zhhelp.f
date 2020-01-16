      subroutine zhhelp (command)
      implicit none
      character command*(*)
      
      print*, 'Usage:'
      print*, '  wvdecomp image outkey '
      print*, '     [kernel=gauss/atrous] [iter=niter] [scalemin=smin] [scalemax=smax] '
      print*, '        [threshold=t] [thresholdmin=tmin]'
      print*, '        [stat=gauss/poisson] [errimg=errorimage]'
      print*, '        [bg=bgimage] [exp=expimage]'
      print*, '        [detectnegative=yes] [considershape=yes/no]'
      print*, '        [savenoise=yes] [save=yes]'
      print*, '        [restore=restorefile]'

      call exit(1)
      return
      end
