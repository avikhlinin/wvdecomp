      subroutine imsmo (z,nx,ny,sigma0)
c
      implicit none
      integer nx,ny
      real z(nx,ny)
      real sigma0
      
      real sigma
      integer w1,w2,w3
      real factor
      
      
      
      if (sigma0.lt.12.0) then
        sigma=nint(sigma0*4.0)/4.0
        if (sigma.lt.1.0) then
          return
        endif
        call imsmo_windows (sigma,w1,w2,w3)
      else
        factor=int(sigma0/12.0)+1
        sigma=nint(sigma0*4.0/factor)/4.0
        call imsmo_windows (sigma,w1,w2,w3)
        w1=nint(w1*factor/2)*2+1
        w2=nint(w2*factor/2)*2+1
        w3=nint(w3*factor/2)*2+1
      endif
        
        
      call conv_rect1(z,nx,ny,w1,w1)
      call conv_rect1(z,nx,ny,w2,w2)
      call conv_rect1(z,nx,ny,w3,w3)
      
      return
      end
      



      subroutine imsmo_windows (sigma,w1,w2,w3)
      implicit none
      real sigma
      integer w1,w2,w3
      if (abs(sigma-1.0).lt.0.01) then
        w1=3
        w2=3
        w3=1
      else if (abs(sigma-1.25).lt.0.01) then
        w1=1
        w2=3
        w3=3
      else if (abs(sigma-1.50).lt.0.01) then
        w1=3
        w2=3
        w3=3
      else if (abs(sigma-1.75).lt.0.01) then
        w1=1
        w2=5
        w3=3
      else if (abs(sigma-2.00).lt.0.01) then
        w1=3
        w2=5
        w3=3
      else if (abs(sigma-2.25).lt.0.01) then
        w1=3
        w2=5
        w3=5
      else if (abs(sigma-2.50).lt.0.01) then
        w1=5
        w2=5
        w3=5
      else if (abs(sigma-2.75).lt.0.01) then
        w1=5
        w2=7
        w3=3
      else if (abs(sigma-3.00).lt.0.01) then
        w1=5
        w2=5
        w3=7
      else if (abs(sigma-3.25).lt.0.01) then
        w1=5
        w2=7
        w3=7
      else if (abs(sigma-3.50).lt.0.01) then
        w1=7
        w2=7
        w3=7
      else if (abs(sigma-3.75).lt.0.01) then
        w1=5
        w2=7
        w3=9
      else if (abs(sigma-4.00).lt.0.01) then
        w1=9
        w2=7
        w3=7
      else if (abs(sigma-4.25).lt.0.01) then
        w1=9
        w2=9
        w3=7
      else if (abs(sigma-4.50).lt.0.01) then
        w1=9
        w2=9
        w3=9
      else if (abs(sigma-4.75).lt.0.01) then
        w1=11
        w2=9
        w3=7
      else if (abs(sigma-5.00).lt.0.01) then
        w1=9
        w2=11
        w3=9
      else if (abs(sigma-5.25).lt.0.01) then
        w1=11
        w2=9
        w3=11
      else if (abs(sigma-5.50).lt.0.01) then
        w1=11
        w2=11
        w3=9
      else if (abs(sigma-5.75).lt.0.01) then
        w1=11
        w2=11
        w3=11
      else if (abs(sigma-6.00).lt.0.01) then
        w1=13
        w2=11
        w3=11
      else if (abs(sigma-6.25).lt.0.01) then
        w1=13
        w2=13
        w3=9
      else if (abs(sigma-6.50).lt.0.01) then
        w1=13
        w2=11
        w3=13
      else if (abs(sigma-6.75).lt.0.01) then
        w1=13
        w2=13
        w3=13
      else if (abs(sigma-7.00).lt.0.01) then
        w1=15
        w2=13
        w3=13
      else if (abs(sigma-7.00).lt.0.01) then
        w1=15
        w2=13
        w3=13
      else if (abs(sigma-7.25).lt.0.01) then
        w1=15
        w2=15
        w3=11
      else if (abs(sigma-7.50).lt.0.01) then
        w1=15
        w2=13
        w3=15
      else if (abs(sigma-7.75).lt.0.01) then
        w1=15
        w2=15
        w3=15
      else if (abs(sigma-8.00).lt.0.01) then
        w1=15
        w2=17
        w3=15
      else if (abs(sigma-8.25).lt.0.01) then
        w1=17
        w2=17
        w3=13
      else if (abs(sigma-8.50).lt.0.01) then
        w1=17
        w2=17
        w3=15
      else if (abs(sigma-8.75).lt.0.01) then
        w1=17
        w2=17
        w3=17
      else if (abs(sigma-9.00).lt.0.01) then
        w1=13
        w2=19
        w3=19
      else if (abs(sigma-9.25).lt.0.01) then
        w1=19
        w2=15
        w3=19
      else if (abs(sigma-9.50).lt.0.01) then
        w1=19
        w2=19
        w3=17
      else if (abs(sigma-9.75).lt.0.01) then
        w1=19
        w2=19
        w3=19
      else if (abs(sigma-10.00).lt.0.01) then
        w1=19
        w2=21
        w3=17
      else if (abs(sigma-10.25).lt.0.01) then
        w1=21
        w2=19
        w3=19
      else if (abs(sigma-10.50).lt.0.01) then
        w1=21
        w2=21
        w3=19
      else if (abs(sigma-10.75).lt.0.01) then
        w1=21
        w2=17
        w3=23
      else if (abs(sigma-11.00).lt.0.01) then
        w1=21
        w2=19
        w3=23
      else if (abs(sigma-11.25).lt.0.01) then
        w1=21
        w2=21
        w3=23
      else if (abs(sigma-11.50).lt.0.01) then
        w1=21
        w2=23
        w3=23
      else if (abs(sigma-11.75).lt.0.01) then
        w1=23
        w2=19
        w3=25
      else if (abs(sigma-12.00).lt.0.01) then
        w1=25
        w2=23
        w3=21
      endif

      return
      end
      
