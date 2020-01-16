      subroutine gausswv (img,z,bg,nx,ny,scale)
c
      implicit none
      integer nx,ny
      real img(nx,ny)
      real z(nx,ny)
      real bg(nx,ny)
      integer scale
c     real w
      integer w11(7),w12(7),w13(7),w21(7),w22(7),w23(7)
      integer ww11,ww12,ww13,ww21,ww22,ww23
      integer iscale
      
c  scale         1    2    3    4    5    6    7
c  sig1         0.0  1.0  2.0  4.0  8.0  16.  32.
c  sig2         1.0  2.0  4.0  8.0  16.  32.  64.
      
      data w11 / 1,   3,   3,   9,  15,  31,  65  /
      data w12 / 1,   3,   5,   7,  17,  35,  53  /
      data w13 / 1,   1,   3,   7,  15,  31,  71  /
      
      data w21 / 3,   3,   9,  15,  31,  65,  127 /
      data w22 / 3,   5,   7,  17,  35,  53,  123 /
      data w23 / 1,   3,   7,  15,  31,  71,  139 /

      integer wvecx(3),wvecy(3)
      
      call mcopy(z,img,nx,ny)
      call mcopy(bg,z,nx,ny)

      if (scale.le.7) then
        ww11 = w11(scale)
        ww12 = w12(scale)
        ww13 = w13(scale)
        ww21 = w21(scale)
        ww22 = w22(scale)
        ww23 = w23(scale)
      else
        ww11 = w11(7)
        ww12 = w12(7)
        ww13 = w13(7)
        ww21 = w21(7)
        ww22 = w22(7)
        ww23 = w23(7)
        do iscale=8,scale
          ww11 = ww11 * 2
          ww12 = ww12 * 2
          ww13 = ww13 * 2
          ww21 = ww21 * 2
          ww22 = ww22 * 2
          ww23 = ww23 * 2
        enddo
        ww11 = ww11 - 1
        ww12 = ww12 - 1
        ww13 = ww13 - 1
        ww21 = ww21 - 1
        ww22 = ww22 - 1
        ww23 = ww23 - 1
      endif

      wvecx(1)=ww11
      wvecx(2)=ww12
      wvecx(3)=ww13
      wvecy(1)=ww11
      wvecy(2)=ww12
      wvecy(3)=ww13
      call conv_rect1_n(z,nx,ny,wvecx,wvecy,3)
*      w=float(ww11**2)*float(ww12**2)*float(ww13**2)
*      w=1.0/w
*      call msarith (z,"*",w,nx*ny)
      
      wvecx(1)=ww21
      wvecx(2)=ww22
      wvecx(3)=ww23
      wvecy(1)=ww21
      wvecy(2)=ww22
      wvecy(3)=ww23
      call conv_rect1_n(bg,nx,ny,wvecx,wvecy,3)
*      w=float(ww21**2)*float(ww22**2)*float(ww23**2)
*      w=1.0/w
*      call msarith (bg,"*",w,nx*ny)
      
      return
      end
      
      subroutine conv_rect1_n(d,nx,ny,wvecx,wvecy,nconv)
      implicit none
      integer nx,ny
      real d(nx,ny)
      integer nconv
      integer wvecx(nconv),wvecy(nconv)
      
      integer nmax
      parameter (nmax=16384)
      double precision row(nmax), drow(nmax)
      integer i,j
      
      double precision w
      integer iconv,nrecx,nrecy

c==Checks
      do iconv=1,nconv
        nrecx=wvecx(iconv)
        nrecy=wvecy(iconv)
        if ((nrecx/2)*2.eq.nrecx.or.(nrecy/2)*2.eq.nrecy) then
          print*,iconv,nrecx,nrecy
          call exiterror('must be odd wvec in conv_rect1_n')
        endif
      enddo
c== Columns
      do i=1,nx
c..copy column
        do j=1,ny
          row(j)=d(i,j)
          drow(j)=row(j)
        enddo
        
        do iconv = 1,nconv
          nrecy = wvecy(iconv)

          call conv_rect1_1D_d (row,drow,ny,nrecy)
c..Copy the result of convolution to the 1-D data          
          if (iconv.lt.nconv) then
            do j=1,ny
              row(j)=drow(j)
            enddo
          endif

        enddo
c..Copy results back to the array
        do j=1,ny
           d(i,j)=sngl(drow(j))
        enddo
      enddo


c== Rows
      do j=1,ny
c..copy row
        do i=1,nx
          row(i)=dble(d(i,j))
          drow(i)=row(i)
        enddo
        
        do iconv = 1,nconv
          nrecx = wvecx(iconv)

          call conv_rect1_1D_d (row,drow,nx,nrecx)
c..Copy the result of convolution to the 1-D data          
          if (iconv.lt.nconv) then
            do i=1,nx
              row(i)=drow(i)
            enddo
          endif

        enddo

c..Copy results back to the array
        do i=1,nx
           d(i,j)=sngl(drow(i))
        enddo
      enddo
      
c==Normalize      
      w=wvecx(1)*wvecy(1)
      do iconv=2,nconv
        w = w*wvecx(iconv)*wvecy(iconv)
      enddo
      w=1.0/w
      do i=1,nx
        do j=1,ny
           d(i,j)=sngl(d(i,j)*w)
        enddo
      enddo
      
      
      return
      end





*xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx






