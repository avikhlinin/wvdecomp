module floodfill90
  private
  public :: flood_fill
  
  integer :: CON, NX, NY
  logical :: SAVE_PIX

  integer :: npix, npixmax, ncall
  integer, dimension(:), allocatable :: X_PIX,Y_PIX
  
  integer, dimension(:), allocatable :: xstack, ystack
  integer :: nstack=-1, nstackmax=-1

contains

  logical function pushpix(x,y)
    implicit none
    integer, intent(in) :: x,y
    integer, dimension(:), allocatable :: wj
    integer status

    if (npix.eq.npixmax) then
      npixmax = npixmax + 10000
      allocate (wj(npixmax),stat=status)
      if (status.ne.0) then
        pushpix = .false.
        return
      endif
      wj(1:npix)=X_PIX
      call move_alloc(wj,X_PIX)
      allocate (wj(npixmax),stat=status)
      if (status.ne.0) then
        pushpix = .false.
        return
      endif
      wj(1:npix)=Y_PIX
      call move_alloc(wj,Y_PIX)
    endif
    npix=npix+1
    X_PIX(npix)=x
    Y_PIX(npix)=y

    pushpix = .true.
  end function pushpix

  logical function pop(x,y)
    implicit none
    integer, intent(out) :: x,y
    
    if (nstack.le.0) then
      pop = .false.
      return
    endif

    x=xstack(nstack)
    y=ystack(nstack)
    nstack = nstack - 1
    pop = .true.
    return
  end function pop

  logical function push(x,y)
    implicit none
    integer, intent(in) :: x,y
    integer, dimension(:), allocatable :: wj
    integer status
    
    if (nstack.lt.0) then ! this means we haven't allocated memory
      nstack = 0
      nstackmax = 10000
      allocate(xstack(nstackmax),ystack(nstackmax))
    endif
    
    if (nstack.eq.nstackmax) then ! allocate more memory

      nstackmax = nstackmax + 10000

      allocate(wj(nstackmax),stat=status)
      if (status.ne.0) then
        push = .false.
        return
      endif
      wj(1:nstack) = xstack
      call move_alloc (wj,xstack)

      allocate(wj(nstackmax),stat=status)
      if (status.ne.0) then
        push = .false.
        return
      endif
      wj(1:nstack) = ystack
      call move_alloc (wj,ystack)

    endif

    nstack = nstack + 1
    xstack(nstack)=x
    ystack(nstack)=y

    push = .true.
    return
    
  end function push
      
  subroutine clear_stack
    if (allocated(xstack)) deallocate(xstack)
    if (allocated(ystack)) deallocate(ystack)
    nstack = -1
    nstackmax = 0
  end subroutine clear_stack

  subroutine flood_fill_4 (mask,i,j,newval,val)
! http://lodev.org/cgtutor/floodfill.html
    implicit none
    integer, dimension(:,:), intent(inout) :: mask
    integer, intent(in) :: i,j, newval, val
    integer :: x,y,y1
    logical :: spanLeft, spanRight

    if(newval.eq.val) return ! avoid infinite loop
    call clear_stack

    if (mask(i,j).ne.val) return
    
    x=i
    y=j

    if(.not.push(x, y)) then
      return
    endif
    
    do while (pop(x,y))
      
      y1 = y
      do while (y1.ge.1 .and. mask(x,y1).eq.val)
        y1 = y1 - 1
      end do
      y1 = y1 + 1
      spanLeft = .false.
      spanRight = .false.
      do while (y1.le.NY .and. mask(x,y1).eq.val)
        mask(x,y1) = newval
        if (SAVE_PIX) then
          if (.not.pushpix(x,y1)) then
            return
          endif
        endif
        if (.not.spanLeft .and. x.gt.1 .and. mask(x-1,y1) .eq.val) then 
          if (.not.push(x-1,y1)) return
          spanLeft = .true.
        else
          if (spanLeft .and. x.gt.1 .and. mask(x-1,y1).ne.val) then
            spanLeft = .false.
          endif
        endif
        if(.not.spanRight .and. x.lt.NX .and. mask(x+1,y1) .eq. val) then
          if(.not.push(x+1,y1)) return
          spanRight = .true.
        else
          if (spanRight .and. x.lt.NX .and. mask(x+1,y1).ne.val) then
            spanRight = .false.
          endif
        endif
        y1=y1+1
      enddo
    enddo

    call clear_stack

  end subroutine flood_fill_4
      

  subroutine flood_fill (mask, i0,j0, newval, val, connect,xpix,ypix)
    integer, dimension(:,:), intent(inout) :: mask
    integer, intent(in) :: i0,j0, newval, val
    integer, intent(in), optional :: connect
    integer, allocatable, dimension(:), intent(inout), optional :: xpix,ypix

    NX = size(mask,1)
    NY = size(mask,2)

    if (present(connect)) then
      CON = connect
    else
      CON = 4
    endif

    if (present(xpix).and.present(ypix)) then
      npix=0
      npixmax = 10000
      allocate (X_PIX(npixmax),Y_PIX(npixmax))
      SAVE_PIX = .true.
    else
      SAVE_PIX = .false.
    endif

    ! write (0,*) i0,j0,newval,val,CON
    ! write (0,*) 'SAVE_PIX=',SAVE_PIX, present(xpix), present(ypix)


    ncall = 0
    if (CON.eq.4) then
      call flood_fill_4 (mask,i0,j0,newval,val)
    else
      call floodfill_w_recursive (mask,i0,j0,newval,val)
    endif
    
    if (SAVE_PIX) then
      if (allocated(xpix)) deallocate(xpix)
      if (allocated(ypix)) deallocate(ypix)
      allocate (xpix(npix),ypix(npix))
      xpix=X_PIX(1:npix)
      ypix=Y_PIX(1:npix)
      deallocate (X_PIX,Y_PIX)
    endif

  end subroutine flood_fill
  
  recursive subroutine floodfill_w_recursive (mask,i0,j0,newval,val)
    implicit none
    integer, dimension(:,:), intent(inout) :: mask
    integer, intent(in) :: i0,j0, newval, val

    integer :: color,x,y
    
    integer, dimension(:), allocatable :: wj

    ncall = ncall + 1

    if (i0.lt.1.or.i0.gt.NX.or.j0.lt.1.or.j0.gt.NY) return

    color = mask(i0,j0)
    x = i0
    y = j0
    if (color.eq.val) then
      do !     while (color == original_color) { 
        mask(x,y)=newval

        if (SAVE_PIX) then
          if (.not.pushpix(x,y)) return
        endif

        call floodfill_w_recursive (mask,x,y+1,newval,val)
        call floodfill_w_recursive (mask,x,y-1,newval,val)
        if (CON.eq.8) then
          call floodfill_w_recursive (mask,x+1,y+1,newval,val)
          call floodfill_w_recursive (mask,x+1,y-1,newval,val)
          call floodfill_w_recursive (mask,x-1,y+1,newval,val)
          call floodfill_w_recursive (mask,x-1,y-1,newval,val)
        endif
        x = x - 1
        if (x.lt.1) exit
        color = mask(x,y)
        if (color.ne.val) exit
      end do

      x = i0 + 1
      y = j0
      if (x.le.NX) then
        color = mask(x,y)

        do
          mask(x,y)=newval

          if (SAVE_PIX) then
            if (.not.pushpix(x,y)) return
          endif

          call floodfill_w_recursive (mask,x,y+1,newval,val)
          call floodfill_w_recursive (mask,x,y-1,newval,val)
          if (CON.eq.8) then
            call floodfill_w_recursive (mask,x+1,y+1,newval,val)
            call floodfill_w_recursive (mask,x+1,y-1,newval,val)
            call floodfill_w_recursive (mask,x-1,y+1,newval,val)
            call floodfill_w_recursive (mask,x-1,y-1,newval,val)
          endif
          x = x+1
          if (x.gt.NX) exit
          color = mask(x,y)
          if (color.ne.val) exit
        end do
      end if
    end if
  end subroutine floodfill_w_recursive
end module floodfill90

subroutine flood_fill_77 (mask,nx,ny,i0,j0,newval,val)
  use floodfill90
  implicit none
  integer, intent(in) :: nx,ny
  integer, dimension(nx,ny), intent(inout) :: mask
  integer, intent(in) :: i0,j0, newval, val

  call flood_fill (mask,i0,j0,newval,val)
  return
end subroutine flood_fill_77
