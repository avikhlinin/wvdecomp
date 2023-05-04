module floodfill_interface
  interface
     subroutine iniFloodFill (img,nx,ny,fortran,connect) &
          bind(c,name='iniFloodFill')
       use iso_c_binding
       integer (c_int) :: img(*)
       integer (c_int), value :: nx,ny,fortran,connect
     end subroutine iniFloodFill

     subroutine FloodFill (x0,y0,fill,orig) &
          bind(c,name='FloodFill')
       use iso_c_binding
       integer (c_int), value :: x0,y0,fill,orig
     end subroutine FloodFill

  end interface
end module floodfill_interface
