module PointMod
implicit none

type Point
  real :: x,y
end type Point

contains
subroutine distance(p,q)
  type(Point) :: p,q
  real :: dist
  dist = sqrt( (q%x-p%x)*(q%x-p%x) + (q%y-p%y)*(q%y-p%y) )
  print *, dist
end subroutine
end module PointMod




program main
use PointMod
implicit none

type(Point) :: p1, p2

p1%x = 0
p1%y = 0
p2%x = 3
p2%y = 4

call distance(p1,p2)
end program main
