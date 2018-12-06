program alloc_array
implicit none

real, dimension(:), allocatable :: x_1d ! Attribute
real, dimension(:,:), allocatable :: x_2d ! allocatable
integer :: n, m, ierror

n = 100
m = 10

print *,x_1d


allocate(x_1d(n), stat=ierror)
if (ierror /= 0) stop 'error x_1d'
x_1d = 5;
print *, x_1d

allocate(x_2d(n,m), stat=ierror)
if (ierror /= 0) stop 'error x_2d'
x_2d = 10;
print *, x_2d


deallocate(x_1d)
deallocate(x_2d)

end program
