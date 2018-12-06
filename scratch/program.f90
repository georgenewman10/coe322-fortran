program sum
implicit none

integer :: a, b, c
a = 5;
b = 10;

c = sums(a,b)
print *, a, b, c


contains
    integer function sums(a,b)
        integer :: a, b
        sums = a+b
    end function
end program


