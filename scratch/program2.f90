program swap
implicit none

integer :: i,j

i = 2
j = 3

print *, i, j
call swaps(i,j)
print *, i, j


contains

subroutine swaps(i, j, i_old, j_old)
    implicit none
    integer, intent(inout) :: i,j
    integer, intent(out) :: i_old, j_old

    k = i
    i = j
    j = k
end subroutine

end program
