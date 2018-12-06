program formats
implicit none
integer :: i
integer, dimension(5) :: A = [1., 2., 3., 4., 5.]
character(len=8) :: n = 'John Doe'

i = 1234;

print '(a,i4,a)', 'i=', i, '<'
print '(a,i7,a)', 'i=', i, '<'




!do i=1,5
!  print '(i4)', A(i)
!enddo

print '(a1,a8,a1)', '>', n, '<' 
print '(a,a,a)', '>', n, '<' 
print '(a,a4,a)', '>', n, '<' 
print '(a,a5,a)', '>', n, '<' 
print '(a,a6,a)', '>', n, '<' 
print '(a,a9,a)', '>', n, '<' 




end program 
