program final5
implicit none
integer :: n,r,p,q,i,y,z,j, temp
integer, dimension(50000) :: primes
integer, dimension(1000,3) :: triple

n=50000
call prime_array_generator(n,primes)


!print *, primes(1)   is 2
!print *, primes(2)   is 3
!print *, primes(3)   is 5
!print *, primes(4)   is 7
!print *, primes(5)   is 11
!print *, primes(6)   is 13
!print *, primes(100) is 541


do i=1,1000                               ! the triple ill be making
  
  do j=1,20000                            ! number from prime list
    p = primes(j)
    q = primes(j+1)
    r = p-(q-p)
    if (any(triple(:,2)==p)) then
      cycle
    endif


    if (any(primes==r)) then
      triple(i,:) = [r,p,q]
      
      exit
    
    endif 
  
  enddo

enddo






do i=1,1000
  print *, 'i: ',i,triple(i,:)
enddo















contains
subroutine prime_array_generator(n,primeArray)
integer :: counter,n
real :: j,i
integer, dimension(n) :: primeArray
logical :: isprime

primeArray(1)=2
counter=1
i=3

do while (counter<n)
  isprime = .true.
  do j=2,(sqrt(i))
    if (mod(i,j)==0) then
      isprime = .false.
      exit
    endif
  enddo
  if (isprime) then
    counter = counter + 1
    primeArray(counter)=i
  endif     
  i = i+1
enddo

end subroutine
end program
