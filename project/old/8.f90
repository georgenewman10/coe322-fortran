! George Newman, gwn266 / Final Fortran project 

program goldbach 
implicit none
integer :: n,r,p,q,i,y,z,j
real :: start,finish
integer, dimension(10000) :: primes
integer, dimension(1000,3) :: triple
integer, dimension(919) :: jvector   
integer, dimension(918) :: distance

call cpu_time(start)
n=10000
call prime_array_generator(n,primes)

jvector(1)=2                    ! creates a vector of the indices at which the p values are found, which speeds 
                                ! up the process as the do j=1,918 loop doesn't have to start from 1 each time

do i=1,10                             
  do j=(jvector(i)+1),10000
    p = primes(j)
    q = primes(j+1)
    r = p-(q-p)

    if (any(primes==r)) then 
      triple(i,:) = [r,p,q]
      jvector(i+1)=j            ! sets the starting j value for the next run, to keep from cycling 
      exit                      ! through pointless j values
    endif
  enddo
enddo





print *, "Each (p) value is a prime number equidistant from 2 other prime numbers"
print *


do i=1,10
  print *, 'Triple (r,p,q): ',triple(i,:)
enddo

call cpu_time(finish)
print *, "Time elapsed: ", finish-start


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
