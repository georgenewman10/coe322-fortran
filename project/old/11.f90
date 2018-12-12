! George Newman, gwn266 / Final Fortran project 

program goldbach 
implicit none
integer :: n,r,p,q,i,y,z,j,dif1,dif2,pcount,qcount,rcount
real :: start,finish
integer, dimension(10000) :: primes
integer, dimension(1000,3) :: triple
integer, dimension(919) :: jvector   
integer, dimension(10000) :: pvec
integer, dimension(10000) :: qvec
integer, dimension(10000) :: rvec
call cpu_time(start)
n=10000
call prime_array_generator(n,primes)
pvec = primes
qvec = primes
rvec = primes
pcount = 3
qcount = 4
rcount = 2


do i=1,10

  r = rvec(i+1)
  p = pvec(i+2)
  q = qvec(i+3)
  e1 = primes(i+4)
  e2 = primes(i+6)
  e3 = primes(i+7)
  e4 = primes(i+8)
  e5 = primes(i+9)
  e6 = primes(i+10)
  e7 = primes(i)
  e8 = primes(i-1)
  e9 = primes(i-2)
  e10 = primes()


  dif1 = p-r
  dif2 = q-p
  

  if (dif1<dif2)
  


  if (dif1==dif2) then
    triple(i,:) = [r,p,q]
  endif

enddo



!print *, "Each (p) value is a prime number equidistant from 2 other prime numbers"
!print *


do i=1,10
  print *, 'Triple (r,p,q): ', triple(i,:)
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
