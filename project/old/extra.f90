! George Newman, gwn266 / Final Fortran project [extra]

program goldbach 
implicit none
logical   :: isprime
integer   :: i,r,p,q,n=10000
real      :: start,finish,j,average
integer, dimension(11000)   :: primes
integer, dimension(10000,3) :: triple
integer, dimension(10000)   :: difference

open(unit=1,file="results/pvalues.txt")
open(unit=2,file="results/differences.txt")
call cpu_time(start)
call prime_array_generator(n,primes)

do i=1,10000
  p = primes(i+2)
  do j = 2,800
    if ( is_prime(p-j) .and. is_prime(p+j) ) then
      r = p-j
      q = p+j
      difference(i) = p-r
      triple(i,:) = [r,p,q]
      exit
    endif
  enddo
enddo

do i=1,9998
  write(2,*) difference(i)
enddo

do i=1,9998
  write(1,*) triple(i,2)
enddo


!write(1,*)
!write(1,*) 'Difference Statistics:'
!write(1,*) 'Average: ', (sum(difference)/size(difference))
!write(1,*) 'Maximum: ', maxval(difference)
!write(1,*) 'Minimum: ', minval(difference)
!write(1,*) 

call cpu_time(finish)
!write(1,*) "Time elapsed: ", finish-start


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



logical function is_prime(x)
  real :: x,i
  is_prime = .true.
  if (x==0) then
    is_prime = .false.
  elseif (x==1) then
    is_prime = .false.
  endif
  do i=2,(sqrt(x))
    if (mod(x,i)==0) then
      is_prime = .false.
      exit
    endif
  enddo
end function

end program
