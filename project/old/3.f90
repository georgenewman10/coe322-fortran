program final3
implicit none
integer :: n=50000,r,p,q,i,y,z,j
integer, dimension(50000) :: primes
integer, dimension(10000,3) :: triple


call prime_array_generator(n,primes)
! at this point, you have a list of the first 50000 prime #s


!i=1

!do j=1,100
!  p = primes(i)
!  q = primes(i+1)
!  r = p-(q-p)
!  if (any(primes==r)) then
!    triple(j,:) = [r,p,q]
    
!  endif
!  i=q
!enddo







print primes(1)






print *, triple(1,:)
print *, triple(2,:)
print *, triple(5,:)













! to print the triples
!do y=1,100                    ! change the 100 to 10000
!  do z=1,3
!    print *, triple(y,z), ' '
!  enddo
!  print *
!enddo






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
