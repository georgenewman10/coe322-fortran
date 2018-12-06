program successiveprimes
implicit none



call prime_list(10)
print *, primes





contains
subroutine prime_list(n)
integer, dimension(n) :: primes 
integer :: n,i,j
logical :: isprime



do k=2,n
  isprime = .true.
  primes(1) = 2
  do i=2,100  !!!!! change 100 to larger number once working
    if (i==2) then
      
    endif

  
    do j=2,(i/2)
      if (mod(i,j)==0) then
        isprime = .false.
      endif
    enddo

  primes(k) = i
  enddo
enddo







end subroutine 
end program

