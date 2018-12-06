!! George Newman, gwn266 // Homework 7, successive prime number generator

program nprimes
implicit none
integer :: x

print *, 'Enter desired number of prime numbers: '
read *, x

call prime_list(x)

contains
subroutine prime_list(n)
integer :: n,i,j,counter    ! n is input, i and j are loop variables, counter is the current number of primes
logical :: isprime          ! flag to simplify counting of primes

counter = 1                 ! starts at 1 because 2 is prime
i = 3                       ! I put i outside of the loop to avoid having a triple loop
print*, 2                   ! no better way to deal with 2 than this


do while (counter<n)        
  isprime = .true.          
  
  do j=2,(i/2)          
    if (mod(i,j)==0) then   ! flag becomes false if it finds a factor
      isprime = .false.
      exit  
    endif
  enddo

  if (isprime) then
    counter = counter + 1
    print*, i
  endif
  
  i = i+1
enddo
end subroutine
end program
