program final1
implicit none
integer :: x,i
integer, dimension(10000) :: primeArray






call prime_list()

print *, primeArray(1)


contains
subroutine prime_list()
integer :: i,j,counter         ! n is input, i and j are loop variables, counter is the current number of primes
logical :: isprime               ! flag to simplify counting of primes
integer, dimension(10000) :: primeArray

primeArray(1)=2

counter = 1                      ! starts at 1 because 2 is prime
i = 3                            ! I put i outside of the loop to avoid having a triple loop
do while (counter<10000)        
  isprime = .true.          
  
  do j=2,(i/2)          
    if (mod(i,j)==0) then        ! flag becomes false if it finds a factor
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
