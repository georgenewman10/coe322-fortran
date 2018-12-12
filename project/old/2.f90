program final2
implicit none
integer :: x,i
integer, dimension(10000) :: primes


call prime_Array_generator()
primes = primeArray
print *, primes(3)


contains
  subroutine prime_Array_generator()
    integer :: i,j,counter,z
    logical :: isprime
    integer, dimension(10000) :: primeArray

    primeArray(1)=2
    
    counter=1
    i=3
    do while (counter<10000)
      isprime = .true.

      do j=2,(i/2)
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


