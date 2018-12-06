! George Newman, gwn266 // homework 6, subroutines and functions


program subroutinesandfunctions
implicit none

logical :: isprime

isprime = prime_test_function(13)
print *, isprime
isprime = prime_test_function(10)
print *, isprime


contains
logical function prime_test_function(x)
  integer :: x,i
 
  prime_test_function = .true.

  if (x==0) then
    prime_test_function = .false.
  elseif (x==1) then
    prime_test_function = .false.
  end if
  
  do i = 2,(x/2)
    if (mod(x,i)==0) then
      prime_test_function = .false.
      exit
    endif
  end do

end function prime_test_function
end program
