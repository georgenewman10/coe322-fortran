! George Newman, gwn266 // homework 4


program try
implicit none

integer :: divisor,i,j
logical :: isprime                  ! flag

do i = 0,100
  isprime = .true.                  ! a flag to be used later, indicating if a factor is found

  if (i==0) then                    ! I could find no elegant solution for 0 and 1 
    print *, i,'is not prime'
    cycle
  else if (i==1) then
    print *, i,'is not prime'
    cycle
  end if

  do j = 2,(i/2)                    ! cycle through list of potential divisors, exit if divisor is found
    
    if (mod(i,j)==0) then
      divisor = j
      isprime = .false.
      exit
    end if

  end do

  if (isprime) then
    print *, i,'is prime'
  else 
    print *, i,'is not prime: it is divisible by', divisor
  end if

end do

end program
