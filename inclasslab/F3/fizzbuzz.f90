! George Newman, gwn266 // In class challenge F3

program fizzbuzz
implicit none

integer :: input

print *, 'Enter an integer: '
read *, input

if ( mod(input,3)==0 .and. mod(input,5)==0  ) then
  print *, "FizzBuzz"
else if (mod(input,5)==0) then
  print *, "Buzz"
else if (mod(input,3)==0) then
  print *, "Fizz"
end if


end program
