! George Newman, gwn266 // In class challenge F4

program divisors
implicit none

integer:: a,b

print *, 'Enter first number: '
read *, a
print *, 'Enter second number: '
read *, b

if (mod(b,a)==0 ) then
  print *,a,'is a divisor of',b
else 
  print *,a,'is not a divisor of ',b
endif

end program
