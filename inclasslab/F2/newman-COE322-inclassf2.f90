       program f2
       implicit none
       real :: a, b, c, v, pi = 3.14159, d, v2
       character(len=10) :: yourname


       print *, 'Enter your name: '
       read *, yourname
       print *, 'Hello, ', yourname

       print *, 'Enter length: '
       read *, a

       print *, 'Enter height: '
       read *, b

       print *, 'Enter width: '
       read *, c

       v = (4./3.) * pi * a**3.0
       v2 = (a * b * c)

       print *, 'Volume of sphere with radius a is: ', v

       print *, 'Volume of rectangular prism is: ', v2



       d = (a*b*c)/7.0
       d = int(d)
       print *, 'Integer version of d is: ', d 

       end program f2
