! George Newman, gwn266 // In class challenge F5

program loops
implicit none

real :: u,v,w

do v = 4,100    
  do u = 3,v
    w = (u*u) + (v*v)
    w = sqrt(w)
    
    if ( w<100 .and. mod(w,1.)==0 ) then
      print *, u,v,w
    else if (w>100) then
      exit
    end if
  end do
end do


end program
