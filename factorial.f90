program factorial
	integer :: fact
	write(*,*) fact(3)
end program factorial

recursive function	fact(m) result(a)
	integer, intent(in) :: m
	integer :: a

	if (m==0) then
		a = 1
	else if (m==1) then
		a = 1
	else
		a = m*(fact(m-1))
	endif
end function
