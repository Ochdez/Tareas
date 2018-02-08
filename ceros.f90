program main

   real(8) :: dx, xmax, xmin, integral,m,b,a,c
   real(8), allocatable, dimension(:) :: x,z,f
   integer :: p,h,n,r,l

   print*,"Ingrese punto a: "
   read(*,*)a
   print*,"Ingrese punto b: "
   read(*,*)b
   print*,"Ingrese numero de puntos entre a y b: "
   read(*,*)h
   p=h+1
   allocate(x(0:P),z(0:p))

   dx = (b-a)/p
 
   do i = 0, p
     x(i) = a + (i*dx)
     print*,x(i)
   enddo

   print*," "
   
   call fsub1(x,p,z)
   do i=0,p
       print*,z(i)
   enddo
   
   n=size(z)
   print*,n
   
   print*," "

   call fsub2(x,n,c,z)
   print*,c
   !do i=0,(p-1)
	!print*,"Cero: ",cero(i)
   !enddo
   

end program main

subroutine fsub1(y,p,f)

   integer, intent(in) :: p
   real(8), dimension(0:p), intent(in) :: y
   real(8), dimension(0:p), intent(out) :: f
   do i=0,p
	f(i)=((y(i)*y(i))-2)
   enddo

end subroutine

subroutine fsub2(y,n,m,f)

   integer, intent(in) :: n
   real(8), intent(out) :: m
   real(8), dimension(0:n), intent(in) :: y
   real(8), dimension(0:n), intent(in) :: f 
   	
   do i=0,n
   	if ((f(i)*f(i+1))<0) then
   		m=((y(i+1)-y(i))*(-f(i))/(f(i+1)-f(i)))+y(i)
   		print*,f(i)," ",f(i+1)
   	endif
   enddo
   	

end subroutine


