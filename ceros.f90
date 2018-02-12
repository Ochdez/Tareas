program main

   integer :: p,h,r
   real(8) :: pi, dx,m,b,a,c,ds,t
   real(8), allocatable, dimension(:) :: x,z,g

   pi = acos(-1.0)
   ds = 0.01
   print*,"Su función es trigonometrica? si-1"
   read(*,*)r
   print*,"Ingrese punto a: "
   read(*,*)a
   print*,"Ingrese punto b: "
   read(*,*)b
   print*,"Ingrese numero de puntos entre a y b: "
   read(*,*)h
   p=h+1
   allocate(x(0:P),z(0:p),g(0:100))

   if (r==1)then
   	dx = (b*pi-a*pi)/p
   else
   	dx = (b-a)/p
   endif

   do i = 0, p
     x(i) = a + (i*dx)
   enddo

   call fsub1(x,p,z)

   open(20,file="Funcion.dat")
    do i=0,p
	  write(20,*)x(i),z(i)
    enddo
   close(20)

   call fsub2(x,n,c,z,ds,g,t)

end program main

subroutine fsub1(y,p,f)

   integer, intent(in) :: p
   real(8), dimension(0:p), intent(in) :: y
   real(8), dimension(0:p), intent(out) :: f
   do i=0,p
      f(i)=(y(i)**2)-2
   enddo

end subroutine

subroutine fsub2(x,p,m,f,ds,g,t)

   integer, intent(in) :: p
   real(8), intent(out) :: m
   real(8), intent(in) :: ds
   real(8), intent(out) :: t
   real(8), dimension(0:p), intent(in) :: x
   real(8), dimension(0:p), intent(in) :: f
   real(8), dimension(0:100), intent(out) :: g

   do i=0,p
   	if ((f(i)*f(i+1))<0) then
   		m=((x(i+1)-x(i))*(-f(i))/(f(i+1)-f(i)))+x(i)
      print*, f(i),f(i+1)
      print*,m

      do j=0,100
        g(j)=((x(i+1)-(x(i)+ds*j))*(-(((x(i)+ds*j)**2)-2))/(f(i+1)-(((x(i)+ds*j)**2)-2)))+(x(i)+ds*j)
        !g(j)=((x(i)+ds*j)**2)-2
        if ( (((x(i)+ds*j)**2)-2)>=0) then
            exit
        endif
        t = g(j)
        !t=(x(i)+ds*j)
        print*,"Aproximacion", j+1," de x = ",t
      enddo

      exit
   	endif
   enddo

end subroutine
