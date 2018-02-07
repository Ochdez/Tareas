program main

  real(8) :: pi, dx, xmax, xmin, integral,a,b
  real(8), allocatable, dimension(:) :: y,z,f
  integer :: p,h,n,r

  pi = acos(-1.0)
  print*,"Ingrese 1 si su función es trigonometria y otro entero si no lo es"
  read(*,*)r
  print*,"Ingrese el punto a"
  read(*,*)a
  print*,"Ingrese el punto b"
  read(*,*)b
  print*,"Ingrese el numero de puntos"
  read(*,*)h
  p=h+1
  allocate(y(0:p),z(0:p))

  if (r==1) then
    dx = ((b*pi)-(a*pi))/p
  else
    dx = (b-a)/p
  endif

  do i = 0, p
    y(i) = a + (i*dx)
  enddo

  call fsub1(y,p,z)

  open(20,file="Funcion.dat")
    do i=0,p
	write(20,*)y(i),z(i)
    enddo
  close(20)


  call fsub2(z,p,dx,integral)
  print*,"integral=", integral

  open(10,file="datos.dat")
	write(10,*)h,integral
  close(10)


end program main

  subroutine fsub1(x,p,f)

    integer, intent(in) :: p
    real(8), dimension(0:p), intent(in) :: x
    real(8), dimension(0:p), intent(out) :: f
    do i=0,p
	     f(i)=sin(x(i))
    enddo


  end subroutine

  subroutine fsub2(w,p,dx,area)
    integer, intent(in) :: p
    real(8), intent(in) :: dx
    real(8), intent(out) :: area
    real(8), dimension(0:p), intent(in) :: w
    area=0
    do i=0,p
      if (w(i)<0) then
        area = area+(dx*w(i)*(-1.0))
      else
        area = area+(dx*w(i))
      endif
    enddo
  end subroutine
