program main

  !implicit none
  real(8) :: pi, l, xmax, xmin
  real(8), allocatable, dimension(:) :: y,z,d,f
  integer :: m,n,p,r

	pi = acos(-1.0)
  print*,"Ingrese un entero multiplo de PI para x minima"
  read(*,*)m
  print*,"Ingrese un entero multiplo de PI para x maxima"
  read(*,*)n
  print*,"Ingrese el numero de puntos"
  read(*,*)p
  allocate(y(0:p),z(0:p),d(0:p),f(0:p))
	print*,"¿Cuantas veces deseas derivar la funcion? "
	read(*,*)r

  xmin = m*pi
  xmax = n*pi
  l = (xmax-xmin)/p

  do i =0, p
    y(i) = xmin + (i*l)
  enddo

  n = size(y)

  call fsub1(y,n,z)
  print*,"z=",z

	!open(10,file="datos.dat") !!
  !  do i=0,p
  !    write(10,*)z(i)
  !  enddo
  !close(10)

	do i=1, r
		n=n-1
  	call fsub2(z,d,n,l)
  	print*,"derivada:",i
		print*,d
		z=d
	enddo

	!open(11,file="derivada.dat") !!
  !  do i=0,p
  !    write(11,*)d(i)
  !  enddo
  !close(11)


end program main

  subroutine fsub1(x,n,f)

    integer, intent(in) :: n
    real(8), dimension(n), intent(in) :: x
    real(8), dimension(n), intent(out) :: f
    f = sin(x)

  end subroutine

  subroutine fsub2(x,d,n,l)

    integer, intent(in) :: n
    real(8), intent(in) :: l
    real(8), dimension(n), intent(in) :: x
    real(8), dimension(n), intent(out) :: d
    do i=0,n
			d(i) = (x(i+1)-x(i))/l
		enddo
  end subroutine
