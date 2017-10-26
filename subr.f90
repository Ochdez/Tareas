program main

  !implicit none
  real(8) :: l, xmax, xmin
  real(8), allocatable, dimension(:) :: y,z,d,f
  integer :: p,r,n,m,w

  print*,"Ingrese un punto minimo"
  read(*,*)xmin
  print*,"Ingrese un punto maximo"
  read(*,*)xmax
  print*,"Ingrese el numero de puntos"
  read(*,*)p
  allocate(y(0:p),z(0:p),d(0:p),f(0:p))
  print*,"¿Cuantas veces deseas derivar la funcion? "
  read(*,*)r

  l = (xmax-xmin)/p

  do i =0, p
    y(i) = xmin + (i*l)
  enddo

  n = size(y)

  call fsub1(y,n,z)
  print*,"z=",z

  m=n
  do i=1, r
     m=m-1
     print*,"m="
     print*,m
     call fsub2(z,d,n,m,l)
     print*,"derivada:"
     print*,i
     print*,d
     z=d
  enddo

end program main

  subroutine fsub1(x,n,f)

    integer, intent(in) :: n
    real(8), dimension(n), intent(in) :: x
    real(8), dimension(n), intent(out) :: f
    f = sin(x)

  end subroutine

  subroutine fsub2(x,d,n,m,l)

    integer, intent(in) :: n
    integer, intent(in) :: m
    real(8), intent(in) :: l
    real(8), dimension(n), intent(in) :: x
    real(8), dimension(m), intent(out) :: d

    do i=0,m
      if (i < m) then
        d(i) = (x(i+1)-x(i))/l
      else
        d(i) = 0
        print*,
      endif
    end do

  end subroutine
