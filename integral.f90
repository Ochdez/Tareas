program main

  !implicit none
  real(8) :: pi, dx, xmax, xmin
  real(8), allocatable, dimension(:) :: y,z,d,f
  integer :: m,n,p,r,h,area

  pi = acos(-1.0)
  print*,"Ingrese el punto a"
  read(*,*)a
  print*,"Ingrese el punto b"
  read(*,*)b
  print*,"Ingrese el numero de puntos"
  read(*,*)h
  p=h+1
  allocate(y(0:p),z(0:p),f(0:p))

  dx = (b-a)/p

  do i = 0, p
    y(i) = a + (i*dx) !Puntos donde se evaluara la funcion
  enddo

  n = size(y)

  call fsub1(y,n,z)
  print*,"=",z

  area = 0
  call fsub2(z,n,dx)


end program main

  subroutine fsub1(x,n,f)

    integer, intent(in) :: n
    real(8), dimension(n), intent(in) :: x
    real(8), dimension(n), intent(out) :: f
    f = x*x

  end subroutine

  subroutine fsub2(x,n,dx,area)
    integer, intent(in) :: n
    real(8), intent(in) :: dx
    real(8), dimension(n), intent(x)
    real(8), dimension(n), intent(area)

    do i=0,n
      if (z(i)=b) then
      area = area + dx*(z(i+1))
      endif
    enddo
