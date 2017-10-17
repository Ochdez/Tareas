program main

  !implicit none
  real(8) :: pi, l, xmax, xmin
  real(8), dimension(0:10) :: y,z,c
  integer :: n, p

  pi = acos(-1.0)
  xmin=0.0
  xmax=2*pi
  p = 10
  l = (xmax-xmin)/p

  do i =0, p
    y(i) = xmin + (i*l)
  enddo

  n = size(y)

  call fsub(y,n,z)
  print*,"z=",z

  call fsub1(y,n,c,l)
  print*,"fp=",c


end program main

  subroutine fsub(x,n,f)

    integer, intent(in) :: n
    real(8), dimension(n), intent(in) :: x
    real(8), dimension(n), intent(out) :: f
    f = sin(x)
  end subroutine

  subroutine fsub1(x,n,f,l)

    integer, intent(in) :: n
    real(8), intent(in) :: l
    real(8), dimension(n), intent(in) :: x
    real(8), dimension(n), intent(out) :: f
    f = (sin(x+l)-sin(x))/l
  end subroutine
