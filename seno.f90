program uno

  !implicit none
  real(8) :: pi, l, xmax, xmin
  real(8), dimension(30) :: y,z
  integer :: n, p

  pi = acos(-1.0)
  xmin=0
  xmax=2*pi
  p = 30
  l = (xmax-xmin)/p

  do i =0, p
    y(i) = m + (i*l)
  enddo

  n = size(y)

  call fsub(y,n,z)
  print*,"z=",z

end program uno

  subroutine fsub(x,n,f)
    integer, intent(in) :: n
    real(8), dimension(n), intent(in) :: x
    real(8), dimension(n), intent(out) :: f
    f = sin(x)
  end subroutine
