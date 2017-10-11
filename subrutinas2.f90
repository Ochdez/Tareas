program main
  implicit none
  real(8), dimension (3) :: y,z
  integer :: n

  y(1)=2.0d0
  y(2)=3.0d0
  y(3)=4.0d0
  n = size(y) !nos dice el tamaño del arreglo

  call fsub(y,n,z)
  print*,"z =",z
end program main

  subroutine fsub(x,n,f)
    implicit none
    integer, intent(in) :: n
    real(8), dimension(n), intent(in) :: x
    real(8), dimension(n), intent(out) :: f
    f = x**2
  end subroutine
