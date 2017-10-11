program main

  implicit none
  real(8) :: x,z
  real(8), external :: f

  x = 2.0d0
  z = f(x)
  print*,"z=", z

end program main


real(8) function f(x)

  implicit none
  real(8), intent(in) :: x !in, porque es solo de entrada, no permite modificar el valor de x.
  f=x**2

end function f
