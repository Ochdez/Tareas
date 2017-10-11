program main

  implicit none
  real(8) :: x,z
  real(8), external :: f

  x = 2.0d0
  print*,"Antes de f: x=",x
  z = f(x)
  print*,"Despued de f: x=",x
  print*,"z=", z

end program main


real(8) function f(x)

  implicit none
  real(8), intent(inout) :: x !inout, de entrada y salida
  f=x**2
  x = 5.0d0

end function f
