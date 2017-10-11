program main

  implicit none
  real(8) :: y,z
  y = 2.0d0
  call fsub(y,z)
  print*,"z=",z

end program main

  subroutine fsub(x,f) !y se relaciona con x y z con f,
    implicit none
    real(8), intent(in) :: x
    real(8), intent(out) :: f

    f=x**2
  end subroutine fsub  
