program main

  !implicit none
  real(8) :: pi, dx, xmax, xmin, integral,a,b
  real(8), allocatable, dimension(:) :: y,z,f
  integer :: p,h,n
  
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
    y(i) = a + (i*dx) 
  enddo
 print*,y
  n = size(y)

  call fsub1(y,n,z)
  print*,"=",z

  
  call fsub2(z,n,dx,integral)
  print*,"integral=", integral


end program main

  subroutine fsub1(x,n,f)

    integer, intent(in) :: n
    real(8), dimension(0:n), intent(in) :: x
    real(8), dimension(0:n), intent(out) :: f
    do i=0,n
	f(i)=x(i)*x(i)
    enddo
	

  end subroutine

  subroutine fsub2(w,n,dx,area)
    integer, intent(in) :: n
    real(8), intent(in) :: dx
    real(8), intent(out) :: area
    real(8), dimension(0:n), intent(in) :: w 
    area=0
    do i=0,n
        area = area+(dx*w(i))
    enddo
  end subroutine  
