program grafica

  !implicit none
  integer :: m,n,p
  real(8) :: pi,l,xmax
  real, allocatable, dimension(:) :: f, x

  pi = acos(-1.0)
  print*,"Ingrese x minima"
  read(*,*)m
  print*,"Ingrese un entero multiplo de PI"
  read(*,*)n
  print*,"Ingrese el numero de puntos"
  read(*,*)p
  allocate(x(0:p),f(0:p))

  xmax = n*pi
  l = (xmax-m)/p

  do i =0, p
    x(i) = m + (i*l)
    print*,x(i)
  enddo

  do i=0, p
    f(i) = sin(x(i))
      print*,i,f(i)
  enddo

  open(10,file="datos.dat")
    do i=0,p
      write(10,*)f(i)
    enddo
  close(10)




end program grafica
