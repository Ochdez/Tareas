program main

  integer :: n,p,h
  real(8) :: pi,l,dx,a
  real(8), allocatable, dimension(:) :: x,f,bn,an,g,w,m

  pi = acos(-1.0)
  print*,"Ingrese el numero de puntos: "
  read(*,*)h
  p=h+1
  l=pi
  allocate(x(0:p),f(0:p),an(1:1000),bn(1:1000),w(0:p),m(0:p),g(0:p))

  pi = acos(-1.0)
  dx = (2*l/p)

  do i = 0, p
    x(i) = -l + (i*dx)
  enddo

  open(10,file="fun.dat")
  do i=0, p
    if (x(i) <= -2)then
      f(i) = 0
    else if (x(i) > -2 .and. x(i)<2) then
      f(i) = 3
    else if (x(i) > 2) then
      f(i) = -3
    endif
    write(10,*)i,f(i)
  enddo
  close(10)

  do i=0,p
    a = a+(dx*f(i)/l)
  enddo
  print*,"a: ",a

  do i=1,1000
    do j=0,p

      an(i)=(((f(j)*cos(i*pi*x(j)/l))/l)*dx)+an(i)

      bn(i)=(((f(j)*sin(i*pi*x(j)/l))/l)*dx)+bn(i)

    enddo
  enddo

  print*,an
  print*,"---------------------------------"
  print*,bn

  do i=0,p
    w(i)=0
    m(i)=0
    do j=1,1000
      w(i) = (an(j)*cos(j*pi*x(i)/l))+w(i)
      m(i) = (bn(j)*sin(j*pi*x(i)/l))+m(i)
    enddo
  enddo

  open(20,file="fourier.dat")
  do i=0,p
    g(i) =  w(i) + m(i)
    write(20,*)i,(g(i)+a/2)
  enddo
  close(20)


end program main
