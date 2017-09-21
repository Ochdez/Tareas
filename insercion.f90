program insercion

  implicit none
  integer :: i,j,h
  real(8) :: a
  real :: M(10)

  open(10,file="datos.dat")
  do i=1,10
    read(10,*)h,a
    M(i) = a
  enddo
  close(10)

  a = 0

  do i=2,10  !elemento que se quiere ordenar adecuadamente
    a = M(i)
    j = i-1
    do while (M(j) > a .and. j>0)
      M(j+1) = M(j)  !Hace el desplazamiento
      !M(i) = a
      j=j-1
       if (j==0) exit
    enddo
    M(j+1) = a
  enddo

  do i=1,10
    print*,M(i)
  enddo


end program insercion
