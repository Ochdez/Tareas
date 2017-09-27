program ordenamiento

  implicit none
  integer :: i,j,k
  real(8) :: a
  real :: M(10)

  open(10,file="datos.dat")
  do i=1,10
    read(10,*)k,a
    M(i)=a
  enddo
  close(10)

  a=0

  do i=1,10
    do j=1,10
      if (M(i) > M(j)) then
        a = M(i) !Recuerda el mayor
        M(i) = M(j) !Manda el menor una posición anterior. Ahora el pivote será el menor de los dos numeros.
        M(j) = a !La posicion que antes era del menor le corresponde al mayor
      endif
    enddo
  enddo

  do i=1,10
    print*,M(i)
  enddo

  open(11,file="ordenados.dat")
  do i=1,10
    a = M(i)
    write(11,*)i,a
  enddo
  close(11)


end program ordenamiento
