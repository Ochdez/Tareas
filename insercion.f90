program insercion

  implicit none
  integer :: i,j,h,n
  real(8) :: a
  real, allocatable, dimension (:) :: M
  
  print*,"Indique el numero de datos que desea ordenar: "
  read(*,*)n
  allocate(M(1:n))

  open(10,file="datos.dat")
  do i=1,n
    read(10,*)h,a
    M(i) = a
  enddo

  do i=2,n  !elemento que se quiere ordenar adecuadamente
    a = M(i) !Recuerda el valor que se va a comparar
    j = i-1
    do while (M(j) < a .and. j>0 ) !Va comparando "a" con los elementos anteriores.
      M(j+1) = M(j)  !Hace el desplazamiento si se cumplen las condiciones.
      j=j-1 !Va reduciendo j hasta llegar a cero
    enddo     !Al no darse la condición el valor del i inicial guardado en a se coloca en
    M(j+1) = a !Hace el cambio. El moyor correspondera al primer elemento. j=1
  enddo

  do i=1,n
    print*,M(i)
  enddo

  open(20,file="orden_insercion.dat")
  do i=1,n
    a = M(i)
    write(20,*)i,a
  enddo

  close(10)
  close(20)

end program insercion
