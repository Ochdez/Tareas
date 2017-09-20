program num_aleatorios

  implicit none
  integer :: i
  real(8) :: a
  integer, dimension(8) :: values !dimension 8
  integer :: seed !semilla

  !instrucciones para generar num aleatorios

  call date_and_time(VALUES=values) !call llama a una rutina relacionada con fecha y hora. VALUES es una de las funciones de la rutina
  seed = values(8)
  call srand(seed)
  !genera numeros al azar realcionados con el reloj de la computadora

  open(10,file="datos.dat")

  do i=1, 10000
     a = rand()
     print*,i,a
     write(10,*)i,a
  enddo

end  program num_aleatorios
