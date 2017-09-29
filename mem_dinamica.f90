program mem_dinamica

  implicit none
  integer :: l,m,n,q,d,s,p,i,j,k
  real, allocatable, dimension(:,:) :: x,y,z


  print*,'PORGRAMA PARA REALIZAR SUMA O PRODUCTO DE MATRICES'
  print*,'Ingrese las filas que tendra su primer atriz: '
  read(*,*)l
  print*,'Ingrese las columnas que tendra su primer matriz: '
  read(*,*)m
   print*,'Ingrese las filas que tendra su segunda matriz'
  read(*,*)q
  print*,'Ingrese las columnas que tendra su segunda matriz'
  read(*,*)n
  allocate(x(1:l,1:m),y(1:q,1:n),z(1:l,1:n))
  
  print*,'Ingrese los elementos de la primer matriz: '
  do i = 1, l
     do j = 1, m
       read(*,*)s
       x(i,j) = s
     end do
  end do


  print*,'ingrese los elementos de sus segunda matriz: '
  do i=1, q
     do j=1, n
       read(*,*)p
       y(i,j) = p
     end do
  end do

  print*,'¿Desea realizar suma o producto? Ingrese 1 si desea suma y cualquier otro entero si desea producto'
  read(*,*)d

  if(d == 1) then

    if(l /= q .or. m /= n) then
      print*,'La suma no puede realizarse. El numero de filas y columnas deben coincidir'

    else

        z = x + y
        print*,'La suma es: ',z
    end if


 else

     if (m /= q) then
        print*,'No es posible realizar el producto.'

     else

        print*,'El producto de sus matrices es: '
        do i = 1, l
           do j = 1, n
              do k = 1, m
                 z(i,j)=z(i,j)+x(i,k)*y(k,j)
              end do
           end do
        end do
        print*,z

     end if

  end if



end program mem_dinamica
