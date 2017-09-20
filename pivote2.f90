program pivote2

  implicit none
  integer :: i,j,k,h,n
  real(8) :: a
  real :: M(1000)


  open(10,file="datos.dat")
  do i=1,1000
    read(10,*)h,a
    M(i) = a
  end do
  close(10)

  a = 0

	print*,"Ingrese el numero que desea usar como pivote:"
	read(*,*)n

	do i=n,1000,(2*(n-1))
    do k=(i-(n-1)), (i+(n-1))
      do j= (i-(n-1)), (i+(n-1))
        if(M(k) > M(j)) then
          a = M(k)
          M(k) = M(j)
          M(j) = a
        end if
      end do
    end do
  end do

  do i=1,1000
    print*,M(i)
  end do

	!open(11,file="orden_pivote.dat")
	!do i=1,1000
	!	a = M(i)
	!	write(11,*)i,a
	!enddo
	!close(11)


end program pivote2
