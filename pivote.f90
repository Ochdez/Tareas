program pivote

  implicit none
  integer :: i,j,k,h
  real(8) :: a
  real :: M(100)

  open(10,file="datos.dat")
  do i=1,100
    read(10,*)h,a
    M(i) = a
  enddo
  close(10)

  a=0

  do i=4,100,6
    do k=(i-3), (i+3)
      do j= (i-3), (i+3)
        if(M(k) > M(j)) then
          a = M(k)
          M(k) = M(j)
          M(j) = a
        endif
      enddo
    enddo
  enddo

  do i=1,100
    print*,M(i)
  enddo






end program pivote
