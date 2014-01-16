program ex2

integer, parameter ::  n=10
real(kind=8) :: g

real(kind=8) :: a(n), b(n), x(n), y(n), temp(n)
real(kind=8) :: M(n,n)
real(kind=8) :: alpha

write(*,*) "Enter factor gamma"
read(*,*) g 

! initialize vectors a, b and matrix M with random numbers between 0 and 1

do i = 1,n
    a(i) = rand(0)
    b(i) = rand(0)
    do j = 1,n
        M(j,i) = rand(0)
    enddo
enddo

! calculate x and y

do i = 1,n
   alpha = 0.0d0
   x(i) = a(i) + g*b(i)
   temp(i) = 0.0d0
   do j = 1,n
      temp(i) = temp(i) + M(i,j)*b(j)
    enddo
    y(i) = a(i) + temp(i)
    alpha = alpha + a(i)*b(i)
enddo

! Print outputs
write(*,*) 'Gamma',g,'dimension',n
write(*,*) 'Vectors a and b'
do i = 1,n
   write(*,*)  a(i), b(i)
enddo

write(*,*) 'Vectors x and y'
do i = 1,n
   write(*,*) x(i), y(i)
enddo

write(*,*) 'Alpha=', alpha

end program

