program fibonacci_series
    implicit none
    integer :: n,i,a,b,c
    print *, 'Enter the number of terms :'
    read *, n
    a=1
    b=2
    print *,"Fibonacci series : "
    if (n>=1) then
     print *,a
    end if
    if (n>=2)then
     print *,b
    end if
    do i=3, n
     c=a+b
     print *,c
     a=b
     b=c
    end do
end program fibonacci_series

