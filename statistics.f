program statistics
    implicit none
    integer :: n,i
    real ::mean,geo_mean,sd,skew,kurt
    real,allocatable :: x(:)
    real :: sum,product,variance,sum_skew,sum_kurt,term
    real :: dev
    print *, 'Enter the number of terms :'
    read *, n
    allocate(x(n))
    print *, "Enter the numbers :"
    do i=1, n
     read *,x(i)
    end do
    sum=0.0
    do i=1, n
     sum=sum+x(i)
    end do
    mean=sum/n
    product=1.0
    do i=1, n
     product=product*x(i)
    end do
    geo_mean=product**(1.0/n)
    variance=0.0
    do i=1, n
     variance=variance+(x(i)-mean)**2
    end do
    sd=sqrt(variance/n)
    sum_skew=0.0
    do i=1, n
     dev=(x(i)-mean)/sd
     sum_skew=sum_skew+dev**3
    end do
    skew=(n/((n-1)*(n-2)))*sum_skew
    sum_kurt=0.0
    do i=1,n
     dev=(x(i)-mean)/sd
     sum_kurt=sum_kurt+dev**4
    end do
    kurt=(n*(n+1)/((n-1)*(n-2)*(n-3)))*sum_kurt-(3*(n-1)**2/((n-2)*(n-3)))
    print *,"Arithmetic mean : ",mean
    print *,"Geometric mean : ",geo_mean
    print *,"Standard deviation : ",sd
    print *,"Skewness : ",skew
    print *,"Kurtosis : ",kurt
end program statistics
