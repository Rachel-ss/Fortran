program find_smallest_largest
    implicit none
    integer :: n,i
    real :: smallest,largest
    real, allocatable :: arr(:)
    print *, 'Enter the number of elements in the array :'
    read *, n
    allocate(arr(n))
    print *, 'Enter the elements of the array :'
    do i = 1, n
            read *, arr(i)
    end do
    smallest=arr(1)
    largest=arr(1)
    do i = 2, n
        if (arr(i)<smallest)then
          smallest=arr(i)
        end if
        if (arr(i)>largest)then
          largest=arr(i)
        end if
    end do
    print *, "The smallest value is : ",smallest
    print *, "The largest value is : ",largest
    deallocate(arr)
end program find_smallest_largest
