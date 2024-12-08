program sort_array
    implicit none
    integer :: n,i,j,temp
    character(len=1) :: order
    real, allocatable :: arr(:)
    print *, 'Enter the number of elements in the array :'
    read *, n
    allocate(arr(n))
    print *, 'Enter the elements of the array :'
    do i = 1, n
            read *, arr(i)
    end do
    print *, "Do you want to sort in ascending(A) or descending(D) order?(A/D)"
    read *,order
    if (order=='A') then
     do i = 1, n-1
      do j=1, n-i
        if (arr(j)>arr(j+1))then
          temp=arr(j)
          arr(j)=arr(j+1)
          arr(j+1)=temp
        end if
      end do
    end do
    else if(order=='D') then
     do i = 1, n-1
      do j=1, n-i
        if (arr(j)<arr(j+1))then
          temp=arr(j)
          arr(j)=arr(j+1)
          arr(j+1)=temp
        end if
      end do
     end do
    end if
    print *, "The sorted array : "
     do i=1, n
     print *,arr(i)
     end do
    deallocate(arr)
end program sort_array

