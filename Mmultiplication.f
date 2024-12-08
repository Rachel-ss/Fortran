program matrix_multiplication
    implicit none
    integer :: i, j, k
    integer :: m, n, p
    real, allocatable :: A(:,:), B(:,:), C(:,:)
    print *, 'Enter the dimensions of matrix A (m x n):'
    read *, m, n
    print *, 'Enter the number of columns of matrix B (n x p):'
    read *, p
    allocate(A(m, n), B(n, p), C(m, p))
    print *, 'Enter the elements of matrix A:'
    do i = 1, m
        do j = 1, n
            read *, A(i, j)
        end do
    end do
    print *, 'Enter the elements of matrix B:'
    do i = 1, n
        do j = 1, p
            read *, B(i, j)
        end do
    end do
    C = 0.0
    do i = 1, m
        do j = 1, p
            do k = 1, n
                C(i, j) = C(i, j) + A(i, k) * B(k, j)
            end do
        end do
    end do
    print *, 'The resulting matrix C is:'
    do i = 1, m
        print *, (C(i, j), j = 1, p)
    end do

end program matrix_multiplication

