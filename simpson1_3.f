program simpsons_rule
    implicit none
    real(8) :: a, b, integral, h
    integer :: n, i
    real(8) :: sum_odd, sum_even, x, f_x

    ! Input the limits of integration and the number of subintervals
    print *, "Enter the lower limit a:"
    read *, a
    print *, "Enter the upper limit b:"
    read *, b
    print *, "Enter the number of subintervals n (even number):"
    read *, n

    ! Check if n is even, as required for Simpson's rule
    if (mod(n, 2) /= 0) then
        print *, "Number of subintervals must be even!"
        stop
    end if

    ! Calculate the step size h
    h = (b - a) / real(n)

    ! Initialize sums
    sum_odd = 0.0
    sum_even = 0.0

    ! Evaluate the sum for the odd and even indices
    do i = 1, n-1
        x = a + i * h
        f_x = f(x)
        if (mod(i, 2) == 0) then
            sum_even = sum_even + f_x
        else
            sum_odd = sum_odd + f_x
        end if
    end do

    ! Apply Simpson's 1/3 Rule
    integral = (h / 3.0) * (f(a) + f(b) + 4.0 * sum_odd + 2.0 * sum_even)

    ! Output the result
    print *, "The approximate value of the integral is: ", integral

contains

    ! Define the function to integrate, f(x) = x^2
    real(8) function f(x)
        real(8), intent(in) :: x
        f = x**2
    end function f

end program simpsons_rule

