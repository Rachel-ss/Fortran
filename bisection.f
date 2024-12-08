program bisection_method
    implicit none
    real :: a, b, c, error, f_a, f_b, f_c
    integer :: iter, max_iter


    error = 0.00001
    max_iter = 1000
    iter = 0


    print *, 'Enter the value of a :'
    read *, a
    print *, 'Enter the value of b :'
    read *, b


    f_a = f(a)
    f_b = f(b)


    if ((f_a * f_b) > 0.0) then
        print *, "The function does not change signs in the given interval. Please try a different interval."
        stop
    end if


    do while (iter < max_iter)
        c = (a + b) / 2.0
        f_c = f(c)


        if (abs(f_c) < error .or. (b - a) / 2.0 < error) then
            print *, "The root is: ", c
            print *, "Function value at root: ", f_c
            print *, "Number of iterations: ", iter
            stop
        end if


        if ((f_a * f_c) < 0.0) then
            b = c
            f_b = f_c
        else
            a = c
            f_a = f_c
        end if


        iter = iter + 1
    end do


    print *, "The method did not converge within the maximum number of iterations."
    print *, "Number of iterations: ", iter

contains


    real function f(x)
        real, intent(in) :: x
        f = (x**2) - 24.0
    end function f

end program bisection_method

