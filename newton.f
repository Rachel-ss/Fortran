program newton_raphson
    implicit none
    real(8) :: x, x_new, f_x, df_x, tol
    integer :: max_iter, iter
    tol = 1.0E-6
    max_iter = 1000
    iter = 0

    print *, 'Enter the initial guess for the root:'
    read *, x

    do while (iter < max_iter)

        f_x = f(x)
        df_x = df(x)


        if (abs(df_x) < tol) then
            print *, 'Derivative is too small, method may fail.'
            stop
        end if


        x_new = x - f_x / df_x


        if (abs(x_new - x) < tol) then
            print *, 'The root is: ', x_new
            print *, 'Function value at root: ', f_x
            print *, 'Number of iterations: ', iter
            stop
        end if


        x = x_new
        iter = iter + 1
    end do


    print *, 'The method did not converge within the maximum number of iterations.'
    print *, 'Number of iterations: ', iter

contains


    real function f(x)
        real(8), intent(in) :: x
        f = (x**2) - 24.0
    end function f


    real function df(x)
        real(8), intent(in) :: x
        df = 2.0 * x
    end function df

end program newton_raphson

