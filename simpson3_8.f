program simpsons3_8
  implicit none
  real(8) :: a, b, integral
  integer :: n

  ! Input: interval [a, b] and number of subintervals n (must be a multiple of 3)
  print *, "Enter the lower limit (a):"
  read *, a
  print *, "Enter the upper limit (b):"
  read *, b
  print *, "Enter the number of subintervals (n), which must be a multiple of 3:"
  read *, n

  ! Check if n is a multiple of 3
  if (mod(n, 3) /= 0) then
     print *, "Error: The number of subintervals (n) must be a multiple of 3."
     stop
  endif

  ! Calculate the integral using Simpson's 3/8 Rule
  integral = simpsons_38_rule(a, b, n)

  ! Output the result
  print *, "The approximate value of the integral is: ", integral

contains

  ! Function to calculate the integral using Simpson's 3/8 Rule
  real(8) function simpsons_38_rule(a, b, n)
    real(8) :: a, b, h, sum, x
    integer :: i
    real(8), external :: f

    ! Calculate the step size h
    h = (b - a) / (3.0d0 * n)

    ! Initial sum with the first and last function values
    sum = f(a) + f(b)

    ! Sum the intermediate points with 3 weights
    do i = 1, n - 1
      x = a + i * 3 * h
      if (mod(i, 3) == 0) then
        sum = sum + 3.0d0 * f(x)
      else
        sum = sum + 3.0d0 * f(x)
      endif
    end do

    ! Multiply by 3h/8 to get the result
    simpsons_38_rule = 3.0d0 * h / 8.0d0 * sum
  end function

  ! Example function to integrate (change this to the desired function)
  real(8) function f(x)
    real(8) :: x
    f = x**2  ! Example function: f(x) = x^2
  end function

end program

