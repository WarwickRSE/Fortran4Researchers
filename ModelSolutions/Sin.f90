PROGRAM MAIN

  IMPLICIT NONE

  INTEGER :: i, sgn
  INTEGER, PARAMETER :: max_iter = 100
  REAL :: term, sin_approx, x

  x = 3.14159/ 8.0 ! Pi/16

  ! Keep a running sign for simplicity
  sgn = -1

  !I compute the first term explicitly here:
  term = x
  sin_approx = term

  ! Only need odd numbered terms so use loop stride for clarity
  DO i = 3, max_iter, 2
    ! A sneaky way to do less computation. Each term adds another factor of x**2 and
    ! 2 contributions to the factorial.
    ! I do not store factorial. Also I do the type conversions explicitly
    ! NB : turning an integer into a real CAN change its value. Doing it for i or even i*(i-1)
    ! will be fine for these values, but for an entire n! can change. But that only
    ! gives us a fractional error
    term = sgn * term * x**2 / REAL(i)/ REAL(i-1)

    sin_approx = sin_approx + term

    sgn = -1 * sgn

  END DO

  PRINT*, "An approximation to sin(x) for x=", x, " is: ", sin_approx

END PROGRAM
