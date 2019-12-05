PROGRAM MAIN

  IMPLICIT NONE

  INTEGER :: i, sgn
  INTEGER, PARAMETER :: max_iter = 100
  REAL :: term, sin_approx, x, last_approx
  REAL :: threshold

  x = 3.14159/ 3.0 ! Pi/16

  threshold = 0.1/100.0 !0.1%

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
    term = sgn * term * x**2 / REAL(i)/ REAL(i-1)

    sin_approx = sin_approx + term

    ! Check fractional change and terminate if below threshold
    IF(ABS((sin_approx-last_approx)/sin_approx) < threshold) THEN
      EXIT
    END IF

    last_approx = sin_approx
    sgn = -1 * sgn

  END DO

  PRINT*, "An approximation to sin(x) for x=", x, " is: ", sin_approx

  ! The value of 'i' after the loop is well defined and is the last iteration we ran
  PRINT*, "Convergence took ", i, " iterations"

END PROGRAM
