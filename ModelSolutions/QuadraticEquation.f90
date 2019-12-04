
PROGRAM main

  IMPLICIT NONE

  REAL :: a, b, c, root1, root2
  REAL :: determinant

  PRINT*, "Enter coefficients for polynomial a x^2 + b x + c: "

  READ(*, *) a, b, c

  determinant = b**2 - 4.0 * a * c

  ! NOTE: we can't decide if the polynomial has only ONE root because
  ! we can't be sure if the determinant is exactly 0 or just too small
  ! to store in our real.

  IF(determinant < 0) THEN
    PRINT*, "The polynomial has no real roots"
  ELSE
    root1 = -b + sqrt(determinant)
    root2 = -b - sqrt(determinant)
    PRINT*, "The roots are ", root1, " and ", root2
  END IF

END PROGRAM
