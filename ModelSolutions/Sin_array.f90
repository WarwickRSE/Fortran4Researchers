PROGRAM main

  IMPLICIT NONE

  REAL :: x_min, x_max, dx, scale_fac
  REAL, PARAMETER :: pi = 3.14159
  REAL, DIMENSION(:), ALLOCATABLE :: sin_x, x
  INTEGER :: i, n_bins

  n_bins = 1000

  ! ALLOCATE the axis and data array
  ! You don't have to use produce this axis, but I chose to
  ALLOCATE(x(n_bins), sin_x(n_bins))

  ! Using a variable for this is clearer, and slightly more efficient
  scale_fac = 2.0 * pi

  ! We can make the x-axis several ways.
  ! I chose to pre-fill it
  x_min = 0.0
  x_max = 1.0
  dx = (x_max - x_min) / REAL(n_bins + 1)

  DO i = 1, n_bins
    x(i) = (x_min + dx * i) * scale_fac
    sin_x(i) = SIN(x(i))
  END DO

  ! Find the changes of sign
  DO i = 1, n_bins-1
    !A few ways to find the relative sign-
    ! Multiply the values. What happens if they're very small? Is this OK?
    ! Use the SIGN function to transfer sign without risking underflows
    ! Do the explicit comparisons (a < 0 and b > 0  or ... Beware of operator order!)
    IF(sin_x(i) * sin_x(i+1) < 0) THEN
      PRINT*, "Sign change at ", x(i)/pi, " pi"
    END IF
  END DO


END PROGRAM
