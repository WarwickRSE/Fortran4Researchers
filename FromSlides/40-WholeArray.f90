PROGRAM whole_array

  IMPLICIT NONE
  REAL, PARAMETER :: pi = 3.14159
  REAL, DIMENSION(10) :: a, b
  INTEGER :: i

  !Set all elements of a to be pi
  a = pi
  !Scale a so that the elements go pi/2, pi, 3pi/2 etc
  DO i = 1, SIZE(a)
    a(i) = a(i) * REAL(i)/2.0
  END DO
  b = SIN(a)
  PRINT *, b

END PROGRAM whole_array
