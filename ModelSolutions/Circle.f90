
PROGRAM main

  IMPLICIT NONE

  REAL :: radius


  PRINT*, "Enter radius of circle and press Enter"

  READ(*, *) radius

  PRINT "(A, F8.3, A)", "The area is ", 3.14159*radius**2, " square units"

END PROGRAM
