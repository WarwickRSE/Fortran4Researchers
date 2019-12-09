MODULE eltest

  IMPLICIT NONE

  CONTAINS
  ELEMENTAL SUBROUTINE double(a)
    !NOTE this function takes a single scalar NOT an array
    INTEGER, INTENT(INOUT) :: a
    a = a * 2
  END SUBROUTINE double

END MODULE eltest

PROGRAM elemental_prog

  USE eltest
  IMPLICIT NONE
  INTEGER, DIMENSION(0:9, -1:10) :: array

  array = 1
  CALL double(array)
  PRINT *, MINVAL(array)

END PROGRAM elemental_prog
