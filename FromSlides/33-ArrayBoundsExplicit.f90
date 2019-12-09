MODULE arraymod
  IMPLICIT NONE
  CONTAINS
  SUBROUTINE test_array(lower, array_in)
    INTEGER, INTENT(IN) :: lower
    INTEGER, DIMENSION(lower:lower+9), INTENT(IN) :: array_in

    PRINT *,'Array is : ', SIZE(array_in), &
        LBOUND(array_in), UBOUND(array_in)

  END SUBROUTINE test_array
END MODULE arraymod

PROGRAM test

  USE arraymod
  IMPLICIT NONE

  INTEGER, DIMENSION(0:9) :: array
  CALL test_array(0, array)

END PROGRAM test
