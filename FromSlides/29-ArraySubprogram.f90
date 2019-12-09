MODULE arraymod
  IMPLICIT NONE
  CONTAINS
  SUBROUTINE test_array(array_in)
    INTEGER, DIMENSION(10), INTENT(IN) :: array_in

    PRINT *,'Array is : ', SIZE(array_in), &
        LBOUND(array_in), UBOUND(array_in)

  END SUBROUTINE test_array
END MODULE arraymod

PROGRAM test

  USE arraymod
  IMPLICIT NONE

  INTEGER, DIMENSION(10) :: array
  CALL test_array(array)

END PROGRAM test
