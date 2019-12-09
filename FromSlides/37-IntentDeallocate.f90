MODULE arraymod
  IMPLICIT NONE
  CONTAINS
  SUBROUTINE test_array(array_in)
    INTEGER, DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: array_in

    PRINT *, 'In function, is array allocated :', ALLOCATED(array_in)
    ALLOCATE(array_in(-10:10))

  END SUBROUTINE test_array
END MODULE arraymod

PROGRAM test

  USE arraymod
  IMPLICIT NONE

  INTEGER, DIMENSION(:), ALLOCATABLE :: array
  ALLOCATE(array(0:9))
  CALL test_array(array)
  PRINT *,'After array is array allocated :', ALLOCATED(array)
  PRINT *,'Shape is', SIZE(array), LBOUND(array), UBOUND(array)

END PROGRAM test
