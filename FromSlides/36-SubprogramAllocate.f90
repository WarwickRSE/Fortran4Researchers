MODULE arraymod
  IMPLICIT NONE
  CONTAINS
  SUBROUTINE test_array(array_in)
    INTEGER, DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: array_in

    IF (ALLOCATED(array_in)) THEN
      PRINT *, 'In function ', SIZE(array_in), LBOUND(array_in), &
          UBOUND(array_in)
      DEALLOCATE(array_in)
    END IF

    ALLOCATE(array_in(-10:10))

  END SUBROUTINE test_array
END MODULE arraymod

PROGRAM test

  USE arraymod
  IMPLICIT NONE

  INTEGER, DIMENSION(:), ALLOCATABLE :: array
  ALLOCATE(array(0:9))
  CALL test_array(array)
  PRINT *, 'After function ', SIZE(array), LBOUND(array), UBOUND(array)

END PROGRAM test
