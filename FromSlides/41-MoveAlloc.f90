PROGRAM move_test

  IMPLICIT NONE
  INTEGER, DIMENSION(:), ALLOCATABLE :: a, b
  INTEGER :: i

  ALLOCATE(a(-5:10))
  DO i = -5, 10
    a(i) = i
  END DO

  CALL MOVE_ALLOC(FROM = a, TO = b)
  PRINT *, 'Bounds of b', LBOUND(b), UBOUND(b)
  PRINT *, 'Values of b', MINVAL(b), MAXVAL(b)
  PRINT *, 'Allocation state (a, b)', ALLOCATED(a), ALLOCATED(b)

END PROGRAM move_test
