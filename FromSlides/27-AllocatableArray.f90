PROGRAM array_test
  IMPLICIT NONE
  INTEGER, DIMENSION(:), ALLOCATABLE :: array
  INTEGER :: array_count

  array_count = 10
  ALLOCATE(array(array_count))
  PRINT *, 'V1 :', SIZE(array), LBOUND(array), UBOUND(array)
  DEALLOCATE(array)
  ALLOCATE(array(0:array_count-1))
  PRINT *, 'V2 :', SIZE(array), LBOUND(array), UBOUND(array)
  DEALLOCATE(array)

END PROGRAM array_test
