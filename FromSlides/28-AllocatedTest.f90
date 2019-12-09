PROGRAM alloc_test
  IMPLICIT NONE
  INTEGER, DIMENSION(:), ALLOCATABLE :: array

  PRINT *, 'Before allocation ', ALLOCATED(array)
  ALLOCATE(array(10))
  PRINT *, 'After allocation ', ALLOCATED(array)
  DEALLOCATE(array)
  PRINT *, 'After deallocation ', ALLOCATED(array)

END PROGRAM alloc_test
