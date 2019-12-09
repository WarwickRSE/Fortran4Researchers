PROGRAM pointer_alloc

  IMPLICIT NONE
  INTEGER, POINTER :: iptr

  ALLOCATE(iptr)
  iptr = 10

  PRINT *, 'iptr is ', iptr
  DEALLOCATE(iptr)

END PROGRAM pointer_alloc
