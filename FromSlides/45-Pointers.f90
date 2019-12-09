PROGRAM pointer_test

  IMPLICIT NONE
  INTEGER, TARGET :: actual_value
  INTEGER, POINTER :: ptr

  ptr => NULL()
  PRINT *,'Association test before', ASSOCIATED(ptr)
  actual_value = 5
  ptr => actual_value
  PRINT *,'Association test after', ASSOCIATED(ptr)
  PRINT *,'Association test specific', ASSOCIATED(ptr, &
      TARGET=actual_value)
  PRINT *,'Pointer value after pointing is ', ptr
  ptr = 10

  PRINT *, 'Actual value after changing pointer is ', actual_value

END PROGRAM pointer_test
