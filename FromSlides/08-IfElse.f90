PROGRAM block_if

  IMPLICIT NONE
  INTEGER :: test, candidate

  test = 2
  candidate = 4

  IF (test == candidate) THEN
   PRINT *, 'Test ', test, ' equal to candidate ', candidate
  ELSE
    PRINT *, 'Test ', test, ' not equal to candidate ', &
        candidate
  END IF

END PROGRAM block_if
