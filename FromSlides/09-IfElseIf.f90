PROGRAM simple_if

  IMPLICIT NONE
  INTEGER :: test, candidate

  test = 2
  candidate = 4

  IF (test < candidate) THEN
    PRINT *, 'Test ', test, ' less than candidate ', candidate
  ELSE IF (test == candidate) THEN
    PRINT *, 'Test ', test, ' equal to candidate ', candidate
  ELSE
    PRINT *, 'Test ', test, ' greater than candidate ', candidate
  END IF

END PROGRAM simple_if
