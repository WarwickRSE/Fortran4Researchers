PROGRAM simple_if

  IMPLICIT NONE
  INTEGER :: test, candidate

  test = 2
  candidate = 4

  IF (test /= candidate) PRINT *, 'Test ', test, &
      ' not equal to candidate ', candidate

END PROGRAM simple_if
