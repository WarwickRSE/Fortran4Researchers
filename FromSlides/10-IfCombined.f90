PROGRAM dual_if

  IMPLICIT NONE
  INTEGER :: test1, candidate1
  INTEGER :: test2, candidate2

  test1 = 2
  candidate1 = 4

  test2 = 6
  candidate2 = 6

  IF (test1 == candidate1 .OR. test2 == candidate2) THEN
    PRINT *,'One or more candidates are matched'
    IF ((test1 == candidate1) .EQV. (test2 == candidate2)) THEN
      PRINT *,' Both candidates matched'
    END IF
  ELSE
    PRINT *, 'Neither candidate matched'
  END IF

END PROGRAM dual_if
