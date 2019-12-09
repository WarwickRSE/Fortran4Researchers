MODULE mymodule

 IMPLICIT NONE
 CONTAINS

  FUNCTION diff(param1, param2)
    INTEGER, INTENT(IN) :: param1
    INTEGER, INTENT(IN) :: param2
    INTEGER :: diff
    diff = param1 - param2
  END FUNCTION diff

END MODULE mymodule

PROGRAM driver
  USE mymodule
  IMPLICIT NONE
  INTEGER :: a, b

  b = 5678
  a = diff(param2 = b, param1 = 1234)

  PRINT *, 'A is ', a
END PROGRAM driver
