MODULE mymodule

 IMPLICIT NONE
 CONTAINS

  SUBROUTINE assignfn(param1, param2)
    INTEGER :: param1, param2
    param2 = param1
  END SUBROUTINE assignfn

END MODULE mymodule

PROGRAM driver
  USE mymodule
  IMPLICIT NONE
  INTEGER :: a, b
  a = 1234

  CALL assignfn(a, b)
END PROGRAM driver
