MODULE mymodule

 IMPLICIT NONE
 CONTAINS

  SUBROUTINE assignfn(arg1, arg2)
    INTEGER :: arg1, arg2
    arg2 = arg1
  END SUBROUTINE assignfn

END MODULE mymodule

PROGRAM driver
  USE mymodule
  IMPLICIT NONE

  CALL assignfn(1234,5678)
END PROGRAM driver
