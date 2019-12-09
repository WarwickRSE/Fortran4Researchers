MODULE mymodule

 IMPLICIT NONE
 CONTAINS

  SUBROUTINE hello_world_sub(arg1, arg2)
    INTEGER :: arg1, arg2
    PRINT *, "arg1 is ", arg1
    PRINT *, "arg2 is ", arg2
  END SUBROUTINE hello_world_sub

END MODULE mymodule

PROGRAM driver
  USE mymodule
  IMPLICIT NONE

  CALL hello_world_sub(1234,5678)
END PROGRAM driver
