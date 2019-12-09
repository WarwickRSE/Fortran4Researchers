MODULE mymodule

 IMPLICIT NONE
 CONTAINS

  SUBROUTINE hello_world_sub(arg_int)
    INTEGER :: arg_int
    PRINT *, "arg_int is ", arg_int
  END SUBROUTINE hello_world_sub

END MODULE mymodule

PROGRAM driver
  USE mymodule
  IMPLICIT NONE

  CALL hello_world_sub(1234)
END PROGRAM driver
