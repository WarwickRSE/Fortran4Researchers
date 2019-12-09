MODULE mymodule

  IMPLICIT NONE
  SAVE

  INTEGER :: module_int
  CONTAINS

  SUBROUTINE hello_world_sub()
    PRINT *, "Hello World"
    PRINT *, "Module_int is", module_int
  END SUBROUTINE hello_world_sub

END MODULE mymodule

PROGRAM driver
  USE mymodule
  IMPLICIT NONE

  module_int = 1234
  CALL hello_world_sub()
END PROGRAM driver
