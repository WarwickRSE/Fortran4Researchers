MODULE mymodule

  IMPLICIT NONE
  SAVE

  INTEGER :: module_int
  INTEGER, PARAMETER :: module_parameter = 10
  REAL :: module_real

END MODULE mymodule

PROGRAM driver

  USE mymodule
  IMPLICIT NONE
  INTEGER :: myint

  PRINT *, module_parameter
  myint = module_parameter
  module_int = module_parameter
  module_real = REAL(module_int)

END PROGRAM driver
