MODULE mymodule

 IMPLICIT NONE
 CONTAINS

  FUNCTION running_max(arg)
    INTEGER, INTENT(IN) :: arg
    INTEGER :: running_max
    INTEGER :: current_max

    !This won't work as you want! You always reset current_max so
    ! any value larger than 0 will be immediately returned
    current_max = 0
    current_max = MAX(current_max, arg)
    running_max = current_max
  END FUNCTION running_max

END MODULE mymodule

PROGRAM driver
  USE mymodule
  IMPLICIT NONE

  PRINT *, running_max(1)
  PRINT *, running_max(10)
  PRINT *, running_max(2)
  PRINT *, running_max(20)

END PROGRAM driver
