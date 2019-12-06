PROGRAM main

  ! ALWAYS include this!
  IMPLICIT NONE

  !Define variables. All definitions before any code
  ! But I can assign a constant value here, even using an expression
  INTEGER :: i = 5*4*3*2
  REAL :: r

  ! Basic PRINT doesn't need any formatting etc
  PRINT*, i

  ! SQRT takes a REAL so we have to use 3.0 not just 3
  ! Fortran is very pedantic about types
  r = SQRT(3.0)
  PRINT*, r

END PROGRAM
