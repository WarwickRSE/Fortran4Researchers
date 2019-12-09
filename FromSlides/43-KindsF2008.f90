PROGRAM kinds

  !Don't have to USE, INTRINSIC, can just USE but INTRINSIC
  !means that the compiler won't try to use one of your
  !modules
  USE ISO_FORTRAN_ENV
  IMPLICIT NONE

  INTEGER(INT8) :: i1
  INTEGER(INT16) :: i2
  INTEGER(INT32) :: i3
  INTEGER(INT64) :: i4
  REAL(REAL32) :: r1
  REAL(REAL64) :: r2
  REAL(REAL128) :: r3

  !Print the largest number that can be held in each variable
  PRINT *, HUGE(i1)
  PRINT *, HUGE(i2)
  PRINT *, HUGE(i3)
  PRINT *, HUGE(i4)
  PRINT *, HUGE(r1)
  PRINT *, HUGE(r2)
  PRINT *, HUGE(r3)

END PROGRAM kinds
