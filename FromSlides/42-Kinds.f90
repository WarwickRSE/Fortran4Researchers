PROGRAM kinds

  IMPLICIT NONE
  INTEGER, PARAMETER :: INT8 = SELECTED_INT_KIND(2)
  INTEGER, PARAMETER :: INT16 = SELECTED_INT_KIND(4)
  INTEGER, PARAMETER :: INT32 = SELECTED_INT_KIND(9)
  INTEGER, PARAMETER :: INT64 = SELECTED_INT_KIND(15)
  INTEGER, PARAMETER :: REAL32 = SELECTED_REAL_KIND(6, 37)
  INTEGER, PARAMETER :: REAL64 = SELECTED_REAL_KIND(15, 307)
  INTEGER, PARAMETER :: REAL128 = SELECTED_REAL_KIND(33, 4931)

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
