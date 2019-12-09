PROGRAM typetest

  IMPLICIT NONE
  !This defines a type
  TYPE :: mytype
    INTEGER :: myint
    REAL :: myreal
  END TYPE

  !This creates an instance of my type
  TYPE(mytype) :: t

  t%myint = 10
  t%myreal = 1.5

  PRINT *, t%myint
  PRINT *, t%myreal

END PROGRAM typetest
