MODULE fnptr

  IMPLICIT NONE
  SAVE

  INTEGER, PARAMETER :: INT32 = SELECTED_INT_KIND(9)

  !This abstract interface defines a type of function that I can now get a
  !pointer to
  ABSTRACT INTERFACE
    FUNCTION opfn(val1, val2)
      !Have to IMPORT int32 to get it from the containing module
      !this keeps the function declaration and the encompassing module
      !separate
      IMPORT INT32
      INTEGER(INT32), INTENT(IN) :: val1, val2
      INTEGER(INT32) :: opfn
    END FUNCTION opfn
  END INTERFACE

  CONTAINS

    !These functions must match variables in kind, intent and type
    !But don't have to have the same variable names etc.
    !Attributes like allocatable and pointer must match as well
    FUNCTION add(val1, val2)
      INTEGER(INT32), INTENT(IN) :: val1, val2
      INTEGER(INT32) :: add

      add = val1 + val2
    END FUNCTION add

    FUNCTION minus(val1, val2)
      INTEGER(INT32), INTENT(IN) :: val1, val2
      INTEGER(INT32) :: minus

      minus = val1 - val2
    END FUNCTION minus

    FUNCTION do_op(val1, val2, op)
      INTEGER(INT32), INTENT(IN) :: val1, val2
      PROCEDURE(opfn) :: op
      INTEGER(INT32) :: do_op

      do_op = op(val1, val2)

    END FUNCTION do_op

END MODULE fnptr

PROGRAM test_ptr

  USE fnptr
  IMPLICIT NONE

  PROCEDURE(opfn), POINTER :: ptr => NULL()

  !Use the function pointer with add
  ptr => add
  PRINT *, ptr(1,2)

  !And the same with minus
  ptr => minus
  PRINT *, ptr(1,2)

  !Now use the function with a function as a parameter
  PRINT *, do_op(1,2,add)
  PRINT *, do_op(1,2,minus)

END PROGRAM
