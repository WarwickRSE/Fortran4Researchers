PROGRAM array_test
  IMPLICIT NONE
  !Runs 1->10
  INTEGER, DIMENSION(10) :: array1
  !Runs 1->10 but now made explicit
  INTEGER, DIMENSION(1:10) :: array2
  !Runs 0->9
  INTEGER, DIMENSION(0:9) :: array3
  !Runs -5->4
  INTEGER, DIMENSION(-5:4) :: array4

  PRINT *, 'Array1 :', SIZE(array1), LBOUND(array1), UBOUND(array1)
  PRINT *, 'Array2 :', SIZE(array2), LBOUND(array2), UBOUND(array2)
  PRINT *, 'Array3 :', SIZE(array3), LBOUND(array3), UBOUND(array3)
  PRINT *, 'Array4 :', SIZE(array4), LBOUND(array4), UBOUND(array4)

END PROGRAM array_test
