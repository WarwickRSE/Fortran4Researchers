PROGRAM array_test
  IMPLICIT NONE
  !Array runs from 1->10
  !Different to most other languages
  !that run 0->9
  INTEGER, DIMENSION(10) :: array
  INTEGER :: i

  !SIZE is an intrinsic function that
  !tells me how large my array is
  DO i = 1, SIZE(array)
    array(i) = i
  END DO

  !Can print whole arrays although large
  !arrays are hard to read
  PRINT *, array

END PROGRAM array_test
