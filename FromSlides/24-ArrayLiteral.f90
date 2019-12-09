PROGRAM array_test
  IMPLICIT NONE
  !Array runs from 1->10
  !Different to most other languages
  !that run 0->9
  INTEGER, DIMENSION(10) :: array

  !Can also use an array literal
  !Surround a comma separated list of
  !numbers with square brackets
  array = [1,2,3,4,5,6,7,8,9,10]

  !Older style of array literal uses (/ /)
  array = (/1,2,3,4,5,6,7,8,9,10/)

  !Can print whole arrays although large
  !arrays are hard to read
  PRINT *, array

END PROGRAM array_test
