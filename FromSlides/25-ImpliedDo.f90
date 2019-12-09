PROGRAM array_test
  IMPLICIT NONE
  !Array runs from 1->10
  !Different to most other languages
  !that run 0->9
  INTEGER, DIMENSION(10) :: array
  INTEGER :: i

  !Can use an IMPLIED DO LOOP
  !to set up this array (c.f. linspace etc.)
  array = [(i,i=1,SIZE(array))]

  !Can print whole arrays although large
  !arrays are hard to read
  PRINT *, array

END PROGRAM array_test
