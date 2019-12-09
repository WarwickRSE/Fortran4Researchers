PROGRAM ranktest

  IMPLICIT NONE
  INTEGER, DIMENSION(0:9, -1:10) :: array1
  INTEGER, DIMENSION(:, :), ALLOCATABLE :: array2

  ALLOCATE(array2(-5:4, 1:10))
  array2(3, 7) = 4

END PROGRAM ranktest
