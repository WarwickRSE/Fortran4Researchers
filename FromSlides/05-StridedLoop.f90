PROGRAM simple_loop

  IMPLICIT NONE
  INTEGER :: loop

  !This loop has a stride and will increment by 2 each time
  !0 -> 2 -> 4 -> 6 -> 8
  DO loop = 0, 9, 2
    PRINT *,"Hello from loop ", loop
  END DO

END PROGRAM simple_loop
