PROGRAM simple_loop
  IMPLICIT NONE
  INTEGER :: loop

  loop = 0
  DO WHILE(loop <= 9)
    PRINT *, "Hello from loop ", loop
    loop = loop + 1
  END DO
END PROGRAM simple_loop
