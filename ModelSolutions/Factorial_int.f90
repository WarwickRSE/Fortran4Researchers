
PROGRAM MAIN

  IMPLICIT NONE

  ! Loop counter and maximum value
  INTEGER :: i, max_val
  ! Variables for current value
  ! If I use an INTEGER, I get exactly the right answer UNTIL I overflow
  ! I have to catch the overflow and do something
  INTEGER(KIND=SELECTED_INT_KIND(15)) :: factorial

  ! TRY the first 20 terms
  max_val = 30

  ! Optional last but to add- sanity check on max_val
  IF(max_val < 1 .OR. max_val > 30) THEN
    PRINT*, "Sequence is too short or long. max_val should be between 1 and 30"
  END IF


  factorial = 1

  ! Loop from first value to max_value, calculating and printing the term
  DO i = 1, max_val

    ! this iterattion will exceed HUGE()
    ! If I continue regardless I wil get a plausible but WRONG answer
    IF(factorial .GT. HUGE(factorial)/i) THEN
      PRINT*, "Value Overflowing!"
      STOP
    END IF
    factorial = factorial * i
    ! Print the calculated value
    PRINT*, "n= ", i, " n!=", factorial

  END DO

END PROGRAM
