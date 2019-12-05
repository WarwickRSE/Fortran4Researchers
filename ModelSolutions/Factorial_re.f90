
PROGRAM MAIN

  IMPLICIT NONE

  ! Loop counter and maximum value
  INTEGER :: i, max_val
  ! Variables for current value
  ! I choose to use a real. THIS WILL GET A SLIGHTLY WRONG ANSWER BUT
  ! it wont overflow nearly as soon. I use a REAL64 here
  INTEGER, PARAMETER :: mykind = SELECTED_REAL_KIND(6, 37)
  !INTEGER, PARAMETER :: mykind = SELECTED_REAL_KIND(15, 307)
  REAL(KIND=mykind) :: factorial

  ! Do the first 50 terms
  max_val = 50

  ! Optional last but to add- sanity check on max_val
  IF(max_val < 1 .OR. max_val > 50) THEN
    PRINT*, "Sequence is too short or long. max_val should be between 1 and 50"
  END IF


  factorial = 1.0

  ! Loop from first value to max_value, calculating and printing the term
  DO i = 1, max_val

    IF(factorial .GT. HUGE(factorial)/i) THEN
      PRINT*, "Value Overflowing!"
      STOP
    END IF

    factorial = factorial * REAL(i)
    ! Print the calculated value
    PRINT*, "n= ", i, " n!=", factorial

  END DO

END PROGRAM
