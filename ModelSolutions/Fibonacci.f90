
PROGRAM MAIN

  ! Loop counter and maximum value
  INTEGER :: i, max_val
  ! Variables for previous sequence vals, and current one
  INTEGER :: fib_first, fib_second, fib_current

  ! Do the first 20 terms
  max_val = 20

  ! Optional last but to add- sanity check on max_val
  IF(max_val < 3 .OR. max_val > 46) THEN
    PRINT*, "Sequence is too short or long. max_val should be between 3 and 46"
  END IF

  ! f(1) = 1 and f(2) = 1
  fib_first = 1
  fib_second = 1

  ! Print the initial 2 values so we see the whole sequence up to max_val
  PRINT*, fib_first
  PRINT*, fib_second

  ! Loop from the third value to max_value, calculating and printing the term
  DO i = 3, max_val

    ! This is the current term
    fib_current = fib_first + fib_second

    ! these are the previous terms, fib_first = f(n-2) and fib_second=f(n-1)
    fib_first = fib_second
    fib_second = fib_current

    ! Print the calculated value
    PRINT*, fib_current

  END DO

END PROGRAM
