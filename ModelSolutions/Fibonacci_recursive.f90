! A recursive version of Fibonacci
! Note that while this works OK as a recursive program
! it actually does far, far more work that it needs to, much
! more than double (actually 2^n -1) fold
! Fpr instance, to calculate Fib(3), it will calculate
! Fib(1) twice, and Fib(2) once. For Fib(4) it calculates
! Fib(3) once, Fib(2) twice, and Fib(1) three times. This is
! actually the Fibonacci sequence all over again, so it grows FAST!

PROGRAM MAIN

  IMPLICIT NONE

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
    fib_current = fib(i)

    ! Print the calculated value
    PRINT*, fib_current

  END DO

! A slightly old-fashioned way to define a function, putting it inside the main program like this.
! It is available inside MAIN, but not outside. But one function here can call another. 
CONTAINS

  ! We have to use a 'RESULT' here because otherwise there is an
  ! ambiguity about where we are setting the result variable and
  ! where we are calling the function. So we have e.g.:
  RECURSIVE FUNCTION fib(n) RESULT(fib_n)
    INTEGER:: fib_n, n

    IF (n < 3) THEN
      fib_n = 1
    ELSE
      fib_n = fib(n-1) + fib(n-2)
    END IF

  END FUNCTION

END PROGRAM
