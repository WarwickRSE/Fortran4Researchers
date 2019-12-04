PROGRAM FIZZBUZZ

  INTEGER :: i, max_num, i3, i5
  CHARACTER(LEN=8) :: output  ! 8 characters can hold up to 10 million

  max_num = 100  ! Classic FizzBuzz runs from 1 to 100

  ! This approach uses two counters. This is a solution you
  ! might try if the MOD function wasn't available, or you might
  ! come up with it if you're not sure how to check "divides-by" mathematically
  ! You could combine this approach with the string-building approach
  ! in the v2 model.

  ! I chose to have my counters count down, but they could go up
  i3 = 3
  i5 = 5

  DO i = 1, max_num

    i3 = i3 - 1
    i5 = i5 - 1

    IF((i3 == 0) .AND. (i5 == 0)) THEN
      PRINT*, "Fizzbuzz"
      i3 = 3
      i5 = 5
    ELSE IF(i3 == 0) THEN
      PRINT*, "Fizz"
      i3 = 3
    ELSE IF(i5 == 0) THEN
      PRINT*, "Buzz"
      i5 = 5
    ELSE
      PRINT '(i8)', i
    END IF

  ENDDO

END PROGRAM
