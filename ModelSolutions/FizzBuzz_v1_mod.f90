PROGRAM FIZZBUZZ

  INTEGER :: i, max_num

  max_num = 100  ! Classic FizzBuzz runs from 1 to 100

  DO i = 1, max_num

    ! Start by considering divides by 3 AND 5
    ! The MOD function returns the remainder of division
    ! SO MOD(4, 3) -> 1 and MOD(7, 5) -> 2 etc
    ! For the first case, we could also do:
    ! IF(MOD(i, 3) == 0 .AND. MOD(i, 5) == 0) THEN
    IF(MOD(i, 15) == 0) THEN
      ! Divides by 15 i.e. by 5 and by 3
      PRINT*, "Fizzbuzz"
    ELSE IF(MOD(i, 3) == 0) THEN
      !Divdes by 3 but NOT 5
      PRINT*, "Fizz"
    ELSE IF(MOD(i, 5) == 0) THEN
      !Divdes by 5 but NOT 3
      PRINT*, "Buzz"
    ELSE
      ! Format as an integer with up to 8 digits
      PRINT'(I8)', i
    END IF

  ENDDO

END PROGRAM
