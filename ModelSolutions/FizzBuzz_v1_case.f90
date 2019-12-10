PROGRAM FIZZBUZZ

  IMPLICIT NONE

  INTEGER :: i, max_num
  max_num = 100 ! Classic FizzBuzz runs from 1 to 100

  DO i = 1, max_num

    ! use a 'CASE' statement instead of an if-else chain
    SELECT CASE(MOD(i, 15))
    CASE (0)
      ! Divides by 15 i.e. by 5 and by 3
      PRINT*, "Fizzbuzz"
    CASE(3, 6, 9, 12)
      !Divdes by 3 but NOT 5
      PRINT*, "Fizz"
    CASE(5, 10)
      !Divdes by 5 but NOT 3
      PRINT*, "Buzz"
    CASE DEFAULT
      ! Format as an integer with up to 8 digits
      PRINT'(I8)', i
    END SELECT

  ENDDO

END PROGRAM
