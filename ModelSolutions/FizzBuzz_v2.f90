PROGRAM FIZZBUZZ

  INTEGER :: i, max_num
  CHARACTER(LEN=8) :: output  ! 8 characters can hold up to 10 million

  max_num = 100  ! Classic FizzBuzz runs from 1 to 100

  DO i=1, max_num
    output = ""  !Output is 8 chars long - it will now contain only spaces

    ! Handle divides-by-3
    IF(MOD(i, 3) .EQ. 0) output = "Fizz"

    ! Handles divides-by-5
    ! By appending not replacing, we automatically handle
    ! division by 15 (except for the upper-case B)
    ! Trim removes spaces, so we get either "" or "Fizz" from this
    IF(MOD(i, 5) .EQ. 0) output = trim(output)//"Buzz"

    ! If the output string is empty, we fill it with 'i'
    IF(output .EQ. "") WRITE(output, '(I8)') i

    ! Print the string, removing leading spaces
    PRINT*, adjustl(output)
  ENDDO

END PROGRAM
