
PROGRAM main

  IMPLICIT NONE

  ! Pick a "long enough" string length
  INTEGER, PARAMETER :: string_len = 50
  INTEGER :: i, j, offset = 2
  ! Array holding alphabet and shifted alphabet
  CHARACTER(LEN=1), DIMENSION(26) :: letters, shifted_letters
  CHARACTER(LEN=string_len) :: word, ciphered_word

  !letters in order
  letters = (/"A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", &
      "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z"/)


  PRINT*, "Enter word to cipher, in all caps:"
  !word should be all upper case and at most 30 characters
  READ(*, '(A)') word
  !word = "BISCUIT"

  !Create array shifted around first. This would work manually:
  !shifted_letters(1:26-offset) = letters(1+offset:26)
  !shifted_letters(26-offset:26) = letters(1:1+offset)

  ! OR we could also look for/remember a function to shift an array around
  shifted_letters = CSHIFT(letters, offset)

  ciphered_word = ""
  DO i = 1, string_len
    DO j = 1, 26
      ! Fortran doesn't let you refer to a single character in a string, but you can
      ! have a length-1 substring. I.e. you need start and end indices in slice
      IF(letters(j) == word(i:i)) THEN
        ciphered_word(i:i) = shifted_letters(j)
        ! Finish the inner loop
        EXIT
      END IF
    END DO
  END DO

  ! Fortran strings are exactly the length given - the end is padded with spaces
  ! so for output we should TRIM this off
  PRINT*, TRIM(word), " becomes ", TRIM(ciphered_word)

END PROGRAM main
