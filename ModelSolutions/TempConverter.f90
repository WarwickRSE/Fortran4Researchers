
PROGRAM main

  IMPLICIT NONE

  REAL :: input_temp, output_temp
  REAL :: factor, factor_sub
  LOGICAL :: FtoC
  CHARACTER (LEN=1) :: unit_in, unit_out

  !Conversion factors to turn F into C
  !these names are OK, but not the best
  factor = 5.0/9.0
  factor_sub = 32.0


  PRINT*, "Enter a temperature in the form 23.5 F or 23.5 C (with a space):"

  ! Minor fancy thing - use READ formatting to take the last character
  ! and assume it's a unit
  ! let the rest of the string be the temperature.
  ! Sadly this crashes if you type it wrong, so it's not a very robust solution...
  READ(*, *) input_temp, unit_in

  ! We can store a comparison into a logical
  FtoC = (unit_in .eq. "F")

  IF(FtoC) THEN
    output_temp = (input_temp - factor_sub)*factor
    unit_out = "C"
  ELSE
    output_temp = input_temp/factor + factor_sub
    unit_out = "F"
  END IF

  ! Print result. Add a space to keep in and out formatting consistent
  PRINT "(F8.3, A, A)", output_temp, " ", unit_out

END PROGRAM
