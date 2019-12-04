
PROGRAM main

  IMPLICIT NONE

  REAL :: v
  REAL, PARAMETER :: c = 299792458 ! m/s
  REAL :: beta, gamma

  PRINT*, "Enter velocity in m/s "

  READ(*, *) v

  ! I check if v > c. Later I check if it is so close to c that gamma overflows
  IF(v > c) THEN
    PRINT*, "Velocity exceeds speed of light! Try something slower"
    STOP
  END IF

  ! I don't bother to check if v << c, because gamma will be effectively
  ! 1 for all physics purposes in that case, so I don't really care about the accuracy

  ! I use the trick of keeping my calculation parts close to 1.
  beta = v/c

  ! Do the gamma calculation in 2 parts. First calculate 1/gamma. I use gamma
  ! as it's own temporary here. This is OK if you're careful
  gamma = sqrt(1.0 - beta*beta)

  ! Check if 1/gamma is too small to represent.
  ! I use "TINY" to get the smallest non-zero number

  IF(gamma < TINY(gamma)) THEN
    PRINT*, "Gamma has overflowed! Try something slower!"
  ELSE
    PRINT*, "Gamma is ", 1.0/gamma
  END IF


END PROGRAM
