
MODULE numerical_rootfinder

  !Implicit none should be in all modules and the main program.

  IMPLICIT NONE

  ! We probably do need Dbl to get these right. This is the
  ! most portable way to get it at the moment
  INTEGER, PARAMETER :: dbl = SELECTED_REAL_KIND(15, 307)

  ! Since we need both the function and its derivative
  ! I make these parameters module-parameters to avoid
  ! mismatches
  ! Normally a, b, c would be terrible variable names, but for
  ! low-order polynomials they are so well known, it is mainly
  ! personal choice if you like them or not
  REAL(KIND=dbl), PARAMETER :: a=-0.31428571428_dbl, b=-0.21549295774_dbl, c=0.20845070422_dbl
  ! We can use the KIND as a "wart" to get literals of the type we want

  CONTAINS

  FUNCTION sample_cubic(x)

    !Intents would be nice but absolutely aren't expected here
    REAL(KIND=dbl), INTENT(IN) :: x
    REAL(KIND=dbl) :: sample_cubic

    ! Store result straight into the return value

    sample_cubic = x*x*x + a*x*x + b*x + c

  END FUNCTION sample_cubic


  !For simplicity here I am coding the analytic derivative
  ! There are other ways, but this makes our life much easier
  FUNCTION sample_cubic_deriv(x)

    !Intents would be nice but absolutely aren't expected here
    REAL(KIND=dbl), INTENT(IN) :: x
    REAL(KIND=dbl) :: sample_cubic_deriv

    ! Store result straight into the return value

    sample_cubic_deriv = 3.0_dbl*x*x + 2.0_dbl*a*x + b

  END FUNCTION sample_cubic_deriv


  FUNCTION root_near(guess)
    ! Find a root of a function near a given guess
    ! We can return any valid root
    ! This variant uses the function 'sample_cubic' and
    ! its analytic derivative 'sample_cubic_deriv'  but this can be
    ! replaced with other functions in one variable

    ! Initial guess for the root
    REAL(KIND=dbl), INTENT(IN) :: guess

    REAL(KIND=dbl):: change
    REAL(KIND=dbl) :: root_near
    REAL(KIND=dbl), PARAMETER :: threshold = 0.001_dbl
    INTEGER, PARAMETER :: max_iter = 50
    INTEGER :: i

    !There are many, many ways to choose to terminate this finder
    ! I am capping to a max number of iterations in case of run-away
    ! and I am aiming for a change in x per iteration of less than
    ! 0.1% of the value. I don't worry about accuracy if we have got
    ! very small and this is an underflow
    ! There are many other good approaches
    ! I also assume there is only one root in the range given

    ! Since we have multiple break conditions, I like to use
    ! a loop limited to max_iter and an explicit break
    ! There are other ways - and plenty of people don't like this
    ! but I think "max_iter" makes the intention clear

    root_near = guess
    ! Start with a number for change that definitely
    ! does not trip the fractional change limit
    change = guess * 100.0
    DO i = 1, max_iter
      !I chose to print the progress in this
      PRINT*, "Iteration", i, "Fractional change", change, "Guess", root_near
      ! I am using a fractional change limit
      IF(ABS(change/root_near) .LT. threshold) EXIT

      ! I am being a bit "clever" to minimise my temporaries here
      ! by storing the change before applying it
      ! I could also store the old and new values, or other
      ! approaches
      change = sample_cubic(root_near)/sample_cubic_deriv(root_near)
      root_near = root_near - change
    END DO

  PRINT*, i, max_iter
    ! If we hit max_iter, we probably don't have a root
    ! This is a dumb way to create a NaN - we should really
    ! use the IEEE module, but I am keeping this simple
    IF(i >= max_iter) root_near = 0.0*1.0/(root_near-root_near)

  END FUNCTION root_near


END MODULE numerical_rootfinder


PROGRAM main

  USE numerical_rootfinder

  IMPLICIT NONE

  ! Select some bounds for finding
  REAL(KIND=dbl) :: guess = 0.555555555_dbl
  REAL(KIND=dbl) :: res

  ! Calculate and print the answer
  ! The Fortran standard lets compilers do something unhelpful
  ! if you PRINT the result of a function which has a print in,
  ! so I generally always capture a result, then print it
  res = root_near(guess)

  PRINT*, "Root is: ", res

END PROGRAM
