!WARNING : this code is slightly overbuilt for the actual problem
! but is a handy illustration of types etc

MODULE numerical_rootfinder

  !Implicit none should be in all modules and the main program.

  IMPLICIT NONE

  ! We probably do need Dbl to get these right. This is the
  ! most portable way to get it at the moment
  INTEGER, PARAMETER :: dbl = SELECTED_REAL_KIND(15, 307)

  ! Type to hold polynomial info
  ! I have made mine work for any order
  ! I assume coefficients go largest power to smallest
  TYPE polynomial_wrapper
    ! Since this holds order+1, but it useful to have as the size of coeffs
    INTEGER :: order_plus
    REAL(KIND=dbl), DIMENSION(:), ALLOCATABLE :: coeffs
  END TYPE

  CONTAINS

  SUBROUTINE define_poly(poly, coeffs, err)

    TYPE(polynomial_wrapper), INTENT(INOUT) :: poly
    REAL(KIND=dbl), DIMENSION(:), INTENT(IN) :: coeffs
    ! I make this optional because maybe I have already checked validity of coeffs
    LOGICAL, OPTIONAL :: err
    LOGICAL :: err_in

    err_in = .FALSE.

    poly%order_plus = SIZE(coeffs)

    ! 0th order poly has no roots, and I arbitrarily limit to max 8
    IF(poly%order_plus < 1 .OR. poly%order_plus > 8) THEN
      PRINT*, "Polynomial order should be between 1 and 8"
      err_in = .TRUE.
    ELSE
      ! In case we're re-defining
      IF(ALLOCATED(poly%coeffs)) DEALLOCATE(poly%coeffs)
      ALLOCATE(poly%coeffs(poly%order_plus))
      poly%coeffs = coeffs
    END IF

    ! Put value in before returning
    IF(PRESENT(err)) err = err_in


  END SUBROUTINE

  ! f is a terrible function name in general, but it matches the equation I am coding
  FUNCTION f(poly, x)
    TYPE(polynomial_wrapper) :: poly
    REAL(KIND=dbl) :: f, x, x_term
    INTEGER :: i

    f = 0.0
    x_term = 1.0

    ! I go backwards so I can accumulate the x powers
    DO i = poly%order_plus, 1,  -1
      f = f + x_term * poly%coeffs(i)
      x_term = x_term * x
    END DO

  END FUNCTION f

  FUNCTION f_prime(poly, x)
    TYPE(polynomial_wrapper) :: poly
    REAL(KIND=dbl) :: f_prime, x, x_term
    INTEGER :: i

    f_prime = 0.0
    x_term = 1.0

    ! I go backwards so I can accumulate the x powers
    DO i = poly%order_plus-1, 1,  -1
      f_prime = f_prime + x_term * poly%coeffs(i) * REAL(poly%order_plus-i)
      x_term = x_term * x
    END DO

  END FUNCTION f_prime


  FUNCTION root_near(poly, guess)
    ! Find a root of a function near a given guess
    ! We can return any valid root

    ! The polynomial
    TYPE(polynomial_wrapper) :: poly
    ! Initial guess for the root
    REAL(KIND=dbl), INTENT(IN) :: guess

    REAL(KIND=dbl):: change
    REAL(KIND=dbl) :: root_near
    REAL(KIND=dbl), PARAMETER :: threshold = 0.001_dbl
    INTEGER, PARAMETER :: max_iter = 50
    INTEGER :: i

    ! See numerical_root for discussion of termination condition

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
      change = f(poly,root_near)/f_prime(poly,root_near)
      root_near = root_near - change
    END DO

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
  TYPE(polynomial_wrapper) :: poly

  CALL define_poly(poly, (/1.0_dbl, -0.31428571428_dbl, &
      -0.21549295774_dbl, 0.20845070422_dbl/))

  ! Calculate and print the answer
  ! The Fortran standard lets compilers do something unhelpful
  ! if you PRINT the result of a function which has a print in,
  ! so I generally always capture a result, then print it
  res = root_near(poly, guess)

  PRINT*, "Root is ", res

END PROGRAM
