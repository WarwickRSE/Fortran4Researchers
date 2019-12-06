!WARNING : this code is overbuilt for the actual problem
! but is a handy illustration of function pointers

MODULE numerical_rootfinder

  !Implicit none should be in all modules and the main program.

  IMPLICIT NONE

  ! We probably do need Dbl to get these right. This is the
  ! most portable way to get it at the moment
  INTEGER, PARAMETER :: dbl = SELECTED_REAL_KIND(15, 307)

  REAL(KIND=dbl), PARAMETER :: a=-0.31428571428_dbl, b=-0.21549295774_dbl, c=0.20845070422_dbl
  ! We can use the KIND as a "wart" to get literals of the type we want

  ! Define the "signature" of our function. "Abstract" means that
  ! this is not defining an interface for a function we've got elsewhere
  ! it is the abstract idea of "any function that looks like this"
  ! Our function is always going to take exactly ONE argument, type dbl
  ! and return a double also. I also add intents
  ABSTRACT INTERFACE
      FUNCTION op (x)
         IMPORT dbl
         REAL(KIND=dbl) :: op
         REAL(KIND=dbl), INTENT(IN) :: x
      END FUNCTION op
  END INTERFACE


  TYPE func
    ! We defined the interface (arguments, return type etc) "op" above
    ! Now we can have a pointer to an unknown function with the same interface
    ! Fortran required this "nopass" attirbute because a Procedure in a Type is
    ! interpreted as a Member function and it is assumed the type itself will be
    ! the first argument (c.f this, self, etc). Nopass means "do not pass the type as
    ! first argument"
    PROCEDURE (op), pointer, nopass :: f, f_prime
  END TYPE func


  CONTAINS


  ! For this version, I have to create the function and it's derivative
  ! explicitly. If I wanted to be super fancy, I might create a function-to-create-a_function
  ! so I can bind whatever coeffcients I wish or use another type, or myriad
  ! other options. Here I use hard-coded cubics
  ! I would probably want these to be in another module in real code
  FUNCTION sample_cubic(x)

    REAL(KIND=dbl), INTENT(IN) :: x
    REAL(KIND=dbl) :: sample_cubic

    sample_cubic = x*x*x + a*x*x + b*x + c

  END FUNCTION sample_cubic

  FUNCTION sample_cubic_deriv(x)

    REAL(KIND=dbl), INTENT(IN) :: x
    REAL(KIND=dbl) :: sample_cubic_deriv

    sample_cubic_deriv = 3.0_dbl*x*x + 2.0_dbl*a*x + b

  END FUNCTION sample_cubic_deriv


  FUNCTION root_near(myfn, guess)
    ! Find a root of a function near a given guess
    ! We can return any valid root

    TYPE(func) :: myfn
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
      change = myfn%f(root_near)/myfn%f_prime(root_near)
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
  TYPE(func) :: myfn

  ! Hook up my type to my actual functions
  myfn%f => sample_cubic
  myfn%f_prime => sample_cubic_deriv

  ! Calculate and print the answer
  ! The Fortran standard lets compilers do something unhelpful
  ! if you PRINT the result of a function which has a print in,
  ! so I generally always capture a result, then print it
  res = root_near(myfn, guess)

  PRINT*, "Root is ", res

END PROGRAM
