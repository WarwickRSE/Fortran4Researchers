  !> @brief A random number generator
  !>
  !> Module wrapping the built in RNG so that
  !> an end user doesn't have to deal with seeding
  !> @author CS Brady

 !I add some docs here
MODULE random_mod

  IMPLICIT NONE

  INTEGER, PARAMETER, PRIVATE :: REAL64 = SELECTED_REAL_KIND(15, 307)

  CONTAINS

  !> @brief Return the next random number
  !> Returns a single random number of type REAL64
  !> Each call returns a new number
  !> @return A random number

  FUNCTION random()

    LOGICAL, SAVE :: init = .FALSE.
    INTEGER :: sz, ct
    INTEGER, DIMENSION(:), ALLOCATABLE :: seed
    REAL(REAL64) :: random

    IF (.NOT. init) THEN
      init = .TRUE.
      CALL RANDOM_SEED(SIZE = sz)
      ALLOCATE(seed(sz))
      CALL SYSTEM_CLOCK(ct)
      seed = ct
      CALL RANDOM_SEED(PUT = seed)
      DEALLOCATE(seed)
    END IF
    CALL RANDOM_NUMBER(random)

  END FUNCTION random
! And I change something here
END MODULE random_mod
