  !> @brief Functions to sleep or pause
  !>
  !> These wrap C functions so that end user doesn't
  !> have to deal with C interoperability
  !> or finding the 'correct' way to invoke sleep
  !> @author CS Brady


MODULE sleep_mod

  ! Uses standard C interoperability stuff
  USE ISO_C_BINDING
  IMPLICIT NONE

  !> Specifies a time value
  TYPE, BIND(C) :: timespec
    INTEGER(C_LONG) :: tv_sec
    INTEGER(C_LONG) :: tv_nsec
  END TYPE timespec


  INTERFACE
    !> Wrapper around C function sleeping for given number of seconds
    !>@param seconds Number of seconds to sleep for
    !>@return The time remaining (seconds-seconds_actually_slept)
    FUNCTION csleep(seconds) RESULT(remaining) BIND(C, NAME='sleep')
      IMPORT C_INT
      INTEGER(C_INT), VALUE :: seconds
      INTEGER(C_INT) :: remaining
    END FUNCTION csleep

    !> Wrapper around C function sleeping for given number of nanoseconds
    !>@param req Number of seconds requested to sleep for
    !>@param rem Number of seconds actually slept for
    !>@return 0 if the correct time was slept for, -1 else
    FUNCTION nanosleep(req, rem) BIND(C, NAME='nanosleep')
      IMPORT C_INT, timespec
      TYPE(timespec) :: req, rem
      INTEGER(C_INT) :: nanosleep
    END FUNCTION nanosleep

  END INTERFACE



  CONTAINS

  !> Wrapper around the sleep function to swallow the return value
  !>@param seconds Number of seconds to sleep for

  SUBROUTINE sleep_sec(seconds)
    INTEGER(C_INT) :: res
    INTEGER(KIND=SELECTED_INT_KIND(9)), INTENT(IN) :: seconds
    res = csleep(seconds)

  END SUBROUTINE sleep_sec

  !> Sleep for a number of milliseconds
  !> For the purposes here, this is the 'sane' shortest time period to use
  !>@param milliseconds Number of milliseconds to sleep for
  SUBROUTINE millisleep(milliseconds)
    INTEGER, INTENT(IN) :: milliseconds
    TYPE(timespec) :: spec_in, spec_out
    INTEGER(C_INT) :: res

    spec_in%tv_sec = milliseconds / 1000
    spec_in%tv_nsec = MOD(milliseconds, 1000) * 1000000
    res = nanosleep(spec_in, spec_out)

  END SUBROUTINE millisleep


  SUBROUTINE busy_sleep_example(seconds)

    REAL, INTENT(IN) :: seconds
    INTEGER(KIND=SELECTED_INT_KIND(15)) :: i, iter
    REAL(KIND=SELECTED_REAL_KIND(15, 307)) :: clock, tmp

    ! Tune to required duration
    clock = 1d6
    iter = INT(seconds*clock, KIND=SELECTED_INT_KIND(15))

    OPEN(file='/dev/null', status='old', unit=101, action='write')
    ! Do a busy-wait loop
    DO i = 1, iter
      tmp = tmp + 1.0 / clock
      WRITE(101, *) tmp
    END DO

    CLOSE(101)

  END SUBROUTINE busy_sleep_example

END MODULE sleep_mod
