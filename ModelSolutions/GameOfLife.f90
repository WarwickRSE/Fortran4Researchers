
! We don't expect multiple modules necessarily,
! but there should be at least one, containing the actual
! working code

MODULE functions

  USE kinds
  USE random_mod

  IMPLICIT NONE

  ! We probably don't need dbl for these problems
  ! but it  to get these right, but any way of
  ! obtaining it is OK

  CONTAINS


  SUBROUTINE setup_automaton_grid_random(grid)

    LOGICAL, DIMENSION(:,:), INTENT(INOUT) :: grid
    INTEGER, DIMENSION(2) :: sz
    INTEGER(KIND=INT32) :: i, j
    REAL(KIND=REAL64) :: rand

    sz = SHAPE(grid)
    grid = .FALSE.

    ! Or setup random values

    DO j = 2, sz(2)-1
      DO i = 2, sz(1)-1
        rand = random()
        IF(rand .LT. 0.2) grid(i, j) = .TRUE.
      END DO
    END DO

  END SUBROUTINE setup_automaton_grid_random

  SUBROUTINE setup_automaton_grid_blob(grid)

    LOGICAL, DIMENSION(:,:), INTENT(INOUT) :: grid
    INTEGER, DIMENSION(2) :: sz

    sz = SHAPE(grid)
    grid = .FALSE.

    ! Setup a blobby in the centre, 1/3rd the size of the grid
    ! For this, I don't bother to be careful about integer division
    grid(sz(1)/3+1:sz(1)*2/3, sz(2)/3+1:sz(2)*2/3) = .TRUE.

  END SUBROUTINE setup_automaton_grid_blob

  SUBROUTINE setup_automaton_grid_glider(grid)

    LOGICAL, DIMENSION(:,:), INTENT(INOUT) :: grid
    INTEGER, DIMENSION(2) :: sz, base

    sz = SHAPE(grid)
    grid = .FALSE.

    base = sz/2
    ! Setup a single glider near the middle of the domain
    grid(base(1)+1:base(1)+3, base(2)) = .TRUE.
    grid(base(1)+3, base(2)+1) = .TRUE.
    grid(base(1)+2, base(2)+2) = .TRUE.

  END SUBROUTINE setup_automaton_grid_glider



  SUBROUTINE advance_automaton_grid(grid)

    LOGICAL, DIMENSION(:,:), INTENT(INOUT) :: grid
    LOGICAL, DIMENSION(:,:), ALLOCATABLE :: new_grid

    INTEGER, DIMENSION(2) :: sz
    INTEGER(KIND=INT32) :: i, j, live_nb

    sz = SHAPE(grid)
    ALLOCATE(new_grid(sz(1), sz(2)))

    ! Clamp edges to .FALSE.
    new_grid(1:sz(1), 1) = .FALSE.
    new_grid(1:sz(1), sz(2)) = .FALSE.
    new_grid(1, 1:sz(2)) = .FALSE.
    new_grid(sz(1), 1:sz(2)) = .FALSE.

    ! Do the middle part
    DO j = 2, sz(2)-1
      DO i = 2, sz(1)-1
        ! Count live cells surrounding current cell

        ! Basic method using explicit additions (could also use a loop)
        ! I chose to format this with extra whitespace for readability
!        live_nb = 0
!        IF(grid(i-1, j-1)) live_nb = live_nb + 1
!        IF(grid(i-1, j))   live_nb = live_nb + 1
!        IF(grid(i-1, j+1)) live_nb = live_nb + 1
!        IF(grid(i,   j-1)) live_nb = live_nb + 1
!        IF(grid(i,   j+1)) live_nb = live_nb + 1
!        IF(grid(i+1, j-1)) live_nb = live_nb + 1
!        IF(grid(i+1, j))   live_nb = live_nb + 1
!        IF(grid(i+1, j+1)) live_nb = live_nb + 1

        ! Cleverer update using an array slice and Intrinsic
        live_nb = COUNT(grid(i-1:i+1, j-1:j+1))
        IF(grid(i, j)) live_nb = live_nb - 1

        ! Do updates according to number of neighbours
        IF(grid(i, j) .AND. live_nb .LT. 2) THEN
          new_grid(i, j) = .FALSE.
        ELSE IF(grid(i, j) .AND. live_nb .GT. 3) THEN
          new_grid(i, j) = .FALSE.
        ELSE IF(.NOT. grid(i, j) .AND. live_nb .EQ. 3) THEN
          new_grid(i, j) = .TRUE.
        ELSE
          new_grid(i, j) = grid(i, j)
        END IF

      END DO

    END DO

    !After doing all steps, copy the new state back
    grid = new_grid

    !We can free new_grid now, but we don't actually NEED to
    DEALLOCATE(new_grid)

  END SUBROUTINE advance_automaton_grid


END MODULE functions



PROGRAM main

  USE kinds
  !Provided pretty printing module
  USE ascii_display
  USE sleep_mod
  USE command_line

  USE functions

  IMPLICIT NONE

  INTEGER(KIND=INT32) :: N, T
  INTEGER(KIND=INT32) :: i
  LOGICAL, DIMENSION(:,:), ALLOCATABLE :: grid
  LOGICAL :: success
  CHARACTER :: init

  ! Get command line arguments, and if N or T are given, use them
  CALL parse_args
  ! If not given, we should use sensible defaults for the grid size
  ! and number of iterations

  ! Best and simplest way to use cmd line module
  success = get_arg("T", T)
  IF( .NOT. success) THEN
    T = 20
  END IF

  ! Or in super-short form
  IF( .NOT. get_arg("N", N)) THEN
    N = 45
  END IF

  ALLOCATE(grid(2*N, N))

  ! As specified in the brief,
  ! I use a single-character flag for the initial state,
  ! either 'R' for random, 'B' for blob
  ! and I default to random if not given or not one of the options
  success = get_arg("init", init)

  IF(init == "B" .OR. init == "b") THEN
    ! Blob in centre of domain
    CALL setup_automaton_grid_blob(grid)
  ELSE  IF(init == "G" .OR. init == "g") THEN
    ! Blob in centre of domain
    CALL setup_automaton_grid_glider(grid)
  ELSE
    ! This will set random states
    CALL setup_automaton_grid_random(grid)
  END IF

  CALL display_array(grid, .TRUE.)
  CALL wait_for_enter_key

  DO i = 1, T

    CALL advance_automaton_grid(grid)
    CALL display_array(grid, .TRUE.)
    !CALL wait_for_enter_key
    CALL sleep_sec(1)
  END DO

END PROGRAM
