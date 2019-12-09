
! This is not a very good module name, but it'll do for this small program
MODULE ising_functions

  USE random_mod
  USE kinds

  IMPLICIT NONE

  CONTAINS


  SUBROUTINE setup_random_spin_grid(grid)

    INTEGER(KIND=INT16), DIMENSION(0:,0:), INTENT(INOUT) :: grid
    INTEGER, DIMENSION(2) :: sz
    INTEGER(KIND=INT32) :: i, j
    REAL(KIND=REAL64) :: rand

    sz = SHAPE(grid)
    grid = 0

    ! All outside edge spins are '+'
    grid(0:sz(1), 0) = 1
    grid(0:sz(1), sz(2)-1) = 1
    grid(0, 0:sz(2)) = 1
    grid(sz(1)-1, 0:sz(2)) = 1

    ! Setup random spins

    DO j = 1, sz(2)-2
      DO i = 1, sz(1)-2
        rand = random()*2.0
        ! note our choice of Int16 gets a bit annoying because
        ! literals default to INT32 so we have to add explicit conversions
        grid(i, j) = INT(2*INT(rand) - 1, KIND=INT16)
      END DO
    END DO

  END SUBROUTINE setup_random_spin_grid


  SUBROUTINE setup_alternating_spin_grid(grid)

    INTEGER(KIND=INT16), DIMENSION(0:,0:), INTENT(INOUT) :: grid
    INTEGER, DIMENSION(2) :: sz
    INTEGER(KIND=INT32) :: i, j, offset

    sz = SHAPE(grid)

    ! Setup alternating spins:
    ! Set all to 1, then reverse every second
    grid = 1
    DO j = 1, sz(2)-2
      ! This makes sure rows alternate first and second cell flipped
      offset = 0
      IF(MOD(j, 2) .EQ. 1) offset = 1
      DO i = offset+1, sz(1)-2, 2
        grid(i, j) = -1
      END DO
    END DO

  END SUBROUTINE setup_alternating_spin_grid



  SUBROUTINE iterate_ising_grid(grid, beta, J, max_iters, flips)

    INTEGER(KIND=INT16), DIMENSION(0:,0:), INTENT(INOUT) :: grid

    INTEGER, DIMENSION(2) :: sz
    INTEGER(KIND=INT32) :: iter, flips
    INTEGER(KIND=INT32) :: max_iters
    REAL(KIND=REAL64) :: rand, prob, beta, delta_E, J
    INTEGER :: rand_x, rand_y

    sz = SHAPE(grid)

    flips = 0

    !Iterate random flips
    DO iter = 1, max_iters

      ! Pick random pos'n, excluding "ghost" cells
      rand_x = 1+INT(random()*(sz(1)-2))
      rand_y = 1+INT(random()*(sz(2)-2))

      ! Calc energy change
      delta_E = 0.0
      ! I use this odd indenting to line up the terms for easier reading
      delta_E = delta_E + J * grid(rand_x-1, rand_y)  *grid(rand_x, rand_y)
      delta_E = delta_E + J * grid(rand_x,   rand_y-1)*grid(rand_x, rand_y)
      delta_E = delta_E + J * grid(rand_x,   rand_y+1)*grid(rand_x, rand_y)
      delta_E = delta_E + J * grid(rand_x+1, rand_y)  *grid(rand_x, rand_y)

      ! Check for acceptance and perform flip if necessary
      IF (delta_E .LT. 0) THEN
        grid(rand_x, rand_y) = - grid(rand_x, rand_y)
        flips = flips + 1
      ELSE
        rand = random()
        prob = EXP(- beta * delta_E)
        IF (rand .LT. prob) THEN
          grid(rand_x, rand_y) = - grid(rand_x, rand_y)
          flips = flips + 1
        END IF
      END IF

    END DO


  END SUBROUTINE iterate_ising_grid


  FUNCTION grid_magnetization(grid)

    INTEGER(KIND=INT16), DIMENSION(0:,0:), INTENT(INOUT) :: grid
    REAL(KIND=REAL64) :: grid_magnetization

    grid_magnetization = SUM(grid)


  END FUNCTION grid_magnetization


END MODULE ising_functions



PROGRAM main

  !Provided pretty printing module
  USE ascii_display
  USE sleep_mod
  USE command_line

  USE ising_functions

  IMPLICIT NONE

  INTEGER(KIND=INT32):: N
  INTEGER(KIND=INT32):: max_iters
  INTEGER(KIND=INT16), DIMENSION(:,:), ALLOCATABLE :: grid
  INTEGER(KIND=INT32) :: flips
  REAL(KIND=REAL64) :: beta, J, init_mag, final_mag
  LOGICAL :: success
  CHARACTER :: init

  ! Get command line arguments, and if N or T are given, use them
  CALL parse_args
  ! If not given, we should use sensible defaults for the grid size
  ! and number of iterations

  ! Best and simplest way to use cmd line module
  success = get_arg("max_iters", max_iters)
  IF( .NOT. success) THEN
    max_iters = 10000
  END IF

  ! Or in super-short form
  IF( .NOT. get_arg("N", N)) THEN
    N = 40
  END IF
  !Now N either has user-entered value, or is set to 40
  ! Note that the get_arg routine DOES NOT guarantee what happens
  ! to our passed variable N if keyword isn't found, so we can't set
  ! default BEFORE trying the get

  IF( .NOT. get_arg("beta", beta)) THEN
    beta = 10.0
  END IF
  IF( .NOT. get_arg("J", J)) THEN
    J = 1.0
  END IF

  ! Easiest to have "ghost cells" using non-zero indices
  ! So 1:N is the real domain, and 0 and N+1 are extra cells
  ALLOCATE(grid(0:N+1, 0:N+1))

  ! I use a single-character flag for the initial state,
  ! either 'R' for random, 'A' for alternating or 'F' for flat (=1)
  ! and I default to random if not given or not one of the options
  success = get_arg("init", init)
  IF(init == "F" .OR. init == "f") THEN
    ! We can set everything up '+1'
    grid = 1
  ELSE IF(init == "A" .OR. init == "a") THEN
    ! This will set alternating
    CALL setup_alternating_spin_grid(grid)
  ELSE
    ! This will set random spins
    CALL setup_random_spin_grid(grid)
  END IF

  init_mag = grid_magnetization(grid)

  !I chose to show the initial state, and wait for Enter to run
  CALL display_3val_array(grid, .TRUE.)
  CALL wait_for_enter_key

  !Run the iterations and then display again
  CALL iterate_ising_grid(grid, beta, J, max_iters, flips)
  CALL display_3val_array(grid, .TRUE.)

  PRINT*,'Flipped ', flips, ' states (', REAL(flips)/REAL(max_iters)*100, '%)'

  final_mag = grid_magnetization(grid)
  PRINT*, "Initial magnetization", init_mag
  PRINT*, "Final magnetization  ", final_mag, "(", (final_mag/init_mag - 1.0)*100.0, "%)"

END PROGRAM
