
!This code is overbuilt for this problem, but shows some nice
! fortran stuff
! Note that this uses extra memory in places to get
! nice elegant operations
! I have deliberately kept the core code as in the simple
! Ising and just wrapped things away.

MODULE ising_functions

  USE random_mod
  USE kinds

  IMPLICIT NONE

  ! No array-of-pointers in Fortran
  TYPE :: ptr_wrapper
    INTEGER(KIND=INT16), POINTER :: ptr
  END TYPE ptr_wrapper


  ! This is VERY overbuilt... But it shows what is possible
  TYPE :: ising_element
    ! Will point into the grids 'state' array
    INTEGER(KIND=INT16), POINTER :: state
    TYPE(ptr_wrapper), DIMENSION(:), ALLOCATABLE :: neighbours
    INTEGER :: el_flips=0 ! count of flips of this element
    CONTAINS
    PROCEDURE :: flip
    PROCEDURE :: magnetization_el
    ! Suppose elements could have different ways to calculate magnetization
  END TYPE ising_element

  ! Since I display the grid etc, I have a plain array
  ! of the current state, as well as the array of elements
  ! The elements can track how
  ! often they are flipped, etc
  TYPE :: ising_grid
    INTEGER :: sz=0
    INTEGER(KIND=INT16), DIMENSION(:, :), POINTER :: state
    TYPE(ising_element), DIMENSION(:,:), POINTER :: elements
    REAL(KIND=REAL64) :: beta=1, J=1
    CONTAINS
    PROCEDURE :: allocate_grid
    PROCEDURE :: iterate
    PROCEDURE :: setup_grid
    PROCEDURE :: magnetization
    PROCEDURE :: flips
    FINAL :: destroy_grid
  END TYPE ising_grid

  CONTAINS

  ! Function(s) for individual elements

  SUBROUTINE flip(this)
    CLASS(ising_element) :: this
    this%state = - this%state
    this%el_flips = this%el_flips + 1
  END SUBROUTINE


  ! Functions for the grid

  SUBROUTINE allocate_grid(this, N)
    CLASS(ising_grid) :: this
    INTEGER :: N, i, j

    this%sz = N

    ! This time, I have the sized domain and limit the neighbours
    ALLOCATE(this%state(1:N, 1:N))
    ALLOCATE(this%elements(1:N, 1:N))

    ! Elements aren't ready yet!!


    ! All of the complexity is now here in the setup, and the iterate routine
    ! Doesn't need to know how we handle boundaries, or how many neighbours we have
    DO i = 1, N
      DO j = 1, N
        ! Do 4 neighbours in a + config. Could do other things
        ! But at the boundaries, only include the cells which exist
        ! We could also do periodic then all cells have exactly 4
        ! Note we could include more distance neignbours, diagonals etc
        this%elements(i,j)%state => this%state(i,j)

        IF ( (i == 1 .OR. i ==N) .AND. (j ==1 .OR. j == N)) THEN
          ! Corners:
          ALLOCATE(this%elements(i,j)%neighbours(2))
          IF(i == 1) THEN
            this%elements(i,j)%neighbours(1)%ptr => this%state(2, j)
          ELSE IF (i == N) THEN
            this%elements(i,j)%neighbours(1)%ptr => this%state(N-1, j)
          END IF
          IF (j == 1) THEN
            this%elements(i,j)%neighbours(2)%ptr => this%state(i, 2)
          ELSE IF (j == N) THEN
            this%elements(i,j)%neighbours(2)%ptr => this%state(i, N-1)
          END IF

        ELSE IF(i == 1 .OR. i ==N) THEN
          ! Non-corner Edges - i
          ALLOCATE(this%elements(i,j)%neighbours(3))
          IF(i == 1) THEN
            this%elements(i,j)%neighbours(1)%ptr => this%state(2, j)
          ELSE IF (i == N) THEN
            this%elements(i,j)%neighbours(1)%ptr => this%state(N-1, j)
          END IF
          this%elements(i,j)%neighbours(2)%ptr => this%state(i, j-1)
          this%elements(i,j)%neighbours(3)%ptr => this%state(i, j+1)

        ELSE IF(j ==1 .OR. j == N) THEN
          ! Non-corner Edges - j
          ALLOCATE(this%elements(i,j)%neighbours(3))
          IF(j == 1) THEN
            this%elements(i,j)%neighbours(1)%ptr => this%state(i, 2)
          ELSE IF (j == N) THEN
            this%elements(i,j)%neighbours(1)%ptr => this%state(i,N-1)
          END IF
          this%elements(i,j)%neighbours(2)%ptr => this%state(i-1, j)
          this%elements(i,j)%neighbours(3)%ptr => this%state(i+1, j)

        ELSE
          ALLOCATE(this%elements(i,j)%neighbours(4))
          this%elements(i,j)%neighbours(1)%ptr => this%state(i-1, j)
          this%elements(i,j)%neighbours(2)%ptr => this%state(i+1, j)
          this%elements(i,j)%neighbours(3)%ptr => this%state(i  , j-1)
          this%elements(i,j)%neighbours(4)%ptr => this%state(i  , j+1)
        END IF
      END DO
    END DO

  END SUBROUTINE allocate_grid

  SUBROUTINE destroy_grid(this)
    ! For finalizer, this arg MUST be TYPE (not CLASS)
    TYPE(ising_grid) :: this

    DEALLOCATE(this%state, this%elements)
    this%sz = 0

  END SUBROUTINE


  ! This function just wraps around the original setup functions
  ! passing the grid through
  SUBROUTINE setup_grid(this, N,  init, beta, J)
    CLASS(ising_grid) :: this
    CHARACTER(LEN=1) :: init
    INTEGER :: N
    REAL(KIND=REAL64) :: beta, J

    CALL allocate_grid(this, N)

    this%beta = beta
    this%J = J

    IF(init == "F" .OR. init == "f") THEN
      ! We can set everything up '+1'
      this%state = 1
    ELSE IF(init == "A" .OR. init == "a") THEN
      ! This will set alternating
      CALL setup_alternating_spin_grid(this%state)
    ELSE
      ! This will set random spins
      CALL setup_random_spin_grid(this%state)
    END IF

  END SUBROUTINE setup_grid

  ! This is not changed a lot from the iterate function in IsingSpin

  SUBROUTINE iterate(this, max_iters)

    CLASS(ising_grid) :: this
    TYPE(ising_element), POINTER :: test_element
    INTEGER(KIND=INT32) :: iter, max_iters, n_neighbour
    REAL(KIND=REAL64) :: rand, prob, delta_E
    INTEGER :: rand_x, rand_y

    !Iterate random flips
    DO iter = 1, max_iters

      ! Pick random pos'n
      rand_x = 1+INT(random()*(this%sz))
      rand_y = 1+INT(random()*(this%sz))

     ! For clarity of the rest of the code, use a temporary pointer
      test_element => this%elements(rand_x, rand_y)

PRINT*, rand_x, rand_y, SIZE(test_element%neighbours), test_element%state
 
      ! Calc energy change
      delta_E = 0.0
      ! Now we just sum over the neighbours, without worrying where they are
      DO n_neighbour = 1, SIZE(test_element%neighbours)
        delta_E = delta_E + this%J * test_element%neighbours(n_neighbour)%ptr * &
            test_element%state
      END DO

      ! Check for acceptance and perform flip if necessary
      IF (delta_E .LT. 0) THEN
        CALL test_element%flip()
      ELSE
        rand = random()
        prob = EXP(- this%beta * delta_E)
        IF (rand .LT. prob) THEN
          CALL test_element%flip()
        END IF
      END IF

    END DO

  END SUBROUTINE

  ! Calculate total flips
  FUNCTION flips(this)

    CLASS(ising_grid) :: this
    INTEGER :: flips

    flips = SUM(this%elements%el_flips)

  END FUNCTION flips



  ! Calculate magnetization
  FUNCTION magnetization(this)

    CLASS(ising_grid) :: this
    REAL(KIND=REAL64) :: magnetization

    magnetization = SUM(this%elements%magnetization_el())


  END FUNCTION magnetization

  ! Calculate magnetization for a single element
  ! ELEMENTAL means the function can be applied element-wise to an array
  PURE ELEMENTAL FUNCTION magnetization_el(this)

    ! We have to specify intent for elemental function
    CLASS(ising_element), INTENT(IN) :: this
    REAL(KIND=REAL64) :: magnetization_el

    ! Suppose we could have a more complex calculation here
    magnetization_el = this%state

  END FUNCTION magnetization_el



!-----------------------These are the original functions from IsingSpin ---------------
  SUBROUTINE setup_random_spin_grid(grid)

    INTEGER(KIND=INT16), DIMENSION(:,:), INTENT(INOUT) :: grid
    INTEGER, DIMENSION(2) :: sz
    INTEGER(KIND=INT32) :: i, j
    REAL(KIND=REAL64) :: rand

    sz = SHAPE(grid)
    grid = 0

    ! Setup random spins
    DO j = 1, sz(2)
      DO i = 1, sz(1)
        rand = random()*2.0
        ! note our choice of Int16 gets a bit annoying because
        ! literals default to INT32 so we have to add explicit conversions
        grid(i, j) = INT(2*INT(rand) - 1, KIND=INT16)
      END DO
    END DO

  END SUBROUTINE setup_random_spin_grid


  SUBROUTINE setup_alternating_spin_grid(grid)

    INTEGER(KIND=INT16), DIMENSION(:,:), INTENT(INOUT) :: grid
    INTEGER, DIMENSION(2) :: sz
    INTEGER(KIND=INT32) :: i, j, offset

    sz = SHAPE(grid)

    ! Setup alternating spins:
    ! Set all to 1, then reverse every second
    grid = 1
    DO j = 1, sz(2)
      ! This makes sure rows alternate first and second cell flipped
      offset = 0
      IF(MOD(j, 2) .EQ. 1) offset = 1
      DO i = offset+1, sz(1), 2
        grid(i, j) = -1
      END DO
    END DO

  END SUBROUTINE setup_alternating_spin_grid


  SUBROUTINE iterate_ising_grid(grid, beta, J, max_iters, flips)

    INTEGER(KIND=INT16), DIMENSION(:,:), INTENT(INOUT) :: grid

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
      rand_x = 1+INT(random()*(sz(1)))
      rand_y = 1+INT(random()*(sz(2)))

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
  INTEGER(KIND=INT32) :: total_flips
  REAL(KIND=REAL64) :: beta, J, init_mag, final_mag
  TYPE(ising_grid) :: grid
  LOGICAL :: success
  CHARACTER :: init

  ! Get command line arguments, and if N or T are given, use them
  CALL parse_args
  ! If not given, we should use sensible defaults for the grid size
  ! and number of iterations

  ! Best and simplest way to use cmd line module
  success = get_arg("max_iters", max_iters)
  IF( .NOT. success) THEN
    max_iters = 100
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

  ! I use a single-character flag for the initial state,
  ! either 'R' for random, 'A' for alternating or 'F' for flat (=1)
  ! and I default to random if not given or not one of the options
  success = get_arg("init", init)
  IF( .NOT. success) THEN
    init = 'r'
  END IF

  CALL grid%setup_grid(N, init, beta, J)

  init_mag = grid%magnetization()

  !I chose to show the initial state, and wait for Enter to run
  CALL display_3val_array(grid%state, .TRUE.)
  CALL wait_for_enter_key

  !Run the iterations and then display again
  CALL grid%iterate(max_iters)
  CALL display_3val_array(grid%state, .TRUE.)

  total_flips = grid%flips()
  PRINT*,'Flipped ', total_flips, ' states (', REAL(total_flips)/REAL(max_iters)*100, '%)'

  final_mag = grid%magnetization()
  PRINT*, "Initial magnetization", init_mag
  PRINT*, "Final magnetization  ", final_mag, "(", (final_mag/init_mag - 1.0)*100.0, "%)"

END PROGRAM
