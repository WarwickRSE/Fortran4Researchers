  !> @brief Functions to parse command line arguments
  !>
  !> Module to read command line arguments to a program
  !> We assume they are of the form name=value. There must
  !> NOT be spaces around the '=' sign!
  !> Value can be extracted as a string, a long-integer
  !> or a double-precision real.
  !> Argument names are limited to 20 chars, and values
  !> to 30 chars as read.
  !>
  !> Note that the only functions you should call from outside are
  !> parse_args, get_arg and get_arg_value
  !> A complete example code is:
  ! @snippet command_line_snippet.f90 Cmd eg
  !> @include command_line_snippet.f90

  !> @author H Ratcliffe

MODULE command_line

  USE kinds

  IMPLICIT NONE

  !> Length of character value string
  INTEGER, PARAMETER :: len = 30

  !> Type containing a key-value pair
  ! Init. to default values
  TYPE cmd_arg
    CHARACTER(LEN=20) :: name = "NULL"
    CHARACTER(LEN=len) :: value = ""
  END TYPE

  ! Interface dispatches to the correct actual function
  ! based on arguments in call
  ! This means we can convert to a requested type
  ! We call like e.g.: get_arg(10, var) or
  ! get_arg("name", var) and we will try and
  ! interpret the value as whatever type var is
  ! We can supply optional arg to those
  ! to capture whether or not the name exists
  ! Note that if a name was present, but the value could
  ! not be parsed, exists will be TRUE
  ! but correct will be FALSE
  !> @brief Read arguments by name or number
  !>
  !> All of the members of this interface take either a 'name' or a 'num'
  !>  parameter, detailling which argument to look up and
  !> the type of the 'val' argument dictates the type
  !> to attempt parsing the argument as
  !
  !> @param name Name to look up (if used)
  !> @param num Arg. number to look up (if used)
  !> @param val Value to read into
  !> @param exists Whether the name was found
  !> @return True if the name is found and parsed, False otherwise
  INTERFACE get_arg
    MODULE PROCEDURE get_arg_num_int, get_arg_name_int
    MODULE PROCEDURE get_arg_num_dbl, get_arg_name_dbl
    MODULE PROCEDURE get_arg_num_str, get_arg_name_str
  END INTERFACE

  ! Module level variables - these are accessible
  ! to all functions in the module
  ! but we make them private to within the module only
  !> The argument list
  TYPE(cmd_arg), DIMENSION(:), ALLOCATABLE :: all_args
  !> The number of arguments
  INTEGER :: num_args = 0
  PRIVATE :: all_args, num_args

  CONTAINS

  !> @brief Parse out command line args
  ! This function can be called multiple times
  ! and will freshly parse ALL arguments each time
  ! We assume these are entered as 'name=value' and
  ! if there is no '=' sign, then value is set to a sentinel
  ! For most flexibility, we assume all values are reals, and
  ! thus we use 'HUGE(1.0)' as the no-value sentinel. This number
  ! is greater than any other real value
  SUBROUTINE parse_args()
    !Grab all command line args and store

    ! Strictly we can't be sure 50 chars is enough
    ! but for command-line args it's enough if we're sensible
    ! We wont overflow, but our strings may get truncated
    CHARACTER(LEN=51) :: arg
    INTEGER :: i, indx

    num_args = COMMAND_ARGUMENT_COUNT()
    IF(num_args > 0) THEN

      ! If this is not the first call, all_args may be already allocated
      ! Deallocate if needed, and allocate to correct size
      IF(ALLOCATED(all_args)) DEALLOCATE(all_args)
      ALLOCATE(all_args(num_args))

      ! Loop over all arguments
      DO i = 1, num_args
        CALL GET_COMMAND_ARGUMENT(i, arg)
        ! Location of the '=' sign
        ! If not found, return value is 0
        indx = INDEX(arg, '=')
        IF(indx > 1) THEN
          ! All characters up to '=', not including it
          ! but with any leading spaces removed
          all_args(i)%name = ADJUSTL(arg(1:indx-1))
          ! All characters after '='
          all_args(i)%value= ADJUSTL(arg(indx+1:))
       ELSE
          all_args(i)%name = TRIM(ADJUSTL(arg))
          ! Value already has a default value, so leave it alone
        END IF
      END DO
    ENDIF

  END SUBROUTINE parse_args

!------------------------------------------------------------------
  ! The next functions let you access the parsed args
  ! You need to have called parse_args first or
  ! there wont be any args!
  ! You don't need to call these explicitly, you should
  ! just use get_arg

  !> @brief Read by number for double precision values
  !> @param num Argument number to read
  !> @param val Value to read into
  !> @param exists Whether the name was found
  !> @return True if the name is found and parsed, False otherwise
  FUNCTION get_arg_num_dbl(num, val, exists)

    LOGICAL :: get_arg_num_dbl
    INTEGER, INTENT(IN) :: num
    REAL(KIND=REAL64), INTENT(OUT) :: val
    LOGICAL, INTENT(OUT), OPTIONAL :: exists
    LOGICAL :: found
    INTEGER :: ierr

    found = .FALSE.
    ! Check requested number is in range
    IF(num <= num_args .AND. num > 0) THEN
      ! READ it from string into value
      ! We don't need to specify the format in general
      READ(all_args(num)%value, *, IOSTAT=ierr) val
      found = .TRUE.
    END IF

    IF(PRESENT(exists)) THEN
      exists = found
    END IF

    ! Return value is whether value is found and correctly parsed
    get_arg_num_dbl = (found .AND. (ierr == 0))

  END FUNCTION get_arg_num_dbl

  !> @brief Read by name for double precision values
  !> @param name Argument name to look up
  !> @param val Value to read into
  !> @param exists Whether the name was found
  !> @return True if the name is found and parsed, False otherwise
  FUNCTION get_arg_name_dbl(name, val, exists)

    LOGICAL :: get_arg_name_dbl
    CHARACTER(LEN=*), INTENT(IN) :: name
    REAL(KIND=REAL64), INTENT(OUT) :: val
    INTEGER :: i
    LOGICAL, INTENT(OUT), OPTIONAL :: exists
    LOGICAL :: found
    INTEGER :: ierr

    found = .FALSE.
    ! Our cmd_arg type is already initialised to the sentinel
    DO i = 1, num_args
      IF(all_args(i)%name == TRIM(ADJUSTL(name))) THEN
        found = .TRUE.
        READ(all_args(i)%value, *, IOSTAT=ierr) val
        EXIT
      END IF
    END DO

    IF(PRESENT(exists)) THEN
      exists = found
    END IF

    ! Return value is whether value is found and correctly parsed
    get_arg_name_dbl = (found .AND. (ierr == 0))

  END FUNCTION get_arg_name_dbl

  !> @brief Read by number for long integer values
  !> @param num Argument number to read
  !> @param val Value to read into
  !> @param exists Whether the name was found
  !> @return True if the name is found and parsed, False otherwise
  FUNCTION get_arg_num_int(num, val, exists)

    LOGICAL :: get_arg_num_int
    INTEGER, INTENT(IN) :: num
    INTEGER(KIND=INT32), INTENT(OUT) :: val
    LOGICAL, INTENT(OUT), OPTIONAL :: exists
    LOGICAL :: found
    INTEGER :: ierr

    found = .FALSE.
    ! Check requested number is in range
    IF(num <= num_args .AND. num > 0) THEN
      ! READ it from string into value
      ! We don't need to specify the format in general
      READ(all_args(num)%value, *, IOSTAT=ierr) val
      found = .TRUE.
    END IF

    IF(PRESENT(exists)) THEN
      exists = found
    END IF

    ! Return value is whether value is found and correctly parsed
    get_arg_num_int = (found .AND. (ierr == 0))

  END FUNCTION get_arg_num_int

  !> @brief Read by name for long integer values
  !> @param name Argument name to look up
  !> @param val Value to read into
  !> @param exists Whether the name was found
  !> @return True if the name is found and parsed, False otherwise
  FUNCTION get_arg_name_int(name, val, exists)

    LOGICAL :: get_arg_name_int
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER(KIND=INT32), INTENT(OUT) :: val
    INTEGER :: i
    LOGICAL, INTENT(OUT), OPTIONAL :: exists
    LOGICAL :: found
    INTEGER :: ierr

    found = .FALSE.
    ! Our cmd_arg type is already initialised to the sentinel
    DO i = 1, num_args
      IF(all_args(i)%name == TRIM(ADJUSTL(name))) THEN
        found = .TRUE.
        READ(all_args(i)%value, *, IOSTAT=ierr) val
        EXIT
      END IF
    END DO

    IF(PRESENT(exists)) THEN
      exists = found
    END IF

    ! Return value is whether value is found and correctly parsed
    get_arg_name_int = (found .AND. (ierr == 0))

  END FUNCTION get_arg_name_int

  !> @brief Read by number for string/character values
  !> @param num Argument number to read
  !> @param val Value to read into
  !> @param exists Whether the name was found - this is already contained in
  !> the return value, but is given for consistency with the other members
  !> @return True if the name is found and parsed, False otherwise
  FUNCTION get_arg_num_str(num, val, exists)

    LOGICAL :: get_arg_num_str
    INTEGER, INTENT(IN) :: num
    CHARACTER(LEN=*), INTENT(OUT) :: val
    LOGICAL, INTENT(OUT), OPTIONAL :: exists
    LOGICAL :: found

    found = .FALSE.
    ! Check requested number is in range
    IF(num <= num_args .AND. num > 0) THEN
      ! READ it from string into value
      ! We don't need to specify the format in general
      val = all_args(num)%value
      found = .TRUE.
    END IF

    IF(PRESENT(exists)) THEN
      exists = found
    END IF

    ! Return value is whether value is found and correctly parsed
    get_arg_num_str = found

  END FUNCTION get_arg_num_str

  !> @brief Read by name for string values
  !> @param name Argument name to look up
  !> @param val Value to read into
  !> @param exists Whether the name was found - this is already contained in
  !> the return value, but is given for consistency with the other members
  !> @return True if the name is found and parsed, False otherwise
  FUNCTION get_arg_name_str(name, val, exists)

    LOGICAL :: get_arg_name_str
    CHARACTER(LEN=*), INTENT(IN) :: name
    CHARACTER(LEN=*), INTENT(OUT) :: val
    INTEGER :: i
    LOGICAL, INTENT(OUT), OPTIONAL :: exists
    LOGICAL :: found

    found = .FALSE.
    ! Our cmd_arg type is already initialised to the sentinel
    DO i = 1, num_args
      IF(all_args(i)%name == TRIM(ADJUSTL(name))) THEN
        found = .TRUE.
        val = all_args(i)%value
        EXIT
      END IF
    END DO

    IF(PRESENT(exists)) THEN
      exists = found
    END IF

    ! Return value is whether value is found and correctly parsed
    get_arg_name_str = found

  END FUNCTION get_arg_name_str


!--------------------------------------------------------------------

  ! These lets you get just the string value from an argument by name
  ! you still need to have called parse_args first!
  !> @brief Lookup an argument by name and return the value as a string
  !> @param name Argument name to look up
  !> @param exists Whether the name was found
  !> @return The string value associated with the given name
  FUNCTION get_arg_value(name, exists)

    CHARACTER(LEN=len) :: get_arg_value
    CHARACTER(LEN=*), INTENT(IN) :: name
    LOGICAL, INTENT(OUT), OPTIONAL :: exists
    TYPE(cmd_arg) :: tmp
    INTEGER :: i
    LOGICAL :: found

    ! Initialise to the default value
    ! Use a temporary to get the default values
    ! This makes sure we match the expected sentinel
    get_arg_value = tmp%value
    found = .FALSE.

    DO i = 1, num_args
      IF(all_args(i)%name .EQ. TRIM(ADJUSTL(name))) THEN
        get_arg_value = all_args(i)%value
        found = .TRUE.
        EXIT
      END IF
    END DO

    IF(PRESENT(exists)) THEN
      exists = found
    END IF

  END FUNCTION get_arg_value


END MODULE command_line
