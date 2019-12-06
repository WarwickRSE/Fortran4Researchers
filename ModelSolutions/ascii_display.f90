  !> @brief Ascii-art display of arrays
  !>
  !> Allows simple display of a Logical or
  !> a 3-valued array. These functions are tailored to
  !> the needs of the assignments, so are not general purpose
  !> @author CS Brady @author H Ratcliffe


MODULE ascii_display


  USE kinds

  IMPLICIT NONE

CONTAINS

  SUBROUTINE wait_for_enter_key()

    PRINT*, "Press Enter to continue"
    READ(*, *)

  END SUBROUTINE wait_for_enter_key

  !> @brief Ascii-art display of Logical array
  !>
  !> Allows simple display of a Logical array, with a title. Each
  !> call will completely clear the terminal display. Optionally you may provide
  !> a title string to be displayed above the array. You may also provide a
  !> specific position to highlight, by using different symbols
  !> @param larr 2-D array of type Logical
  !> @param show_borders Whether to display borders around the array
  !> @param highlight_position Use special symbols for the specified position
  !> @param title Title string for array
  !>
  !> @author CS Brady

  SUBROUTINE display_array(larr, show_borders, highlight_position, title)

    LOGICAL, INTENT(IN), DIMENSION(:,:) :: larr
    LOGICAL, INTENT(IN), OPTIONAL :: show_borders
    INTEGER, DIMENSION(2), INTENT(IN), OPTIONAL :: highlight_position
    CHARACTER(LEN=*), OPTIONAL :: title
    LOGICAL :: borders
    INTEGER, DIMENSION(2) :: sizes, special_position
    INTEGER :: ix, iy
    CHARACTER(LEN=1) :: c
    CHARACTER(LEN=4), PARAMETER :: clrstr = CHAR(27)//'[2J'

    borders = .TRUE.
    IF (PRESENT(show_borders)) borders = show_borders

    special_position = (/0, 0/)
    IF (PRESENT(highlight_position)) special_position = highlight_position

    WRITE(*,'(A)') clrstr

    sizes = SHAPE(larr)
    IF (PRESENT(title)) THEN
      WRITE(*, '(A)') title
    END IF

    IF (borders) WRITE(*, '(A)') REPEAT('=', sizes(1)+2)
    DO iy = sizes(2), 1, -1
      IF (borders) WRITE(*, '(A)', ADVANCE='NO') '|'
      DO ix = 1, sizes(1)
        IF (larr(ix,iy)) THEN
          IF (ix .EQ. special_position(1) &
              .AND. iy .EQ. special_position(2)) THEN
            c = '8'
          ELSE
            c = '#'
          END IF
        ELSE
          IF (ix .EQ. special_position(1) &
              .AND. iy .EQ. special_position(2)) THEN
            c = 'O'
          ELSE
            c = ' '
          END IF
        END IF
        WRITE(*, '(A)', ADVANCE='NO') c
      END DO
      IF (borders) WRITE(*, '(A)', ADVANCE='NO') '|'
      WRITE(*, '(A)') ''
    END DO
    IF (borders) WRITE(*, '(A)') REPEAT('=', sizes(1)+2)

  END SUBROUTINE display_array

  !> @brief Ascii-art display of 3-valued array
  !>
  !> Allows simple display of a three-valued array, either +ve, -ve or 0
  !> with a title. Each
  !> call will completely clear the terminal display. Optionally you may provide
  !> a title string to be displayed above the array. You may also provide a
  !> specific position to highlight, by using different symbols
  !> @param arr 2-D array of type Int16
  !> @param show_borders Whether to display borders around the array
  !> @param title Title string for array
  !>
  !> @author H Ratcliffe

  SUBROUTINE display_3val_array(arr, show_borders, title)

    INTEGER(KIND=INT16), INTENT(IN), DIMENSION(:,:) :: arr
    LOGICAL, INTENT(IN), OPTIONAL :: show_borders
    CHARACTER(LEN=*), OPTIONAL :: title
    LOGICAL :: borders
    INTEGER, DIMENSION(2) :: sizes
    INTEGER :: ix, iy
    CHARACTER(LEN=1) :: c
    CHARACTER(LEN=4), PARAMETER :: clrstr = CHAR(27)//'[2J'

    borders = .TRUE.
    IF (PRESENT(show_borders)) borders = show_borders

    WRITE(*,'(A)') clrstr

    sizes = SHAPE(arr)
    IF (PRESENT(title)) THEN
      WRITE(*, '(A)') title
    END IF

    IF (borders) WRITE(*, '(A)') REPEAT('=', sizes(1)+2)
    DO iy = sizes(2), 1, -1
      IF (borders) WRITE(*, '(A)', ADVANCE='NO') '|'
      DO ix = 1, sizes(1)
        IF (arr(ix,iy) .GT. 0) THEN
          c = '+'
        ELSE IF (arr(ix,iy) .LT. 0) THEN
          c = '-'
        ELSE
          c = ' '
        END IF
        WRITE(*, '(A)', ADVANCE='NO') c
      END DO
      IF (borders) WRITE(*, '(A)', ADVANCE='NO') '|'
      WRITE(*, '(A)') ''
    END DO
    IF (borders) WRITE(*, '(A)') REPEAT('=', sizes(1)+2)

  END SUBROUTINE display_3val_array


END MODULE ascii_display
