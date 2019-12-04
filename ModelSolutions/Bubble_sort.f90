
PROGRAM main

  ! Number of items in the list (fixed)
  INTEGER, PARAMETER :: n = 10000
  ! I use a compile-time fixed array here. Could also use an ALLOCATABLE
  INTEGER, DIMENSION(n) :: dat
  INTEGER :: i, tmp_dat
  ! I use two flags for clarity. This is not necessary
  LOGICAL :: sorted, swapped


  ! Create reverse ordered data. This is the worst case for Bubble Sort
  ! and should take time proportional to n^2
  ! If I create ordered data, I should only take time proportional to n
  DO i = 1, n
    dat(i) = n-i + 1
  END DO

  ! Start assuming not sorted. For Bubble sort, the first pass through the data
  ! will show if it was already sorted, so we don't need to check
  sorted = .FALSE.

  DO WHILE (.NOT. sorted)
    ! swapped tracks if we swapped any items this time
    swapped = .FALSE.
    ! Pairwise comparision, so run to n-1
    DO i = 1, n-1
      ! If items are misordered:
      IF(dat(i) > dat(i+1)) THEN
        ! Exchange values using a temporary
        tmp_dat = dat(i+1)
        dat(i+1) = dat(i)
        dat(i) = tmp_dat
        swapped = .TRUE.
      END IF
    END DO
    ! When we made no swaps we know array is sorted!
    ! Don't need to do this, but it's clearer to me than
    ! setting sorted to .TRUE. on each go around
    sorted = .NOT. swapped
  END DO

  ! Print the sorted data
  PRINT*, dat

END PROGRAM
