PROGRAM main

  IMPLICIT NONE

  REAL :: sd1, sd2, av, scale_fac
  REAL :: x_min, x_max, dx, x
  REAL :: sum_of_diffs, sum_of_vals, sum_of_sqs
  REAL :: offset
  INTEGER :: i, n_bins

  n_bins = 1000

  ! Using a variable for this is clearer, and slightly more efficient
  scale_fac = 2.0 * 3.14159
  ! I use a variable here too for clarity and ease of changing
  offset = 1000.0


  ! We can make the x-axis several ways.
  ! Finding dx and incrementing x in the loop is one.
  ! Another is to use the loop index itself and do  e.g. REAL(i)/REAL(n_bins-1)

  x_min = 0.0
  x_max = 2.0
  dx = (x_max - x_min) / REAL(n_bins + 1)


  ! Initialise all the running sums to 0
  sum_of_diffs = 0.0
  sum_of_vals = 0.0
  sum_of_sqs = 0.0

  x = x_min

  !Calculate average, and 1-pass SD running vals
  DO i = 1, n_bins

    sum_of_vals = sum_of_vals + (offset + sin(scale_fac * x))

    sum_of_sqs = sum_of_sqs + (offset + sin(scale_fac * x))**2

    x = x + dx

  END DO

  !Finish calc of average as it is needed for the 2-pass SD
  av = sum_of_vals / REAL(n_bins)


  x = x_min

  ! Calculate 2-pass SD running val
  DO i = 1, n_bins

    sum_of_diffs = sum_of_diffs + (offset + sin(scale_fac * x) - av)**2

    x = x + dx

  END DO

  !Finish calcs and print

  sd2 = sqrt(1.0/REAL(n_bins -1) * sum_of_diffs)

  sd1 = sqrt(1.0/REAL(n_bins -1) * (sum_of_sqs - REAL(n_bins) * av*av))

  PRINT'(A, I5, A, F8.3, A, F8.3)', "Number of bins: ",  n_bins, " Offset: ", &
       offset, " Average Value: ", av
  PRINT'(A, F8.3, A, F8.3)', "Two-pass SD: ", sd2, " One-pass SD: ", sd1


END PROGRAM
